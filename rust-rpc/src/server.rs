
// Copyright 2018 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

use clap::{App, Arg};
use futures::{future, prelude::*};
use service::World;
use std::{
    io,
    net::{IpAddr, SocketAddr},
};
use tarpc::{
    context,
    server::{self, Channel, Handler},
    tokio_serde::formats::Json,
};

use lazy_static::lazy_static;
use std::sync::Mutex;
use std::collections::HashMap;
use std::collections::BTreeSet;


lazy_static! {
    static ref GRAPH: Mutex<HashMap<String, Graph>> = Mutex::new(HashMap::new());
}

struct DSU {
    num_components: usize,
    parents: Vec<usize>,
    sizes: Vec<usize>
}

impl DSU {
    fn new(n: usize) -> Self {
        Self {
            num_components: n,
            parents: (0..n).collect(),
            sizes: vec![1; n]
        }
    }

    fn find(&self, x: usize) -> usize {
        if self.parents[x] == x {
            x
        } else {
            self.find(self.parents[x])
        }
    }

    fn merge(&mut self, u: usize, v: usize) -> bool {
        let mut node_u = self.find(u);
        let mut node_v = self.find(v);
        if self.sizes[v] < self.sizes[u] {
            let node_t = node_u;
            node_u = node_v;
            node_v = node_t;
        }
        if node_u == node_v {
            false
        } else {
            self.num_components -= 1;
            self.parents[node_u] = node_v;
            self.sizes[node_v] += self.sizes[node_u];
            self.sizes[node_u] = 0;
            true
        }
    }
}

struct Graph {
    n: usize,
    edges: BTreeSet<(usize, usize, usize)>
}

impl Graph {
    fn new(num_nodes: usize) -> Graph {
        Graph {
            n: num_nodes,
            edges: BTreeSet::new(),
        }
    }

    fn add_edge(&mut self, u: usize, v: usize, w: usize) {
        self.edges.insert((w, u - 1, v - 1));
    }

    fn details(&self) -> String {
        format!("Graph has {:?} nodes with {:?} as it's edges.", self.n, self.edges)
    }

    fn get_mst(&self) -> usize {
        let mut dsu: DSU = DSU::new(self.n);
        let mut ans: usize = 0;
        for &(w, u, v) in self.edges.iter() {
            if dsu.merge(u, v) {
                ans += w
            }
        }
        if dsu.num_components == 1 { ans } else { usize::max_value() }
    }
}

// This is the type that implements the generated World trait. It is the business logic
// and is used to start the server.
#[derive(Clone)]
struct HelloServer(SocketAddr);


#[tarpc::server]
impl World for HelloServer {
    async fn hello(self, _: context::Context, name: String) -> String {
        format!("Hello, {}! You are connected from {:?}.", name, self.0)
    }
    async fn new_graph(self, _: context::Context, name: String, num_nodes: usize) {
        GRAPH.lock().unwrap().insert(name, Graph::new(num_nodes));
    }
    async fn add_edge(self, _: context::Context, name: String, u: usize, v: usize, w: usize) {
        match GRAPH.lock().unwrap().get_mut(&name) {
            Some(graph) => graph.add_edge(u, v, w),
            _ => (),
        };
    }
    async fn get_details(self, _: context::Context, name: String) -> String {
        match GRAPH.lock().unwrap().get_mut(&name) {
            Some(graph) => graph.details(),
            _ => "Graph not found".to_string(),
        }
    }
    async fn get_mst(self, _: context::Context, name: String) -> usize {
        let weight;
        match GRAPH.lock().unwrap().get(&name) {
            Some(graph) => weight = graph.get_mst(),
            _ => weight = usize::max_value(),
        }
        weight
    }
}

#[tokio::main]
async fn main() -> io::Result<()> {
    env_logger::init();

    let flags = App::new("Graph Remote Calls")
        .version("0.1")
        .author("Animesh Sinha <animesh.sinha@research.iiit.ac.in>")
        .about("Implements Prims MST as a remote procedure call.")
        .arg(
            Arg::with_name("port")
                .short("p")
                .long("port")
                .value_name("NUMBER")
                .help("Sets the port number to listen on")
                .required(true)
                .takes_value(true),
        )
        .get_matches();

    let port = flags.value_of("port").unwrap();
    let port = port
        .parse()
        .unwrap_or_else(|e| panic!(r#"--port value "{}" invalid: {}"#, port, e));

    let server_addr = (IpAddr::from([0, 0, 0, 0]), port);

    // JSON transport is provided by the json_transport tarpc module. It makes it easy
    // to start up a serde-powered json serialization strategy over TCP.
    let mut listener = tarpc::serde_transport::tcp::listen(&server_addr, Json::default).await?;
    listener.config_mut().max_frame_length(4294967296);
    listener
        // Ignore accept errors.
        .filter_map(|r| future::ready(r.ok()))
        .map(server::BaseChannel::with_defaults)
        // Limit channels to 1 per IP.
        .max_channels_per_key(1, |t| t.as_ref().peer_addr().unwrap().ip())
        // serve is generated by the service attribute. It takes as input any type implementing
        // the generated World trait.
        .map(|channel| {
            let server = HelloServer(channel.as_ref().as_ref().peer_addr().unwrap());
            channel.respond_with(server.serve()).execute()
        })
        // Max 10 channels.
        .buffer_unordered(10)
        .for_each(|_| async {})
        .await;

    Ok(())
}
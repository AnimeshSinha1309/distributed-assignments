// Copyright 2018 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

/// This is the service definition. It looks a lot like a trait definition.
/// It defines one RPC, hello, which takes one arg, name, and returns a String.
#[tarpc::service]
pub trait World {
    /// Returns a greeting for name.
    async fn hello(name: String) -> String;
    async fn new_graph(name: String, num_nodes: usize);
    async fn add_edge(name: String, u: usize, v: usize, w: usize);
    async fn get_mst(name: String) -> usize;
    async fn get_details(name: String) -> String;
}

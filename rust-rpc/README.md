# Rust Remote Procedure Calls

## Notes on the Implementation

On the server side:
* We parse the port number using CLAP for arg-parsing
* We maintain a Graph Class to provide easy access to Prim's MST using rs-graph
* We wrap a global GRAPH object, which is a hashmap of all graphs in a lazy-static
* We add basic api endpoints as functions in implementation of the World trait
* The World trait is bound to the tarpc service

On the client side:
* We parse the server address using CLAP for arg-parsing
* Connect to the server over a TCP channel
* We start a REPL loop
* We keep calling the stub functions over the Tarpc service


## Running the Code

Ensure that you have rust installed. If not, the following command will setup Rust and it's package manager Cargo for you and add them to your path.

`curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`


On the server side, run the following command:

`cargo run --bin server -- --port 8000`

On another terminal, for the client, run the following command:

`cargo run --bin client -- --server_addr 127.0.0.1:8000`

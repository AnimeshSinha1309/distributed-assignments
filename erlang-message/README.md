# Assignment 2: Distributed Programming in Erlang

## Problem Approaches

### Problem 1: Message Passing

This problem just involves the basic elements of code that we need in all programs:
* Reading from a file (Read Integers function takes each line to a list of ints)
* Writing to a file (Opening, Closing and appending to file object)
* Passing and Recieving messages
* Handling lists, Taking Head, Tail, Expansion and Concatenation of lists
* Spawning processing and thinking concurrently.

### Problem 2: Shortest Path

Here we implemented distributed Bellman Ford Algorithm. The first 100 odd lines simply implement a single process Bellman Ford ending in the run function, which runs Bellman ford on a single processor. Then we use these methods to add concurrency, finally making a function called main, which serves the entire purpose:

There is a master process and several slave processes. The tasks of the slave processes are as follows:
* We spawn processes and send them a process ID
* We send all of them a message of what edges are assigned to them
* Keep open a function endpoint that takes as input the Distance vector and outputs the relaxed values in that vector

It serves to maintain the independent processes as sorts of API requests that relax a subset of edges. Then we use this answer from our master process.
* Starts by starting the Slave Processes.
* Reads the input file.
* Gets an initial distance vector.
* Recursively calls a relaxer function n times.
* The relaxer function goes and gets each process to relax the edge weights onto the distance vector.
* Writes to the output file.

## Benefits of Distributed Programming with Erlang

### Why Functional?

We gave up Variables, Loops, etc. so that there is no Global state. The compiler is smart enough to parallelize all functions that do not depend on the output of each other. This is indeed a key realization to seeing why our code here scales better, since all of our requests come much faster than the relaxations.

### So did we achieve anything?

The idea of distributing to a pre-determined number of processes seems wrong. We spent a lot of effort, getting rid of all memory access, to ensure that processes are cheap, and each edge can have a process so that we can think concurrently. This project, given the value of P before hand and not allowing us to set it to the number of edges, seems to be an abuse of the idea of Thinking Concurrently.

## How to run

```shell
erlc 2018113001_1.erl
erlc 2018113001_2.erl
erl -noshell -s 2018113001_1 main q1.inp q1.out -s init stop
erl -noshell -s 2018113001_2 main q2.inp q2.out -s init stop
```
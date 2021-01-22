# Running the App

To run the app, ensure that MPI is installed. The type out the following commands:
`mpic++ qx.cpp -o qx.out`
`mpirun -np 4 qx.out qx.inp qx.ans`
The input should be provided in `qx.inp` and substitute the value of `x` with any of `1`, `2`, or `3` depending on which question to run.

# Idea behind the solutions

In all problems the root process will be called the master, and all processes including the master itself will be called slaves.

## Question 1

The idea is here trivial, and only an introduction to message passing:
* Make mater distribute the range of values on all the processors.
* Get the slaves to compute the value of the sum in given range.
* Return and sum the values on master.

## Question 2

Due to a small number of processes, we parralelly quicksort and then k-way merge:
* Input the array, and segment it in `n_processes` equal parts.
* Send all these to the slave processes.
* Quick sort on all the slave processes.
* Mail the sorted vectors back to master and assemble in 2D array.
* Perform k-way merge, maintaining pointers to all subarrays.
* Print the final answer on master.

## Question 3

Here we use Jones-Plassmann algorithm to parallelize the edge coloring algorithm:
* Compute the Line graph (edges to nodes).
* Randomly (or in our case serially) assign the number.
* Set the color vector to size of `num_edges` and value -1.
* Send copy of the graph and color vector on all nodes.
* For each node compute segments of the array they need to solve.
* The slaves now attempt to compute the color vector. Iteratively perform:
  - If any of the neighbors with a lower index are not assigned, don't assign color.
  - Otherwise assign the lowest free color amongst neighbors.
  - Send all the solved colored arrays from slaves to the master.
  - Make the master merge all the arrays and broadcast to all the slaves.
* When all colors are assigned, terminate the loop and print the answer.


/* MPI Program Template */

#include <fstream>
#include <iostream>
#include <string>

#include "mpi.h"
using namespace std;

typedef long long int ll;

#define PUSH_TASK 2001
#define MASTER_PROCESS 0

int main(int argc, char **argv) {
    int rank, numprocs;
    MPI_Status status;
    int start_val, end_val;

    /* start up MPI */
    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);

    /*synchronize all processes*/
    MPI_Barrier(MPI_COMM_WORLD);
    double tbeg = MPI_Wtime();

    /* write your code here */
    double answer = 0.0;

    if (rank == MASTER_PROCESS) {
        int n = 0;
        ifstream fin;
        fin.open(argv[1]);
        fin >> n;
        fin.close();

        /* distribute a portion of the bector to each child process */
        int NUMBERS_PER_PROCESS = (n + numprocs - 1) / numprocs;
        for (int proc_id = 0; proc_id < numprocs; proc_id++) {
            start_val = proc_id * NUMBERS_PER_PROCESS + 1;
            end_val = (proc_id + 1) * NUMBERS_PER_PROCESS;
            end_val = end_val > n ? n : end_val;

            MPI_Send(&start_val, 1, MPI_INT, proc_id, PUSH_TASK,
                     MPI_COMM_WORLD);
            MPI_Send(&end_val, 1, MPI_INT, proc_id, PUSH_TASK, MPI_COMM_WORLD);
        }
    }
    MPI_Recv(&start_val, 1, MPI_INT, MASTER_PROCESS, PUSH_TASK, MPI_COMM_WORLD,
             &status);
    MPI_Recv(&end_val, 1, MPI_INT, MASTER_PROCESS, PUSH_TASK, MPI_COMM_WORLD,
             &status);
    // printf("@DEBUG: %d got range(%d, %d).\n", rank, start_val, end_val);
    double sum = 0.0;
    for (int i = start_val; i <= end_val; i++) {
        sum += 1.0 / (i * i);
    }
    MPI_Reduce(&sum, &answer, 1, MPI_DOUBLE, MPI_SUM, MASTER_PROCESS,
               MPI_COMM_WORLD);

    /* Compute the time taken and print the answer */

    MPI_Barrier(MPI_COMM_WORLD);
    double elapsedTime = MPI_Wtime() - tbeg;
    double maxTime;
    MPI_Reduce(&elapsedTime, &maxTime, 1, MPI_DOUBLE, MPI_MAX, 0,
               MPI_COMM_WORLD);

    if (rank == MASTER_PROCESS) {
        ofstream fout;
        fout.open(argv[2]);
        fout << answer << "\n";
        cout << "Total time (s): " << maxTime << endl;
        fout.close();
    }

    /* shut down MPI */
    MPI_Finalize();
    return 0;
}
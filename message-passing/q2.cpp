/* MPI Program Template */

#include <algorithm>
#include <cstdio>
#include <fstream>
#include <string>
#include <vector>

#include "mpi.h"

using namespace std;

#define PUSH_TASK 2001
#define MASTER_PROCESS 0

void send_vector(vector<int> v, int proc_id, int sz = -1, int st = -1) {
    int size = sz == -1 ? v.size() : sz;
    int *address = st == -1 ? &v[0] : &v[st];
    MPI_Send(&size, 1, MPI_INT, proc_id, PUSH_TASK, MPI_COMM_WORLD);
    MPI_Send(address, size, MPI_INT, proc_id, PUSH_TASK, MPI_COMM_WORLD);
}

vector<int> recv_vector(int proc_id) {
    MPI_Status status;
    vector<int> v;
    int size = 0;
    MPI_Recv(&size, 1, MPI_INT, proc_id, PUSH_TASK, MPI_COMM_WORLD, &status);
    v.resize(size);
    MPI_Recv(&v[0], size, MPI_INT, proc_id, PUSH_TASK, MPI_COMM_WORLD, &status);
    return v;
}

void quick_sort(vector<int> &v, int l, int r) {
    if (l < r) {
        int anchor = v[r - 1];
        int idx = l;
        for (int i = l; i < r; i++) {
            if (v[i] < anchor) {
                swap(v[i], v[idx]);
                idx++;
            }
        }
        swap(v[r - 1], v[idx]);
        quick_sort(v, l, idx);
        quick_sort(v, idx + 1, r);
    }
    return;
}

int main(int argc, char **argv) {
    int rank, numprocs;

    /* start up MPI */
    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);

    /*synchronize all processes*/
    MPI_Barrier(MPI_COMM_WORLD);
    double t_start = MPI_Wtime();
    double answer = 0.0;

    int n = 0;
    if (rank == MASTER_PROCESS) {
        /* take the inputs on master */
        ifstream fin;
        fin.open(argv[1]);
        fin >> n;
        vector<int> arr(n);
        for (auto &el : arr)
            fin >> el;
        fin.close();
        /* distribute a portion of the bector to each child process */
        int CHUNKSIZE = (n + numprocs - 1) / numprocs;
        for (int proc_id = 0; proc_id < numprocs; proc_id++) {
            int start_pos = proc_id * CHUNKSIZE;
            int size = start_pos + CHUNKSIZE <= n ? CHUNKSIZE : n - start_pos;
            send_vector(arr, proc_id, size, start_pos);
        }
    }

    /* sort the received the vector */
    vector<int> vec = recv_vector(MASTER_PROCESS);
    quick_sort(vec, 0, vec.size());
    send_vector(vec, MASTER_PROCESS);

    if (rank == MASTER_PROCESS) {
        /* get the vectors and merge them */
        vector<vector<int>> merger(numprocs);
        for (int proc_id = 0; proc_id < numprocs; proc_id++) {
            merger[proc_id] = recv_vector(proc_id);
            merger[proc_id].push_back(INT32_MAX);
        }
        vector<int> ans;
        vector<int> pointer(numprocs, 0);
        for (int i = 0; i < n; i++) {
            int pos = -1;
            for (int j = 0; j < numprocs; j++) {
                if (pos == -1 ||
                    merger[j][pointer[j]] < merger[pos][pointer[pos]])
                    pos = j;
            }
            ans.push_back(merger[pos][pointer[pos]]);
            pointer[pos]++;
        }

        ofstream fout;
        fout.open(argv[2]);
        for (int el : ans)
            fout << el << " ";
        fout << "\n";
        fout.close();
    }

    /* Compute the time taken and print the answer */
    MPI_Barrier(MPI_COMM_WORLD);
    double elapsedTime = MPI_Wtime() - t_start;
    double maxTime;
    MPI_Reduce(&elapsedTime, &maxTime, 1, MPI_DOUBLE, MPI_MAX, 0,
               MPI_COMM_WORLD);

    if (rank == MASTER_PROCESS) {
        cout << "Total time (s): " << maxTime << endl;
    }

    /* shut down MPI */
    MPI_Finalize();
    return 0;
}
/* MPI Program Template */

#include <cstdio>
#include <string>
#include <vector>
#include <algorithm>

#include "mpi.h"

using namespace std;

#define PUSH_TASK 2001
#define MASTER_PROCESS 0

template <typename Type>
istream &operator>>(istream &in, vector<Type> &vec) {
   int n = vec.size();
   for (int i = 0; i < n; i++)
       in >> vec[i];
   return in;
}
template <typename Type>
ostream &operator<<(ostream &out, vector<Type> &vec) {
    for (auto val : vec)
        out << val << " ";
    out << endl;
    return out;
}

void send_vector(vector<int> v, int proc_id, int sz=-1, int st=-1) {
    int size = sz == -1 ? v.size() : sz;
    int* address = st == -1 ? &v[0] : &v[st];
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

int main(int argc, char** argv) {
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
        cin >> n;
        vector<int> arr(n);
        for (auto &el : arr)
            cin >> el;
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
    sort(vec.begin(), vec.end());
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
                if (pos == -1 || merger[j][pointer[j]] < merger[pos][pointer[pos]])
                    pos = j;
            }
            ans.push_back(merger[pos][pointer[pos]]);
            pointer[pos]++;
        }

        cout << ans;
    }

    /* Compute the time taken and print the answer */
    MPI_Barrier(MPI_COMM_WORLD);
    double elapsedTime = MPI_Wtime() - t_start;
    double maxTime;
    MPI_Reduce(&elapsedTime, &maxTime, 1, MPI_DOUBLE, MPI_MAX, 0,
               MPI_COMM_WORLD);

    if (rank == MASTER_PROCESS) {
        printf("Total time (s): %f\n", maxTime);
    }

    /* shut down MPI */
    MPI_Finalize();
    return 0;
}
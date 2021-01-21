/* MPI Program Template */

#include <algorithm>
#include <cstdio>
#include <set>
#include <string>
#include <vector>

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

void send_vector(vector<int> v, int proc_id = -1, int sz = -1, int st = -1) {
    int size = sz == -1 ? v.size() : sz;
    int *address = st == -1 ? &v[0] : &v[st];
    MPI_Send(&size, 1, MPI_INT, proc_id, MASTER_PROCESS, MPI_COMM_WORLD);
    MPI_Send(address, size, MPI_INT, proc_id, MASTER_PROCESS, MPI_COMM_WORLD);
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

class Graph {
   public:
    vector<vector<int>> g;
    vector<int> u, v;
    int n, m;
    vector<int> c;

    Graph(int n, int m, vector<int> u, vector<int> v) {
        this->u = u;
        this->v = v;
        this->n = n;
        this->m = m;
        this->c.assign(m, -1);
        g.resize(m);
        vector<vector<int>> adjacent_edges(n);
        for (int i = 0; i < m; i++) {
            for (int el : adjacent_edges[u[i]])
                this->g[i].push_back(el), this->g[el].push_back(i);
            for (int el : adjacent_edges[v[i]])
                this->g[i].push_back(el), this->g[el].push_back(i);
            adjacent_edges[u[i]].push_back(i);
            adjacent_edges[v[i]].push_back(i);
        }
    }

    void send(int proc = -1) {
        MPI_Bcast(&(this->n), 1, MPI_INT, MASTER_PROCESS, MPI_COMM_WORLD);
        MPI_Bcast(&(this->m), 1, MPI_INT, MASTER_PROCESS, MPI_COMM_WORLD);
        send_vector(this->u, proc);
        send_vector(this->v, proc);
    }

    static Graph input() {
        int n, m;
        cin >> n >> m;
        vector<int> u(m), v(m);
        for (int i = 0; i < m; i++) {
            cin >> u[i] >> v[i];
            u[i]--, v[i]--;
        }
        return *(new Graph(n, m, u, v));
    }
};

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

    if (rank == MASTER_PROCESS) {
        Graph g = Graph::input();
        g.send();
        cout << "Done sending" << endl;
    }
    Graph g = g.recv();
    cout << rank << ": " << g.n << " " << g.m << "\n" << g.g;
    int start = ((g.m + numprocs - 1) / numprocs) * rank;
    int end = ((g.m + numprocs - 1) / numprocs) * (rank + 1);
    if (end > g.m)
        end = g.m;

    /* It's coloring time */
    while (true) {
        for (int i = start; i < end; i++) {
            if (g.c[i] != -1)
                continue;
            set<int> colors_used;
            for (int neighbor : g.g[i]) {
                if (neighbor < i && g.c[neighbor] == -1) {
                    g.c[i] = -1;
                    break;
                } else {
                    colors_used.insert(g.c[neighbor]);
                    while (colors_used.count(g.c[i]) != 0)
                        g.c[i]++;
                }
            }
        }
        send_vector(g.c, MASTER_PROCESS);

        if (rank == MASTER_PROCESS) {
            for (int proc = 0; proc < numprocs; proc++) {
                vector<int> update = recv_vector(proc);
                for (int i = 0; i < g.m; i++) {
                    if (g.c[i] == -1)
                        g.c[i] = update[i];
                }
            }
            send_vector(g.c);
        }

        g.c = recv_vector(MASTER_PROCESS);
        if (*min_element(g.c.begin(), g.c.end()) > -1)
            break;
    }

    if (rank == MASTER_PROCESS) {
        cout << g.c;
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
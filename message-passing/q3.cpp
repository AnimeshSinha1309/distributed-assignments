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

class Graph {
   public:
    vector<vector<int>> g;
    int n, m;
    vector<int> c;

    Graph(int n, int m, vector<int> u, vector<int> v) {
        this->n = n;
        this->m = m;
        this->c.assign(m, -1);
        g.resize(m);
        vector<vector<int>> adjacent_edges(n);
        for (int i = 0; i < m; i++) {
            adjacent_edges[u[i]].push_back(i);
            adjacent_edges[v[i]].push_back(i);
        }
        for (int node = 0; node < n; node++) {
            for (int i = 0; i < adjacent_edges[node].size(); i++) {
                for (int j = 0; j < i; j++) {
                    int s = adjacent_edges[node][i];
                    int t = adjacent_edges[node][j];
                    g[s].push_back(t);
                    g[t].push_back(s);
                }
            }
        }
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

    int n, m;
    if (rank == MASTER_PROCESS)
        cin >> n >> m;
    MPI_Bcast(&n, 1, MPI_INT, MASTER_PROCESS, MPI_COMM_WORLD);
    MPI_Bcast(&m, 1, MPI_INT, MASTER_PROCESS, MPI_COMM_WORLD);
    vector<int> u(m), v(m);
    if (rank == MASTER_PROCESS)
        for (int i = 0; i < m; i++) {
            cin >> u[i] >> v[i];
            u[i]--, v[i]--;
        }
    MPI_Bcast(&u[0], m, MPI_INT, MASTER_PROCESS, MPI_COMM_WORLD);
    MPI_Bcast(&v[0], m, MPI_INT, MASTER_PROCESS, MPI_COMM_WORLD);
    Graph g(n, m, u, v);

    int start = ((g.m + numprocs - 1) / numprocs) * rank;
    int end = ((g.m + numprocs - 1) / numprocs) * (rank + 1);
    if (end > g.m)
        end = g.m;
    cout << "Markers: " << start << ' ' << end << endl;

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
        MPI_Send(&g.c[0], g.m, MPI_INT, MASTER_PROCESS, PUSH_TASK,
                 MPI_COMM_WORLD);
        cout << rank << " Coloring " << g.c;

        if (rank == MASTER_PROCESS) {
            for (int proc = 0; proc < numprocs; proc++) {
                vector<int> update(g.m);
                MPI_Status status;
                MPI_Recv(&update[0], g.m, MPI_INT, proc, PUSH_TASK,
                         MPI_COMM_WORLD, &status);
                cout << proc << " recv: " << update;
                for (int i = 0; i < g.m; i++) {
                    if (g.c[i] == -1)
                        g.c[i] = update[i];
                }
            }
        }

        MPI_Bcast(&g.c[0], g.m, MPI_INT, MASTER_PROCESS, MPI_COMM_WORLD);
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
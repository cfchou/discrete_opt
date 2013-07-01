
#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <iterator>

using namespace std;

vector<int> run_dp(int const n, int const k, vector<int> const & vs,
    vector<int> const & ws);
vector<int> dp_by_row(unsigned int const i, int const k, 
    vector<int> const & vs, vector<int> const & ws, vector<int> & row);

int main(int argc, char const * argv[]) {
    if (argc < 2) {
        cout << "cpsolver filename" << endl;
        return 0;
    }
    ifstream fs;
    fs.open(argv[1]);
    if (!fs.is_open()) {
        cout << "[ERR] open" << endl;
        return -1;
    }
    int N = 0;
    int K = 0;
    vector<int> vs(1, 0);
    vector<int> ws(1, 0);
    if (fs.good()) {
        string line;
        getline(fs, line);
        stringstream ss(line);
        ss >> N >> K;
    }
    while (fs.good()) {
        int v, w;
        string line;
        getline(fs, line);
        stringstream ss(line);
        ss >> v >> w;
        vs.push_back(v);
        ws.push_back(w);
    }
    fs.close();
    
    vector<int> rt = run_dp(N, K, vs, ws);
    cout << rt[K] << endl;
    return 0;
}

vector<int> run_dp(int const n, int const k, vector<int> const & vs,
    vector<int> const & ws)
{
    vector<int> row(k + 1, 0);
    return dp_by_row(1, k, vs, ws, row);
}

/*
 0-1 Knapsack Formula:
 V(w, i) = if (w < w_i) then V(w, i - 1)
           else max [V(w - w_i, i - 1) + v_i,
                     V(w, i - 1)]
*/
vector<int> dp_by_row(unsigned int const i, int const k, 
    vector<int> const & vs, vector<int> const & ws, vector<int> & row)
{
    /*
    copy(row.begin(), row.end(), ostream_iterator<int>(cout, ", "));
    cout << endl;
    */
    if (0 == i % 10)
        cout << i << endl;
    if (i > vs.size()) return row;
    for (int cap = k; cap > 0; --cap) {
        if (cap >= ws[i]) {
            row[cap] = max(row[cap], row[cap - ws[i]] + vs[i]);             
        }
    }
    return dp_by_row(i + 1, k, vs, ws, row);
}












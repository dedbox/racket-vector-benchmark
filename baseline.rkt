#lang c

#include <chrono>
#include <cmath>
#include <glm/glm.hpp>
#include <iostream>

using namespace std;
using namespace std::chrono;

const int RUNS = 5;

double random_number() {
  return static_cast<double>(rand()) / static_cast<double>(RAND_MAX);
}

template<glm::length_t L, typename T, glm::qualifier Q>
glm::vec<L, T, Q>* random_vecs(int W) {
  glm::vec<L, T, Q>* vs = new glm::vec<L, T, Q>[W];
  for (int j = 0; j < W; ++j)
    for (int i = 0; i < L; ++i)
      vs[j][i] = random_number();
  return vs;
}

template<int L>
int benchmark(int W) {
  glm::vec<L, double, glm::defaultp>* vs = random_vecs<L, double, glm::defaultp>(W);
  steady_clock::time_point t0 = steady_clock::now();
  int count = 0;
  while (steady_clock::now() - t0 < milliseconds(100)) {
    ++count;
    for (int j = 1; j < L; ++j)
      vs[0] *= vs[j];
  }
  delete [] vs;
  return count;
}

template<int L>
int average_benchmark(int W, int runs) {
  int counts[runs];
  for (int i = 0; i < runs; ++i)
    counts[i] = benchmark<L>(W);
  int sum = 0;
  for (int i = 0; i < runs; ++i)
    sum += counts[i];
  return round(10.0 * sum / runs);
}

// template<N>
// int one_benchmark(int M) {
  
// }

int main() {
  srand(1);

  steady_clock::time_point t0 = steady_clock::now();

  cout << "N" "\t" "M" "\t";
  for (int N=0; N < RUNS; ++N)
    cout << "\t" << N + 1 << "\t";
  cout << "average" "\t" "stddev" << endl;

  for (int N=0; N < 4; ++N) {
    for (int M=0; M < 16; ++M) {
      cout << N << "\t" << M << "\t";
      int counts[RUNS];
      for (int i=0; i < RUNS; ++i)
        counts[runs] = benchmark 
    }
  }

  // for (int M=0; M < 16; ++M) {
  //   int count[
  // }

  // int counts[4][5];
  // for (int M=0; M < 16; ++M) {
  //   counts[0][M] = benchmark<1>(M + 1);
  //   counts[1][M] = benchmark<1>(M + 1);
  //   counts[2][M] = benchmark<2>(M + 1);
  //   counts[3][M] = benchmark<3>(M + 1);
  // }

  // int sums[4];
  // for (int N = 0; N < 4; ++N) {
  //   sums[N] = 0;
  //   for (int M = 0; M < 16; ++M)
  //     sums[N] += counts[N][M];
  // }

  // int averages[4];
  // for (int N = 0; N < 4; ++N)
  //   averages[N] = sums[N] / 5;

  // int 

  // cout << N << flush;
  //   cout << "\t" << average_benchmark<1>(N, 5) << flush;
  //   cout << "\t" << average_benchmark<2>(N, 5) << flush;
  //   cout << "\t" << average_benchmark<3>(N, 5) << flush;
  //   cout << "\t" << average_benchmark<4>(N, 5) << flush;
  //   cout << endl;
  // }

  return 0;
}

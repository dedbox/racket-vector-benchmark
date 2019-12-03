#include <chrono>
#include <cmath>
#include <glm/glm.hpp>
#include <iostream>

using namespace std;
using namespace std::chrono;

// const int RUNS = 1;
// const int M_max = 4;
// const int N_max = 16;

// double random_number() {
//   return static_cast<double>(rand()) / static_cast<double>(RAND_MAX);
// }

// template<glm::length_t M, typename T, glm::qualifier Q>
// glm::vec<M, T, Q>* random_vecs(int N) {
//   glm::vec<M, T, Q>* vs = new glm::vec<M, T, Q>[N];
//   for (int j = 0; j < N; ++j)
//     for (int i = 0; i < M; ++i)
//       vs[j][i] = random_number();
//   return vs;
// }

// int main() {
//   srand(1);

//   steady_clock::time_point t0 = steady_clock::now();

//   cout << "# components\t# vectors\tcount" << endl;
//   for (int N=2; N <= 16; ++N) {
//     benchmark<2>(N);
//     cout << 2 << "\t" << N << "\t" << benchmark<2>(N) << endl;
//   }
//   for (int N=2; N <= 16; ++N) {
//     benchmark<3>(N);
//     cout << 3 << "\t" << N << "\t" << benchmark<3>(N) << endl;
//   }
//   for (int N=2; N <= 16; ++N) {
//     benchmark<4>(N);
//     cout << 4 << "\t" << N << "\t" << benchmark<4>(N) << endl;
//   }

//   return 0;
// }

// template<int M>
// int benchmark(int N) {
//   glm::vec<M, double, glm::defaultp>* vs = random_vecs<M, double, glm::defaultp>(N);
//   steady_clock::time_point t0 = steady_clock::now();
//   int count = 0;
//   while (steady_clock::now() - t0 < milliseconds(1000)) {
//     ++count;
//     for (int j = 1; j < M; ++j)
//       vs[0] *= vs[j];
//   }
//   delete [] vs;
//   return count;
// }

int main() {
  int count1 = 0;
  steady_clock::time_point t1 = steady_clock::now();
  glm::dvec4 v1(1.0000001, 1.0000001, 1.0000001, 1.0000001);
  glm::dvec4 v2(1.0000001, 1.0000001, 1.0000001, 1.0000001);
  while (steady_clock::now() - t1 < milliseconds(1000)) {
    v1 = v1 * v2;
    ++count1;
  }
  cout << "\t" << count1 << flush;

  int count2 = 0;
  steady_clock::time_point t2 = steady_clock::now();
  while (steady_clock::now() - t2 < milliseconds(1000))
    ++count2;
  cout << "\t" << count2 << flush;

  return 0;
}

#ifndef PSEUDOVECTOR_HPP
#define PSEUDOVECTOR_HPP

#include <Rcpp.h>

// a thin wrapper that gives us the vector recycling behaviour of R:
template <int T, typename U>
struct PseudoVector {
  PseudoVector(Rcpp::Vector<T>& v_p) : v(v_p), sz(v_p.size()) { }
  inline U& operator[](size_t i) { return i<sz ? v[i] : v[i%sz]; }
  inline size_t size() const { return sz; }
  inline bool isScalar() const { return v.size()==1; }
private:
  Rcpp::Vector<T>& v;
  const size_t sz;
};
template <int T, typename U>
struct ConstPseudoVector {
  ConstPseudoVector(const Rcpp::Vector<T>& v_p) : v(v_p), sz(v_p.size()) { }
  inline const U& operator[](size_t i) const { return i<sz ? v[i] : v[i%sz]; }
  inline size_t size() const { return sz; }
  inline bool isScalar() const { return v.size()==1; }
private:
    const Rcpp::Vector<T>& v;
    const size_t sz;
};

#endif

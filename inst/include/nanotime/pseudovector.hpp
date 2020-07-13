#ifndef NANOTIME_PSEUDOVECTOR_HPP
#define NANOTIME_PSEUDOVECTOR_HPP

#include <Rcpp.h>


namespace nanotime {

  // a thin wrapper that gives us the vector recycling behaviour of R:
  template <int T, typename U, typename R=U>
  struct PseudoVector {
    PseudoVector(Rcpp::Vector<T>& v_p) : v(v_p), sz(v_p.size()) { }
    inline R& operator[](R_xlen_t i) {
      return i<sz ? *reinterpret_cast<R*>(&v[i]) : *reinterpret_cast<R*>(v[i%sz]);
    }
    inline R_xlen_t size() const { return sz; }
    inline bool isScalar() const { return v.size()==1; }
  private:
    Rcpp::Vector<T>& v;
    const R_xlen_t sz;
  };
  template <int T, typename U, typename R=U>
  struct ConstPseudoVector {
    ConstPseudoVector(const Rcpp::Vector<T>& v_p) : v(v_p), sz(v_p.size()) { }
    inline const R& operator[](R_xlen_t i) const {
      return i<sz ? *reinterpret_cast<const R*>(&v[i]) : *reinterpret_cast<const R*>(&v[i%sz]);
    }
    inline R_xlen_t size() const { return sz; }
    inline bool isScalar() const { return v.size()==1; }
  private:
    const Rcpp::Vector<T>& v;
    const R_xlen_t sz;
  };

  // character is a special case that needs a bit of help:
  template <>
  struct ConstPseudoVector<STRSXP,  const Rcpp::CharacterVector::const_Proxy> {
    ConstPseudoVector(const Rcpp::Vector<STRSXP>& v_p) : v(v_p), sz(v_p.size()) { }
    inline const Rcpp::CharacterVector::const_Proxy operator[](R_xlen_t i) const { return i<sz ? v[i] : v[i%sz]; }
    inline R_xlen_t size() const { return sz; }
    inline bool isScalar() const { return v.size()==1; }
  private:
    const Rcpp::Vector<STRSXP>& v;
    const R_xlen_t sz;
  };

} // end namespace nanotime

#endif

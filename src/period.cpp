#include <sstream>
#include <regex>
#include <Rcpp.h>
#include "date.h"
#include <RcppCCTZ_API.h>
#include "nanotime/period.hpp"
#include "nanotime/duration.hpp"
#include "nanotime/pseudovector.hpp"
#include "nanotime/utilities.hpp"

// From R>=4.3.0 the typedef of Rcomplex changed. Some modern strict
//   compilers (e.g. 2024 clang -Wmissing-braces) get tripped up by
//   'Rcomplex{re, im}' vs. 'Rcomplex{{re, im}}', the latter being technically
//   more correct for the new typedef, though for many compilers this will
//   "just work". See #134 and the comments in R_Ext/Complex.h.
inline Rcomplex makeComplex(double re, double im) {
  Rcomplex ret;
  ret.r = re;
  ret.i = im;
  return ret;
}

using namespace nanotime;


// for debug reasons...
// cf https://stackoverflow.com/a/16692519
template<typename Clock, typename Duration>
std::ostream &operator<<(std::ostream &stream,
                         const std::chrono::time_point<Clock, Duration> &time_point) {
  // updated with https://stackoverflow.com/a/71442312/23224962 and using C++17
  return stream << std::chrono::duration_cast<std::chrono::nanoseconds>(time_point.time_since_epoch()).count();
}


period::period(const std::string& str) {
  const char* s = str.c_str();
  const char* e = s + str.size();

  months = 0;
  days   = 0;
  dur    = std::chrono::seconds(0);

  int n;
  if (s < e && (*s == '/' || (*s != '-' && s+2 < e && s[2] == ':'))) goto getduration;
  // test the case where we have only a negative duration:
  if (s < e && *s == '-' &&
      ((s+3 < e && s[3] == ':') || (s+2 < e && s[2] == ':'))) {      
    --s;  // because getduration will increment it
    goto getduration;
  }
  if (!readNumber(s, e, n, true) || s == e) throw std::range_error("cannot parse nanoperiod");
  if (*s == 'y') {
    months += 12*n;
    ++s;
    if (s < e) {
      if (*s == '/') goto getduration;
      if (!readNumber(s, e, n, true) || s == e) throw std::range_error("cannot parse nanoperiod");
    }
    else {
      return;
    }
  }      
  if (*s == 'm') {
    months += n;
    ++s;
    if (s < e) {
      if (*s == '/') goto getduration;
      if (!readNumber(s, e, n, true) || s == e) throw std::range_error("cannot parse nanoperiod");      
    }
    else {
      return;
    }
  }      
  if (*s == 'w') {
    days += 7*n;
    ++s;
    if (s < e) {
      if (*s == '/') goto getduration;
      if (!readNumber(s, e, n, true) || s == e) throw std::range_error("cannot parse nanoperiod");
    }
    else {
      return;
    }
  }
  if (*s == 'd') {
    days += n;
    ++s;
    if (s < e) { 
      if (*s == '/') goto getduration;
      if (!readNumber(s, e, n, true) || s == e) throw std::range_error("cannot parse nanoperiod");
    }
    else {
      return;
    }
  }

  // we've succeeded a readNumber, so this means we've
  // actually read into the duration; so backtrack and use the already
  // existing function to parse a duration:
getduration:                    // # nocov
  try {
    dur = from_string(++s);
  }
  catch (...) {
    throw std::range_error("cannot parse nanoperiod");
  }
}


std::string nanotime::to_string(const period& p) {
  std::stringstream ss;
  ss << p.getMonths() << "m" << p.getDays() << "d/" << to_string(p.getDuration());
  return ss.str();
}


template <typename T>
period nanotime::operator*(const period& p, T d) {
  return period(p.getMonths()*d, 
                p.getDays()*d,
                duration(static_cast<int64_t>(d*p.getDuration().count())*
                                 duration(1)));
}

template <typename T>
period nanotime::operator/(const period& p, T d) {
  if (d == 0) {
    throw std::logic_error("divide by zero");
  }
  return period(p.getMonths()/d, 
                p.getDays()/d,
                duration(static_cast<int64_t>(p.getDuration().count()/d)*
                                 duration(1)));
}

// bool operator>(const period& p1, const period& p2) {
//   // this is actually a difficult proposition, so for this calculation
//   // we take into account the average lengths. This means that in
//   // certain specific applications p1 might be <= to p2. But at any
//   // rate this should work for all practical purposes:
//   const auto YEAR = 365.25 * 24h;
//   const auto MONTH = YEAR/12;
//   return p1.getMonths()*MONTH + p1.getDays()*24h < p2.getMonths()*MONTH + p2.getDays()*24h;
// }

bool nanotime::operator==(const period& p1, const period& p2) {
  return 
    p1.getMonths() == p2.getMonths() && 
    p1.getDays() == p2.getDays() &&
    p1.getDuration() == p2.getDuration();
}

bool nanotime::operator!=(const period& p1, const period& p2) {
  return !(p1 == p2);
}


struct double2 {
  double d1;
  double d2;
};

union period_union {
  struct period_alias {
    int32_t i1;
    int32_t i2;
    int64_t i3;  
  } prd;
  double2 dbl2;
};
  

// const int PRDSZ   = sizeof(period_union)/sizeof(double);
// const int INT64SZ = 1;
// const int NANOSZ  = 1;
// const int REALSZ  = 1;

// see Rcpp/inst/include/Rcpp/vector/instantiation.h where NumericVector and al. are defined
typedef ConstPseudoVector<REALSXP, double>   ConstPseudoVectorInt64;
typedef ConstPseudoVector<REALSXP, double>   ConstPseudoVectorNano;
typedef ConstPseudoVector<CPLXSXP, Rcomplex> ConstPseudoVectorPrd;
typedef ConstPseudoVector<REALSXP, double>   ConstPseudoVectorDbl;
typedef ConstPseudoVector<CPLXSXP, Rcomplex> ConstPseudoVectorIval;
typedef ConstPseudoVector<STRSXP,  const Rcpp::CharacterVector::const_Proxy> ConstPseudoVectorChar;
typedef ConstPseudoVector<INTSXP,  int32_t>   ConstPseudoVectorInt;
typedef ConstPseudoVector<LGLSXP,  int32_t>   ConstPseudoVectorBool;

typedef PseudoVector<REALSXP, double>   PseudoVectorInt64;
typedef PseudoVector<REALSXP, double>   PseudoVectorNano;
typedef PseudoVector<CPLXSXP, Rcomplex> PseudoVectorPrd;

// [[Rcpp::export]]
Rcpp::ComplexVector period_from_string_impl(Rcpp::CharacterVector str) {
  Rcpp::ComplexVector res(str.size());
  for (R_xlen_t i=0; i<str.size(); ++i) {
    period prd(Rcpp::as<std::string>(str[i]));
    period_union pu = { { prd.getMonths(), prd.getDays(), prd.getDuration().count() } };
    res[i] = makeComplex(pu.dbl2.d1, pu.dbl2.d2 );
  }
  if (str.hasAttribute("names")) {
    res.names() = str.names();
  }
  return assignS4("nanoperiod", res);
}


// [[Rcpp::export]]
Rcpp::ComplexVector period_from_parts_impl(Rcpp::IntegerVector months_v, Rcpp::IntegerVector days_v, Rcpp::NumericVector dur_v) {
  checkVectorsLengths(months_v, days_v, dur_v);
  Rcpp::ComplexVector res(getVectorLengths(months_v, days_v, dur_v));
  if (res.size()) {
    const ConstPseudoVectorInt months(months_v);
    const ConstPseudoVectorInt days(days_v);
    const ConstPseudoVectorDbl dur(dur_v);
  
    for (R_xlen_t i=0; i<res.size(); ++i) {
      const auto dur_i = *reinterpret_cast<const int64_t*>(&dur[i]);
      period_union pu = { { months[i], days[i], dur_i } };
      res[i] = makeComplex(pu.dbl2.d1, pu.dbl2.d2 );
    }
  }
  return assignS4("nanoperiod", res);
}


// [[Rcpp::export]]
Rcpp::CharacterVector period_to_string_impl(Rcpp::ComplexVector prd) {
  Rcpp::CharacterVector res(prd.size());
  for (R_xlen_t i=0; i<prd.size(); ++i) {
    period pu;
    memcpy(&pu, reinterpret_cast<const char*>(&prd[i]), sizeof(period));
    if (pu.isNA()) {
      res[i] = NA_STRING;
    }
    else {
      res[i] = to_string(*reinterpret_cast<period*>(&pu));
    }
  }
  if (prd.hasAttribute("names")) {
    Rcpp::CharacterVector prdnm(prd.names());
    Rcpp::CharacterVector nm(prdnm.size());
    for (R_xlen_t i=0; i<nm.size(); ++i) {
      nm[i] = prdnm[i];
    }
    if (prd.hasAttribute("names")) {
      res.names() = prd.names();
    }
    res.names() = nm;
  }
  return res;
}

typedef std::numeric_limits< double > dbl;

// [[Rcpp::export]]
Rcpp::ComplexVector period_from_integer64_impl(Rcpp::NumericVector i64) {
  Rcpp::ComplexVector res(i64.size());
  for (R_xlen_t i=0; i<i64.size(); ++i) {
    auto elt = *reinterpret_cast<std::int64_t*>(&i64[i]);
    if (elt == NA_INTEGER64) {
      period_union pu = { { NA_INTEGER, NA_INTEGER, NA_INTEGER64 } };
      res[i] = makeComplex(pu.dbl2.d1, pu.dbl2.d2 );
    }
    else {
      period_union pu = { { 0, 0, elt } };
      res[i] = makeComplex(pu.dbl2.d1, pu.dbl2.d2 );
    }
  }
  if (i64.hasAttribute("names")) {
    res.names() = i64.names();
  }
  return assignS4("nanoperiod", res);
}


// [[Rcpp::export]]
Rcpp::ComplexVector period_from_integer_impl(Rcpp::IntegerVector iint) {
  Rcpp::ComplexVector res(iint.size());
  for (R_xlen_t i=0; i<iint.size(); ++i) {
    if (iint[i] == NA_INTEGER) {
      period_union pu = { { NA_INTEGER, NA_INTEGER, NA_INTEGER64 } };
      res[i] = makeComplex(pu.dbl2.d1, pu.dbl2.d2 );
    }
    else {
      period_union pu = { { 0, 0, static_cast<std::int64_t>(iint[i]) } };
      res[i] = makeComplex(pu.dbl2.d1, pu.dbl2.d2 );
    }
  }
  if (iint.hasAttribute("names")) {
    res.names() = iint.names();
  }
  return assignS4("nanoperiod", res);
}


// [[Rcpp::export]]
Rcpp::ComplexVector period_from_double_impl(Rcpp::NumericVector dbl) {
  Rcpp::ComplexVector res(dbl.size());
  for (R_xlen_t i=0; i<dbl.size(); ++i) {
    if (ISNA(dbl[i])) {
      period_union pu = { { NA_INTEGER, NA_INTEGER, NA_INTEGER64 } };
      res[i] = makeComplex(pu.dbl2.d1, pu.dbl2.d2 );
    }
    else {
      period_union pu = { { 0, 0, static_cast<std::int64_t>(dbl[i]) } };
      res[i] = makeComplex(pu.dbl2.d1, pu.dbl2.d2 );
    }
  }
  if (dbl.hasAttribute("names")) {
    res.names() = dbl.names();
  }
  return assignS4("nanoperiod", res);
}


// [[Rcpp::export]]
Rcpp::ComplexVector plus_period_period_impl(const Rcpp::ComplexVector e1_nv,
                                            const Rcpp::ComplexVector e2_nv) {
  checkVectorsLengths(e1_nv, e2_nv);
  Rcpp::ComplexVector res(getVectorLengths(e1_nv, e2_nv));
  if (res.size()) {
    const ConstPseudoVectorPrd e1_n(e1_nv);
    const ConstPseudoVectorPrd e2_n(e2_nv);
    for (R_xlen_t i=0; i<res.size(); ++i) {
      period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
      period pu2; memcpy(&pu2, reinterpret_cast<const char*>(&e2_n[i]), sizeof(period));
      auto prd = pu1 + pu2;
      memcpy(&res[i], &prd, sizeof(prd));
    }
    copyNames(e1_nv, e2_nv, res);
  }
  return assignS4("nanoperiod", res);
}


// unary '-'
// [[Rcpp::export]]
Rcpp::ComplexVector minus_period_impl(const Rcpp::ComplexVector e1_cv) {
  const ConstPseudoVectorPrd e1_n(e1_cv);
  Rcpp::ComplexVector res(e1_cv.size());
  for (R_xlen_t i=0; i<res.size(); ++i) {
    period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
    auto prd = -pu1;
    memcpy(&res[i], reinterpret_cast<const char*>(&prd), sizeof(prd));
  }
  copyNames(e1_cv, e1_cv, res);
  return assignS4("nanoperiod", res);
}


// [[Rcpp::export]]
Rcpp::ComplexVector minus_period_period_impl(const Rcpp::ComplexVector e1_cv,
                                             const Rcpp::ComplexVector e2_cv) {
  checkVectorsLengths(e1_cv, e2_cv);
  Rcpp::ComplexVector res(getVectorLengths(e1_cv, e2_cv));
  if (res.size()) {
    const ConstPseudoVectorPrd e1_n(e1_cv);
    const ConstPseudoVectorPrd e2_n(e2_cv);
  
    for (R_xlen_t i=0; i<res.size(); ++i) {
      period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
      period pu2; memcpy(&pu2, reinterpret_cast<const char*>(&e2_n[i]), sizeof(period));
      auto prd = pu1 - pu2;
      memcpy(&res[i], reinterpret_cast<const char*>(&prd), sizeof(prd));
    }
    copyNames(e1_cv, e2_cv, res);
  }
  return assignS4("nanoperiod", res);
}


template <typename OP>
Rcpp::LogicalVector compare_period_period(const Rcpp::ComplexVector e1_cv,
                                          const Rcpp::ComplexVector e2_cv,
                                          const OP& op) {
  checkVectorsLengths(e1_cv, e2_cv);
  Rcpp::LogicalVector res(getVectorLengths(e1_cv, e2_cv));
  if (res.size()) {
    const ConstPseudoVectorPrd e1_n(e1_cv);
    const ConstPseudoVectorPrd e2_n(e2_cv);
    for (R_xlen_t i=0; i<res.size(); ++i) {
      period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
      period pu2; memcpy(&pu2, reinterpret_cast<const char*>(&e2_n[i]), sizeof(period));
      res[i] = op(pu1, pu2);
    }
    copyNames(e1_cv, e2_cv, res);
  }
  return res;
}

// [[Rcpp::export]]
Rcpp::LogicalVector eq_period_period_impl(const Rcpp::ComplexVector e1_p,
                                          const Rcpp::ComplexVector e2_p) {
  return compare_period_period(e1_p, e2_p, std::equal_to<period>());
}

// [[Rcpp::export]]
Rcpp::LogicalVector ne_period_period_impl(const Rcpp::ComplexVector e1_p,
                                          const Rcpp::ComplexVector e2_p) {
  return compare_period_period(e1_p, e2_p, std::not_equal_to<period>());
}

// [[Rcpp::export]]
Rcpp::ComplexVector plus_period_integer64_impl(const Rcpp::ComplexVector e1_cv,
                                               const Rcpp::NumericVector e2_nv) {
  checkVectorsLengths(e1_cv, e2_nv);
  Rcpp::ComplexVector res(getVectorLengths(e1_cv, e2_nv));
  if (res.size()) {
    const ConstPseudoVectorPrd   e1_n(e1_cv);
    const ConstPseudoVectorInt64 e2_n(e2_nv);
    for (R_xlen_t i=0; i<res.size(); ++i) {
      period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
      duration dur; memcpy(&dur, reinterpret_cast<const char*>(&e2_n[i]), sizeof(dur));
      pu1 = plus(pu1, dur);
      memcpy(&res[i], &pu1, sizeof(pu1));
    }
    copyNames(e1_cv, e2_nv, res);
  }
  return assignS4("nanoperiod", res);
}


// [[Rcpp::export]]
Rcpp::ComplexVector  minus_period_integer64_impl(const Rcpp::ComplexVector e1_cv,
                                                 const Rcpp::NumericVector e2_nv) {
  checkVectorsLengths(e1_cv, e2_nv);
  Rcpp::ComplexVector res(getVectorLengths(e1_cv, e2_nv));
  if (res.size()) {
    const ConstPseudoVectorPrd   e1_n(e1_cv);
    const ConstPseudoVectorInt64 e2_n(e2_nv);
    for (R_xlen_t i=0; i<res.size(); ++i) {
      period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
      duration dur; memcpy(&dur, reinterpret_cast<const char*>(&e2_n[i]), sizeof(dur));
      pu1 = minus(pu1, dur);
      memcpy(&res[i], &pu1, sizeof(pu1));
    }
    copyNames(e1_cv, e2_nv, res);
  }
  return assignS4("nanoperiod", res);
}

// [[Rcpp::export]]
Rcpp::ComplexVector multiplies_period_integer64_impl(const Rcpp::ComplexVector e1_cv,
                                                     const Rcpp::NumericVector e2_nv) {
  checkVectorsLengths(e1_cv, e2_nv);
  Rcpp::ComplexVector res(getVectorLengths(e1_cv, e2_nv));
  const ConstPseudoVectorPrd   e1_n(e1_cv);
  const ConstPseudoVectorInt64 e2_n(e2_nv);
  for (R_xlen_t i=0; i<res.size(); ++i) {
    period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
    uint64_t m; memcpy(&m, reinterpret_cast<const char*>(&e2_n[i]), sizeof(m));
    pu1 = pu1 * m;
    memcpy(&res[i], &pu1, sizeof(pu1));
  }
  copyNames(e1_cv, e2_nv, res);
  return assignS4("nanoperiod", res);
}

// [[Rcpp::export]]
Rcpp::ComplexVector divides_period_integer64_impl(const Rcpp::ComplexVector e1_cv,
                                                  const Rcpp::NumericVector e2_nv) {
  checkVectorsLengths(e1_cv, e2_nv);
  Rcpp::ComplexVector res(getVectorLengths(e1_cv, e2_nv));
  if (res.size()) {
    const ConstPseudoVectorPrd   e1_n(e1_cv);
    const ConstPseudoVectorInt64 e2_n(e2_nv);
    for (R_xlen_t i=0; i<res.size(); ++i) {
      period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
      uint64_t m; memcpy(&m,   reinterpret_cast<const char*>(&e2_n[i]), sizeof(m));
      pu1 = pu1 / m;
      memcpy(&res[i], &pu1, sizeof(pu1));
    }
    copyNames(e1_cv, e2_nv, res);
  }
  return assignS4("nanoperiod", res);
}

// [[Rcpp::export]]
Rcpp::ComplexVector multiplies_period_double_impl(const Rcpp::ComplexVector e1_cv,
                                                  const Rcpp::NumericVector e2_nv) {
  checkVectorsLengths(e1_cv, e2_nv);
  Rcpp::ComplexVector res(getVectorLengths(e1_cv, e2_nv));
  if (res.size()) {
    const ConstPseudoVectorPrd e1_n(e1_cv);
    const ConstPseudoVectorDbl e2_n(e2_nv);
    for (R_xlen_t i=0; i<res.size(); ++i) {
      period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
      double m;   memcpy(&m,   reinterpret_cast<const char*>(&e2_n[i]), sizeof(m));
      pu1 = pu1 * m;
      memcpy(&res[i], &pu1, sizeof(pu1));
    }
    copyNames(e1_cv, e2_nv, res);
  }
  return assignS4("nanoperiod", res);
}

// [[Rcpp::export]]
Rcpp::ComplexVector divides_period_double_impl(const Rcpp::ComplexVector e1_cv,
                                               const Rcpp::NumericVector e2_nv) {
  checkVectorsLengths(e1_cv, e2_nv);
  Rcpp::ComplexVector res(getVectorLengths(e1_cv, e2_nv));
  if (res.size()) {
    const ConstPseudoVectorPrd e1_n(e1_cv);
    const ConstPseudoVectorDbl e2_n(e2_nv);
    for (R_xlen_t i=0; i<res.size(); ++i) {
      period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
      double m;   memcpy(&m,   reinterpret_cast<const char*>(&e2_n[i]), sizeof(m));
      pu1 = pu1 / m;
      memcpy(&res[i], &pu1, sizeof(pu1));
    }
    copyNames(e1_cv, e2_nv, res);
  }
  return assignS4("nanoperiod", res);
}

// [[Rcpp::export]]
Rcpp::ComplexVector minus_integer64_period_impl(const Rcpp::NumericVector e1_nv,
                                                const Rcpp::ComplexVector e2_cv) {
  checkVectorsLengths(e1_nv, e2_cv);
  Rcpp::ComplexVector res(getVectorLengths(e1_nv, e2_cv));
  if (res.size()) {
    const ConstPseudoVectorInt64 e1_n(e1_nv);
    const ConstPseudoVectorPrd   e2_n(e2_cv);
    for (R_xlen_t i=0; i<res.size(); ++i) {
      period pu2;           memcpy(&pu2, reinterpret_cast<const char*>(&e2_n[i]), sizeof(pu2));
      duration dur; memcpy(&dur, reinterpret_cast<const char*>(&e1_n[i]), sizeof(dur));
      pu2 = minus(dur, pu2);
      memcpy(&res[i], &pu2, sizeof(pu2));
    }
    copyNames(e1_nv, e2_cv, res);
  }
  return assignS4("nanoperiod", res);
}

// [[Rcpp::export]]
Rcpp::NumericVector plus_nanotime_period_impl(const Rcpp::NumericVector   e1_nv,
                                              const Rcpp::ComplexVector   e2_cv,
                                              const Rcpp::CharacterVector tz_v) {
  checkVectorsLengths(e1_nv, e2_cv, tz_v);
  Rcpp::ComplexVector res(getVectorLengths(e1_nv, e2_cv, tz_v));
  if (res.size()) {
    const ConstPseudoVectorNano e1_n(e1_nv);
    const ConstPseudoVectorPrd  e2_n(e2_cv);
    const ConstPseudoVectorChar tz(tz_v);

    for (R_xlen_t i=0; i<res.size(); ++i) {
      dtime nano; memcpy(&nano, reinterpret_cast<const char*>(&e1_n[i]), sizeof(nano));
      period prd; memcpy(&prd, reinterpret_cast<const char*>(&e2_n[i]), sizeof(prd));
      auto dt = plus(nano, prd, Rcpp::as<std::string>(tz[i]));
      memcpy(&res[i], &dt, sizeof(dt));
    }
    copyNames(e1_nv, e2_cv, res);
  }
  return assignS4("nanotime", res, "integer64");
}

// [[Rcpp::export]]
Rcpp::NumericVector minus_nanotime_period_impl(const Rcpp::NumericVector   e1_nv,
                                               const Rcpp::ComplexVector   e2_cv,
                                               const Rcpp::CharacterVector tz_v) {
  checkVectorsLengths(e1_nv, e2_cv, tz_v);
  Rcpp::ComplexVector res(getVectorLengths(e1_nv, e2_cv, tz_v));
  if (res.size()) {
    const ConstPseudoVectorNano e1_n(e1_nv);
    const ConstPseudoVectorPrd  e2_n(e2_cv);
    const ConstPseudoVectorChar tz(tz_v);

    for (R_xlen_t i=0; i<res.size(); ++i) {
      dtime nano; memcpy(&nano, reinterpret_cast<const char*>(&e1_n[i]), sizeof(nano));
      period prd; memcpy(&prd, reinterpret_cast<const char*>(&e2_n[i]), sizeof(prd));
      auto dt = minus(nano, prd, Rcpp::as<std::string>(tz[i % tz.size()]));
      memcpy(&res[i], &dt, sizeof(dt));
    }
    copyNames(e1_nv, e2_cv, res);
  }
  return assignS4("nanotime", res, "integer64");
}

// [[Rcpp::export]]
Rcpp::ComplexVector plus_nanoival_period_impl(const Rcpp::ComplexVector   e1_cv,
                                              const Rcpp::ComplexVector   e2_cv,
                                              const Rcpp::CharacterVector tz_v) {
  checkVectorsLengths(e1_cv, e2_cv, tz_v);
  Rcpp::ComplexVector res(getVectorLengths(e1_cv, e2_cv, tz_v));
  if (res.size()) {
    const ConstPseudoVectorIval e1_n (e1_cv);
    const ConstPseudoVectorPrd  e2_n (e2_cv);
    const ConstPseudoVectorChar tz(tz_v);

    for (R_xlen_t i=0; i<res.size(); ++i) {
      interval ival; memcpy(&ival, reinterpret_cast<const char*>(&e1_n[i]), sizeof(ival));
      period prd; memcpy(&prd, reinterpret_cast<const char*>(&e2_n[i]), sizeof(prd));
      auto res_ival = plus(ival, prd, Rcpp::as<std::string>(tz[i % tz.size()]));
      memcpy(&res[i], &res_ival, sizeof(res_ival));
    }
    copyNames(e1_cv, e2_cv, res);
  }
  return assignS4("nanoival", res);
}

// [[Rcpp::export]]
Rcpp::ComplexVector minus_nanoival_period_impl(const Rcpp::ComplexVector   e1_cv,
                                               const Rcpp::ComplexVector   e2_cv,
                                               const Rcpp::CharacterVector tz_v) {
  checkVectorsLengths(e1_cv, e2_cv, tz_v);
  Rcpp::ComplexVector res(getVectorLengths(e1_cv, e2_cv, tz_v));
  if (res.size()) {
    const ConstPseudoVectorIval e1_n (e1_cv);
    const ConstPseudoVectorPrd  e2_n (e2_cv);
    const ConstPseudoVectorChar tz(tz_v);
  
    for (R_xlen_t i=0; i<res.size(); ++i) {
      interval ival; memcpy(&ival, reinterpret_cast<const char*>(&e1_n[i]), sizeof(ival));
      period prd; memcpy(&prd, reinterpret_cast<const char*>(&e2_n[i]), sizeof(prd));
      auto res_ival = minus(ival, prd, Rcpp::as<std::string>(tz[i % tz.size()]));
      memcpy(&res[i], &res_ival, sizeof(res_ival));
    }
    copyNames(e1_cv, e2_cv, res);
  }
  return assignS4("nanoival", res);
}


// [[Rcpp::export]]
Rcpp::NumericVector period_month_impl(const Rcpp::ComplexVector e_n) {
  Rcpp::NumericVector res(e_n.size());
  for (R_xlen_t i=0; i<e_n.size(); ++i) {
    period prd; memcpy(&prd, reinterpret_cast<const char*>(&e_n[i]), sizeof(period));
    if (prd.isNA()) {
      res[i] = NA_REAL;
    }
    else {
      res[i] = prd.getMonths();
    }
  }
  if (e_n.hasAttribute("names")) {
    res.names() = e_n.names();
  }
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector period_day_impl(const Rcpp::ComplexVector e_n) {
  Rcpp::NumericVector res(e_n.size());
  for (R_xlen_t i=0; i<e_n.size(); ++i) {
    period prd; memcpy(&prd, reinterpret_cast<const char*>(&e_n[i]), sizeof(period));
    if (prd.isNA()) {
      res[i] = NA_REAL;
    }
    else {
      res[i] = prd.getDays();
    }
  }
  if (e_n.hasAttribute("names")) {
    res.names() = e_n.names();
  }
  return res;
}


// [[Rcpp::export]]
Rcpp::S4 period_duration_impl(const Rcpp::ComplexVector e_n) {
  Rcpp::NumericVector res(e_n.size());
  for (R_xlen_t i=0; i<e_n.size(); ++i) {
    period prd; memcpy(&prd, reinterpret_cast<const char*>(&e_n[i]), sizeof(period));
    if (prd.isNA()) {
      auto dur = duration::min();
      memcpy(&res[i], &dur, sizeof(dur));
    }
    else {
      auto dur = prd.getDuration();
      memcpy(&res[i], &dur, sizeof(dur));
    }
  }
  if (e_n.hasAttribute("names")) {
    res.names() = e_n.names();
  }
  return assignS4("nanoduration", res, "integer64");
}


// [[Rcpp::export]]
Rcpp::LogicalVector period_isna_impl(const Rcpp::ComplexVector cv) {
  Rcpp::LogicalVector res(cv.size());
  for (R_xlen_t i=0; i<cv.size(); ++i) {
    period prd;
    Rcomplex c = cv[i];
    memcpy(&prd, reinterpret_cast<const char*>(&c), sizeof(c));
    res[i] = prd.isNA();
  }
  res.names() = cv.names();
  return res;
}


constexpr duration abs(duration d) {
  return d >= d.zero() ? d : -d;
}

// This gives back a `nanotime` sequence for a `by` that is a `period`:
// [[Rcpp::export]]
Rcpp::NumericVector period_seq_from_to_impl(const Rcpp::NumericVector from_nv,
                                            const Rcpp::NumericVector to_nv,
                                            const Rcpp::ComplexVector by_cv,
                                            const std::string tz) {
  const ConstPseudoVectorNano from_n(from_nv);
  const ConstPseudoVectorNano to_n(to_nv);
  const ConstPseudoVectorPrd  by_n(by_cv);
  dtime from; memcpy(&from, reinterpret_cast<const char*>(&from_n[0]), sizeof(from));
  dtime to;   memcpy(&to,   reinterpret_cast<const char*>(&to_n[0]),   sizeof(to));
  period by;          memcpy(&by,   reinterpret_cast<const char*>(&by_n[0]),   sizeof(by));

  std::vector<dtime> res{from};

  auto diff = to - from;
  auto pos = diff >= std::chrono::seconds(0);
  auto dist = abs(diff);
  auto olddist = dist;
  for (;;) {
    auto next = plus(res.back(), by, tz);
    if (pos ? next > to : next < to) break;
    res.push_back(next);
    olddist = dist;
    dist = abs(to - next);
    if (dist >= olddist) {
      Rcpp::stop("incorrect specification for 'to'/'by'"); // # nocov
    }
  }

  Rcpp::NumericVector res_rcpp(res.size());
  memcpy(&res_rcpp[0], &res[0], sizeof(dtime)*res.size());
  return assignS4("nanotime", res_rcpp, "integer64");
}

// This gives back a `nanotime` sequence for a `by` that is a `period`:
// [[Rcpp::export]]
Rcpp::NumericVector period_seq_from_length_impl(const Rcpp::NumericVector from_nv,
                                                const Rcpp::ComplexVector by_cv,
                                                const Rcpp::NumericVector n_nv,
                                                const std::string tz) {
  const ConstPseudoVectorNano from_n(from_nv);
  const ConstPseudoVectorPrd  by_n(by_cv);
  const ConstPseudoVectorNano n_n(n_nv);

  dtime from; memcpy(&from, reinterpret_cast<const char*>(&from_n[0]), sizeof(from));
  period by;          memcpy(&by,   reinterpret_cast<const char*>(&by_n[0]),   sizeof(by));
  size_t n;           memcpy(&n,    reinterpret_cast<const char*>(&n_n[0]),    sizeof(n));

  std::vector<dtime> res{from};

  for (size_t i=1; i<n; ++i) {
    res.push_back(plus(res[i-1], by, tz));
  }

  Rcpp::NumericVector res_rcpp(res.size());
  memcpy(&res_rcpp[0], &res[0], sizeof(dtime)*res.size());
  return assignS4("nanotime", res_rcpp, "integer64");
}

static Rcomplex getNA_complex() {
  static const auto p = period(std::numeric_limits<int32_t>::min(),
                               std::numeric_limits<int32_t>::min(),
                               duration::zero());
  Rcomplex c;
  memcpy(&c, &p, sizeof(p));  
  return c;
}

// [[Rcpp::export]]
Rcpp::ComplexVector period_subset_numeric_impl(const Rcpp::ComplexVector& v, const Rcpp::NumericVector& idx) {
  Rcpp::ComplexVector res(0);
  std::vector<Rcomplex> res_c;    // by declaring it here we can make subset logical agnostic to 'Rcomplex'
  subset_numeric(v, idx, res, res_c, getNA_complex);
  return assignS4("nanoperiod", res);
}


// [[Rcpp::export]]
Rcpp::ComplexVector period_subset_logical_impl(const Rcpp::ComplexVector& v, const Rcpp::LogicalVector& idx_p) {
  const ConstPseudoVectorBool idx(idx_p);
  Rcpp::ComplexVector res(0);
  std::vector<Rcomplex> res_c;    // by declaring it here we can make subset logical agnostic to 'Rcomplex'
  subset_logical(v, idx, res, res_c, getNA_complex);
  return assignS4("nanoperiod", res);
}

#include <sstream>
#include <regex>
#include <Rcpp.h>
#include "period.hpp"
#include "duration.hpp"
#include "date.h"

// for debug reasons...
// the following code from: https://stackoverflow.com/a/16692519
template<typename Clock, typename Duration>
std::ostream &operator<<(std::ostream &stream,
                         const std::chrono::time_point<Clock, Duration> &time_point) {
  const time_t time = Clock::to_time_t(time_point);
#if __GNUC__ > 4 || \
   ((__GNUC__ == 4) && __GNUC_MINOR__ > 8 && __GNUC_REVISION__ > 1)
    // Maybe the put_time will be implemented later?
    struct tm tm;
  localtime_r(&time, &tm);
  return stream << std::put_time(&tm, "%c"); // Print standard date&time
#else
  char buffer[26];
  ctime_r(&time, buffer);
  buffer[24] = '\0';  // Removes the newline that is added
  return stream << buffer;
#endif
}


extern "C" int getOffset(long long s, const char* tzstr);

using namespace std::chrono_literals;

static inline Global::duration getOffsetCnv(const Global::dtime& dt, const std::string& z) {
  typedef int GET_OFFSET_FUN(long long, const char*); 
  GET_OFFSET_FUN *getOffset = (GET_OFFSET_FUN *) R_GetCCallable("RcppCCTZ", "getOffset" );

  auto offset = getOffset(std::chrono::duration_cast<std::chrono::seconds>(dt.time_since_epoch()).count(), z.c_str());
  return Global::duration(offset).count() * 1s;
}

period::period() : months(0), days(0), dur(0s) { }

period::period(int32_t months_p, int32_t days_p, Global::duration dur_p) : 
  months(months_p), days(days_p), dur(dur_p) { }


period::period(const std::string& str) {
  using namespace std::string_literals;
  const char* s = str.c_str();
  const char* e = s + str.size();

  months = 0;
  days   = 0;
  dur    = 0s;

  int n;
  if (s < e && (*s == '/' || (s+2 < e && s[2] == ':'))) goto getduration;
  if (!Global::readNumber(s, e, n, true) || s == e) throw std::range_error("cannot parse period");
  if (*s == 'y') {
    months += 12*n;
    ++s;
    if (s < e) {
      if (*s == '/') goto getduration;
      if (!Global::readNumber(s, e, n, true) || s == e) throw std::range_error("cannot parse period");
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
      if (!Global::readNumber(s, e, n, true) || s == e) throw std::range_error("cannot parse period");      
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
      if (!Global::readNumber(s, e, n, true) || s == e) throw std::range_error("cannot parse period");
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
      if (!Global::readNumber(s, e, n, true) || s == e) throw std::range_error("cannot parse period");
    }
    else {
      return;
    }
  }

  // we've succeeded a Global::readNumber, so this means we've
  // actually read into the duration; so backtrack and use the already
  // existing function to parse a duration:
getduration:
  try {
    dur = from_string(++s);
  }
  catch (...) {
    throw std::range_error("cannot parse period");
  }
}


std::string to_string(const period& p) {
  std::stringstream ss;
  ss << p.getMonths() << "m" << p.getDays() << "d/" << to_string(p.getDuration());
  return ss.str();
}


Global::dtime plus(const Global::dtime& dt, const period& p, const std::string& z) {
  auto offset = getOffsetCnv(dt, z);
  auto res = dt;
  if (p.getMonths()) {
    auto dt_floor = date::floor<date::days>(dt + offset);
    auto timeofday_offset = (dt + offset) - dt_floor;
    auto dt_ymd = date::year_month_day{dt_floor};
    dt_ymd += date::months(p.getMonths());
    res = date::sys_days(dt_ymd) - offset + timeofday_offset;
  }
  offset = getOffsetCnv(dt, z);
  res += p.getDays()*24h;
  res += p.getDuration();
  auto newoffset = getOffsetCnv(res, z);
  if (newoffset != offset) {
    res += offset - newoffset; // adjust for DST or any other event that changed the TZ
  }
  return res;
}

Global::dtime plus(const period& p, const Global::dtime& dt, const std::string& z) {
  return plus(dt, p, z);
}

Global::dtime minus(const Global::dtime& dt, const period& p, const std::string& z) {
  return plus(dt, -p, z);
}


interval plus(const interval& i, const period& p, const std::string& z) {
  return interval(plus(i.s, p, z), plus(i.e, p, z), i.sopen, i.eopen);
}
  
interval plus(const period& p, const interval& i, const std::string& z) {
  return plus(i, p, z);
}

interval minus(const interval& i, const period& p, const std::string& z) {
  return plus(i, -p, z);
}


period operator+(const period& p1, const period& p2) {
  return period(p1.getMonths()+p2.getMonths(), 
                    p1.getDays()+p2.getDays(), 
                    p1.getDuration()+p2.getDuration());
}

period operator-(const period& p) {
  return period(-p.getMonths(), -p.getDays(), -p.getDuration());
}

period operator-(const period& p1, const period& p2) {
  return period(p1.getMonths()-p2.getMonths(), 
                    p1.getDays()-p2.getDays(),
                    p1.getDuration()-p2.getDuration());
}

period operator*(const period& p, double d) {
  return period(p.getMonths()*d, 
                    p.getDays()*d,
                    Global::duration(static_cast<int64_t>(d*p.getDuration().count())*
                                     Global::duration(1)));
}

period operator*(double d, const period& p) {
  return p * d;
}

period operator/(const period& p, double d) {
  if (d == 0) {
    throw std::logic_error("divide by zero");
  }
  return period(p.getMonths()/d, 
                    p.getDays()/d,
                    Global::duration(static_cast<int64_t>(p.getDuration().count()/d)*
                                     Global::duration(1)));
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

bool operator==(const period& p1, const period& p2) {
  return 
    p1.getMonths() == p2.getMonths() && 
    p1.getDays() == p2.getDays() &&
    p1.getDuration() == p2.getDuration();
}

bool operator!=(const period& p1, const period& p2) {
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
  

template <int T, typename U, int SCALSZ>
struct PseudoVector {
  PseudoVector(const Rcpp::Vector<T>& v_p, size_t sz_p=0) : 
    v(v_p), scalar(v.size() == SCALSZ), sz(sz_p) { }
  inline const U& operator[](size_t i) const { return scalar ? v[i%SCALSZ] : v[i]; }
  inline size_t size() const { return scalar ? sz : v.size(); }
  inline bool isScalar() const { return scalar; }
private:
  const Rcpp::Vector<T>& v;
  const bool scalar;
  const size_t sz;
};

const int PRDSZ   = sizeof(period_union)/sizeof(double);
const int INT64SZ = 1;
const int NANOSZ  = 1;

// see Rcpp/inst/include/Rcpp/vector/instantiation.h where NumericVector and al. are defined
typedef PseudoVector<REALSXP, double, INT64SZ> PseudoNumericVectorInt64;
typedef PseudoVector<REALSXP, double, NANOSZ>  PseudoNumericVectorNano;
typedef PseudoVector<REALSXP, double, PRDSZ>   PseudoNumericVectorPrd;


// get the size of the resulting vector (i.e. if we have a scalar on
// one hand and a vector of lenght 10 on the other, the result will be
// of length 10. It's important to know that, both for result vector
// allocation and for the use of 'PseudoVector'.
template <int T, int U, int DIVV=1, int DIVU=1>
size_t get_res_sz(const Rcpp::Vector<T>& v, const Rcpp::Vector<U> u) {
  if (v.size() == 0) return 0;
  else if (v.size()/DIVV == 1) return u.size()/DIVU*DIVV;
  else if (u.size() == 0) return 0;
  else if (u.size()/DIVU == 1) return v.size();
  else if (u.size()/DIVU == v.size()/DIVV) return v.size();
  else ::Rf_error("vector size mismatch"); // what does R message say LLL?
}


RcppExport SEXP period_from_string(SEXP s) {
  try {
    Rcpp::CharacterVector str(s);
    Rcpp::NumericVector res(str.size() * 2);
    for (size_t i=0; i<str.size() * 2; i += 2) {
      period prd(Rcpp::as<std::string>(str[i/2]));
      period_union pu = { prd.getMonths(), prd.getDays(), prd.getDuration().count() };
      res[i] = pu.dbl2.d1;
      res[i+1] = pu.dbl2.d2;
    }
    return res;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP period_to_string(SEXP p) {
  try {
    Rcpp::NumericVector prd(p);
    Rcpp::CharacterVector res(prd.size() / 2);
    for (size_t i=0; i<prd.size(); i += 2) {
      period_union pu;
      pu.dbl2.d1 = prd[i];
      pu.dbl2.d2 = prd[i+1];
      res[i/2] = to_string(*reinterpret_cast<period*>(&pu.prd));
    }
    if (prd.hasAttribute("names")) {
      Rcpp::CharacterVector prdnm(prd.names());
      Rcpp::CharacterVector nm(prdnm.size()/2);
      for (size_t i=0; i<nm.size(); ++i) {
        nm[i] = prdnm[i*2];
      }
      res.names() = nm;
    } 
    return res;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


Rcpp::CharacterVector copyNamesOut(const Rcpp::CharacterVector& nm, int typesz_nm, int typesz_res) {
  if (nm.size() == 0) return nm;
  else {
    Rcpp::CharacterVector res(nm.size() / typesz_nm * typesz_res);
    for (size_t i_res=0, i_nm=0; i_res<res.size(); i_res += typesz_res, i_nm += typesz_nm) {
      res[i_res] = nm[i_nm]; 
    }
    return res;
  }
}


Rcpp::CharacterVector getNames(const Rcpp::CharacterVector& nm1, int typesz1, bool scalar1, 
                               const Rcpp::CharacterVector& nm2, int typesz2, bool scalar2) {
  if      (nm1.size() == 0)      return copyNamesOut(nm2, typesz2, typesz1);
  else if (nm2.size() == 0)      return copyNamesOut(nm1, typesz1, typesz2);
  else if (scalar1 && !scalar2)  return copyNamesOut(nm2, typesz2, typesz1);
  else if (scalar2 && !scalar1)  return copyNamesOut(nm1, typesz1, typesz2);
  else                           return copyNamesOut(nm1, typesz1, typesz2);
}


template <typename OP>
SEXP op_period_period(SEXP e1_p, SEXP e2_p, const OP& op) {
  try {
    const Rcpp::NumericVector e1_nv(e1_p);
    const Rcpp::NumericVector e2_nv(e2_p);
    const PseudoNumericVectorPrd e1_n(e1_nv, get_res_sz<REALSXP,REALSXP,PRDSZ,PRDSZ>(e1_nv, e2_nv));
    const PseudoNumericVectorPrd e2_n(e2_nv, e1_n.size());
    Rcpp::NumericVector res(e1_n.size());
    for (size_t i=0; i<res.size(); i += 2) {
      period_union pu1;
      pu1.dbl2.d1 = e1_n[i];
      pu1.dbl2.d2 = e1_n[i+1];
      period_union pu2;
      pu2.dbl2.d1 = e2_n[i];
      pu2.dbl2.d2 = e2_n[i+1];
      auto prd = op(*reinterpret_cast<period*>(&pu1.prd), *reinterpret_cast<period*>(&pu2.prd));
      period_union prdu;
      memcpy(&prdu, &prd, sizeof(prd)); 
      res[i] = prdu.dbl2.d1;
      res[i+1] = prdu.dbl2.d2;
    }
    auto nm1 = e1_nv.hasAttribute("names") ?
      Rcpp::CharacterVector(e1_nv.names()) : Rcpp::CharacterVector(0);
    auto nm2 = e2_nv.hasAttribute("names") ?
      Rcpp::CharacterVector(e2_nv.names()) : Rcpp::CharacterVector(0);
    auto resnames = getNames(nm1, PRDSZ, e1_n.isScalar(), nm2, PRDSZ, e2_n.isScalar());
    if (resnames.size()) {
      res.names() = resnames;
    }
    Rcpp::CharacterVector cl = Rcpp::CharacterVector::create("period");
    cl.attr("package") = "nanotime";
    res.attr(".S3Class") = "integer64";
    res.attr("class") = cl;
    SET_S4_OBJECT(res);
    return Rcpp::S4(res);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP plus_period_period(SEXP e1_p, SEXP e2_p) {
  return op_period_period(e1_p, e2_p, std::plus<period>());
}


RcppExport SEXP minus_period_period(SEXP e1_p, SEXP e2_p) {
  return op_period_period(e1_p, e2_p, std::minus<period>());
}


template <typename OP>
SEXP op_period_integer64(SEXP e1_p, SEXP e2_p, const OP& op) {
  try {
    const Rcpp::NumericVector e1_nv(e1_p);
    const Rcpp::NumericVector e2_nv(e2_p);
    const PseudoNumericVectorPrd e1_n(e1_nv, get_res_sz<REALSXP,REALSXP,PRDSZ,INT64SZ>(e1_nv, e2_nv));
    const PseudoNumericVectorInt64 
      e2_n(e2_nv, get_res_sz<REALSXP,REALSXP,INT64SZ,PRDSZ>(e2_nv, e1_nv));
    Rcpp::NumericVector res(e1_n.size());
    for (size_t i=0; i<e1_n.size(); i += 2) {
      period_union pu1;
      pu1.dbl2.d1 = e1_n[i];
      pu1.dbl2.d2 = e1_n[i+1];
      pu1.prd.i3 = op(pu1.prd.i3, e2_n[i / 2]);
      period_union prdu;
      memcpy(&res[i], &pu1, sizeof(pu1)); 
    }
    auto nm1 = e1_nv.hasAttribute("names") ?
      Rcpp::CharacterVector(e1_nv.names()) : Rcpp::CharacterVector(0);
    auto nm2 = e2_nv.hasAttribute("names") ?
      Rcpp::CharacterVector(e2_nv.names()) : Rcpp::CharacterVector(0);
    auto resnames = getNames(nm1, PRDSZ, e1_n.isScalar(), nm2, 1, e2_n.isScalar());
    if (resnames.size()) {
      res.names() = resnames;
    }
    Rcpp::CharacterVector cl = Rcpp::CharacterVector::create("period");
    cl.attr("package") = "nanotime";
    res.attr(".S3Class") = "integer64";
    res.attr("class") = cl;
    SET_S4_OBJECT(res);
    return Rcpp::S4(res);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP plus_period_integer64(SEXP e1_p, SEXP e2_p) {
  return op_period_integer64(e1_p, e2_p, std::plus<int64_t>());
}

  
RcppExport SEXP minus_period_integer64(SEXP e1_p, SEXP e2_p) {
  return op_period_integer64(e1_p, e2_p, std::minus<int64_t>());
}

  
template <typename OP>
SEXP op_integer64_period(SEXP e1_p, SEXP e2_p, const OP& op) {
  try {
    const Rcpp::NumericVector e1_nv(e1_p);
    const Rcpp::NumericVector e2_nv(e2_p);
    const PseudoNumericVectorPrd e2_n(e2_nv, get_res_sz<REALSXP,REALSXP,PRDSZ,INT64SZ>(e2_nv, e1_nv));
    const PseudoNumericVectorInt64 
      e1_n(e2_nv, get_res_sz<REALSXP,REALSXP,INT64SZ,PRDSZ>(e1_nv, e2_nv));
    Rcpp::NumericVector res(e2_n.size());
    for (size_t i=0; i<e2_n.size(); i += 2) {
      period_union pu2;
      pu2.dbl2.d1 = e2_n[i];
      pu2.dbl2.d2 = e2_n[i+1];
      pu2.prd.i3 = op(e1_n[i / 2], pu2.prd.i3);
      period_union prdu;
      memcpy(&res[i], &pu2, sizeof(pu2)); 
    }
    auto nm1 = e1_nv.hasAttribute("names") ?
      Rcpp::CharacterVector(e1_nv.names()) : Rcpp::CharacterVector(0);
    auto nm2 = e2_nv.hasAttribute("names") ?
      Rcpp::CharacterVector(e2_nv.names()) : Rcpp::CharacterVector(0);
    auto resnames = getNames(nm1, 1, e1_n.isScalar(), nm2, PRDSZ, e2_n.isScalar());
    if (resnames.size()) {
      res.names() = resnames;
    }
    Rcpp::CharacterVector cl = Rcpp::CharacterVector::create("period");
    cl.attr("package") = "nanotime";
    res.attr(".S3Class") = "integer64";
    res.attr("class") = cl;
    SET_S4_OBJECT(res);
    return Rcpp::S4(res);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP plus_integer64_period(SEXP e1_p, SEXP e2_p) {
  return op_integer64_period(e1_p, e2_p, std::plus<int64_t>());
}

  
RcppExport SEXP minus_integer64_period(SEXP e1_p, SEXP e2_p) {
  return op_integer64_period(e1_p, e2_p, std::minus<int64_t>());
}


template <typename OP>
SEXP op_nanotime_period(SEXP e1_p, SEXP e2_p, SEXP tz_p, const OP& op) {
  try {
    const Rcpp::NumericVector e1_nv(e1_p);
    const Rcpp::NumericVector e2_nv(e2_p);
    const PseudoNumericVectorNano 
      e1_n(e1_nv, get_res_sz<REALSXP,REALSXP,NANOSZ,PRDSZ>(e1_nv, e2_nv));
    const PseudoNumericVectorPrd e2_n(e2_nv, get_res_sz<REALSXP,REALSXP,NANOSZ,PRDSZ>(e2_nv, e1_nv));
    Rcpp::NumericVector res(e1_n.size());
    Rcpp::CharacterVector tz(tz_p);
    for (size_t i=0; i<e1_n.size(); i += 2) {
      period_union pu2;
      pu2.dbl2.d1 = e2_n[i];
      pu2.dbl2.d2 = e2_n[i+1];
      auto dt = op(*reinterpret_cast<const Global::dtime*>(&e1_n[i]),
                   *reinterpret_cast<period*>(&pu2.prd),
                   Rcpp::as<std::string>(tz[0]));
      res[i] = *reinterpret_cast<double*>(&dt);
      Rcpp::Rcout << dt << std::endl;
    }
    auto nm1 = e1_nv.hasAttribute("names") ?
      Rcpp::CharacterVector(e1_nv.names()) : Rcpp::CharacterVector(0);
    auto nm2 = e2_nv.hasAttribute("names") ?
      Rcpp::CharacterVector(e2_nv.names()) : Rcpp::CharacterVector(0);
    auto resnames = getNames(nm1, PRDSZ, e1_n.isScalar(), nm2, PRDSZ, e2_n.isScalar());
    if (resnames.size()) {
      res.names() = resnames;
    }
    return res;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


template<typename T, typename U>
struct pplus {
  constexpr T operator()(const T& t, const U& u, const std::string& tz) const {
    return plus(t, u, tz);
  }
};

RcppExport SEXP plus_nanotime_period(SEXP e1_p, SEXP e2_p, SEXP tz_p) {
  return op_nanotime_period(e1_p, e2_p, tz_p, pplus<Global::dtime, period>());
}


template<class T, typename U>
struct pminus {
  constexpr T operator()(const T& t, const U& u, const std::string& tz) const {
    return minus(t, u, tz);
  }
};

RcppExport SEXP minus_nanotime_period(SEXP e1_p, SEXP e2_p, SEXP tz_p) {
  return op_nanotime_period(e1_p, e2_p, tz_p, pminus<Global::dtime, period>());
}


RcppExport SEXP period_month(SEXP e_p) {
  try {
    const Rcpp::NumericVector e_n(e_p);
    Rcpp::NumericVector res(e_n.size() / 2);
    for (size_t i=0; i<e_n.size(); i += 2) {
      period_union pu;
      pu.dbl2.d1 = e_n[i];
      pu.dbl2.d2 = e_n[i+1];
      res[i/2] = reinterpret_cast<period*>(&pu.prd)->getMonths();
    }
    if (e_n.hasAttribute("names")) {
      res.names() = getNames(Rcpp::CharacterVector(0), PRDSZ, false,
                             Rcpp::CharacterVector(e_n.names()), PRDSZ, false);
    }
    return res;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP period_day(SEXP e_p) {
  try {
    const Rcpp::NumericVector e_n(e_p);
    Rcpp::NumericVector res(e_n.size() / 2);
    for (size_t i=0; i<e_n.size(); i += 2) {
      period_union pu;
      pu.dbl2.d1 = e_n[i];
      pu.dbl2.d2 = e_n[i+1];
      res[i/2] = reinterpret_cast<period*>(&pu.prd)->getDays();
    }
    if (e_n.hasAttribute("names")) {
      res.names() = getNames(Rcpp::CharacterVector(0), PRDSZ, false,
                             Rcpp::CharacterVector(e_n.names()), PRDSZ, false);
    }
    return res;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP period_duration(SEXP e_p) {
  try {
    const Rcpp::NumericVector e_n(e_p);
    Rcpp::NumericVector res(e_n.size() / 2);
    for (size_t i=0; i<e_n.size(); i += 2) {
      period_union pu;
      pu.dbl2.d1 = e_n[i];
      pu.dbl2.d2 = e_n[i+1];
      auto dur = reinterpret_cast<period*>(&pu.prd)->getDuration();
      res[i/2] = *reinterpret_cast<double*>(&dur);
    }
    if (e_n.hasAttribute("names")) {
      res.names() = getNames(Rcpp::CharacterVector(0), PRDSZ, false,
                             Rcpp::CharacterVector(e_n.names()), PRDSZ, false);
    }
    Rcpp::CharacterVector cl = Rcpp::CharacterVector::create("duration");
    cl.attr("package") = "nanotime";
    res.attr(".S3Class") = "integer64";
    res.attr("class") = cl;
    SET_S4_OBJECT(res);
    return Rcpp::S4(res);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


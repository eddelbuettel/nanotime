#include <sstream>
#include <regex>
#include <Rcpp.h>
#include "period.hpp"
#include "duration.hpp"
#include "date.h"
#include "pseudovector.hpp"


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

static inline Global::duration getOffsetCnv(const Global::dtime& dt, const std::string& z) {
  typedef int GET_OFFSET_FUN(long long, const char*); 
  GET_OFFSET_FUN *getOffset = (GET_OFFSET_FUN *) R_GetCCallable("RcppCCTZ", "getOffset" );

  auto offset = getOffset(std::chrono::duration_cast<std::chrono::seconds>(dt.time_since_epoch()).count(), z.c_str());
  return Global::duration(offset).count() * std::chrono::seconds(1);
}

period::period() : months(0), days(0), dur(std::chrono::seconds(0)) { }

period::period(int32_t months_p, int32_t days_p, Global::duration dur_p) : 
  months(months_p), days(days_p), dur(dur_p) { }


period::period(const std::string& str) {
  const char* s = str.c_str();
  const char* e = s + str.size();

  months = 0;
  days   = 0;
  dur    = std::chrono::seconds(0);

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


period plus (const period&    p, Global::duration d) {
  return period(p.getMonths(), p.getDays(), p.getDuration() + d);
}
period plus (Global::duration d, const period& p) {
  return period(p.getMonths(), p.getDays(), p.getDuration() + d);
}
period minus(const period&    p, Global::duration d){
  return period(p.getMonths(), p.getDays(), p.getDuration() - d);
}
period minus(Global::duration d, const period& p) {
  return period(-p.getMonths(), -p.getDays(), -p.getDuration() + d);
}


Global::dtime plus (const Global::dtime& dt, const period& p, const std::string& z) {
  auto res = dt;
  auto offset = getOffsetCnv(res, z);
  if (p.getMonths()) {
    auto dt_floor = date::floor<date::days>(dt + offset);
    auto timeofday_offset = (dt + offset) - dt_floor;
    auto dt_ymd = date::year_month_day{dt_floor};
    dt_ymd += date::months(p.getMonths());
    res = date::sys_days(dt_ymd) - offset + timeofday_offset;
  }
  offset = getOffsetCnv(dt, z);
  res += p.getDays()*std::chrono::hours(24);
  res += p.getDuration();
  auto newoffset = getOffsetCnv(res, z);
  if (newoffset != offset) {
    res += offset - newoffset; // adjust for DST or any other event that changed the TZ
  }
  return res;
}

Global::dtime plus (const period& p, const Global::dtime& dt, const std::string& z) {
  return plus(dt, p, z);
}

Global::dtime minus(const Global::dtime& dt, const period& p, const std::string& z) {
  return plus(dt, -p, z);
}

interval plus(const interval& i, const period& p, const std::string& z) {
  return interval(plus(Global::dtime{Global::duration{i.s}}, p, z),
                  plus(Global::dtime{Global::duration{i.e}}, p, z), i.sopen, i.eopen);
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

template <typename T>
period operator*(const period& p, T d) {
  return period(p.getMonths()*d, 
                p.getDays()*d,
                Global::duration(static_cast<int64_t>(d*p.getDuration().count())*
                                 Global::duration(1)));
}

template <typename T>
period operator/(const period& p, T d) {
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
  

// const int PRDSZ   = sizeof(period_union)/sizeof(double);
// const int INT64SZ = 1;
// const int NANOSZ  = 1;
// const int REALSZ  = 1;

// see Rcpp/inst/include/Rcpp/vector/instantiation.h where NumericVector and al. are defined
typedef ConstPseudoVector<REALSXP, double>   ConstPseudoVectorInt64;
typedef ConstPseudoVector<REALSXP, double>   ConstPseudoVectorNano;
typedef ConstPseudoVector<CPLXSXP, Rcomplex> ConstPseudoVectorPrd;
typedef ConstPseudoVector<REALSXP, double>   ConstPseudoVectorDbl;

typedef PseudoVector<REALSXP, double>   PseudoVectorInt64;
typedef PseudoVector<REALSXP, double>   PseudoVectorNano;
typedef PseudoVector<CPLXSXP, Rcomplex> PseudoVectorPrd;


RcppExport SEXP period_from_string(SEXP s) {
  try {
    Rcpp::CharacterVector str(s);
    Rcpp::ComplexVector res(str.size());
    for (R_xlen_t i=0; i<str.size(); ++i) {
      period prd(Rcpp::as<std::string>(str[i]));
      period_union pu = { prd.getMonths(), prd.getDays(), prd.getDuration().count() };
      res[i] = Rcomplex{pu.dbl2.d1, pu.dbl2.d2 };
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
    Rcpp::ComplexVector prd(p);
    Rcpp::CharacterVector res(prd.size());
    for (R_xlen_t i=0; i<prd.size(); ++i) {
      period pu;
      memcpy(&pu, reinterpret_cast<const char*>(&prd[i]), sizeof(period));
      res[i] = to_string(*reinterpret_cast<period*>(&pu));
    }
    if (prd.hasAttribute("names")) {
      Rcpp::CharacterVector prdnm(prd.names());
      Rcpp::CharacterVector nm(prdnm.size());
      for (R_xlen_t i=0; i<nm.size(); ++i) {
        nm[i] = prdnm[i];
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


RcppExport SEXP period_from_integer64(SEXP s) {
  try {
    Rcpp::NumericVector str(s);
    Rcpp::ComplexVector res(str.size());
    for (R_xlen_t i=0; i<str.size(); ++i) {
      period_union pu = { 0, 0, *reinterpret_cast<std::int64_t*>(&str[i]) };
      res[i] = Rcomplex{pu.dbl2.d1, pu.dbl2.d2 };
    }
    return res;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP period_from_integer(SEXP s) {
  try {
    Rcpp::IntegerVector str(s);
    Rcpp::ComplexVector res(str.size());
    for (R_xlen_t i=0; i<str.size(); ++i) {
      period_union pu = { 0, 0, static_cast<std::int64_t>(str[i]) };
      res[i] = Rcomplex{pu.dbl2.d1, pu.dbl2.d2 };
    }
    return res;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP period_from_double(SEXP s) {
  try {
    Rcpp::NumericVector str(s);
    Rcpp::ComplexVector res(str.size());
    for (R_xlen_t i=0; i<str.size(); ++i) {
      period_union pu = { 0, 0, static_cast<std::int64_t>(str[i]) };
      res[i] = Rcomplex{pu.dbl2.d1, pu.dbl2.d2 };
    }
    return res;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


Rcpp::CharacterVector copyNamesOut(const Rcpp::CharacterVector& nm) {
  if (nm.size() == 0) return nm;
  else {
    Rcpp::CharacterVector res(nm.size());
    for (R_xlen_t i=0; i<res.size(); ++i) {
      res[i] = nm[i]; 
    }
    return res;
  }
}



static Rcpp::CharacterVector getNames(const Rcpp::CharacterVector& nm1, bool scalar1, 
                                      const Rcpp::CharacterVector& nm2, bool scalar2) {
  if      (nm1.size() == 0)      return copyNamesOut(nm2);
  else if (nm2.size() == 0)      return copyNamesOut(nm1);
  else if (scalar1 && !scalar2)  return copyNamesOut(nm2);
  else if (scalar2 && !scalar1)  return copyNamesOut(nm1);
  else                           return copyNamesOut(nm1);
}


template <int T1, int T2, int T3>
void copyNames(const Rcpp::Vector<T1>& e1_cv,
               const Rcpp::Vector<T2>& e2_cv,
               bool isScalar_e1,
               bool isScalar_e2,
               Rcpp::Vector<T3>& res) {
  auto nm1 = e1_cv.hasAttribute("names") ?
    Rcpp::CharacterVector(e1_cv.names()) : Rcpp::CharacterVector(0);
  auto nm2 = e2_cv.hasAttribute("names") ?
    Rcpp::CharacterVector(e2_cv.names()) : Rcpp::CharacterVector(0);
  auto resnames = getNames(nm1, isScalar_e1,
                           nm2, isScalar_e2);
  if (resnames.size()) {
    res.names() = resnames;
  }
}


template<int R>
static SEXP assignS4(const char* classname, Rcpp::Vector<R>& res) {
  Rcpp::CharacterVector cl = Rcpp::CharacterVector::create(classname);
  cl.attr("package") = "nanotime";
  res.attr("class") = cl;
  SET_S4_OBJECT(res);
  return Rcpp::S4(res);
}


RcppExport SEXP plus_period_period(SEXP e1_p, SEXP e2_p) {
  try {
    const Rcpp::ComplexVector e1_nv(e1_p);
    const Rcpp::ComplexVector e2_nv(e2_p);
    const ConstPseudoVectorPrd e1_n(e1_nv);
    const ConstPseudoVectorPrd e2_n(e2_nv);
    Rcpp::ComplexVector res(std::max(e1_nv.size(), e2_nv.size()));
    PseudoVectorPrd pres(res);
    for (R_xlen_t i=0; i<res.size(); ++i) {
      period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
      period pu2; memcpy(&pu2, reinterpret_cast<const char*>(&e2_n[i]), sizeof(period));
      auto prd = pu1 + pu2;
      memcpy(&pres[i], &prd, sizeof(prd)); 
    }
    copyNames(e1_nv, e2_nv, e1_n.isScalar(), e2_n.isScalar(), res);    
    return assignS4("period", res);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP minus_period_period(SEXP e1_p, SEXP e2_p) {
  try {
    const Rcpp::ComplexVector e1_cv(e1_p);
    const Rcpp::ComplexVector e2_cv(e2_p);
    const ConstPseudoVectorPrd e1_n(e1_cv);
    const ConstPseudoVectorPrd e2_n(e2_cv);
    Rcpp::ComplexVector res(std::max(e1_cv.size(), e2_cv.size()));
    PseudoVectorPrd pres(res); // wrap it to get correct indexing
    for (R_xlen_t i=0; i<res.size(); ++i) {
      period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
      period pu2; memcpy(&pu2, reinterpret_cast<const char*>(&e2_n[i]), sizeof(period));
      auto prd = pu1 - pu2;
      memcpy(&pres[i], reinterpret_cast<const char*>(&prd), sizeof(prd)); 
    }
    copyNames(e1_cv, e2_cv, e1_n.isScalar(), e2_n.isScalar(), res);    
    return assignS4("period", res);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


template <typename OP>
SEXP compare_period_period(SEXP e1_p, SEXP e2_p, const OP& op) {
  try {
    const Rcpp::ComplexVector  e1_cv(e1_p);
    const Rcpp::ComplexVector  e2_cv(e2_p);
    const ConstPseudoVectorPrd e1_n(e1_cv);
    const ConstPseudoVectorPrd e2_n(e2_cv);
    Rcpp::LogicalVector res(std::max(e1_n.size(), e2_n.size()));
    for (R_xlen_t i=0; i<res.size(); ++i) {
      period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
      period pu2; memcpy(&pu2, reinterpret_cast<const char*>(&e2_n[i]), sizeof(period));
      res[i] = op(pu1, pu2);
    }
    copyNames(e1_cv, e2_cv, e1_n.isScalar(), e2_n.isScalar(), res);    
    return res;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}

RcppExport SEXP eq_period_period(SEXP e1_p, SEXP e2_p) {
  return compare_period_period(e1_p, e2_p, std::equal_to<period>());
}

RcppExport SEXP ne_period_period(SEXP e1_p, SEXP e2_p) {
  return compare_period_period(e1_p, e2_p, std::not_equal_to<period>());
}


RcppExport SEXP plus_period_integer64(SEXP e1_p, SEXP e2_p) {
  try {
    const Rcpp::ComplexVector e1_cv(e1_p);
    const Rcpp::NumericVector e2_nv(e2_p);
    const ConstPseudoVectorPrd   e1_n(e1_cv);
    const ConstPseudoVectorInt64 e2_n(e2_nv);
    Rcpp::ComplexVector res(std::max(e1_n.size(), e2_n.size()));
    PseudoVectorPrd pres(res);
    for (size_t i=0; i<pres.size(); ++i) {
      period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
      Global::duration dur; memcpy(&dur, reinterpret_cast<const char*>(&e2_n[i]), sizeof(dur));
      pu1.setDuration(pu1.getDuration() + dur);
      memcpy(&pres[i], &pu1, sizeof(pu1)); 
    }
    copyNames(e1_cv, e2_nv, e1_n.isScalar(), e2_n.isScalar(), res);    
    return assignS4("period", res);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP minus_period_integer64(SEXP e1_p, SEXP e2_p) {
  try {
    const Rcpp::ComplexVector e1_cv(e1_p);
    const Rcpp::NumericVector e2_nv(e2_p);
    const ConstPseudoVectorPrd   e1_n(e1_cv);
    const ConstPseudoVectorInt64 e2_n(e2_nv);
    Rcpp::ComplexVector res(std::max(e1_n.size(), e2_n.size()));
    PseudoVectorPrd pres(res);
    for (size_t i=0; i<pres.size(); ++i) {
      period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
      Global::duration dur; memcpy(&dur, reinterpret_cast<const char*>(&e2_n[i]), sizeof(dur));
      pu1.setDuration(pu1.getDuration() - dur);
      memcpy(&pres[i], &pu1, sizeof(pu1)); 
    }
    copyNames(e1_cv, e2_nv, e1_n.isScalar(), e2_n.isScalar(), res);    
    return assignS4("period", res);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP multiplies_period_integer64(SEXP e1_p, SEXP e2_p) {
  try {
    const Rcpp::ComplexVector e1_cv(e1_p);
    const Rcpp::NumericVector e2_nv(e2_p);
    const ConstPseudoVectorPrd   e1_n(e1_cv);
    const ConstPseudoVectorInt64 e2_n(e2_nv);
    Rcpp::ComplexVector res(std::max(e1_n.size(), e2_n.size()));
    PseudoVectorPrd pres(res);
    for (size_t i=0; i<pres.size(); ++i) {
      period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
      uint64_t m; memcpy(&m, reinterpret_cast<const char*>(&e2_n[i]), sizeof(m));
      pu1 = pu1 * m;
      memcpy(&pres[i], &pu1, sizeof(pu1)); 
    }
    copyNames(e1_cv, e2_nv, e1_n.isScalar(), e2_n.isScalar(), res);    
    return assignS4("period", res);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP divides_period_integer64(SEXP e1_p, SEXP e2_p) {
  try {
    const Rcpp::ComplexVector    e1_cv(e1_p);
    const Rcpp::NumericVector    e2_nv(e2_p);
    const ConstPseudoVectorPrd   e1_n(e1_cv);
    const ConstPseudoVectorInt64 e2_n(e2_nv);
    Rcpp::ComplexVector res(std::max(e1_n.size(), e2_n.size()));
    PseudoVectorPrd pres(res);
    for (size_t i=0; i<pres.size(); ++i) {
      period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
      uint64_t m; memcpy(&m,   reinterpret_cast<const char*>(&e2_n[i]), sizeof(m));
      pu1 = pu1 / m;
      memcpy(&pres[i], &pu1, sizeof(pu1)); 
    }
    copyNames(e1_cv, e2_nv, e1_n.isScalar(), e2_n.isScalar(), res);    
    return assignS4("period", res);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP multiplies_period_double(SEXP e1_p, SEXP e2_p) {
  try {
    const Rcpp::ComplexVector  e1_cv(e1_p);
    const Rcpp::NumericVector  e2_nv(e2_p);
    const ConstPseudoVectorPrd e1_n(e1_cv);
    const ConstPseudoVectorDbl e2_n(e2_nv);
    Rcpp::ComplexVector res(std::max(e1_n.size(), e2_n.size()));
    PseudoVectorPrd pres(res);
    for (size_t i=0; i<pres.size(); ++i) {
      period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
      double m;   memcpy(&m,   reinterpret_cast<const char*>(&e2_n[i]), sizeof(m));
      pu1 = pu1 * m;
      memcpy(&pres[i], &pu1, sizeof(pu1)); 
    }
    copyNames(e1_cv, e2_nv, e1_n.isScalar(), e2_n.isScalar(), res);    
    return assignS4("period", res);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP divides_period_double(SEXP e1_p, SEXP e2_p) {
  try {
    const Rcpp::ComplexVector  e1_cv(e1_p);
    const Rcpp::NumericVector  e2_nv(e2_p);
    const ConstPseudoVectorPrd e1_n(e1_cv);
    const ConstPseudoVectorDbl e2_n(e2_nv);
    Rcpp::ComplexVector res(std::max(e1_n.size(), e2_n.size()));
    PseudoVectorPrd pres(res);
    for (size_t i=0; i<pres.size(); ++i) {
      period pu1; memcpy(&pu1, reinterpret_cast<const char*>(&e1_n[i]), sizeof(period));
      double m;   memcpy(&m,   reinterpret_cast<const char*>(&e2_n[i]), sizeof(m));
      pu1 = pu1 / m;
      memcpy(&pres[i], &pu1, sizeof(pu1)); 
    }
    copyNames(e1_cv, e2_nv, e1_n.isScalar(), e2_n.isScalar(), res);    
    return assignS4("period", res);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP minus_integer64_period(SEXP e1_p, SEXP e2_p) {
  try {
    const Rcpp::NumericVector    e1_nv(e1_p);
    const Rcpp::ComplexVector    e2_cv(e2_p);
    const ConstPseudoVectorInt64 e1_n(e1_nv);
    const ConstPseudoVectorPrd   e2_n(e2_cv);
    Rcpp::ComplexVector res(std::max(e1_n.size(), e2_n.size()));
    PseudoVectorPrd pres(res);
    for (size_t i=0; i<pres.size(); ++i) {
      period pu2;           memcpy(&pu2, reinterpret_cast<const char*>(&e2_n[i]), sizeof(pu2));
      Global::duration dur; memcpy(&dur, reinterpret_cast<const char*>(&e1_n[i]), sizeof(dur));
      pu2 = minus(dur, pu2);
      memcpy(&pres[i], &pu2, sizeof(pu2));
    }
    copyNames(e1_nv, e2_cv, e1_n.isScalar(), e2_n.isScalar(), res);    
    return assignS4("period", res);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP plus_nanotime_period(SEXP e1_p, SEXP e2_p, SEXP tz_p) {
  try {
    const Rcpp::NumericVector e1_nv(e1_p);
    const Rcpp::ComplexVector e2_cv(e2_p);
    const ConstPseudoVectorNano e1_n(e1_nv);
    const ConstPseudoVectorPrd  e2_n(e2_cv);
    Rcpp::ComplexVector res(std::max(e1_n.size(), e2_n.size()));
    PseudoVectorPrd pres(res);
    const Rcpp::CharacterVector tz(tz_p);
    for (size_t i=0; i<pres.size(); ++i) {
      Global::dtime nano; memcpy(&nano, reinterpret_cast<const char*>(&e1_n[i]), sizeof(nano));
      period prd; memcpy(&prd, reinterpret_cast<const char*>(&e2_n[i]), sizeof(prd));      
      auto dt = plus(nano, prd, Rcpp::as<std::string>(tz[i]));
      memcpy(&pres[i], &dt, sizeof(dt));
    }
    copyNames(e1_nv, e2_cv, e1_n.isScalar(), e2_n.isScalar(), res);    
    return assignS4("nanotime", res);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP minus_nanotime_period(SEXP e1_p, SEXP e2_p, SEXP tz_p) {
  try {
    const Rcpp::NumericVector   e1_nv(e1_p);
    const Rcpp::ComplexVector   e2_cv(e2_p);
    const ConstPseudoVectorNano e1_n(e1_nv);
    const ConstPseudoVectorPrd  e2_n(e2_cv);
    Rcpp::ComplexVector res(std::max(e1_n.size(), e2_n.size()));
    PseudoVectorPrd pres(res);
    const Rcpp::CharacterVector tz(tz_p);
    for (size_t i=0; i<pres.size(); ++i) {
      Global::dtime nano; memcpy(&nano, reinterpret_cast<const char*>(&e1_n[i]), sizeof(nano));
      period prd; memcpy(&prd, reinterpret_cast<const char*>(&e2_n[i]), sizeof(prd));      
      auto dt = minus(nano, prd, Rcpp::as<std::string>(tz[i % tz.size()]));
      memcpy(&pres[i], &dt, sizeof(dt));
    }
    copyNames(e1_nv, e2_cv, e1_n.isScalar(), e2_n.isScalar(), res);    
    return assignS4("nanotime", res);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP period_month(SEXP e_p) {
  try {
    const Rcpp::ComplexVector e_n(e_p);
    Rcpp::NumericVector res(e_n.size());
    for (R_xlen_t i=0; i<e_n.size(); ++i) {
      period prd; memcpy(&prd, reinterpret_cast<const char*>(&e_n[i]), sizeof(period));
      res[i] = prd.getMonths();
    }
    if (e_n.hasAttribute("names")) {
      res.names() = getNames(Rcpp::CharacterVector(0), false,
                             Rcpp::CharacterVector(e_n.names()), false);
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
    const Rcpp::ComplexVector e_n(e_p);
    Rcpp::NumericVector res(e_n.size());
    for (R_xlen_t i=0; i<e_n.size(); ++i) {
      period prd; memcpy(&prd, reinterpret_cast<const char*>(&e_n[i]), sizeof(period));
      res[i] = prd.getDays();
    }
    if (e_n.hasAttribute("names")) {
      res.names() = getNames(Rcpp::CharacterVector(0), false,
                             Rcpp::CharacterVector(e_n.names()), false);
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
    const Rcpp::ComplexVector e_n(e_p);
    Rcpp::NumericVector res(e_n.size());
    for (R_xlen_t i=0; i<e_n.size(); ++i) {
      period prd; memcpy(&prd, reinterpret_cast<const char*>(&e_n[i]), sizeof(period));
      auto dur = prd.getDuration();
      memcpy(&res[i], &dur, sizeof(dur));
    }
    if (e_n.hasAttribute("names")) {
      res.names() = getNames(Rcpp::CharacterVector(0), false,
                             Rcpp::CharacterVector(e_n.names()), false);
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


#include <sstream>
#include <regex>
#include <Rcpp.h>
#include "period.hpp"
#include "duration.hpp"
#include "date.h"


extern "C" int getOffset(long long s, const char* tzstr);

static inline Global::duration getOffsetCnv(const Global::dtime& dt, const std::string& z) {
  typedef int SUBSET_DT_FUN(long long, const char*); 
  SUBSET_DT_FUN *subsetDT = (SUBSET_DT_FUN *) R_GetCCallable("rcppcctz", "getOffset" );

  return Global::duration(subsetDT(dt.time_since_epoch().count(), z.c_str()));
}

using namespace std::chrono_literals;

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
  int pos = -1;
  int direction = 1;            // unimportant, it's a fresh start
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
  direction = res >= dt ? 1 : -1;
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
  

RcppExport SEXP period_from_string(SEXP s) {
  try {
    Rcpp::CharacterVector str(s);
    Rcpp::NumericVector res(str.size() * 2);
    for (size_t i=0; i<str.size() * 2; i += 2) {
      period prd(Rcpp::as<std::string>(str[i]));
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
      res[i] = to_string(*reinterpret_cast<period*>(&pu.prd));
    }
    return res;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}

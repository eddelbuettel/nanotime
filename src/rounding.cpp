#include <Rcpp.h>
#include "nanotime/period.hpp"
#include "nanotime/utilities.hpp"


using namespace nanotime;


// import function from 'RcppCCTZ':
static inline duration getOffsetCnv(const dtime& dt, const std::string& z) {
  typedef int GET_OFFSET_FUN(long long, const char*, int&); 
  GET_OFFSET_FUN *getOffset = (GET_OFFSET_FUN *) R_GetCCallable("RcppCCTZ", "_RcppCCTZ_getOffset_nothrow" );

  int offset;
  int res = getOffset(std::chrono::duration_cast<std::chrono::seconds>(dt.time_since_epoch()).count(), z.c_str(), offset);
  if (res < 0) {
    Rcpp::stop("Cannot retrieve timezone '%s'.", z.c_str());
  }
  return duration(offset).count() * std::chrono::seconds(1);
}


// C++-level floor and ceiling functions:

// support for rounding 'dtime':
enum class RoundingPrecision : uint64_t { NANO, MICRO, MILLI, SECOND, MINUTE, HOUR, DAY, WEEK, MONTH, YEAR };


static bool isMultipleOf(duration d1, duration d2) {
  if (d2.count() % d1.count() == 0) {                // d1 strictly positive
    return true;
  } else {
    return false;
  }
}


static bool isMultipleOf(const period& p1, const period& p2) {
  // the multiple is considered at the month level, the day level and the duration level;
  // we know going in here that p2 was artifically generated to have only one of the three portions non-zero:
  if (p1.getMonths()) {
    if (p2.getMonths() && !p2.getDays() && p2.getDuration() == duration::zero()) {
      return p2.getMonths() % p1.getMonths() == 0;
    }
  } else if (p1.getDays()) {    // for function completeness as we never execute this branch in this setting: #nocov start
    if (!p2.getMonths() && p2.getDays() && p2.getDuration() == duration::zero()) {
      return p2.getDays() % p1.getDays() == 0;
    }
  } else if (p1.getDuration() != duration::zero()) {  // for function completeness as we never execute this branch in this setting
    if (!p2.getMonths() && !p2.getDays() && p2.getDuration() == duration::zero()) {
      return isMultipleOf(p1.getDuration(), p2.getDuration());
    }
  }

  return false; // for function completeness as we never execute this branch in this setting #nocov end
}


static RoundingPrecision selectPrecision(const duration d) {
  if      (d < std::chrono::microseconds{1})
    return isMultipleOf(d, std::chrono::microseconds{1}) ? RoundingPrecision::MICRO : RoundingPrecision::NANO;
  else if (d < std::chrono::milliseconds{1})
    return isMultipleOf(d, std::chrono::milliseconds{1}) ? RoundingPrecision::MILLI : RoundingPrecision::MICRO;
  else if (d < std::chrono::seconds{1})
    return isMultipleOf(d, std::chrono::seconds{1}) ? RoundingPrecision::SECOND : RoundingPrecision::MILLI;
  else if (d < std::chrono::minutes{1})
    return isMultipleOf(d, std::chrono::minutes{1}) ? RoundingPrecision::MINUTE : RoundingPrecision::SECOND;
  else if (d < std::chrono::hours{1})
    return isMultipleOf(d, std::chrono::hours{1}) ? RoundingPrecision::HOUR : RoundingPrecision::MINUTE;
  else
    return RoundingPrecision::HOUR;
}


static RoundingPrecision selectPrecision(const period p) {
  const auto year = period(12, 0, duration::zero());
  if       (p.getMonths() >= 1)
    return isMultipleOf(p, year) ? RoundingPrecision::YEAR : RoundingPrecision::MONTH;
  else if  (p.getDays() >= 1)
    return RoundingPrecision::DAY;
  else if  (p.getDuration() >= std::chrono::hours{1})
    return (isMultipleOf(p.getDuration(), std::chrono::hours{24})) ? RoundingPrecision::DAY : selectPrecision(p.getDuration());
  else
    return selectPrecision(p.getDuration());
}


static dtime floor(dtime t, RoundingPrecision p) {
  using namespace std::chrono;
  if (t.time_since_epoch().count() >= 0) {
    switch (p) {
    case RoundingPrecision::HOUR: // not reachable in this context #nocov
      return time_point_cast<nanotime::duration>(time_point_cast<hours>(t)); // not reachable in this context #nocov
    case RoundingPrecision::MINUTE:
      return time_point_cast<nanotime::duration>(time_point_cast<minutes>(t));
    case RoundingPrecision::SECOND:
      return time_point_cast<nanotime::duration>(time_point_cast<seconds>(t));
    case RoundingPrecision::MILLI:
      return time_point_cast<nanotime::duration>(time_point_cast<milliseconds>(t));
    case RoundingPrecision::MICRO:
      return time_point_cast<nanotime::duration>(time_point_cast<microseconds>(t));
    case RoundingPrecision::NANO:
      return time_point_cast<nanotime::duration>(time_point_cast<nanoseconds>(t));
    default:                                            // not reachable in this context #nocov
      throw std::out_of_range("unknown rounding type"); // not reachable in this context #nocov
    }
  }
  else {
    switch (p) {
    case RoundingPrecision::HOUR: // not reachable in this context #nocov
      return time_point_cast<nanotime::duration>(time_point_cast<hours>(t) - hours{1}); // not reachable in this context #nocov
    case RoundingPrecision::MINUTE:
      return time_point_cast<nanotime::duration>(time_point_cast<minutes>(t) - minutes{1});
    case RoundingPrecision::SECOND:
      return time_point_cast<nanotime::duration>(time_point_cast<seconds>(t) - seconds{1});
    case RoundingPrecision::MILLI:
      return time_point_cast<nanotime::duration>(time_point_cast<milliseconds>(t) - milliseconds{1});
    case RoundingPrecision::MICRO:
      return time_point_cast<nanotime::duration>(time_point_cast<microseconds>(t) - microseconds{1});
    case RoundingPrecision::NANO:
      return time_point_cast<nanotime::duration>(time_point_cast<nanoseconds>(t));
    default:                                            // not reachable in this context #nocov
      throw std::out_of_range("unknown rounding type"); // not reachable in this context #nocov
    }  
  }
}


static dtime floor_tz(const dtime t, RoundingPrecision p, const std::string& z) {
  using namespace std::chrono;
  switch (p) {
  case RoundingPrecision::HOUR: {
    auto t_offset = t + getOffsetCnv(t, z.c_str());
    auto t_hours = time_point_cast<nanotime::duration>(time_point_cast<hours>(t_offset));
    if (t.time_since_epoch() < nanotime::duration::zero() && t_hours > t_offset) {
      t_hours -= hours{1};
    }
    return t_hours - getOffsetCnv(t_hours, z.c_str());
  }
  case RoundingPrecision::DAY: {
    auto t_days = date::floor<date::days>(t + getOffsetCnv(t, z.c_str()));
    return t_days - getOffsetCnv(t_days, z.c_str());
  }
  case RoundingPrecision::MONTH: {
    auto t_days = date::floor<date::days>(t + getOffsetCnv(t, z.c_str()));
    auto ymd = date::year_month_day(t_days);
    t_days = date::sys_days(ymd.year()/ymd.month()/date::day(1));
    return t_days - getOffsetCnv(t_days, z.c_str());
  }
  case RoundingPrecision::YEAR: {
    auto t_days = date::floor<date::days>(t + getOffsetCnv(t, z.c_str()));
    auto ymd = date::year_month_day(t_days);
    t_days = date::sys_days(ymd.year()/date::month(1)/date::day(1));
    return t_days - getOffsetCnv(t_days, z.c_str());
  }
  default:
    return floor(t, p);
  }
}


static const std::vector<dtime> makegrid(const dtime start,
                                         bool absolute_start,          // is start absolute (e.g no rounding)
                                         const dtime end,
                                         const period p,
                                         const std::string& tz) {
  const auto precision = selectPrecision(p);
  const auto start_0   = absolute_start ? start : floor_tz(start, precision, tz);
  const auto end_0     = plus(end, p, tz);

  std::vector<dtime> res;
  auto c = start_0;
  for (; c < start_0; c = plus(c, p, tz)) { }

  for(; c <= end_0; c = plus(c, p, tz)) {
    res.push_back(c);
  }
  return res;
}


static void ceilingtogrid(const dtime* dt, const uint64_t n_dt, const std::vector<dtime>& grid, dtime* res) {
  if (grid.size() <= 1) {
    throw std::range_error("ceilingtogrid: invalid 'grid' argument"); // not reachable in this context #nocov
  }

  size_t iy = 0;

  for (size_t ix=0; ix < n_dt; ++ix) {
    while (dt[ix] > grid[iy]) ++iy;
    res[ix] = grid[iy];         // this is safe by grid construction
  }
}


// this is really the same as 
static void floortogrid(const dtime* dt, const uint64_t n_dt, const std::vector<dtime>& grid, dtime* res) {
  if (grid.size() <= 1) {
    throw std::range_error("floortogrid: invalid 'grid' argument"); // not reachable in this context #nocov
  }
  
  size_t iy = 1;

  for (size_t ix=0; ix < n_dt; ++ix) {
    while (dt[ix] >= grid[iy]) ++iy;
    res[ix] = grid[iy-1];      // this is safe by grid construction
  }
}


// [[Rcpp::export]]
Rcpp::NumericVector ceiling_tz_impl(const Rcpp::NumericVector&   nt_v,      // vector of 'nanotime'
                                    const Rcpp::ComplexVector&   prd_v,     // scalar period
                                    const Rcpp::NumericVector&   orig_v,    // origin                                    
                                    const Rcpp::CharacterVector& tz_v) {    // scalar timezone
  // check tz and orig are scalar:
  if (orig_v.size() > 1) {
    Rcpp::stop("'origin' must be scalar");
  }
  if (tz_v.size() > 1) {
    Rcpp::stop("'tz' must be scalar");
  }

  period prd; memcpy(&prd, reinterpret_cast<const char*>(&prd_v[0]), sizeof(period));
  const auto tz = Rcpp::as<std::string>(tz_v[0]);

  // period must be strictly positive
  if ((prd.getMonths() < 0 || prd.getDays() < 0 || prd.getDuration() < duration::zero()) ||
      prd == period{0, 0, duration::zero()}) {
    Rcpp::stop("'precision' must be strictly positive");
  }

  const dtime* dt = reinterpret_cast<const dtime*>(&nt_v[0]);
  
  dtime origin;
  if (orig_v.size()) {
    origin = *reinterpret_cast<const dtime*>(&orig_v[0]);
    if (dt[0] > plus(origin, prd, tz)) {
      Rcpp::stop("when specifying 'origin', the first interval must contain at least one observation");
    }
  }
  
  const auto grid = orig_v.size() ?
    makegrid(origin, true,  dt[nt_v.size()-1], prd, tz) :
    makegrid(dt[0],  false, dt[nt_v.size()-1], prd, tz);

  Rcpp::NumericVector res(nt_v.size());
  auto res_dt = reinterpret_cast<dtime*>(&res[0]);
  
  ceilingtogrid(dt, nt_v.size(), grid, res_dt);

  return assignS4("nanotime", res, "integer64");
}


// [[Rcpp::export]]
Rcpp::NumericVector ceiling_impl(const Rcpp::NumericVector& nt_v,      // vector of 'nanotime'
                                 const Rcpp::NumericVector& dur_v,     // scalar duration
                                 const Rcpp::NumericVector& orig_v) {  // scalar origin

  // check orig is scalar:
  if (orig_v.size() > 1) {
    Rcpp::stop("'origin' must be scalar");
  }

  int64_t dur; memcpy(&dur, reinterpret_cast<const char*>(&dur_v[0]), sizeof(int64_t));

  // duration must be strictly positive
  if (dur < 0) {
    Rcpp::stop("'precision' must be strictly positive");
  }

  const auto* dt = reinterpret_cast<const int64_t*>(&nt_v[0]);
  Rcpp::NumericVector res(nt_v.size());
  auto res_dur = reinterpret_cast<int64_t*>(&res[0]);
  const auto origin = orig_v.size() ? *reinterpret_cast<const int64_t*>(&orig_v[0]) : 0;

  for (R_xlen_t i=0; i < res.size(); ++i) {
    res_dur[i] = ((dt[i] - origin) / dur) * dur + origin;
    if (res_dur[i] > 0 && res_dur[i] < dt[i]) { // round up
      res_dur[i] += dur;
    }
  }  
    
  return assignS4("nanotime", res, "integer64");
  return res;
}

 
// [[Rcpp::export]]
Rcpp::NumericVector floor_tz_impl(const Rcpp::NumericVector&   nt_v,      // vector of 'nanotime'
                                  const Rcpp::ComplexVector&   prd_v,     // scalar period
                                  const Rcpp::NumericVector&   orig_v,    // origin
                                  const Rcpp::CharacterVector& tz_v) {    // scalar timezone
  // check tz and orig are scalar:
  if (orig_v.size() > 1) {
    Rcpp::stop("'origin' must be scalar");
  }
  if (tz_v.size() > 1) {
    Rcpp::stop("'tz' must be scalar");
  }

  const auto tz = Rcpp::as<std::string>(tz_v[0]);
  period prd; memcpy(&prd, reinterpret_cast<const char*>(&prd_v[0]), sizeof(period));

  // period must be strictly positive
  if ((prd.getMonths() < 0 || prd.getDays() < 0 || prd.getDuration() < duration::zero()) ||
      prd == period{0, 0, duration::zero()}) {
    Rcpp::stop("'precision' must be strictly positive");
  }

  const auto dt = reinterpret_cast<const dtime*>(&nt_v[0]);
  
  dtime origin;
  if (orig_v.size()) {
    origin = *reinterpret_cast<const dtime*>(&orig_v[0]);
    if (dt[0] > plus(origin, prd, tz)) {
      Rcpp::stop("when specifying 'origin', the first interval must contain at least one observation");
    }
  }
  

  // additionally if origin is supplied, verify it's not more than one interval before the first observation LLL
  
  const auto grid = orig_v.size() ?
    makegrid(origin, true,  dt[nt_v.size()-1], prd, tz) :
    makegrid(dt[0],  false, dt[nt_v.size()-1], prd, tz);

  Rcpp::NumericVector res(nt_v.size());
  auto res_dt = reinterpret_cast<dtime*>(&res[0]);
  
  floortogrid(dt, nt_v.size(), grid, res_dt);

  return assignS4("nanotime", res, "integer64");
}


// [[Rcpp::export]]
Rcpp::NumericVector floor_impl(const Rcpp::NumericVector& nt_v,      // vector of 'nanotime'
                               const Rcpp::NumericVector& dur_v,     // scalar duration
                               const Rcpp::NumericVector& orig_v) {  // origin

  // check orig are scalar:
  if (orig_v.size() > 1) Rcpp::stop("'origin' must be scalar");

  int64_t dur; memcpy(&dur, reinterpret_cast<const char*>(&dur_v[0]), sizeof(int64_t));

  // duration must be strictly positive
  if (dur < 0) {
    Rcpp::stop("'precision' must be strictly positive");
  }

  const auto* dt = reinterpret_cast<const int64_t*>(&nt_v[0]);
  Rcpp::NumericVector res(nt_v.size());
  auto res_dur = reinterpret_cast<int64_t*>(&res[0]);
  const auto origin = orig_v.size() ? *reinterpret_cast<const int64_t*>(&orig_v[0]) : 0;

  for (R_xlen_t i=0; i < res.size(); ++i) {
    res_dur[i] = ((dt[i] - origin) / dur) * dur + origin;
    if (res_dur[i] < 0 && res_dur[i] > dt[i]) {
      res_dur[i] -= dur;
    }
  }  
  
  return assignS4("nanotime", res, "integer64");
}

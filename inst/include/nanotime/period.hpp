#ifndef NANOTIME_PERIOD_HPP
#define NANOTIME_PERIOD_HPP


#include "globals.hpp"
#include "interval.hpp"
#include <RcppCCTZ_API.h>


namespace nanotime {

  inline duration getOffsetCnv(const dtime& dt, const std::string& z) {
    int offset;
    int res = RcppCCTZ::getOffset(std::chrono::duration_cast<std::chrono::seconds>(dt.time_since_epoch()).count(), z.c_str(), offset);
    if (res < 0) {
      Rcpp::stop("Cannot retrieve timezone '%s'.", z.c_str()); // ## nocov
    }

    return duration(offset).count() * std::chrono::seconds(1);
  }


  struct period {
    typedef int32_t month_t ;
    typedef int32_t day_t;

    period() : months(0), days(0), dur(std::chrono::seconds(0)) { }
    period(int32_t months_p, int32_t days_p, duration dur_p) : months(months_p), days(days_p), dur(dur_p) {
      // we have to ensure that NA is uniform across all construction
      // possibilities so that functions that compare equality do so
      // correctly:
      if (months == std::numeric_limits<int32_t>::min() ||
          days   == std::numeric_limits<int32_t>::min() ||
          dur    == duration::min()) {
        months = std::numeric_limits<int32_t>::min();
        days = std::numeric_limits<int32_t>::min();
        dur = duration::zero();
      }
    }
    period(const std::string& s);

    inline month_t getMonths() const { return months; }
    inline day_t   getDays()   const { return days; }
    inline duration getDuration() const { return dur; }
    inline void setMonths(int64_t m) { months = m;  }
    inline void setDays(int64_t d)   { days   = d;  }
    inline void setDuration(duration d)    { dur = d;  }
    inline void addMonths(int64_t m) { months += m; }
    inline void addDays(int64_t d)   { days   += d; }
    inline void addDuration(duration d) { dur    += d; }

    inline bool operator==(const period& p) { return months==p.months && days==p.days && dur==p.dur; }
    inline bool operator!=(const period& p) { return months!=p.months || days!=p.days || dur!=p.dur; }

    inline bool isNA() const { return months == std::numeric_limits<int32_t>::min() || dur == duration::min(); }
  
  private:
    month_t months;
    day_t   days;
    duration dur;
  };



  inline period operator-(const period& p) {
    return period(-p.getMonths(), -p.getDays(), -p.getDuration());
  }
  inline period operator+(const period& p1, const period& p2) {
    return period(p1.getMonths()+p2.getMonths(), 
                  p1.getDays()+p2.getDays(), 
                  p1.getDuration()+p2.getDuration());
  }
  inline period operator-(const period& p1, const period& p2) {
    return period(p1.getMonths()-p2.getMonths(), 
                  p1.getDays()-p2.getDays(),
                  p1.getDuration()-p2.getDuration());
  }

  
  inline period plus (const period& p, duration d) {
    return period(p.getMonths(), p.getDays(), p.getDuration() + d);
  }
  inline period plus (duration d,      const period& p) {
    return plus(p, d);
  }
  inline period minus(const period& p, duration d) {
    return period(p.getMonths(), p.getDays(), p.getDuration() - d);
  }  
  inline period minus(duration d,      const period& p) {
    return period(-p.getMonths(), -p.getDays(), -p.getDuration() + d);
  }

  
  inline dtime plus(const dtime& dt, const period& p, const std::string& z) {
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
    auto new_offset = getOffsetCnv(res, z);
    // adjust for DST or any other event that changed the TZ, but only
    // if the adjustment does not put us back in the old offset:
    if (new_offset != offset) {
      auto res_potential = res + offset - new_offset; // adjust
      auto adjusted_offset = getOffsetCnv(res_potential, z);
      if (adjusted_offset == new_offset) { // are we still in the new offset?
        res = res_potential;               // if so, then keep the adjustment
      }
    }
    return res;
  }
  inline dtime plus (const period& p, const dtime& dt, const std::string& z) {
    return plus(dt, p, z);
  }
  inline dtime minus(const dtime& dt, const period& p, const std::string& z) {
    return plus(dt, -p, z);
  }

  
  inline interval plus (const interval& i, const period& p,   const std::string& z) {
    return interval(plus(dtime{duration{i.s}}, p, z),
                    plus(dtime{duration{i.e}}, p, z), i.sopen, i.eopen);
  }
  inline interval plus (const period& p,   const interval& i, const std::string& z) {
    return plus(i, p, z);
  }
  inline interval minus(const interval& i, const period& p,   const std::string& z) {
    return plus(i, -p, z);
  }

  
  template <typename T>
  period operator*(const period& p, T d);
  template <typename T>
  period operator/(const period& p, T d);

  bool operator==(const period& p1, const period& p2);
  bool operator!=(const period& p1, const period& p2);

  /// This operator is meaningless, but needs to be defined because of
  /// the intended template usage.
  //inline bool operator<(const period& p1, const period& p2) { return false; }

  std::string to_string(const period& p);

} // end namespace nanotime
  
#endif

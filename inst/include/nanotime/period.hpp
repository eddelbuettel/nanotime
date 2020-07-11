#ifndef NANOTIME_PERIOD_HPP
#define NANOTIME_PERIOD_HPP


#include "globals.hpp"
#include "interval.hpp"


namespace nanotime {

  struct period {
    typedef int32_t month_t ;
    typedef int32_t day_t;

    period();
    period(int32_t months_p, int32_t days_p, duration dur_p);
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


  period plus (const period& p, duration d);
  period plus (duration d,      const period& p);
  period minus(const period& p, duration d);
  period minus(duration d,      const period& p);

  dtime plus (const dtime& dt, const period& p, const std::string& z);
  dtime plus (const period& p, const dtime& dt, const std::string& z);
  dtime minus(const dtime& dt, const period& p, const std::string& z);
  interval plus (const interval& i, const period& p,   const std::string& z);
  interval plus (const period& p,   const interval& i, const std::string& z);
  interval minus(const interval& i, const period& p,   const std::string& z);

  period operator-(const period& p);
  period operator+(const period& p1, const period& p2);
  period operator-(const period& p1, const period& p2);

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

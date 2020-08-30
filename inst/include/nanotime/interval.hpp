#ifndef NANOTIME_INTERVAL_HPP
#define NANOTIME_INTERVAL_HPP

#include <chrono>
#include <cstdint>
#include <sstream>
#include "globals.hpp"


namespace nanotime {

  struct interval {

#ifdef WORDS_BIGENDIAN
    constexpr interval() : s(0), sopen(0), e(0), eopen(0) { }
#else
    constexpr interval() : sopen(0), s(0), eopen(0), e(0) { }
#endif

    interval(dtime s_p, dtime e_p, int sopen_p, int eopen_p)
#ifdef WORDS_BIGENDIAN
      : s(s_p.time_since_epoch().count()), sopen(sopen_p),
        e(e_p.time_since_epoch().count()), eopen(eopen_p) {
#else
      : sopen(sopen_p), s(s_p.time_since_epoch().count()),
        eopen(eopen_p), e(e_p.time_since_epoch().count()) {
#endif
      // if any of the constructor parameters is NA, we construct an NA interval:
      if (s_p.time_since_epoch() == duration::min() || e_p.time_since_epoch() == duration::min() ||
          sopen_p == NA_INTEGER || eopen_p == NA_INTEGER) {
        s = IVAL_NA;
        e = IVAL_NA;
      } else {
        if (s_p.time_since_epoch().count() < IVAL_MIN || e_p.time_since_epoch().count() < IVAL_MIN) {
          s = IVAL_NA;
          e = IVAL_NA;
          Rf_warning("NAs produced by time overflow (remember that interval times are coded with 63 bits)");  
        }
        if (s_p.time_since_epoch().count() > IVAL_MAX || e_p.time_since_epoch().count() > IVAL_MAX) {
          s = IVAL_NA;
          e = IVAL_NA;
          Rf_warning("NAs produced by time overflow (remember that interval times are coded with 63 bits)");
        }
        if (s > e) {
          std::stringstream ss;
          ss << "interval end (" << e << ") smaller than interval start (" << s << ")";
          throw std::range_error(ss.str());
        }
      }
    }

    // interval(std::int64_t s_p, std::int64_t e_p, bool sopen_p, bool eopen_p)
    //   : sopen(sopen_p), s(s_p), eopen(eopen_p), e(e_p) {
    //   if (s > e) {
    //     std::stringstream ss;
    //     ss << "interval end (" << e << ") smaller than interval start (" << s << ")";
    //     throw std::range_error(ss.str());
    //   }
    // }

    dtime getStart() const { return dtime(duration(s)); }
    dtime getEnd() const { return dtime(duration(e)); }
    bool isNA() const { return s == IVAL_NA; }
  
#ifdef WORDS_BIGENDIAN
    std::int64_t s : 63;
    bool sopen : 1; // encode if the interval's start boundary is open (true) or closed (false)
    std::int64_t e : 63;
    bool eopen : 1; // encode if the interval's end   boundary is open (true) or closed (false)
#else
    bool sopen : 1; // encode if the interval's start boundary is open (true) or closed (false)
    std::int64_t s : 63;
    bool eopen : 1; // encode if the interval's end   boundary is open (true) or closed (false)
    std::int64_t e : 63;
#endif

    static const std::int64_t IVAL_MAX =  4611686018427387903LL;
    static const std::int64_t IVAL_MIN = -4611686018427387903LL;
    static const std::int64_t IVAL_NA  = -4611686018427387904LL;
  };

  // operators:

  inline duration operator-(const interval& i1, const interval& i2) {
    return duration(i1.s - i2.s);
  }

  inline bool operator==(const interval& i1, const interval& i2) {
    return i1.s == i2.s && i1.e == i2.e && i1.sopen == i2.sopen &&
      i1.eopen == i2.eopen;
  }

  inline bool operator!=(const interval& i1, const interval& i2) {
    return !(i1 == i2);
  }

  inline bool operator<=(const interval& i1, const interval& i2) {
    if (i1.s < i2.s) return true;
    if (i1.s == i2.s) {
      if (!i1.sopen &&  i2.sopen) return true;
      if (i1.sopen  && !i2.sopen) return false;
      // here we know that s1.sopen == s2.sopen
      if (i1.e < i2.e) return true;
      if (i1.e == i2.e) {
        if (i1.eopen ==  i2.eopen) return true;
        if (i1.eopen && !i2.eopen) return true;
      }
    }
    return false;
  }

  inline bool operator<(const interval& i1, const interval& i2) {
    if (i1.s < i2.s) return true;
    if (i1.s == i2.s) {
      if (!i1.sopen &&  i2.sopen) return true;
      if (i1.sopen  && !i2.sopen) return false;
      // here we know that s1.sopen == s2.sopen
      if (i1.e < i2.e) return true;
      if (i1.e == i2.e) {
        if (i1.eopen ==  i2.eopen) return false;
        if (i1.eopen && !i2.eopen) return true;
      }
    }
    return false;
  }

  inline bool operator>(const interval& i1, const interval& i2) {
    return !(i1 <= i2);
  }

  inline bool operator>=(const interval& i1, const interval& i2) {
    return !(i1 < i2);
  }

  inline bool operator<(const dtime& i1, const interval& i2) {
    if (i1.time_since_epoch().count() < i2.s) return true;
    if (i1.time_since_epoch().count() == i2.s) return i2.sopen;
    return false;
  }

  inline bool operator>(const dtime& i1, const interval& i2) {
    if (i1.time_since_epoch().count() >  i2.e) return true;
    if (i1.time_since_epoch().count() == i2.e) return i2.eopen;
    return false;
  }

  inline interval operator+(const interval& i, const duration d) {
    // test duration is not > 63-bits, and after that the constructor can test for overflow:
    return interval(i.getStart() + d, i.getEnd() + d, i.sopen, i.eopen);
  }
  
  inline interval operator-(const interval& i, const duration d) {
    // test duration is not > 63-bits, and after that the constructor can test for underflow:
    return interval(i.getStart() - d, i.getEnd() - d, i.sopen, i.eopen);
  }

  inline interval operator+(const duration d, const interval& i) {
    // test duration is not > 63-bits, and after that the constructor can test for overflow:
    return interval(i.getStart() + d, i.getEnd() + d, i.sopen, i.eopen);
  }

  // interval components comparators:
  inline bool start_lt(dtime s1, bool sopen1, dtime s2, bool sopen2) {
    if (s1 < s2) return true;
    if (s1 > s2) return false;
    return !sopen1 && sopen2;
  }
  inline bool start_gt(dtime s1, bool sopen1, dtime s2, bool sopen2) {
    if (s1 > s2) return true;
    if (s1 < s2) return false;
    return sopen1 && !sopen2;
  }
  inline bool start_le(dtime s1, bool sopen1, dtime s2, bool sopen2) {
    return !start_gt(s1, sopen1, s2, sopen2);
  }
  inline bool start_ge(dtime s1, bool sopen1, dtime s2, bool sopen2) {
    return !start_lt(s1, sopen1, s2, sopen2);
  }
  inline bool end_lt(dtime e1, bool eopen1, dtime e2, bool eopen2) {
    if (e1 < e2) return true;
    if (e1 > e2) return false;
    return eopen1 && !eopen2;
  }
  inline bool end_gt(dtime e1, bool eopen1, dtime e2, bool eopen2) {
    if (e1 > e2) return true;
    if (e1 < e2) return false;
    return !eopen1 && eopen2;
  }
  inline bool end_le(dtime e1, bool eopen1, dtime e2, bool eopen2) {
    return !end_gt(e1, eopen1, e2, eopen2);
  }
  inline bool end_ge(dtime e1, bool eopen1, dtime e2, bool eopen2) {
    return !end_lt(e1, eopen1, e2, eopen2);
  }

  // interval comparators:
  inline bool start_lt(const interval& i1, const interval& i2) {
    return start_lt(i1.getStart(), i1.sopen, i2.getStart(), i2.sopen);
  }
  inline bool start_gt(const interval& i1, const interval& i2) {
    return start_gt(i1.getStart(), i1.sopen, i2.getStart(), i2.sopen);
  }
  inline bool start_le(const interval& i1, const interval& i2) {
    return !start_gt(i1,i2);
  }
  inline bool start_ge(const interval& i1, const interval& i2) {
    return !start_lt(i1,i2);
  }
  inline bool end_lt(const interval& i1, const interval& i2) {
    return end_lt(i1.getEnd(), i1.eopen, i2.getEnd(), i2.eopen);
  }
  inline bool end_gt(const interval& i1, const interval& i2) {
    return end_gt(i1.getEnd(), i1.eopen, i2.getEnd(), i2.eopen);
  }
  inline bool end_le(const interval& i1, const interval& i2) {
    return !end_gt(i1,i2);
  }
  inline bool end_ge(const interval& i1, const interval& i2) {
    return !end_lt(i1,i2);
  }

  /// True if the end of 'i1' is smaller than the start of 'i2'. This
  /// tests that 'i1' and 'i2' are disjoint and 'i2' is after 'i1'.
  inline bool end_lt_start(const interval& i1, const interval& i2) {
    return end_lt(i1.getEnd(), i1.eopen, i2.getStart(), i2.sopen);
  }
  inline bool end_gt_start(const interval& i1, const interval& i2) {
    return end_gt(i1.getEnd(), i1.eopen, i2.getStart(), i2.sopen);
  }
  /// True if the end of 'i1' is greater or equal than the start of
  /// 'i2'. This tests that 'i1' and 'i2' "touch" and that 'i2' is
  /// after 'i1'.
  inline bool end_ge_start(const interval& i1, const interval& i2) {
    return !end_lt_start(i1, i2);
  }
  inline bool end_le_start(const interval& i1, const interval& i2) {
    return !end_gt_start(i1, i2);
  }


  // Unions --------------------------------------------------------
  /// In unions, we have the following rules: oo is disjoint, but oc,
  /// co, and cc touch
  // interval components comparators:
  inline bool union_start_lt(dtime s1, bool sopen1, dtime s2, bool sopen2) {
    if (s1 < s2) return true;
    if (s1 > s2) return false;
    return sopen1 || sopen2;
  }
  inline bool union_start_gt(dtime s1, bool sopen1, dtime s2, bool sopen2) {
    if (s1 > s2) return true;
    if (s1 < s2) return false;
    return sopen1 || sopen2;
  }
  inline bool union_start_le(dtime s1, bool sopen1, dtime s2, bool sopen2) {
    return !union_start_gt(s1, sopen1, s2, sopen2);
  }
  inline bool union_start_ge(dtime s1, bool sopen1, dtime s2, bool sopen2) {
    return !union_start_lt(s1, sopen1, s2, sopen2);
  }
  inline bool union_end_lt(dtime e1, bool eopen1, dtime e2, bool eopen2) {
    if (e1 < e2) return true;
    if (e1 > e2) return false;
    return eopen1 && eopen2;
  }
  inline bool union_end_gt(dtime e1, bool eopen1, dtime e2, bool eopen2) {
    if (e1 > e2) return true;
    if (e1 < e2) return false;
    return eopen1 && eopen2;
  }
  inline bool union_end_le(dtime e1, bool eopen1, dtime e2, bool eopen2) {
    return !union_end_gt(e1, eopen1, e2, eopen2);
  }
  inline bool union_end_ge(dtime e1, bool eopen1, dtime e2, bool eopen2) {
    return !union_end_lt(e1, eopen1, e2, eopen2);
  }

  // interval comparators:
  inline bool union_start_lt(const interval& i1, const interval& i2) {
    return union_start_lt(i1.getStart(), i1.sopen, i2.getStart(), i2.sopen);
  }
  inline bool union_start_gt(const interval& i1, const interval& i2) {
    return union_start_gt(i1.getStart(), i1.sopen, i2.getStart(), i2.sopen);
  }
  inline bool union_start_le(const interval& i1, const interval& i2) {
    return !union_start_gt(i1,i2);
  }
  inline bool union_start_ge(const interval& i1, const interval& i2) {
    return !union_start_lt(i1,i2);
  }
  inline bool union_end_lt(const interval& i1, const interval& i2) {
    return union_end_lt(i1.getEnd(), i1.eopen, i2.getEnd(), i2.eopen);
  }
  inline bool union_end_gt(const interval& i1, const interval& i2) {
    return union_end_gt(i1.getEnd(), i1.eopen, i2.getEnd(), i2.eopen);
  }
  inline bool union_end_le(const interval& i1, const interval& i2) {
    return !union_end_gt(i1,i2);
  }
  inline bool union_end_ge(const interval& i1, const interval& i2) {
    return !union_end_lt(i1,i2);
  }

  /// True if the end of 'i1' is smaller than the start of 'i2'. This
  /// tests that 'i1' and 'i2' are disjoint and 'i2' is after 'i1'.
  inline bool union_end_lt_start(const interval& i1, const interval& i2) {
    return union_end_lt(i1.getEnd(), i1.eopen, i2.getStart(), i2.sopen);
  }
  inline bool union_end_gt_start(const interval& i1, const interval& i2) {
    return union_end_gt(i1.getEnd(), i1.eopen, i2.getStart(), i2.sopen);
  }
  /// True if the end of 'i1' is greater or equal than the start of
  /// 'i2'. This tests that 'i1' and 'i2' "touch" and that 'i2' is
  /// after 'i1'.
  inline bool union_end_ge_start(const interval& i1, const interval& i2) {
    return !union_end_lt_start(i1, i2);
  }
  inline bool union_end_le_start(const interval& i1, const interval& i2) {
    return !union_end_gt_start(i1, i2);
  }

} // end namespace nanotime
  
#endif

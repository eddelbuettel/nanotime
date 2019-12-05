#ifndef INTERVAL_HPP
#define INTERVAL_HPP


#include <chrono>
#include <cstdint>
#include <sstream>
#include "globals.hpp"


struct interval {

  constexpr interval() 
    : sopen(0), s(0), eopen(0), e(0) { }

  interval(Global::dtime s_p, Global::dtime e_p, int sopen_p, int eopen_p)
    : sopen(sopen_p), s(s_p.time_since_epoch().count()),
      eopen(eopen_p), e(e_p.time_since_epoch().count()) {
    // if any of the contructor parameters is NA, we construct an NA interval:
    if (s_p.time_since_epoch() == Global::duration::min() || e_p.time_since_epoch() == Global::duration::min() ||
        sopen_p == NA_INTEGER || eopen_p == NA_INTEGER) {
      s = IVAL_NA;
      e = IVAL_NA;
    } else {
      if (s_p.time_since_epoch().count() < IVAL_MIN || e_p.time_since_epoch().count() < IVAL_MIN) {
        throw std::range_error("underflow");      
      }
      if (s_p.time_since_epoch().count() > IVAL_MAX || e_p.time_since_epoch().count() > IVAL_MAX) {
        throw std::range_error("overflow");      
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

  Global::dtime getStart() const { return Global::dtime(Global::duration(s)); }
  Global::dtime getEnd() const { return Global::dtime(Global::duration(e)); }
  bool isNA() const { return s == IVAL_NA; }
  
  bool sopen : 1; // encode if the interval's start boundary is open (true) or closed (false)
  std::int64_t s : 63;
  bool eopen : 1; // encode if the interval's end   boundary is open (true) or closed (false)
  std::int64_t e : 63;

  static const std::int64_t IVAL_MAX =  4611686018427387903LL;
  static const std::int64_t IVAL_MIN = -4611686018427387903LL;
  static const std::int64_t IVAL_NA  = -4611686018427387904LL;
};

// operators:

inline Global::duration operator-(const interval& i1, const interval& i2) {
  return Global::duration(i1.s - i2.s);
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

inline bool operator<(const Global::dtime& i1, const interval& i2) {
  if (i1.time_since_epoch().count() < i2.s) return true;
  if (i1.time_since_epoch().count() == i2.s) return i2.sopen;
  return false;
}

inline bool operator>(const Global::dtime& i1, const interval& i2) {
  if (i1.time_since_epoch().count() >  i2.e) return true;
  if (i1.time_since_epoch().count() == i2.e) return i2.eopen;
  return false;
}

inline interval operator+(const interval& i, const Global::duration d) {
  // test duration is not > 63-bits, and after that the constructor can test for overflow:
  return interval(i.getStart() + d, i.getEnd() + d, i.sopen, i.eopen);
}
  
inline interval operator-(const interval& i, const Global::duration d) {
  // test duration is not > 63-bits, and after that the constructor can test for underflow:
  return interval(i.getStart() - d, i.getEnd() - d, i.sopen, i.eopen);
}

inline interval operator+(const Global::duration d, const interval& i) {
  // test duration is not > 63-bits, and after that the constructor can test for overflow:
  return interval(i.getStart() + d, i.getEnd() + d, i.sopen, i.eopen);
}

// interval components comparators:
inline bool start_lt(Global::dtime s1, bool sopen1, Global::dtime s2, bool sopen2) {
  if (s1 < s2) return true;
  if (s1 > s2) return false;
  return !sopen1 && sopen2;
}
inline bool start_gt(Global::dtime s1, bool sopen1, Global::dtime s2, bool sopen2) {
  if (s1 > s2) return true;
  if (s1 < s2) return false;
  return sopen1 && !sopen2;
}
inline bool start_le(Global::dtime s1, bool sopen1, Global::dtime s2, bool sopen2) {
  return !start_gt(s1, sopen1, s2, sopen2);
}
inline bool start_ge(Global::dtime s1, bool sopen1, Global::dtime s2, bool sopen2) {
  return !start_lt(s1, sopen1, s2, sopen2);
}
inline bool end_lt(Global::dtime e1, bool eopen1, Global::dtime e2, bool eopen2) {
  if (e1 < e2) return true;
  if (e1 > e2) return false;
  return eopen1 && !eopen2;
}
inline bool end_gt(Global::dtime e1, bool eopen1, Global::dtime e2, bool eopen2) {
  if (e1 > e2) return true;
  if (e1 < e2) return false;
  return !eopen1 && eopen2;
}
inline bool end_le(Global::dtime e1, bool eopen1, Global::dtime e2, bool eopen2) {
  return !end_gt(e1, eopen1, e2, eopen2);
}
inline bool end_ge(Global::dtime e1, bool eopen1, Global::dtime e2, bool eopen2) {
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
inline bool union_start_lt(Global::dtime s1, bool sopen1, Global::dtime s2, bool sopen2) {
  if (s1 < s2) return true;
  if (s1 > s2) return false;
  return sopen1 || sopen2;
}
inline bool union_start_gt(Global::dtime s1, bool sopen1, Global::dtime s2, bool sopen2) {
  if (s1 > s2) return true;
  if (s1 < s2) return false;
  return sopen1 || sopen2;
}
inline bool union_start_le(Global::dtime s1, bool sopen1, Global::dtime s2, bool sopen2) {
  return !union_start_gt(s1, sopen1, s2, sopen2);
}
inline bool union_start_ge(Global::dtime s1, bool sopen1, Global::dtime s2, bool sopen2) {
  return !union_start_lt(s1, sopen1, s2, sopen2);
}
inline bool union_end_lt(Global::dtime e1, bool eopen1, Global::dtime e2, bool eopen2) {
  if (e1 < e2) return true;
  if (e1 > e2) return false;
  return eopen1 && eopen2;
}
inline bool union_end_gt(Global::dtime e1, bool eopen1, Global::dtime e2, bool eopen2) {
  if (e1 > e2) return true;
  if (e1 < e2) return false;
  return eopen1 && eopen2;
}
inline bool union_end_le(Global::dtime e1, bool eopen1, Global::dtime e2, bool eopen2) {
  return !union_end_gt(e1, eopen1, e2, eopen2);
}
inline bool union_end_ge(Global::dtime e1, bool eopen1, Global::dtime e2, bool eopen2) {
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

#endif

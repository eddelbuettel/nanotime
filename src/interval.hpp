#ifndef INTERVAL_HPP
#define INTERVAL_HPP


#include <chrono>
#include <cstdint>
#include "globals.hpp"


struct interval {
 
  constexpr interval() 
    : s(Global::dtime::duration::zero()), e(Global::dtime::duration::zero()), sopen(0), eopen(0) { }

  interval(Global::dtime s_p, Global::dtime e_p, uint32_t sopen_p, uint32_t eopen_p)
    : s(s_p), e(e_p), sopen(sopen_p), eopen(eopen_p) {
    if (s > e) {
      throw std::range_error("interval end smaller than interval start");
    }
  }

  Global::dtime s;
  Global::dtime e;
#if __BYTE_ORDER == __BIG_ENDIAN  
  uint32_t sopen;     // encode if the interval's lower boundary is
  // open (true) or closed (false)
  uint32_t eopen;
#elif __BYTE_ORDER == __LITTLE_ENDIAN
  uint32_t eopen;     // encode if the interval's lower boundary is
  // open (true) or closed (false)
  uint32_t sopen;
#else
#error __BYTE_ORDER is neither __LITTLE_ENDIAN nor __BIG_ENDIAN
#endif
};

// operators:

inline Global::duration operator-(const interval& i1, const interval& i2) {
  return i1.s - i2.s;
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
  if (i1 < i2.s) return true;
  if (i1 == i2.s) return i2.sopen;
  return false;
}

inline bool operator>(const Global::dtime& i1, const interval& i2) {
  if (i1 >  i2.e) return true;
  if (i1 == i2.e) return i2.eopen;
  return false;
}

inline interval operator+(const interval& i, const Global::duration d) {
  return interval(i.s + d, i.e + d, i.sopen, i.eopen);
}
  
inline interval operator-(const interval& i, const Global::duration d) {
  return interval(i.s - d, i.e - d, i.sopen, i.eopen);
}

inline interval operator+(const Global::duration d, const interval& i) {
  return interval(i.s + d, i.e + d, i.sopen, i.eopen);
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
  return start_lt(i1.s, i1.sopen, i2.s, i2.sopen);
}
inline bool start_gt(const interval& i1, const interval& i2) {
  return start_gt(i1.s, i1.sopen, i2.s, i2.sopen);
}
inline bool start_le(const interval& i1, const interval& i2) {
  return !start_gt(i1,i2);
}
inline bool start_ge(const interval& i1, const interval& i2) {
  return !start_lt(i1,i2);
}
inline bool end_lt(const interval& i1, const interval& i2) {
  return end_lt(i1.e, i1.eopen, i2.e, i2.eopen);
}
inline bool end_gt(const interval& i1, const interval& i2) {
  return end_gt(i1.e, i1.eopen, i2.e, i2.eopen);
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
  return end_lt(i1.e, i1.eopen, i2.s, i2.sopen);
}
inline bool end_gt_start(const interval& i1, const interval& i2) {
  return end_gt(i1.e, i1.eopen, i2.s, i2.sopen);
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


#endif

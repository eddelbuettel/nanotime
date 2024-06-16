#include <iostream>
#include <functional>
#include <Rcpp.h>
#include <RcppCCTZ_API.h>
#include "nanotime/interval.hpp"
#include "nanotime/pseudovector.hpp"
#include "nanotime/utilities.hpp"
#include "cctz/civil_time.h"
#include "cctz/time_zone.h"


using namespace nanotime;


struct double2 {
  double d1;
  double d2;
};


union ival_union {
  struct ival_alias {
    bool sopen : 1;
    int64_t s : 63;  
    bool eopen : 1;
    int64_t e : 63;  
  } ival;
  double2 dbl2;
};


typedef ConstPseudoVector<CPLXSXP, Rcomplex> ConstPseudoVectorIval;
typedef ConstPseudoVector<REALSXP, double>   ConstPseudoVectorDur;
typedef ConstPseudoVector<REALSXP, double>   ConstPseudoVectorNum;
typedef ConstPseudoVector<LGLSXP,  int>      ConstPseudoVectorLgl;
typedef ConstPseudoVector<STRSXP,  const Rcpp::CharacterVector::const_Proxy> ConstPseudoVectorChar;


// for debug reasons...
// cf https://stackoverflow.com/a/16692519
template<typename Clock, typename Duration>
std::ostream &operator<<(std::ostream &stream,
                         const std::chrono::time_point<Clock, Duration> &time_point) {
  // updated with https://stackoverflow.com/a/71442312/23224962 and using C++17
  return stream << std::chrono::duration_cast<std::chrono::nanoseconds>(time_point.time_since_epoch()).count();
}

#if 0
static void print(const interval& i) {
  Rcpp::Rcout << (i.sopen() ? "-" : "+") << i.s() << "->" << i.e() << (i.eopen() ? "-" : "+") << std::endl;
}
#endif

template <typename T, typename U>
static Rcpp::List intersect_idx(const T* v1, size_t v1_size, const U* v2, size_t v2_size) 
{
  // because 'push_back' on R structures is crazy expensive, we use STL vector and copy afterwards:
  std::vector<double> res_first;
  std::vector<double> res_second;
  size_t i1 = 0, i2 = 0;
  while (i1 < v1_size && i2 < v2_size) {
    if (v1[i1] < v2[i2]) {
      ++i1;
    } else if (v1[i1] > v2[i2]) {
      ++i2;
    } else { 
      if (v1_size==0 || v1[i1] != v1[i1-1]) {
        res_first.push_back(i1+1);
        res_second.push_back(i2+1);
      }      
      ++i1;
      //++i2; this is correct for T==U, but not for example when
      //T==time and U==interval
    }
  }

  // copy out result:
  Rcpp::NumericVector res_first_rcpp(res_first.size());
  Rcpp::NumericVector res_second_rcpp(res_second.size());
  if (res_first.size() > 0) memcpy(&res_first_rcpp[0],  &res_first[0],  sizeof(double)*res_first.size());
  if (res_second.size() > 0) memcpy(&res_second_rcpp[0], &res_second[0], sizeof(double)*res_second.size());
  
  return Rcpp::List::create(Rcpp::Named("x") = res_first_rcpp,
                            Rcpp::Named("y") = res_second_rcpp);
}


// do a fast intersect without caring about the second index, and
// return the result as boolean; this is useful for `data.table`
// subsetting:
template <typename T, typename U>
static std::vector<int> intersect_idx_logical(const T* v1, size_t v1_size, const U* v2, size_t v2_size) 
{
  std::vector<int> res(v1_size);
  size_t i1 = 0, i2 = 0;
  while (i1 < v1_size && i2 < v2_size) {
    if (v1[i1] < v2[i2]) {
      res[i1] = FALSE;
      ++i1;
    } else if (v1[i1] > v2[i2]) {
      ++i2;
    } else { 
      if (v1_size==0 || v1[i1] != v1[i1-1]) {
        res[i1] = TRUE;
      }      
      ++i1;
    }
  }
  return res;
}


/// intersect_idx T=dtime, U=dtime doesn't need specialization.
/// intersect_idx T=dtime, U=tz::interval doesn't need specialization.
/// intersect_idx interval/interval doesn't make sense.

// RcppExport SEXP _intersect_idx_time_time(SEXP sv1, SEXP sv2)
// {
//   const Rcpp::NumericVector nv1(sv1);
//   const Rcpp::NumericVector nv2(sv2);  
//   const size_t v1_size = nv1.size();
//   const size_t v2_size = nv2.size();
//   const dtime* v1 = reinterpret_cast<const dtime*>(&nv1[0]);
//   const dtime* v2 = reinterpret_cast<const dtime*>(&nv2[0]);
//   return intersect_idx(v1, v1_size, v2, v2_size);
// }


// [[Rcpp::export]]
Rcpp::List nanoival_intersect_idx_time_interval_impl(const Rcpp::NumericVector nv1,
                                                     const Rcpp::ComplexVector nv2) {
  const dtime* v1 = reinterpret_cast<const dtime*>(&nv1[0]);
  const interval*      v2 = reinterpret_cast<const interval*>(&nv2[0]);
  return intersect_idx(v1, nv1.size(), v2, nv2.size());
}


// [[Rcpp::export]]
Rcpp::LogicalVector nanoival_intersect_idx_time_interval_logical_impl(const Rcpp::NumericVector nv1,
                                                             const Rcpp::ComplexVector nv2) {
  const dtime* v1 = reinterpret_cast<const dtime*>(&nv1[0]);
  const interval*      v2 = reinterpret_cast<const interval*>(&nv2[0]);
  auto res_c = intersect_idx_logical(v1, nv1.size(), v2, nv2.size());
  Rcpp::LogicalVector res(nv1.size());
  if (nv1.size() > 0) memcpy(&res[0], &res_c[0], sizeof(int)*nv1.size());
  return res;
}


// [[Rcpp::export]]
Rcpp::S4 nanoival_intersect_time_interval_impl(const Rcpp::NumericVector nv1,
                                               const Rcpp::ComplexVector nv2) {
  std::vector<dtime> res;
  const dtime* v1 = reinterpret_cast<const dtime*>(&nv1[0]);
  const interval* v2 = reinterpret_cast<const interval*>(&nv2[0]);

  R_xlen_t i1 = 0, i2 = 0;
  while (i1 < nv1.size() && i2 < nv2.size()) {
    if (v1[i1] < v2[i2]) {
      ++i1;
    } else if (v1[i1] > v2[i2]) {
      ++i2;
    } else {
      if (res.size()==0 || v1[i1] != res.back()) {
        res.push_back(v1[i1]);
      }
      ++i1;
    }
  }

  if (res.size() > 0) {
    double* res_start = reinterpret_cast<double*>(&res[0]);
    double* res_end   = res_start + res.size();
    auto final_res = Rcpp::NumericVector(res_start, res_end);
    return assignS4("nanotime", final_res, "integer64");
  } else {
    auto final_res = Rcpp::NumericVector(0);
    return assignS4("nanotime", final_res, "integer64");
  }
}

// [[Rcpp::export]]
Rcpp::NumericVector nanoival_setdiff_time_interval_impl(const Rcpp::NumericVector nv1,
                                                        const Rcpp::ComplexVector nv2) {
  std::vector<dtime> res;
  const dtime* v1 = reinterpret_cast<const dtime*>(&nv1[0]);
  const interval* v2 = reinterpret_cast<const interval*>(&nv2[0]);

  R_xlen_t i1 = 0, i2 = 0;
  while (i1 < nv1.size() && i2 < nv2.size()) {
    if (v1[i1] < v2[i2]) {
      res.push_back(v1[i1++]);
    } else if (v1[i1] > v2[i2]) {
      ++i2;
    } else {
      ++i1;
    }
  }
  // pick up elts left in v1:
  while (i1 < nv1.size()) {
    res.push_back(v1[i1++]);
  }
  
  double* res_start = reinterpret_cast<double*>(&res[0]);
  double* res_end   = res_start + res.size();
  return Rcpp::NumericVector(res_start, res_end);
}

// [[Rcpp::export]]
Rcpp::ComplexVector nanoival_union_impl(const Rcpp::ComplexVector nv1,
                                        const Rcpp::ComplexVector nv2) {
  // assume 'nanoival1/2' were sorted at the R level
  std::vector<interval> res;
  const interval* v1 = reinterpret_cast<const interval*>(&nv1[0]);
  const interval* v2 = reinterpret_cast<const interval*>(&nv2[0]);

  R_xlen_t i1 = 0, i2 = 0;
  if (nv1.size() > 0 && nv2.size() > 0) {
    auto v1_lt_v2 = start_lt(v1[i1], v2[i2]);
    auto start = v1_lt_v2 ? v1[i1].getStart() : v2[i2].getStart();
    auto sopen = v1_lt_v2 ? v1[i1].sopen() : v2[i2].sopen();
  
    for (;;) {
      if (union_end_ge_start(v1[i1], v2[i2]) && union_end_le(v1[i1], v2[i2])) {
        // v1 |------------|         or     |--------|
        // v2      |------------|         |------------|
        if (i1 >= nv1.size() - 1) {
          // if equal ends, have to do the union of the eopens:
          auto eopen = union_end_le(v2[i2], v1[i1]) ? v1[i1].eopen() && v2[i2].eopen() : v2[i2].eopen();
          // v2 interval done, as there's no more v1 elts to overlap
          res.push_back(interval(start, v2[i2].getEnd(), sopen, eopen));
          ++i1; ++i2;
          break;
        }
        ++i1;
      } else if (union_end_ge_start(v2[i2], v1[i1]) && union_end_le(v2[i2], v1[i1])) {
        // v1      |------------|   or    |------------|
        // v2 |------------|                |--------|
        if (i2 >= nv2.size() - 1) {
          // if equal ends, have to do the union of the eopens:
          auto eopen = union_end_le(v1[i1], v2[i2]) ? v1[i1].eopen() && v2[i2].eopen() : v1[i1].eopen();
          // v1 interval done, as there's no more v2 elts to overlap
          res.push_back(interval(start, v1[i1].getEnd(), sopen, eopen));
          ++i1; ++i2;
          break;
        }
        ++i2;
      } else {
        if (v1[i1].getEnd() == v2[i2].getEnd() && v1[i1].eopen() && v2[i2].eopen()) {
          // special case where the intervals are ends are equal and open:
          res.push_back(interval(start, v1[i1].getEnd(), sopen, v1[i1].eopen()));
          ++i1; ++i2;
        } else if (union_end_lt(v1[i1], v2[i2])) {
          // no interval overlap
          res.push_back(interval(start, v1[i1].getEnd(), sopen, v1[i1].eopen()));
          ++i1;
          } else {
          // no interval overlap
          res.push_back(interval(start, v2[i2].getEnd(), sopen, v2[i2].eopen()));
          ++i2;
        }
        // set the start of the next interval:
        if (i1 < nv1.size() && i2 < nv2.size()) {
          auto v1_lt_v2 = start_lt(v1[i1], v2[i2]);
          start = v1_lt_v2 ? v1[i1].getStart() : v2[i2].getStart();
          sopen = v1_lt_v2 ? v1[i1].sopen() : v2[i2].sopen();
        } else {
          break;
        }
      }
    }
  }
  // remaining non-overlapping intervals in v1:
  while (i1 < nv1.size()) {
    res.push_back(v1[i1++]);
  }
  while (i2 < nv2.size()) {
    res.push_back(v2[i2++]);
  }

  // build the ComplexVector that we will return to R:
  Rcpp::ComplexVector finalres(res.size());
  memcpy(&finalres[0], &res[0], sizeof(Rcomplex)*res.size());
  return finalres;
}

// [[Rcpp::export]]
Rcpp::ComplexVector nanoival_intersect_impl(const Rcpp::ComplexVector nv1,
                                            const Rcpp::ComplexVector nv2) {
  // assume 'nanoival1/2' were sorted at the R level
  std::vector<interval> res;
  const interval* v1 = reinterpret_cast<const interval*>(&nv1[0]);
  const interval* v2 = reinterpret_cast<const interval*>(&nv2[0]);

  R_xlen_t i1 = 0, i2 = 0;
  while (i1 < nv1.size() && i2 < nv2.size()) {
    if (v1[i1].getEnd() < v2[i2].getStart() || (v1[i1].getEnd() == v2[i2].getStart() && (v1[i1].eopen() || v2[i2].sopen()))) {
      ++i1;
      continue;
    } else if (v2[i2].getEnd() < v1[i1].getStart() || (v2[i2].getEnd() == v1[i1].getStart() && (v1[i1].sopen() || v2[i2].eopen()))) {
      ++i2;
      continue;
    } else {
      auto v1_gt_v2 = start_gt(v1[i1], v2[i2]);
      auto start = v1_gt_v2 ? v1[i1].getStart() : v2[i2].getStart();
      auto sopen = v1_gt_v2 ? v1[i1].sopen() : v2[i2].sopen();
        
      if (end_lt(v1[i1], v2[i2])) {
        res.push_back(interval(start, v1[i1].getEnd(), sopen, v1[i1].eopen()));
        ++i1;
      } else {
        res.push_back(interval(start, v2[i2].getEnd(), sopen, v2[i2].eopen()));
        ++i2;
      }
    }
  }
  
  // build the ComplexVector that we will return to R:
  Rcpp::ComplexVector finalres(res.size());
  if (res.size() > 0) memcpy(&finalres[0], &res[0], sizeof(Rcomplex)*res.size());
  return assignS4("nanoival", finalres);
}

// [[Rcpp::export]]
Rcpp::ComplexVector nanoival_setdiff_impl(const Rcpp::ComplexVector nv1,
                                          const Rcpp::ComplexVector nv2) {
  // assume 'nanoival1/2' were sorted at the R level
  std::vector<interval> res;
  const interval* v1 = reinterpret_cast<const interval*>(&nv1[0]);
  const interval* v2 = reinterpret_cast<const interval*>(&nv2[0]);

  R_xlen_t i1 = 0, i2 = 0;
  auto start = v1[i1].getStart();
  auto sopen = v1[i1].sopen();
  while (i1 < nv1.size() && i2 < nv2.size()) {
    if (end_lt_start(v1[i1], v2[i2])) {
      // |-------------|
      //                 |------------|
      res.push_back(interval(start, v1[i1].getEnd(), sopen, v1[i1].eopen()));
      if (++i1 >= nv1.size()) break;
      start = v1[i1].getStart();
      sopen = v1[i1].sopen();
    } else if (start_lt(v2[i2].getEnd(), v2[i2].eopen(), start, sopen)) {
      //                 |------------|
      // |-------------|
      ++i2;
    } else if (start_lt(start, sopen, v2[i2].getStart(), v2[i2].sopen())) {
      // |-------------|         or   |-------------|
      //        |------------|           |-------|
      res.push_back(interval(start, v2[i2].getStart(), sopen, !v2[i2].sopen()));
      if (end_gt(v1[i1], v2[i2])) {
        // |-------------|
        //    |-------|
        start = v2[i2].getEnd();
        sopen = !v2[i2].eopen();
        ++i2;
      } else {
        // |-------------|
        //        |------------|
        if (++i1 >= nv1.size()) break;
        start = v1[i1].getStart();
        sopen = v1[i1].sopen();
      }
    } else if (start_ge(start, sopen, v2[i2].getStart(), v2[i2].sopen()) &&
               end_ge(v2[i2], v1[i1])) {
      //    |-------|
      // |-------------|
      if (++i1 >= nv1.size()) break;
      start = v1[i1].getStart();
      sopen = v1[i1].sopen();
    } else {
      //         |------------|
      //     |----------|
      start = v2[i2].getEnd();
      sopen = !v2[i2].eopen();
      ++i2;
    }

  }
  // remaining non-overlapping intervals in v1:
  if (i1 < nv1.size()) {
    res.push_back(interval(start, v1[i1].getEnd(), sopen, v1[i1].eopen()));
    ++i1;
    while (i1 < nv1.size()) {
      res.push_back(v1[i1++]);
    }
  }
  
  // build the ComplexVector that we will return to R:
  Rcpp::ComplexVector finalres(res.size());
  if (res.size() > 0) memcpy(&finalres[0], &res[0], sizeof(Rcomplex)*res.size());
  return finalres;
}


// [[Rcpp::export]]
bool nanoival_is_unsorted_impl(const Rcpp::ComplexVector nvec,
                               const Rcpp::LogicalVector strictlyvec) {
  if (strictlyvec.size() == 0) {
    Rcpp::stop("argument 'strictly' cannot have length 0");
  }
  const bool strictlybool = strictlyvec[0];
  const interval* ival_ptr = reinterpret_cast<const interval*>(&nvec[0]);
  const auto ival_len = nvec.size();
  if (strictlybool) {
    for (R_xlen_t i=1; i<ival_len; ++i) {
      if (ival_ptr[i-1] >= ival_ptr[i]) {
        return true;
      }
    }
  }
  else {
    for (R_xlen_t i=1; i<ival_len; ++i) {
      if (ival_ptr[i-1] > ival_ptr[i]) {
        return true;
      }
    }
  }
  return false;
}

// [[Rcpp::export]]
const Rcpp::ComplexVector nanoival_sort_impl(const Rcpp::ComplexVector nvec,
                                             const Rcpp::LogicalVector decreasingvec) {
  Rcpp::ComplexVector res = clone(nvec);
  interval* ival_ptr = reinterpret_cast<interval*>(&res[0]);
  const auto ival_len = res.size();
  interval* start = ival_ptr;
  interval* end   = start + ival_len;

  if (decreasingvec.size() == 0) {
    Rcpp::stop("argument 'decreasing' cannot have length 0");
  } else if (!decreasingvec[0]) {
    std::sort(start, end);
  }
  else {
    std::sort(start, end, std::greater<interval>());
  }
  return res;
}

// [[Rcpp::export]]
const Rcpp::ComplexVector nanoival_sort_impl2(const Rcpp::ComplexVector nvec,	// #nocov start
                                              bool decreasing) {
  Rcpp::ComplexVector res = clone(nvec);
  interval* ival_ptr = reinterpret_cast<interval*>(&res[0]);
  const auto ival_len = res.size();
  interval* start = ival_ptr;
  interval* end   = start + ival_len;

  if (!decreasing) {
    std::sort(start, end);
  } else {
    std::sort(start, end, std::greater<interval>());
  }
  return res;
}                                                                               // #nocov end

template<typename COMP>
Rcpp::LogicalVector nanoival_comp(const Rcpp::ComplexVector v1,
                                  const Rcpp::ComplexVector v2, COMP cmp) {
  checkVectorsLengths(v1, v2);
  Rcpp::LogicalVector res(getVectorLengths(v1, v2));
  if (res.size()) {
    const ConstPseudoVectorIval cv1(v1);
    const ConstPseudoVectorIval cv2(v2);
    const interval* n1_ptr = reinterpret_cast<const interval*>(&cv1[0]);
    const interval* n2_ptr = reinterpret_cast<const interval*>(&cv2[0]);

    for (R_xlen_t i=0; i<res.size(); ++i) {
      res[i] = cmp(n1_ptr[i], n2_ptr[i]);
    }
  
    copyNames(v1, v2, res);
  }
  return res;
}

// [[Rcpp::export]]
Rcpp::LogicalVector nanoival_lt_impl(const Rcpp::ComplexVector n1, const Rcpp::ComplexVector n2) {
  return nanoival_comp(n1, n2, std::less<interval>());
}

// [[Rcpp::export]]
Rcpp::LogicalVector nanoival_le_impl(const Rcpp::ComplexVector n1, const Rcpp::ComplexVector n2) {
  return nanoival_comp(n1, n2, std::less_equal<interval>());
}

// [[Rcpp::export]]
Rcpp::LogicalVector nanoival_gt_impl(const Rcpp::ComplexVector n1, const Rcpp::ComplexVector n2) {
  return nanoival_comp(n1, n2, std::greater<interval>());
}

// [[Rcpp::export]]
Rcpp::LogicalVector nanoival_ge_impl(const Rcpp::ComplexVector n1, const Rcpp::ComplexVector n2) {
  return nanoival_comp(n1, n2, std::greater_equal<interval>());
}

// [[Rcpp::export]]
Rcpp::LogicalVector nanoival_eq_impl(const Rcpp::ComplexVector n1, const Rcpp::ComplexVector n2) {
  return nanoival_comp(n1, n2, std::equal_to<interval>());
}

// [[Rcpp::export]]
Rcpp::LogicalVector nanoival_ne_impl(Rcpp::ComplexVector n1, Rcpp::ComplexVector n2) {
  return nanoival_comp(n1, n2, std::not_equal_to<interval>());
}


template<typename OP>
Rcpp::ComplexVector nanoival_op(const Rcpp::ComplexVector cv1,
                                const Rcpp::NumericVector nv2, OP op) {
  checkVectorsLengths(cv1, nv2);
  Rcpp::ComplexVector res(getVectorLengths(cv1, nv2));
  if (res.size()) {
    const ConstPseudoVectorIval e1(cv1);
    const ConstPseudoVectorDur  e2(nv2);
    
    for (R_xlen_t i=0; i<res.size(); ++i) {
      const interval ival = *reinterpret_cast<const interval*>(&e1[i]);
      const duration dur = *reinterpret_cast<const duration*>(&e2[i]);
      const auto ires = op(ival, dur);
      const Rcomplex *ptr = reinterpret_cast<const Rcomplex*>(&ires);
      res[i] = *ptr;
    }
  
    copyNames(cv1, nv2, res);
  }
  return res;
}

// [[Rcpp::export]]
Rcpp::ComplexVector nanoival_plus_impl(const Rcpp::ComplexVector n1, const Rcpp::NumericVector n2) {
  return nanoival_op(n1, n2, nanotime_ops::plus<interval, duration, interval>());
}

// [[Rcpp::export]]
Rcpp::ComplexVector nanoival_minus_impl(const Rcpp::ComplexVector n1, const Rcpp::NumericVector n2) {
  return nanoival_op(n1, n2, nanotime_ops::minus<interval, duration, interval>());
}

/// union_idx T=dtime, U=dtime doesn't need specialization.
/// union_idx T=dtime, U=tz::interval doesn't make sense.
/// union_idx interval/interval doesn't make sense.


/// setdiff_idx T=dtime, U=dtime doesn't need specialization.
/// setdiff_idx T=interval, U=interval doesn't make sense
/// setdiff_idx T=dtime, U=interval:


template <typename T, typename U>
static Rcpp::NumericVector setdiff_idx(const T* v1, size_t v1_size, const U* v2, size_t v2_size) {
  std::vector<double> res_first;
  size_t i1 = 0, i2 = 0;
  while (i1 < v1_size && i2 < v2_size) {
    if (v1[i1] < v2[i2]) {
      res_first.push_back(i1+1);
      ++i1;
    } else if (v1[i1] > v2[i2]) {
      ++i2;
    } else { 
      ++i1;
    }
  }

  // pick up elts left in v1:
  while (i1 < v1_size) {
    res_first.push_back(i1+1);
    ++i1;
  }

  // copy out result:
  Rcpp::NumericVector res_first_rcpp(res_first.size());
  if (res_first.size() > 0) memcpy(&res_first_rcpp[0],  &res_first[0],  sizeof(double)*res_first.size());

  return res_first_rcpp;
}

// [[Rcpp::export]]
Rcpp::NumericVector nanoival_setdiff_idx_time_interval_impl(const Rcpp::NumericVector nv1,
                                                            const Rcpp::ComplexVector cv2) {
  const dtime* v1 = reinterpret_cast<const dtime*>(&nv1[0]);
  const interval* v2 = reinterpret_cast<const interval*>(&cv2[0]);
  return setdiff_idx(v1, nv1.size(), v2, cv2.size());
}

// [[Rcpp::export]]
Rcpp::S4 nanoival_new_impl(const Rcpp::NumericVector sv,
                           const Rcpp::NumericVector ev,
                           const Rcpp::LogicalVector sopenv,
                           const Rcpp::LogicalVector eopenv) {

  // handle the special case where one of the operands has 0-length:
  Rcpp::ComplexVector res(getVectorLengths(sv, ev, sopenv, eopenv));

  checkVectorsLengths(sv, ev, sopenv, eopenv);
  const ConstPseudoVectorNum nvs(sv);
  const ConstPseudoVectorNum nve(ev);
  const ConstPseudoVectorLgl lvs(sopenv);
  const ConstPseudoVectorLgl lve(eopenv);

  for (R_xlen_t i=0; i < res.size(); ++i) {
    const double d1 = nvs[i];
    const double d2 = nve[i];
    dtime i1, i2;
    memcpy(&i1, reinterpret_cast<const char*>(&d1), sizeof(d1));
    memcpy(&i2, reinterpret_cast<const char*>(&d2), sizeof(d2));
    const interval ival { i1, i2, lvs[i], lve[i] };
    memcpy(&res[i], &ival, sizeof(ival));
  }
  return assignS4("nanoival", res);
}


// R accessor functions:
// [[Rcpp::export]]
Rcpp::NumericVector nanoival_get_start_impl(const Rcpp::ComplexVector cv) {
  Rcpp::NumericVector res(cv.size());
  for (R_xlen_t i=0; i<cv.size(); ++i) {
    interval ival;
    Rcomplex c = cv[i];
    memcpy(&ival, reinterpret_cast<const char*>(&c), sizeof(c));
    if (ival.isNA()) {
      double d;
      memcpy(&d, reinterpret_cast<const char*>(&NA_INTEGER64), sizeof(NA_INTEGER64));
      res[i] = d;
    }
    else {
      std::int64_t start = ival.s();
      double d;
      memcpy(&d, reinterpret_cast<const char*>(&start), sizeof(start));
      res[i] = d;
    }
  }
  assignS4("nanotime", res, "integer64");
  res.names() = cv.names();
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector nanoival_get_end_impl(const Rcpp::ComplexVector cv) {
  Rcpp::NumericVector res(cv.size());
  for (R_xlen_t i=0; i<cv.size(); ++i) {
    interval ival;
    Rcomplex c = cv[i];
    memcpy(&ival, reinterpret_cast<const char*>(&c), sizeof(c));
    if (ival.isNA()) {
      double d;
      memcpy(&d, reinterpret_cast<const char*>(&NA_INTEGER64), sizeof(NA_INTEGER64));
      res[i] = d;
    }
    else {
      std::int64_t end = ival.e();
      double d;
      memcpy(&d, reinterpret_cast<const char*>(&end), sizeof(end));
      res[i] = d;
    }
  }
  assignS4("nanotime", res, "integer64");
  res.names() = cv.names();
  return res;
}


// [[Rcpp::export]]
Rcpp::LogicalVector nanoival_get_sopen_impl(const Rcpp::ComplexVector cv) {
  Rcpp::LogicalVector res(cv.size());
  for (R_xlen_t i=0; i<cv.size(); ++i) {
    interval ival;
    Rcomplex c = cv[i];
    memcpy(&ival, reinterpret_cast<const char*>(&c), sizeof(c));
    if (ival.isNA()) {
      res[i] = NA_LOGICAL;
    }
    else {
      res[i] = ival.sopen();
    }
  }
  res.names() = cv.names();
  return res;
}


// [[Rcpp::export]]
Rcpp::LogicalVector nanoival_get_eopen_impl(const Rcpp::ComplexVector cv) {
  Rcpp::LogicalVector res(cv.size());
  for (R_xlen_t i=0; i<cv.size(); ++i) {
    interval ival;
    Rcomplex c = cv[i];
    memcpy(&ival, reinterpret_cast<const char*>(&c), sizeof(c));
    if (ival.isNA()) {
      res[i] = NA_LOGICAL;
    }
    else {
      res[i] = ival.eopen();
    }
  }
  res.names() = cv.names();
  return res;
}

// [[Rcpp::export]]
Rcpp::LogicalVector nanoival_isna_impl(const Rcpp::ComplexVector cv) {
  Rcpp::LogicalVector res(cv.size());
  for (R_xlen_t i=0; i<cv.size(); ++i) {
    interval ival;
    Rcomplex c = cv[i];
    memcpy(&ival, reinterpret_cast<const char*>(&c), sizeof(c));
    res[i] = ival.isNA();
  }
  res.names() = cv.names();
  return res;
}


static Rcomplex readNanoival(const char*& sp, const char* const se, const char* tzstr) {
  // read the +- at the beginning:
  if (sp >= se || (*sp != '+' && *sp != '-')) {
    throw std::range_error("Error parsing");
  }
  auto sopen = *sp++ == '+' ? false : true;
  
  auto ss = readDtime(sp, se);
  if (ss.tzstr.size() && strnlen_(tzstr, MAX_TZ_STR_LENGTH)) {
    throw std::range_error("timezone is specified twice: in the string and as an argument");
  }
  skipWhitespace(sp, se);

  // read the middle portion
  if (sp+2 >= se || (*sp != '-' && *(sp+1) != '>')) {
    throw std::range_error("Error parsing");
  }
  sp += 2;
    
  skipWhitespace(sp, se);
  auto es = readDtime(sp, se-1); // -1 because we don't want to read the -+ as a timezone
  if (es.tzstr.size() && strnlen_(tzstr, MAX_TZ_STR_LENGTH)) {
    throw std::range_error("timezone is specified twice: in the string and as an argument"); // ## nocov
  }

  // read the +- at the end:
  if (sp >= se || (*sp != '+' && *sp != '-')) {
    throw std::range_error("Error parsing aa");
  }
  auto eopen = *sp++ == '+' ? false : true;
    
  // check we read until the end
  if (sp != se) {
    throw std::range_error("Error parsing");
  }

  const cctz::civil_second start_cvt(ss.y, ss.m, ss.d, ss.hh, ss.mm, ss.ss);
  cctz::time_point<cctz::seconds> start_tp;
  const char* tzstr_start  = ss.tzstr.size() ? ss.tzstr.c_str() : tzstr;
  int cvt_res = RcppCCTZ::convertToTimePoint(start_cvt, tzstr_start, start_tp);
  if (cvt_res < 0) {
    Rcpp::stop("Cannot retrieve timezone '%s'.", tzstr_start); // ## nocov
  }
  auto start = dtime{std::chrono::nanoseconds((start_tp.time_since_epoch().count() - ss.offset) * 1000000000ll + ss.ns)};

  const cctz::civil_second end_cvt(es.y, es.m, es.d, es.hh, es.mm, es.ss);
  cctz::time_point<cctz::seconds> end_tp;
  const char* tzstr_end  = es.tzstr.size() ? es.tzstr.c_str() : tzstr;
  cvt_res = RcppCCTZ::convertToTimePoint(end_cvt, tzstr_end, end_tp);
  if (cvt_res < 0) {
    Rcpp::stop("Cannot retrieve timezone '%s'.", tzstr_end);
  }
  auto end = dtime{std::chrono::nanoseconds((end_tp.time_since_epoch().count() - es.offset) * 1000000000ll + es.ns)};
  
  Rcomplex res;
  const interval ival { start, end, sopen, eopen };
  memcpy(&res, &ival, sizeof(ival));
  return res;
}


// [[Rcpp::export]]
Rcpp::ComplexVector nanoival_make_impl(const Rcpp::CharacterVector nt_v,
                                       const Rcpp::CharacterVector tz_v) {
  // handle the special case where one of the operands has 0-length:
  if (nt_v.size() == 0 || tz_v.size() == 0) {
    Rcpp::ComplexVector res(0);
    return assignS4("nanoival", res);
  }
  
  checkVectorsLengths(nt_v, tz_v);
  ConstPseudoVectorChar nt(nt_v);
  ConstPseudoVectorChar tz(tz_v);
  Rcpp::ComplexVector res(nt.size());
  for (R_xlen_t i=0; i<nt.size(); ++i) {
    const char* str = nt[i];
    res[i] = readNanoival(str, str + nt[i].size(), tz[i]);
  }
  copyNames(nt_v, tz_v, res);
  return assignS4("nanoival", res);
}


Rcomplex getNA_ival() {
  static const auto p = interval(dtime(duration::min()),
                                 dtime(duration::min()), // #nocov (seems like the cov tool fails us here!)
                                 NA_INTEGER, NA_INTEGER);
  Rcomplex c;
  memcpy(&c, &p, sizeof(p));
  return c;
}


// [[Rcpp::export]]
Rcpp::ComplexVector nanoival_subset_numeric_impl(const Rcpp::ComplexVector& v, const Rcpp::NumericVector& idx) {
  Rcpp::ComplexVector res(0);
  std::vector<Rcomplex> res_c;    // by declaring it here we can make subset logical agnostic to 'Rcomplex'
  subset_numeric(v, idx, res, res_c, getNA_ival);
  return assignS4("nanoival", res);
}


// [[Rcpp::export]]
Rcpp::ComplexVector nanoival_subset_logical_impl(const Rcpp::ComplexVector& v, const Rcpp::LogicalVector& idx_p) {
  const ConstPseudoVectorLgl idx(idx_p);
  Rcpp::ComplexVector res(0);
  std::vector<Rcomplex> res_c;
  subset_logical(v, idx, res, res_c, getNA_ival);
  return assignS4("nanoival", res);
}

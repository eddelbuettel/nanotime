#include <iostream>
#include <functional>
#include <Rcpp.h>
#include "interval.hpp"
#include "pseudovector.hpp"
#include "utilities.hpp"
#include "cctz/civil_time.h"
#include "cctz/time_zone.h"


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

#if 0
static void print(const interval& i) {
  Rcpp::Rcout << (i.sopen ? "-" : "+") << i.s << "->" << i.e << (i.eopen ? "-" : "+") << std::endl;
}
#endif

template <typename T, typename U>
static Rcpp::List intersect_idx(const T* v1, size_t v1_size, const U* v2, size_t v2_size) 
{
  Rcpp::NumericVector res_first;
  Rcpp::NumericVector res_second;
  size_t i1 = 0, i2 = 0;
  while (i1 < v1_size && i2 < v2_size) {
    if (v1[i1] < v2[i2]) {
      ++i1;
    } else if (v1[i1] > v2[i2]) {
      ++i2;
    } else { 
      if (v1_size==0 || v1[i1] != v1[i1-1]) {
        res_first.push_back(i1+1);  // push_back too slow? LLL
        res_second.push_back(i2+1); // push_back too slow? LLL
      }      
      ++i1;
      //++i2; this is correct for T==U, but not for example when
      //T==time and U==interval
    }
  }
  return Rcpp::List::create(Rcpp::Named("x") = res_first,
                            Rcpp::Named("y") = res_second);
}


/// intersect_idx T=Global::dtime, U=Global::dtime doesn't need specialization.
/// intersect_idx T=Global::dtime, U=tz::interval doesn't need specialization.
/// intersect_idx interval/interval doesn't make sense.

// RcppExport SEXP _intersect_idx_time_time(SEXP sv1, SEXP sv2)
// {
//   const Rcpp::NumericVector nv1(sv1);
//   const Rcpp::NumericVector nv2(sv2);  
//   const size_t v1_size = nv1.size();
//   const size_t v2_size = nv2.size();
//   const Global::dtime* v1 = reinterpret_cast<const Global::dtime*>(&nv1[0]);
//   const Global::dtime* v2 = reinterpret_cast<const Global::dtime*>(&nv2[0]);
//   return intersect_idx(v1, v1_size, v2, v2_size);
// }


RcppExport SEXP _nanoival_intersect_idx_time_interval(SEXP sv1, SEXP sv2)
{
  const Rcpp::NumericVector nv1(sv1);
  const Rcpp::ComplexVector nv2(sv2);  
  const Global::dtime* v1 = reinterpret_cast<const Global::dtime*>(&nv1[0]);
  const interval*      v2 = reinterpret_cast<const interval*>(&nv2[0]);
  return intersect_idx(v1, nv1.size(), v2, nv2.size());
}


RcppExport SEXP _nanoival_intersect_time_interval(SEXP nanotime, SEXP nanoival) {
  try {
    std::vector<Global::dtime> res;
    const Rcpp::NumericVector nv1(nanotime);
    const Rcpp::ComplexVector nv2(nanoival);  
    const Global::dtime* v1 = reinterpret_cast<const Global::dtime*>(&nv1[0]);
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
  
    double* res_start = reinterpret_cast<double*>(&res[0]);
    double* res_end   = res_start + res.size();
    return Rcpp::NumericVector(res_start, res_end);  
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}

RcppExport SEXP _nanoival_setdiff_time_interval(SEXP nanotime, SEXP nanoival) {
  try {
    std::vector<Global::dtime> res;
    const Rcpp::NumericVector nv1(nanotime);
    const Rcpp::ComplexVector nv2(nanoival);  
    const Global::dtime* v1 = reinterpret_cast<const Global::dtime*>(&nv1[0]);
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
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}

RcppExport SEXP _nanoival_union(SEXP nanoival1, SEXP nanoival2) {
  try {
    // assume 'nanoival1/2' were sorted at the R level
    std::vector<interval> res;
    const Rcpp::ComplexVector nv1(nanoival1);
    const Rcpp::ComplexVector nv2(nanoival2);
    const interval* v1 = reinterpret_cast<const interval*>(&nv1[0]);
    const interval* v2 = reinterpret_cast<const interval*>(&nv2[0]);

    R_xlen_t i1 = 0, i2 = 0;
    if (nv1.size() > 0 && nv2.size() > 0) {
      auto v1_lt_v2 = start_lt(v1[i1], v2[i2]);
      auto start = v1_lt_v2 ? v1[i1].getStart() : v2[i2].getStart();
      auto sopen = v1_lt_v2 ? v1[i1].sopen : v2[i2].sopen;
  
      for (;;) {
        if (union_end_ge_start(v1[i1], v2[i2]) && union_end_le(v1[i1], v2[i2])) {
          // v1 |------------|         or     |--------|
          // v2      |------------|         |------------|
          if (i1 >= nv1.size() - 1) {
            // if equal ends, have to do the union of the eopens:
            auto eopen = union_end_le(v2[i2], v1[i1]) ? v1[i1].eopen && v2[i2].eopen : v2[i2].eopen;
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
            auto eopen = union_end_le(v1[i1], v2[i2]) ? v1[i1].eopen && v2[i2].eopen : v1[i1].eopen;
            // v1 interval done, as there's no more v2 elts to overlap
            res.push_back(interval(start, v1[i1].getEnd(), sopen, eopen));
            ++i1; ++i2;
            break;
          }
          ++i2;
        } else {
          if (v1[i1].getEnd() == v2[i2].getEnd() && v1[i1].eopen && v2[i2].eopen) {
            // special case where the intervals are ends are equal and open:
            res.push_back(interval(start, v1[i1].getEnd(), sopen, v1[i1].eopen));
            ++i1; ++i2;            
          } else if (union_end_lt(v1[i1], v2[i2])) {
            // no interval overlap
            res.push_back(interval(start, v1[i1].getEnd(), sopen, v1[i1].eopen));
            ++i1;
          } else {
            // no interval overlap
            res.push_back(interval(start, v2[i2].getEnd(), sopen, v2[i2].eopen));
            ++i2;
          }
          // set the start of the next interval:
          if (i1 < nv1.size() && i2 < nv2.size()) {
            auto v1_lt_v2 = start_lt(v1[i1], v2[i2]);
            start = v1_lt_v2 ? v1[i1].getStart() : v2[i2].getStart();
            sopen = v1_lt_v2 ? v1[i1].sopen : v2[i2].sopen;            
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
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}

RcppExport SEXP _nanoival_intersect(SEXP nanoival1, SEXP nanoival2) {
  try {
    // assume 'nanoival1/2' were sorted at the R level
    std::vector<interval> res;
    const Rcpp::ComplexVector nv1(nanoival1);
    const Rcpp::ComplexVector nv2(nanoival2);
    const interval* v1 = reinterpret_cast<const interval*>(&nv1[0]);
    const interval* v2 = reinterpret_cast<const interval*>(&nv2[0]);

    R_xlen_t i1 = 0, i2 = 0;
    while (i1 < nv1.size() && i2 < nv2.size()) {
      if (v1[i1].getEnd() < v2[i2].getStart() || (v1[i1].getEnd() == v2[i2].getStart() && (v1[i1].eopen || v2[i2].sopen))) {
        ++i1;
        continue;
      } else if (v2[i2].getEnd() < v1[i1].getStart() || (v2[i2].getEnd() == v1[i1].getStart() && (v1[i1].sopen || v2[i2].eopen))) {
        ++i2;
        continue;
      } else {
        auto v1_gt_v2 = start_gt(v1[i1], v2[i2]);
        auto start = v1_gt_v2 ? v1[i1].getStart() : v2[i2].getStart();
        auto sopen = v1_gt_v2 ? v1[i1].sopen : v2[i2].sopen;
        
        if (end_lt(v1[i1], v2[i2])) {
          res.push_back(interval(start, v1[i1].getEnd(), sopen, v1[i1].eopen));
          ++i1;
        } else {
          res.push_back(interval(start, v2[i2].getEnd(), sopen, v2[i2].eopen));
          ++i2;
        }
      }
    }
  
    // build the ComplexVector that we will return to R:
    Rcpp::ComplexVector finalres(res.size());
    memcpy(&finalres[0], &res[0], sizeof(Rcomplex)*res.size());
    return finalres;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}

RcppExport SEXP _nanoival_setdiff(SEXP nanoival1, SEXP nanoival2) {
  try {
    // assume 'nanoival1/2' were sorted at the R level
    std::vector<interval> res;
    const Rcpp::ComplexVector nv1(nanoival1);
    const Rcpp::ComplexVector nv2(nanoival2);
    const interval* v1 = reinterpret_cast<const interval*>(&nv1[0]);
    const interval* v2 = reinterpret_cast<const interval*>(&nv2[0]);

    R_xlen_t i1 = 0, i2 = 0;
    auto start = v1[i1].getStart();
    auto sopen = v1[i1].sopen;
    while (i1 < nv1.size() && i2 < nv2.size()) {
      if (end_lt_start(v1[i1], v2[i2])) {
        // |-------------|
        //                 |------------|
        res.push_back(interval(start, v1[i1].getEnd(), sopen, v1[i1].eopen));
        if (++i1 >= nv1.size()) break;
        start = v1[i1].getStart();
        sopen = v1[i1].sopen;
      } else if (start_lt(v2[i2].getEnd(), v2[i2].eopen, start, sopen)) {
        //                 |------------|
        // |-------------|
        ++i2;
      } else if (start_lt(start, sopen, v2[i2].getStart(), v2[i2].sopen)) {
        // |-------------|         or   |-------------| 
        //        |------------|           |-------|
        res.push_back(interval(start, v2[i2].getStart(), sopen, !v2[i2].sopen));
        if (end_gt(v1[i1], v2[i2])) {
          // |-------------| 
          //    |-------|
          start = v2[i2].getEnd();
          sopen = !v2[i2].eopen;
          ++i2;
        } else {
          // |-------------|
          //        |------------|
          if (++i1 >= nv1.size()) break;
          start = v1[i1].getStart();
          sopen = v1[i1].sopen;
        }
      } else if (start_ge(start, sopen, v2[i2].getStart(), v2[i2].sopen) &&
                 end_ge(v2[i2], v1[i1])) {
        //    |-------|
        // |-------------| 
        if (++i1 >= nv1.size()) break;
        start = v1[i1].getStart();
        sopen = v1[i1].sopen;
      } else {
        //         |------------|
        //     |----------| 
        start = v2[i2].getEnd();
        sopen = !v2[i2].eopen;
        ++i2;
      }

    }
    // remaining non-overlapping intervals in v1:
    if (i1 < nv1.size()) {
      res.push_back(interval(start, v1[i1].getEnd(), sopen, v1[i1].eopen));
      ++i1;
      while (i1 < nv1.size()) {
        res.push_back(v1[i1++]);
      }        
    }
  
    // build the ComplexVector that we will return to R:
    Rcpp::ComplexVector finalres(res.size());
    memcpy(&finalres[0], &res[0], sizeof(Rcomplex)*res.size());
    return finalres;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP _nanoival_is_unsorted(SEXP nanoival, SEXP strictly) {
  try {
    const Rcpp::ComplexVector nvec(nanoival);
    const Rcpp::NumericVector strictlyvec(strictly);
    if (strictlyvec.size() == 0) {
      Rcpp::stop("argument 'strictly' cannot have length 0");
    }
    const bool strictlybool = strictlyvec[0];
    const interval* ival_ptr = reinterpret_cast<const interval*>(&nvec[0]);
    const auto ival_len = nvec.size();
    if (strictlybool) {
      for (R_xlen_t i=1; i<ival_len; ++i) {
        if (ival_ptr[i-1] >= ival_ptr[i]) {
          return Rcpp::LogicalVector(true);
        }
      }
    }
    else {
      for (R_xlen_t i=1; i<ival_len; ++i) {
        if (ival_ptr[i-1] > ival_ptr[i]) {
          return Rcpp::LogicalVector(true);
        }
      }
    }
    return Rcpp::LogicalVector(false);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP _nanoival_sort(SEXP nanoival, SEXP decreasing) {
  try {
    const Rcpp::ComplexVector nvec(nanoival);
    Rcpp::ComplexVector res = clone(nvec);
    interval* ival_ptr = reinterpret_cast<interval*>(&res[0]);
    const auto ival_len = res.size();
    interval* start = ival_ptr;
    interval* end   = start + ival_len;
    const Rcpp::LogicalVector decreasingvec(decreasing);
    
    if (decreasingvec.size() == 0) {
      Rcpp::stop("argument 'decreasing' cannot have length 0");      
    } else if (!decreasingvec[0]) {
      std::sort(start, end);
    }
    else {
      std::sort(start, end, std::greater<interval>());
    }

    return res;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


template<typename COMP>
SEXP nanoival_comp(SEXP n1, SEXP n2, COMP cmp) {
  try {
    const Rcpp::ComplexVector cv1(n1);
    const Rcpp::ComplexVector cv2(n2);
    if (cv1.size() != cv2.size()) {
      Rcpp::stop(std::string("object lengths mismatch"));
    }
    const auto ival_len = cv1.size();
    Rcpp::LogicalVector res(ival_len);
    const interval* n1_ptr = reinterpret_cast<const interval*>(&cv1[0]);
    const interval* n2_ptr = reinterpret_cast<const interval*>(&cv2[0]);

    for (R_xlen_t i=0; i<ival_len; ++i) {
      res[i] = cmp(n1_ptr[i], n2_ptr[i]);
    }
  
    copyNames(cv1, cv2, res);
    return res;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}

RcppExport SEXP _nanoival_lt(SEXP n1, SEXP n2) {
  return nanoival_comp(n1, n2, std::less<interval>());
}

RcppExport SEXP _nanoival_le(SEXP n1, SEXP n2) {
  return nanoival_comp(n1, n2, std::less_equal<interval>());
}

RcppExport SEXP _nanoival_gt(SEXP n1, SEXP n2) {
  return nanoival_comp(n1, n2, std::greater<interval>());
}

RcppExport SEXP _nanoival_ge(SEXP n1, SEXP n2) {
  return nanoival_comp(n1, n2, std::greater_equal<interval>());
}

RcppExport SEXP _nanoival_eq(SEXP n1, SEXP n2) {
  return nanoival_comp(n1, n2, std::equal_to<interval>());
}

RcppExport SEXP _nanoival_ne(SEXP n1, SEXP n2) {
  return nanoival_comp(n1, n2, std::not_equal_to<interval>());
}


template<typename OP>
SEXP nanoival_op(SEXP n1, SEXP n2, OP op) {
  try {

    const Rcpp::ComplexVector   cv1(n1);
    const Rcpp::NumericVector   nv2(n2);
    const ConstPseudoVectorIval e1(cv1);
    const ConstPseudoVectorDur  e2(nv2);
    Rcpp::ComplexVector res(std::max(e1.size(), e2.size()));
    
    for (R_xlen_t i=0; i<res.size(); ++i) {
      const interval ival = *reinterpret_cast<const interval*>(&e1[i]);
      const Global::duration dur = *reinterpret_cast<const Global::duration*>(&e2[i]);
      const auto ires = op(ival, dur);
      res[i] = *reinterpret_cast<const Rcomplex*>(&ires);
    }
  
    copyNames(cv1, nv2, res);    
    return res;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}

RcppExport SEXP _nanoival_plus(SEXP n1, SEXP n2) {
  return nanoival_op(n1, n2, Global::plus<interval, Global::duration, interval>());
}

RcppExport SEXP _nanoival_minus(SEXP n1, SEXP n2) {
  return nanoival_op(n1, n2, Global::minus<interval, Global::duration, interval>());
}

/// union_idx T=Global::dtime, U=Global::dtime doesn't need specialization.
/// union_idx T=Global::dtime, U=tz::interval doesn't make sense.
/// union_idx interval/interval doesn't make sense.


/// setdiff_idx T=Global::dtime, U=Global::dtime doesn't need specialization.
/// setdiff_idx T=interval, U=interval doesn't make sense
/// setdiff_idx T=Global::dtime, U=interval:


template <typename T, typename U>
static Rcpp::NumericVector setdiff_idx(const T* v1, size_t v1_size, const U* v2, size_t v2_size)
{
  Rcpp::NumericVector res_first;
  size_t i1 = 0, i2 = 0;
  while (i1 < v1_size && i2 < v2_size) {
    if (v1[i1] < v2[i2]) {
      res_first.push_back(i1+1); // too expensive LLL
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
 
  return res_first;
}


RcppExport SEXP _nanoival_setdiff_idx_time_interval(SEXP sv1, SEXP sv2)
{
  try {
    const Rcpp::NumericVector nv1(sv1);
    const Rcpp::ComplexVector cv2(sv2);  
    const Global::dtime* v1 = reinterpret_cast<const Global::dtime*>(&nv1[0]);
    const interval*      v2 = reinterpret_cast<const interval*>(&cv2[0]);
    return setdiff_idx(v1, nv1.size(), v2, cv2.size());
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP _nanoival_new(SEXP start, SEXP end, SEXP sopen, SEXP eopen) {
  const Rcpp::NumericVector sv(start);
  const Rcpp::NumericVector ev(end);
  const Rcpp::LogicalVector sopenv(sopen);
  const Rcpp::LogicalVector eopenv(eopen);
  const ConstPseudoVectorNum nvs(sv);
  const ConstPseudoVectorNum nve(ev);
  const ConstPseudoVectorLgl lvs(sopenv);
  const ConstPseudoVectorLgl lve(eopenv);

  Rcpp::ComplexVector res(nvs.size());
  try {
    for (R_xlen_t i=0; i < nvs.size(); ++i) {
      const double d1 = nvs[i];
      const double d2 = nve[i];
      Global::dtime i1, i2;
      memcpy(&i1, reinterpret_cast<const char*>(&d1), sizeof(d1));
      memcpy(&i2, reinterpret_cast<const char*>(&d2), sizeof(d2));
      const interval ival { i1, i2, lvs[i], lve[i] };
      memcpy(&res[i], &ival, sizeof(ival));
    }
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }

  return assignS4("nanoival", res);
}


// R accessor functions:
RcppExport SEXP _nanoival_get_start(SEXP sv) {
  const Rcpp::ComplexVector cv(sv);
  Rcpp::NumericVector res(cv.size());
  for (R_xlen_t i=0; i<cv.size(); ++i) {
    interval ival;
    Rcomplex c = cv[i];
    memcpy(&ival, reinterpret_cast<const char*>(&c), sizeof(c));
    std::int64_t start = ival.s;
    double d;
    memcpy(&d, reinterpret_cast<const char*>(&start), sizeof(start));
    res[i] = d;
  }
  res.names() = cv.names();
  return assignS4("nanotime", res, "integer64");
}


RcppExport SEXP _nanoival_get_end(SEXP sv) {
  const Rcpp::ComplexVector cv(sv);
  Rcpp::NumericVector res(cv.size());
  for (R_xlen_t i=0; i<cv.size(); ++i) {
    interval ival;
    Rcomplex c = cv[i];
    memcpy(&ival, reinterpret_cast<const char*>(&c), sizeof(c));
    std::int64_t end = ival.e;
    double d;
    memcpy(&d, reinterpret_cast<const char*>(&end), sizeof(end));
    res[i] = d;
  }
  res.names() = cv.names();
  return assignS4("nanotime", res, "integer64");
}


RcppExport SEXP _nanoival_get_sopen(SEXP sv) {
  const Rcpp::ComplexVector cv(sv);
  Rcpp::LogicalVector res(cv.size());
  for (R_xlen_t i=0; i<cv.size(); ++i) {
    interval ival;
    Rcomplex c = cv[i];
    memcpy(&ival, reinterpret_cast<const char*>(&c), sizeof(c));
    res[i] = ival.sopen;
  }
  res.names() = cv.names();
  return res;
}


RcppExport SEXP _nanoival_get_eopen(SEXP sv) {
  const Rcpp::ComplexVector cv(sv);
  Rcpp::LogicalVector res(cv.size());
  for (R_xlen_t i=0; i<cv.size(); ++i) {
    interval ival;
    Rcomplex c = cv[i];
    memcpy(&ival, reinterpret_cast<const char*>(&c), sizeof(c));
    res[i] = ival.eopen;
  }
  res.names() = cv.names();
  return res;
}


static Rcomplex readNanoival(const char*& sp, const char* const se, const char* tzstr) {
  try {
    // read the +- at the beginning:
    if (sp >= se || (*sp != '+' && *sp != '-')) {
      throw std::range_error("Error parsing");
    }
    auto sopen = *sp++ == '+' ? false : true;
  
    auto ss = Global::readDtime(sp, se, tzstr);
    Global::skipWhitespace(sp, se);

    // read the middle portion
    if (sp+2 >= se || (*sp != '-' && *(sp+1) != '>')) {
      throw std::range_error("Error parsing");
    }
    sp += 2;
    
    Global::skipWhitespace(sp, se);
    auto es = Global::readDtime(sp, se-1, tzstr); // -1 because we don't want to read the -+ as a timezone

    // read the +- at the end:
    if (sp >= se || (*sp != '+' && *sp != '-')) {
      throw std::range_error("Error parsing aa");
    }
    auto eopen = *sp++ == '+' ? false : true;
    
    // check we read until the end
    if (sp != se) {
      throw std::range_error("Error parsing");
    }

    typedef Global::time_point<Global::seconds> CONVERT_TO_TIMEPOINT(const cctz::civil_second& cs, const char* tzstr);
    CONVERT_TO_TIMEPOINT *convertToTimePoint = (CONVERT_TO_TIMEPOINT*)  R_GetCCallable("RcppCCTZ", "_RcppCCTZ_convertToTimePoint" );

    const cctz::civil_second start_cvt(ss.y, ss.m, ss.d, ss.hh, ss.mm, ss.ss);    
    // warn double specification of timezone LLL
    auto start_tp = convertToTimePoint(start_cvt, ss.tzstr.size() ? ss.tzstr.c_str() : tzstr);  
    auto start = Global::dtime{std::chrono::nanoseconds((start_tp.time_since_epoch().count() - ss.offset) * 1000000000ll + ss.ns)};

    const cctz::civil_second end_cvt(es.y, es.m, es.d, es.hh, es.mm, es.ss);    
    // warn double specification of timezone LLL
    auto end_tp = convertToTimePoint(end_cvt, es.tzstr.size() ? es.tzstr.c_str() : tzstr);  
    auto end = Global::dtime{std::chrono::nanoseconds((end_tp.time_since_epoch().count() - es.offset) * 1000000000ll + es.ns)};
  
    Rcomplex res;
    const interval ival { start, end, sopen, eopen };
    memcpy(&res, &ival, sizeof(ival));
    return res;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }

  return Rcomplex{0,0};         // not reached
}


RcppExport SEXP _nanoival_make(SEXP nt_p, SEXP tz_p) {
  const Rcpp::CharacterVector nt_v(nt_p);
  const Rcpp::CharacterVector tz_v(tz_p);
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

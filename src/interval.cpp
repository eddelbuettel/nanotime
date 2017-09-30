#include <iostream>
#include <functional>
#include <Rcpp.h>
#include "interval.hpp"

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

static void print(const interval& i) {
  Rcpp::Rcout << (i.sopen ? "-" : "+") << i.s << "->" << i.e << (i.eopen ? "-" : "+") << std::endl;
}


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
  return Rcpp::List::create(Rcpp::Named("x")  = res_first,
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
  const Rcpp::NumericVector nv2(sv2);  
  const size_t v1_size = nv1.size();
  const size_t v2_size = nv2.size() / 3;
  const Global::dtime* v1 = reinterpret_cast<const Global::dtime*>(&nv1[0]);
  const interval*      v2 = reinterpret_cast<const interval*>(&nv2[0]);
  return intersect_idx(v1, v1_size, v2, v2_size);
}


RcppExport SEXP _nanoival_intersect_time_interval(SEXP nanotime, SEXP nanoival) {
  try {
    std::vector<Global::dtime> res;
    const Rcpp::NumericVector nv1(nanotime);
    const Rcpp::NumericVector nv2(nanoival);  
    const size_t v1_size = nv1.size();
    const size_t v2_size = nv2.size() / 3;
    const Global::dtime* v1 = reinterpret_cast<const Global::dtime*>(&nv1[0]);
    const interval* v2 = reinterpret_cast<const interval*>(&nv2[0]);

    size_t i1 = 0, i2 = 0;
    while (i1 < v1_size && i2 < v2_size) {
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
    const Rcpp::NumericVector nv2(nanoival);  
    const size_t v1_size = nv1.size();
    const size_t v2_size = nv2.size() / 3;
    const Global::dtime* v1 = reinterpret_cast<const Global::dtime*>(&nv1[0]);
    const interval* v2 = reinterpret_cast<const interval*>(&nv2[0]);

    size_t i1 = 0, i2 = 0;
    while (i1 < v1_size&& i2 < v2_size) {
      if (v1[i1] < v2[i2]) {
        res.push_back(v1[i1++]);
      } else if (v1[i1] > v2[i2]) {
        ++i2;
      } else { 
        ++i1;
      }
    }
    // pick up elts left in v1:
    while (i1 < v1_size) {
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
    const Rcpp::NumericVector nv1(nanoival1);
    const Rcpp::NumericVector nv2(nanoival2);
    const size_t v1_size = nv1.size() / 3;
    const size_t v2_size = nv2.size() / 3;
    const interval* v1 = reinterpret_cast<const interval*>(&nv1[0]);
    const interval* v2 = reinterpret_cast<const interval*>(&nv2[0]);

    size_t i1 = 0, i2 = 0;
    if (v1_size > 0 && v2_size > 0) {
      auto v1_lt_v2 = start_lt(v1[i1], v2[i2]);
      auto start = v1_lt_v2 ? v1[i1].s : v2[i2].s;
      auto sopen = v1_lt_v2 ? v1[i1].sopen : v2[i2].sopen;
  
      for (;;) {
        if (end_ge_start(v1[i1], v2[i2]) && end_le(v1[i1], v2[i2])) {
          // v1 |------------|         or     |--------|
          // v2      |------------|         |------------|
          if (++i1 >= v1_size) {
            // v2 interval done, as there's no more v1 elts to overlap
            res.push_back(interval(start, v2[i2].e, sopen, v2[i2].eopen));
            ++i2;
            break;
          }
        } else if (end_ge_start(v2[i2], v1[i1]) && end_le(v2[i2], v1[i1])) {
          // v1      |------------|   or    |------------|
          // v2 |------------|                |--------|
          if (++i2 >= v2_size) {
            // v2 interval done, as there's no more v2 elts to overlap
            res.push_back(interval(start, v1[i1].e, sopen, v1[i1].eopen));
            ++i1;
            break;
          }
        } else {
          // no interval overlap
          if (end_lt(v1[i1], v2[i2])) {
            res.push_back(interval(start, v1[i1].e, sopen, v1[i1].eopen));
            ++i1;
          } else {
            res.push_back(interval(start, v2[i2].e, sopen, v2[i2].eopen));
            ++i2;
          }
          // set the start of the next interval:
          if (i1 < v1_size && i2 < v2_size) {
            start = std::min(v1[i1].s, v2[i2].s);
          } else {
            break;
          }
        }  
      }
    }
    // remaining non-overlapping intervals in v1:
    while (i1 < v1_size) {
      res.push_back(v1[i1++]);
    }
    while (i2 < v2_size) {
      res.push_back(v2[i2++]);
    }
 
    double* res_start = reinterpret_cast<double*>(&res[0]);
    double* res_end   = res_start + res.size() * 3;
    return Rcpp::NumericVector(res_start, res_end);  
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
    const Rcpp::NumericVector nv1(nanoival1);
    const Rcpp::NumericVector nv2(nanoival2);
    const size_t v1_size = nv1.size() / 3;
    const size_t v2_size = nv2.size() / 3;
    const interval* v1 = reinterpret_cast<const interval*>(&nv1[0]);
    const interval* v2 = reinterpret_cast<const interval*>(&nv2[0]);

    size_t i1 = 0, i2 = 0;
    while (i1 < v1_size && i2 < v2_size) {
      if (v1[i1].s >= v2[i2].e) {
        ++i1;
        continue;
      } else if (v2[i2].s >= v1[i1].e) {
        ++i1;
        continue;
      }

      auto v1_gt_v2 = start_gt(v1[i1], v2[i2]);
      auto start = v1_gt_v2 ? v1[i1].s : v2[i2].s;
      auto sopen = v1_gt_v2 ? v1[i1].sopen : v2[i2].sopen;

      if (end_lt(v1[i1], v2[i2])) {
        res.push_back(interval(start, v1[i1].e, sopen, v1[i1].eopen));
        ++i1;
      } else {
        res.push_back(interval(start, v2[i2].e, sopen, v2[i2].eopen));
        ++i2;
      }
    }
  
    double* res_start = reinterpret_cast<double*>(&res[0]);
    double* res_end   = res_start + res.size() * 3;
    return Rcpp::NumericVector(res_start, res_end);
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
    const Rcpp::NumericVector nv1(nanoival1);
    const Rcpp::NumericVector nv2(nanoival2);
    const size_t v1_size = nv1.size() / 3;
    const size_t v2_size = nv2.size() / 3;
    const interval* v1 = reinterpret_cast<const interval*>(&nv1[0]);
    const interval* v2 = reinterpret_cast<const interval*>(&nv2[0]);

    size_t i1 = 0, i2 = 0;
    auto start = v1[i1].s;
    auto sopen = v1[i1].sopen;
    while (i1 < v1_size && i2 < v2_size) {
      if (end_lt_start(v1[i1], v2[i2])) {
        // |-------------|
        //                 |------------|
        res.push_back(interval(start, v1[i1].e, sopen, v1[i1].eopen));
        if (++i1 >= v1_size) break;
        start = v1[i1].s;
        sopen = v1[i1].sopen;
      } else if (start_lt(v2[i2].e, v2[i2].eopen, start, sopen)) {
        //                 |------------|
        // |-------------|
        ++i2;
      } else if (start_lt(start, sopen, v2[i2].s, v2[i2].sopen)) {
        // |-------------|         or   |-------------| 
        //        |------------|           |-------|
        res.push_back(interval(start, v2[i2].s, sopen, !v2[i2].sopen));
        if (end_gt(v1[i1], v2[i2])) {
          // |-------------| 
          //    |-------|
          start = v2[i2].e;
          sopen = !v2[i2].eopen;
          ++i2;
        } else {
          // |-------------|
          //        |------------|
          if (++i1 >= v1_size) break;
          start = v1[i1].s;
          sopen = v1[i1].sopen;
        }
      } else if (start_ge(start, sopen, v2[i2].s, v2[i2].sopen) &&
                 end_ge(v2[i2], v1[i1])) {
        //    |-------|
        // |-------------| 
        if (++i1 >= v1_size) break;
        start = v1[i1].s;
        sopen = v1[i1].sopen;
      } else {
        //         |------------|
        //     |----------| 
        start = v2[i2].e;
        sopen = !v2[i2].eopen;
        ++i2;
      }

    }
    // remaining non-overlapping intervals in v1:
    if (i1 < v1_size) {
      res.push_back(interval(start, v1[i1].e, sopen, v1[i1].eopen));
      ++i1;
      while (i1 < v1_size) {
        res.push_back(v1[i1++]);
      }        
    }
  
    double* res_start = reinterpret_cast<double*>(&res[0]);
    double* res_end   = res_start + res.size() * 3;
    return Rcpp::NumericVector(res_start, res_end);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP _nanoival_is_unsorted(SEXP nanoival, SEXP strictly) {
  try {
    const Rcpp::NumericVector nvec(nanoival);
    const Rcpp::NumericVector strictlyvec(strictly);
    if (strictlyvec.size() == 0) {
      Rcpp::stop("argument 'strictly' cannot have length 0");
    }
    const bool strictlybool = strictlyvec[0];
    const interval* ival_ptr = reinterpret_cast<const interval*>(&nvec[0]);
    const auto ival_len = nvec.size() / 3;
    if (strictlybool) {
      for (size_t i=1; i<ival_len; ++i) {
        if (ival_ptr[i-1] >= ival_ptr[i]) {
          return Rcpp::LogicalVector(true);
        }
      }
    }
    else {
      for (size_t i=1; i<ival_len; ++i) {
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
    const Rcpp::NumericVector nvec(nanoival);
    Rcpp::NumericVector res(nvec);
    interval* ival_ptr = reinterpret_cast<interval*>(&res[0]);
    const auto ival_len = res.size() / 3;
    interval* start = ival_ptr;
    interval* end   = start + ival_len;

    if (!Rcpp::LogicalVector(decreasing)[0]) {
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
    const Rcpp::NumericVector nv1(n1);
    const Rcpp::NumericVector nv2(n2);
    if (nv1.size() != nv2.size()) {
      Rcpp::stop(std::string("object lengths mismatch"));
    }
    const auto ival_len = nv1.size() / 3;
    Rcpp::LogicalVector res(ival_len);
    const interval* n1_ptr = reinterpret_cast<const interval*>(&nv1[0]);
    const interval* n2_ptr = reinterpret_cast<const interval*>(&nv2[0]);

    for (size_t i=0; i<ival_len; ++i) {
      res[i] = cmp(n1_ptr[i], n2_ptr[i]);
    }
  
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


/// union_idx T=Global::dtime, U=Global::dtime doesn't need specialization.
/// union_idx T=Global::dtime, U=tz::interval doesn't make sense.
/// union_idx interval/interval doesn't make sense.


/// setdiff_idx T=Global::dtime, U=Global::dtime doesn't need specialization.
/// setdiff_idx T=interval, U=interval doesn't make sense
/// setdiff_idx T=Global::dtime, U=interval:


template <typename T, typename U>
static Rcpp::List setdiff_idx(const T* v1, size_t v1_size, const U* v2, size_t v2_size)
{
  Rcpp::NumericVector res_first;
  Rcpp::NumericVector res_second;
  size_t i1 = 0, i2 = 0;
  while (i1 < v1_size && i2 < v2_size) {
    if (v1[i1] < v2[i2]) {
      res_first.push_back(i1+1);
      ++i1;
    } else if (v1[i1] > v2[i2]) {
      res_second.push_back(i2+1);
      ++i2;
    } else { 
      ++i1;
      ++i2;
    }
  }

  // pick up elts left in v1:
  while (i1 < v1_size) {
    res_first.push_back(i1+1);
    ++i1;
  }
  // pick up elts left in v2:
  while (i2 < v2_size) {
    res_second.push_back(i2+1);
    ++i2;
  }
  
  return Rcpp::List::create(Rcpp::Named("x") = res_first,
                            Rcpp::Named("y") = res_second);
}


RcppExport SEXP _nanoival_setdiff_idx_time_interval(SEXP sv1, SEXP sv2)
{
  try {
    const Rcpp::NumericVector nv1(sv1);
    const Rcpp::NumericVector nv2(sv2);  
    const size_t v1_size = nv1.size();
    const size_t v2_size = nv2.size() / 3;
    const Global::dtime* v1 = reinterpret_cast<const Global::dtime*>(&nv1[0]);
    const interval*      v2 = reinterpret_cast<const interval*>(&nv2[0]);
    return setdiff_idx(v1, v1_size, v2, v2_size);
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}

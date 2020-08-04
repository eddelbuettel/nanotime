#include <iostream>
#include <functional>
#include <Rcpp.h>
#include "nanotime/globals.hpp"
#include "nanotime/utilities.hpp"
#include "nanotime/pseudovector.hpp"


using namespace nanotime;


typedef ConstPseudoVector<REALSXP, double>   ConstPseudoVectorInt64;
typedef ConstPseudoVector<STRSXP,  const Rcpp::CharacterVector::const_Proxy> ConstPseudoVectorChar;
typedef ConstPseudoVector<LGLSXP,  std::int32_t> ConstPseudoVectorLgl;


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

// [[Rcpp::export]]
Rcpp::IntegerVector nanotime_wday_impl(const Rcpp::NumericVector tm_v,
                                       const Rcpp::CharacterVector tz_v) {
  checkVectorsLengths(tm_v, tz_v);
  Rcpp::IntegerVector    res(getVectorLengths(tm_v, tz_v));
  if (res.size()) {
    ConstPseudoVectorInt64 tm(tm_v);
    ConstPseudoVectorChar  tz(tz_v);

    for (R_xlen_t i=0; i<res.size(); ++i) {
      const auto tz_i = Rcpp::as<std::string>(tz[i]);
      const auto tm_i = *reinterpret_cast<const dtime*>(&tm[i]);
      const auto offset = getOffsetCnv(tm_i, tz_i.c_str());
      date::sys_days t_days = date::floor<date::days>(tm_i + offset);
      res[i] = unsigned(date::weekday(t_days).c_encoding());
    }
    copyNames(tm_v, tz_v, res);
  }
  return res;
}

// [[Rcpp::export]]
Rcpp::IntegerVector nanotime_mday_impl(const Rcpp::NumericVector tm_v,
                                       const Rcpp::CharacterVector tz_v) {
  checkVectorsLengths(tm_v, tz_v);
  Rcpp::IntegerVector    res(getVectorLengths(tm_v, tz_v));
  if (res.size()) {
    ConstPseudoVectorInt64 tm(tm_v);
    ConstPseudoVectorChar  tz(tz_v);
    for (R_xlen_t i=0; i<res.size(); ++i) {
      const auto tz_i = Rcpp::as<std::string>(tz[i]);
      const auto tm_i = *reinterpret_cast<const dtime*>(&tm[i]);
      const auto offset = getOffsetCnv(tm_i, tz_i.c_str());
      date::sys_days t_days = date::floor<date::days>(tm_i + offset);
      date::weekday wd(t_days);
      res[i] = unsigned(date::year_month_day(t_days).day());
    }
    copyNames(tm_v, tz_v, res);
  }
  return res;
}

// [[Rcpp::export]]
Rcpp::IntegerVector nanotime_month_impl(const Rcpp::NumericVector tm_v,
                                        const Rcpp::CharacterVector tz_v) {
  checkVectorsLengths(tm_v, tz_v);
  Rcpp::IntegerVector    res(getVectorLengths(tm_v, tz_v));
  if (res.size()) {
    ConstPseudoVectorInt64 tm(tm_v);
    ConstPseudoVectorChar  tz(tz_v);

    for (R_xlen_t i=0; i<res.size(); ++i) {
      const auto tz_i = Rcpp::as<std::string>(tz[i]);
      const auto tm_i = *reinterpret_cast<const dtime*>(&tm[i]);
      const auto offset = getOffsetCnv(tm_i, tz_i.c_str());
      date::sys_days t_days = date::floor<date::days>(tm_i + offset);
      date::weekday wd(t_days);
      res[i] = unsigned(date::year_month_day(t_days).month());
    }
    copyNames(tm_v, tz_v, res);
  }
  return res;
}

// [[Rcpp::export]]
Rcpp::IntegerVector nanotime_year_impl(const Rcpp::NumericVector tm_v,
                                       const Rcpp::CharacterVector tz_v) {
  checkVectorsLengths(tm_v, tz_v);
  Rcpp::IntegerVector    res(getVectorLengths(tm_v, tz_v));
  if (res.size()) {
    ConstPseudoVectorInt64 tm(tm_v);
    ConstPseudoVectorChar  tz(tz_v);

    for (R_xlen_t i=0; i<res.size(); ++i) {
      const auto tz_i = Rcpp::as<std::string>(tz[i]);
      const auto tm_i = *reinterpret_cast<const dtime*>(&tm[i]);
      const auto offset = getOffsetCnv(tm_i, tz_i.c_str());
      date::sys_days t_days = date::floor<date::days>(tm_i + offset);
      date::weekday wd(t_days);
      res[i] = int(date::year_month_day(t_days).year());
    }
    copyNames(tm_v, tz_v, res);
  }
  return res;
}


static std::int64_t readNanotime(const char*& sp, const char* const se, const char* tzstr) {
  auto tt = readDtime(sp, se);

  // check we read until the end
  if (sp != se)
    Rcpp::stop("Error parsing");

  if (tt.tzstr.size() && strnlen(tzstr, MAX_TZ_STR_LENGTH)) 
    Rcpp::stop("timezone is specified twice: in the string and as an argument");
    
  const cctz::civil_second cvt(tt.y, tt.m, tt.d, tt.hh, tt.mm, tt.ss);

  typedef int CONVERT_TO_TIMEPOINT(const cctz::civil_second& cs, const char* tzstr, time_point<seconds>& tp);
  CONVERT_TO_TIMEPOINT *convertToTimePoint =
    (CONVERT_TO_TIMEPOINT*) R_GetCCallable("RcppCCTZ", "_RcppCCTZ_convertToTimePoint_nothrow");

  const auto final_tzstr = tt.tzstr.size() ? tt.tzstr.c_str() : tzstr;
  if (final_tzstr[0] == 0)
    Rcpp::stop("Error parsing");

  time_point<seconds> tp;
  int res = convertToTimePoint(cvt, final_tzstr, tp);
  if (res < 0) {
    Rcpp::stop("Cannot retrieve timezone '%s'.", final_tzstr);
  }
  
  return (tp.time_since_epoch().count() - tt.offset) * 1000000000ll + tt.ns;
}


// [[Rcpp::export]]
Rcpp::NumericVector nanotime_make_impl(const Rcpp::CharacterVector nt_v,
                                       const Rcpp::CharacterVector tz_v) {
  checkVectorsLengths(nt_v, tz_v);
  Rcpp::NumericVector res(getVectorLengths(nt_v, tz_v));
  if (res.size()) {
    ConstPseudoVectorChar nt(nt_v);
    ConstPseudoVectorChar tz(tz_v);
    for (R_xlen_t i=0; i<res.size(); ++i) {
      auto str = (const char*)(nt[i]);
      const auto t = readNanotime(str, str + nt[i].size(), tz[i]);
      const double *ptr = reinterpret_cast<const double*>(&t);
      res[i] = *ptr;
    }
    copyNames(nt_v, tz_v, res);
  }
  return assignS4("nanotime", res, "integer64");
}


static double getNA_nanotime() {
  const int64_t i64 = std::numeric_limits<std::int64_t>::min();
  double res;
  memcpy(&res, &i64, sizeof(res));
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector nanotime_subset_numeric_impl(const Rcpp::NumericVector& v, const Rcpp::NumericVector& idx) {
  Rcpp::NumericVector res(0);
  std::vector<double> res_c;    // by declaring it here we can make subset logical agnostic to double
  subset_numeric(v, idx, res, res_c, getNA_nanotime);
  return assignS4("nanotime", res, "integer64");
}


// [[Rcpp::export]]
Rcpp::NumericVector nanotime_subset_logical_impl(const Rcpp::NumericVector& v, const Rcpp::LogicalVector& idx_p) {
  const ConstPseudoVectorLgl idx(idx_p);
  Rcpp::NumericVector res(0);
  std::vector<double> res_c;    // by declaring it here we can make subset logical agnostic to double
  subset_logical(v, idx, res, res_c, getNA_nanotime);
  return assignS4("nanotime", res, "integer64");
}

#include <iostream>
#include <functional>
#include <Rcpp.h>
#include "globals.hpp"
#include "utilities.hpp"
#include "pseudovector.hpp"


typedef ConstPseudoVector<STRSXP,  const Rcpp::CharacterVector::const_Proxy> ConstPseudoVectorChar;


static std::int64_t readNanotime(const char*& sp, const char* const se, const char* tzstr) {
  try {
    auto tt = Global::readDtime(sp, se, tzstr);

    // check we read until the end
    if (sp != se) {
      throw std::range_error("Error parsing");
    }
     
    const cctz::civil_second cvt(tt.y, tt.m, tt.d, tt.hh, tt.mm, tt.ss);

    typedef Global::time_point<Global::seconds> CONVERT_TO_TIMEPOINT(const cctz::civil_second& cs, const char* tzstr);
    CONVERT_TO_TIMEPOINT *convertToTimePoint = (CONVERT_TO_TIMEPOINT*)  R_GetCCallable("RcppCCTZ", "_RcppCCTZ_convertToTimePoint" );

    // warn double specification of timezone LLL

    auto final_tzstr = tt.tzstr.size() ? tt.tzstr.c_str() : tzstr;
    if (final_tzstr[0] == 0) {
      throw std::range_error("Error parsing");
    }
    auto tp = convertToTimePoint(cvt, final_tzstr);
    return (tp.time_since_epoch().count() - tt.offset) * 1000000000ll + tt.ns;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }

  return 0;                     // not reached
}


RcppExport SEXP _nanotime_make(SEXP nt_p, SEXP tz_p) {
  const Rcpp::CharacterVector nt_v(nt_p);
  const Rcpp::CharacterVector tz_v(tz_p);
  ConstPseudoVectorChar nt(nt_v);
  ConstPseudoVectorChar tz(tz_v);
  Rcpp::NumericVector res(nt.size());
  for (R_xlen_t i=0; i<nt.size(); ++i) {
    auto str = (const char*)(nt[i]);
    const auto t = readNanotime(str, str + nt[i].size(), tz[i]);
    res[i] = *reinterpret_cast<const double*>(&t);
  }
  copyNames(nt_v, tz_v, res);
  return assignS4("nanotime", res, "integer64");
}


#include <chrono>
#include <cstdint>
#include <sstream>
#include <iomanip>
#include <regex>
#include <stdexcept>
#include <tuple>
#include <limits>
#include <Rcpp.h>
#include "duration.hpp"
#include "pseudovector.hpp"


Global::duration from_string(const std::string& str) {
  Global::duration d = std::chrono::seconds(0);
  const char* s = str.c_str();
  const char* e = s + str.size();

  auto sign = 1;
  if (s < e && *s == '-') {
    sign = -1;
    ++s;
  }

  int n;
  if (!Global::readNumber(s, e, n, false)) throw std::range_error("cannot parse duration");

  if (s < e && *s == ':') {
    // we've got HHH:MM:SS format
    d += n * std::chrono::hours(1);
    ++s;
    if (s + 5 > e || !isdigit(s[0]) || !isdigit(s[1]) || 
        s[2] != ':' || !isdigit(s[3]) || !isdigit(s[4])) {
      throw std::range_error("cannot parse duration");
    }
    d += ((s[0] - '0')*10 + (s[1] - '0'))*std::chrono::minutes(1);

    // treat seconds in the general way:
    n = (s[3] - '0')*10 + (s[4] - '0');
    s += 5;
  }

  d += n * std::chrono::seconds(1);
  if (s == e) return sign*d;

  if (*s++ != '.') throw std::range_error("cannot parse duration");
  Global::duration mul = std::chrono::milliseconds(100);
  unsigned i = 0;
  while (s < e) {
    if (mul < std::chrono::nanoseconds(1)) throw std::range_error("cannot parse duration");
    if ((i == 3 || i == 6) && *s == '_') { ++s; continue; }
    ++i;
    if (!isdigit(*s)) throw std::range_error("cannot parse duration");
    d += (*s - '0') * mul;
    mul /= 10;
    ++s;
  }

  if (s == e) 
    return sign*d;
  else
    throw std::range_error("cannot parse duration");
}


std::string to_string(Global::duration d) {
  std::stringstream ss;

  if (d == Global::duration::min()) {
    ss << "NA";
  }
  else {
    // handle hh:mm:ss
    if (d < std::chrono::seconds(0)) {
      ss << '-';
      d *= -1;
    }
    auto h = d / std::chrono::hours(1);
    d -= h * std::chrono::seconds(3600);
    auto min = d / std::chrono::minutes(1);
    d -= min * std::chrono::seconds(60);
    auto s = d / std::chrono::seconds(1);
    d -= s * std::chrono::seconds(1);
    ss << std::setfill ('0') 
       << std::setw(2) << h  << ':' 
       << std::setw(2) << min  << ':' 
       << std::setw(2) << s;
  
    // now handle nanoseconds 000_000_000
    auto ms = d / std::chrono::milliseconds(1);
    d -= ms * std::chrono::milliseconds(1);
    auto us = d / std::chrono::microseconds(1);
    d -= us * std::chrono::microseconds(1);
    auto ns = d / std::chrono::nanoseconds(1);
    d -= ns * std::chrono::nanoseconds(1);

    if (ms || us || ns) {
      ss << '.';
      ss << std::setfill ('0') << std::setw(3) << ms;
      if (us || ns) {
        ss << '_' << std::setfill('0') << std::setw(3) << us;
        if (ns) {
          ss << '_' << std::setfill('0') << std::setw(3) << ns;
        }
      }
    }
  }
  
  return ss.str();
}


RcppExport SEXP duration_from_string(SEXP s) {
  try {
    Rcpp::CharacterVector str(s);
    Rcpp::NumericVector res(str.size());
    for (R_xlen_t i=0; i<str.size(); ++i) {
      auto dur = from_string(Rcpp::as<std::string>(str[i]));
      res[i] = *reinterpret_cast<double*>(&dur);
    }
    return res;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


RcppExport SEXP duration_to_string(SEXP d) {
  try {
    Rcpp::NumericVector dur(d);
    Rcpp::CharacterVector res(dur.size());
    for (R_xlen_t i=0; i<dur.size(); ++i) {
      const auto dur_i = reinterpret_cast<const Global::duration*>(&dur[i]);
      res[i] = to_string(*dur_i);
    }
    if (dur.hasAttribute("names")) {
      res.names() = dur.names();
    } 
    return res;
  } catch(std::exception &ex) {	
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
  return R_NilValue;             // not reached
}


typedef ConstPseudoVector<REALSXP, double>   ConstPseudoVectorInt64;

RcppExport SEXP make_duration(SEXP h, SEXP m, SEXP s, SEXP n) {
  const Rcpp::NumericVector    h_nv(h);
  const Rcpp::NumericVector    m_nv(m);
  const Rcpp::NumericVector    s_nv(s);
  const Rcpp::NumericVector    n_nv(n);
  const ConstPseudoVectorInt64 h_i(h_nv);
  const ConstPseudoVectorInt64 m_i(m_nv);
  const ConstPseudoVectorInt64 s_i(s_nv);
  const ConstPseudoVectorInt64 n_i(n_nv);
  Rcpp::NumericVector res(std::max(std::max(h_i.size(), m_i.size()),
                                   std::max(s_i.size(), n_i.size())));
                                   
  for (R_xlen_t i=0; i<res.size(); ++i) {
    auto dur = (h_i[i]*3600 + m_i[i]*60 + s_i[i]) * 1e9 + n_i[i];
    res[i] = *reinterpret_cast<double*>(&dur);
  }
  // make this part of a utility package (it's used in 'period' too) LLL
  Rcpp::CharacterVector cl = Rcpp::CharacterVector::create("duration");
  cl.attr("package") = "nanotime";
  res.attr(".S3Class") = "integer64";
  res.attr("class") = cl;
  SET_S4_OBJECT(res);
  return Rcpp::S4(res);

  return res;
}

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


using namespace std::literals;


Global::duration from_string(const std::string& str) {
  Global::duration d = 0s;
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
    d += n * 1h;
    ++s;
    if (s + 5 > e || !isdigit(s[0]) || !isdigit(s[1]) || 
        s[2] != ':' || !isdigit(s[3]) || !isdigit(s[4])) {
      throw std::range_error("cannot parse duration");
    }
    d += ((s[0] - '0')*10 + (s[1] - '0'))*1min;

    // treat seconds in the general way:
    n = (s[3] - '0')*10 + (s[4] - '0');
    s += 5;
  }

  d += n * 1s;
  if (s == e) return sign*d;

  if (*s++ != '.') throw std::range_error("cannot parse duration");
  Global::duration mul = 100ms;
  unsigned i = 0;
  while (s < e) {
    if (mul < 1ns) throw std::range_error("cannot parse duration");
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
    if (d < 0s) {
      ss << '-';
      d *= -1;
    }
    auto h = d / 1h;
    d -= h * 3600s;
    auto min = d / 1min;
    d -= min * 60s;
    auto s = d / 1s;
    d -= s * 1s;
    ss << std::setfill ('0') 
       << std::setw(2) << h  << ':' 
       << std::setw(2) << min  << ':' 
       << std::setw(2) << s;
  
    // now handle nanoseconds 000_000_000
    auto ms = d / 1ms;
    d -= ms * 1ms;
    auto us = d / 1us;
    d -= us * 1us;
    auto ns = d / 1ns;
    d -= ns * 1ns;

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

#include <chrono>
#include <cstdint>
#include <sstream>
#include <iomanip>
#include <regex>
#include <stdexcept>
#include <tuple>
#include <limits>
#include <Rcpp.h>
#include "nanotime/duration.hpp"
#include "nanotime/pseudovector.hpp"
#include "nanotime/utilities.hpp"


using namespace nanotime;


duration nanotime::from_string(const std::string& str) {
  duration d = std::chrono::seconds(0);
  const char* s = str.c_str();
  const char* e = s + str.size();

  auto sign = 1;
  if (s < e && *s == '-') {
    sign = -1;
    ++s;
  }

  int n;
  if (!readNumber(s, e, n, false)) throw std::range_error("cannot parse nanoduration");

  if (s < e && *s == ':') {
    // we've got HHH:MM:SS format
    d += n * std::chrono::hours(1);
    ++s;
    if (s + 5 > e || !isdigit(s[0]) || !isdigit(s[1]) || 
        s[2] != ':' || !isdigit(s[3]) || !isdigit(s[4])) {
      throw std::range_error("cannot parse nanoduration");
    }
    d += ((s[0] - '0')*10 + (s[1] - '0'))*std::chrono::minutes(1);

    // treat seconds in the general way:
    n = (s[3] - '0')*10 + (s[4] - '0');
    s += 5;
  }

  d += n * std::chrono::seconds(1);
  if (s == e) return sign*d;

  if (*s++ != '.') throw std::range_error("cannot parse nanoduration");
  duration mul = std::chrono::milliseconds(100);
  unsigned i = 0;
  while (s < e) {
    if (mul < std::chrono::nanoseconds(1)) throw std::range_error("cannot parse nanoduration");
    if ((i == 3 || i == 6) && *s == '_') { ++s; continue; }
    ++i;
    if (!isdigit(*s)) throw std::range_error("cannot parse nanoduration");
    d += (*s - '0') * mul;
    mul /= 10;
    ++s;
  }

  return sign*d;
}


std::string nanotime::to_string(duration d) {
  std::stringstream ss;

  if (is_na(d)) {
    ss << "";
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


bool nanotime::is_na(duration d) {
  return d == duration::min();
}


// [[Rcpp::export]]
Rcpp::NumericVector duration_from_string_impl(Rcpp::CharacterVector str) {
  Rcpp::NumericVector res(str.size());
  for (R_xlen_t i=0; i<str.size(); ++i) {
    auto dur = from_string(Rcpp::as<std::string>(str[i]));
    double* ptr = reinterpret_cast<double*>(&dur);
    res[i] = *ptr;
  }
  if (str.hasAttribute("names")) {
    res.names() = str.names();
  }
  return assignS4("nanoduration", res, "integer64");
}

// [[Rcpp::export]]
Rcpp::CharacterVector duration_to_string_impl(Rcpp::NumericVector dur) {
  Rcpp::CharacterVector res(dur.size());
  for (R_xlen_t i=0; i<dur.size(); ++i) {
    const auto dur_i = reinterpret_cast<const duration*>(&dur[i]);
      
    res[i] = to_string(*dur_i);
    if (res[i].size() == 0) {
      res[i] = NA_STRING;
    }
  }
  if (dur.hasAttribute("names")) {
    res.names() = dur.names();
  }
  return res;
}

// [[Rcpp::export]]
Rcpp::LogicalVector duration_is_na_impl(Rcpp::NumericVector dur) {
  Rcpp::LogicalVector res(dur.size());
  for (R_xlen_t i=0; i<dur.size(); ++i) {
    const auto dur_i = reinterpret_cast<const duration*>(&dur[i]);
      
    res[i] = is_na(*dur_i);
  }
  if (dur.hasAttribute("names")) {
    res.names() = dur.names();
  }
  return res;
}

typedef ConstPseudoVector<REALSXP, double>   ConstPseudoVectorInt64;
typedef ConstPseudoVector<LGLSXP,  std::int32_t> ConstPseudoVectorLgl;


// [[Rcpp::export]]
Rcpp::NumericVector make_duration_impl(const Rcpp::NumericVector h_nv,
                                       const Rcpp::NumericVector m_nv,
                                       const Rcpp::NumericVector s_nv,
                                       const Rcpp::NumericVector n_nv) {
  const ConstPseudoVectorInt64 h_i(h_nv);
  const ConstPseudoVectorInt64 m_i(m_nv);
  const ConstPseudoVectorInt64 s_i(s_nv);
  const ConstPseudoVectorInt64 n_i(n_nv);
  Rcpp::NumericVector res(std::max(std::max(h_i.size(), m_i.size()),
                                   std::max(s_i.size(), n_i.size())));
                                   
  for (R_xlen_t i=0; i<res.size(); ++i) {
    auto h64 = *reinterpret_cast<const std::int64_t*>(&h_i[i]);
    auto m64 = *reinterpret_cast<const std::int64_t*>(&m_i[i]);
    auto s64 = *reinterpret_cast<const std::int64_t*>(&s_i[i]);
    auto n64 = *reinterpret_cast<const std::int64_t*>(&n_i[i]);
    auto dur = (h64*3600 + m64*60 + s64) * 1000000000L + n64;
    double *ptr = reinterpret_cast<double*>(&dur);
    res[i] = *ptr;
  }
  return assignS4("nanoduration", res, "integer64");
}


static double getNA_nanoduration() {
  const int64_t i64 = std::numeric_limits<std::int64_t>::min();
  double res;
  memcpy(&res, &i64, sizeof(res));
  return res;
}


// [[Rcpp::export]]
Rcpp::NumericVector nanoduration_subset_numeric_impl(const Rcpp::NumericVector& v, const Rcpp::NumericVector& idx) {
  Rcpp::NumericVector res(0);
  std::vector<double> res_c;    // by declaring it here we can make subset logical agnostic to double
  subset_numeric(v, idx, res, res_c, getNA_nanoduration);
  return assignS4("nanoduration", res, "integer64");
}


// [[Rcpp::export]]
Rcpp::NumericVector nanoduration_subset_logical_impl(const Rcpp::NumericVector& v, const Rcpp::LogicalVector& idx_p) {
  const ConstPseudoVectorLgl idx(idx_p);
  Rcpp::NumericVector res(0);
  std::vector<double> res_c;    // by declaring it here we can make subset logical agnostic to double
  subset_logical(v, idx, res, res_c, getNA_nanoduration);
  return assignS4("nanoduration", res, "integer64");
}

#ifndef NANOTIME_GLOBALS_HPP
#define NANOTIME_GLOBALS_HPP

#include <chrono>
#include <cctype>
#include "date.h"
#include "cctz/civil_time.h"
#include "cctz/time_zone.h"


namespace nanotime {


  using duration = std::chrono::nanoseconds;
  using dtime = std::chrono::time_point<std::chrono::system_clock, duration>;

  inline bool readNumber(const char*& s, const char* e, int& n, bool dosign) {
    n = 1;
    auto sorig = s;
    int sign = 1;
    if (dosign && *s == '-') {
      sign = -1;
      ++s;
    }
    if (s == e || !isdigit(*s)) {
      s = sorig;
      return false;
    }
    else {
      n *= *s - '0';
      s++;
    }
   
    while (s < e && *s >= '0' && *s <= '9') {
      n = 10*n + (*s - '0');
      ++s;
    } 

    n *= sign;
    return true;
  }


  /// Read an integer. This functions does not read beyond the end of
  /// 'sp' (i.e. 'se') and does not read more that expectmax
  /// characters. If the number of characters read is smaller than
  /// 'expectmin' the function raises an exception.
  inline uint64_t readInt(const char*& sp, 
                        const char* const se,
                        const int expectmin,
                        const int expectmax) {
    const char* s = sp;
    int res = 0;
    int i;
    for (i=0; i<expectmax && sp < se; ++i) {
      if (*sp >= '0' && *sp <= '9') {
        res = 10 * res + *sp++ - 0x30;
      }
      else {
        break;
      }
    }
    if (sp - s >= expectmin) {
      return res;
    }
    else {
      throw std::range_error("cannot parse datetime element");
    }
  }

  inline const std::string readString(const char*& sp, const char* const se) {
    const char* const s = sp;
    while (sp < se) {
      if ((*sp >= 'A' && *sp <= 'Z') || 
          (*sp >= 'a' && *sp <= 'z') || 
          (*sp == '_')               || 
          (*sp >= '/' && *sp <= '9')) {
        ++sp;
      }
      else {
        break;
      }
    }  
    if (sp > s) {
      return std::string(s, sp-s);
    } 
    else {
      throw std::range_error("cannot parse datetime timezone"); // # nocov
    }
  }


  inline void skipWhitespace(const char*& sp, const char* const se) {
    while (sp < se) {
      if (*sp == ' ' || *sp == '\t') {
        ++sp;
      }
      else {
        break;
      }
    }
  }

  struct tmdet {
    unsigned y;
    unsigned m;
    unsigned d;
    unsigned hh;
    unsigned mm;
    unsigned ss;
    std::int64_t ns;
    std::string tzstr;
    std::int64_t offset;
  };

  
  inline tmdet readDtime(const char*& sp, const char* const se) 
  {
    try {
      const unsigned y = readInt(sp, se, 4, 4);
      if (*sp == ' ' || *sp == '-' || *sp == '/') ++sp;
      const unsigned m = readInt(sp, se, 2, 2);
      if (*sp == ' ' || *sp == '-' || *sp == '/') ++sp;
      const unsigned d = readInt(sp, se, 2, 2);

      skipWhitespace(sp, se);
      if (sp < se && *sp == 'T') ++sp;

      unsigned h, mn, s;
      if (isdigit(*sp) || *(sp-1) == 'T') {       // the time is optional
        h = readInt(sp, se, 2, 2);
        if (*sp == ':') ++sp;
        mn = readInt(sp, se, 2, 2);
        if (*sp == ':') ++sp;
        s = readInt(sp, se, 2, 2);
      } else {
        h = mn = s = 0;
      }

      // optional fractional part
      std::int64_t mul = 100000000;
      std::int64_t ns = 0;
      if (*sp == '.') {
        ++sp;
        unsigned i = 0;
        while (sp < se && mul >= 1) {
          if ((i == 3 || i == 6) && *sp == '_') { ++sp; continue; }
          ++i;
          if (!isdigit(*sp)) break;
          ns += (*sp - '0') * mul;
          mul /= 10;
          ++sp;
        }
      }

      skipWhitespace(sp, se);
      
      // not as much as we could test, but good enough without too much of
      // a performance hit:
      if (m < 1 || m > 12) throw std::range_error("month must be >= 1 and <= 12");
      if (d < 1 || d > 31) throw std::range_error("day must be >= 1 and <= 31");
      if (h > 23) throw std::range_error("hour must be < 24");
      if (mn > 59) throw std::range_error("minute must be < 60");
      if (s > 59) throw std::range_error("second must be < 60"); // debatable

      // optional offset
      std::string tzstr_str;    // need to persist this copy!
      std::int64_t offset = 0;
      if (*sp == '+' || *sp == '-') {
        int64_t sign = *sp == '-' ? -1 : 1;
        int64_t h_offset = readInt(++sp, se, 2, 2);
        if (*sp != ':' && *sp != ' ') {
          throw std::range_error("Error parsing offset");
        }
	int64_t m_offset = readInt(++sp, se, 2, 2);
        offset = sign * h_offset * 3600 + m_offset * 60;
        tzstr_str = "UTC";
      } else if (isalpha(*sp)) {
        // or timezone       
        tzstr_str = readString(sp, se);
      }
      skipWhitespace(sp, se);

      if (tzstr_str == "Z") {   // consider "Z" a shorhand for "UTC"
        tzstr_str = "UTC";	// #nocov
      }
      return tmdet{y, m, d, h, mn, s, ns, tzstr_str, offset};
      
    } catch(std::exception &ex) {	
      forward_exception_to_r(ex);
    } catch(...) { 
      ::Rf_error("c++ exception (unknown reason)"); 
    }
    // not reached:
    return tmdet{0, 0, 0, 0, 0, 0, 0, "", 0};;
  }

  template<int T, class TT, class I, typename getNA>
  void subset_numeric(const Rcpp::Vector<T>& v, const I& pindx, Rcpp::Vector<T>& res, std::vector<TT>& res_c, getNA fna) {
    if (!v.hasAttribute("names")) {
      for (R_xlen_t i = 0; i < pindx.size(); i++) {
        auto ii = pindx[i];
        if (ii < 0) {
          Rcpp::stop("only 0's may be mixed with negative subscripts"); // #nocov
        }
        if (0 < ii && ii <= v.length()) {
          ii--;
          res_c.push_back(v[ii]);
        }  else if (ii != 0) { /* out of bounds or NA */
          res_c.push_back(fna());
        }
      }
      res = Rcpp::Vector<T>(res_c.size());
      for (uint64_t i = 0; i < res_c.size(); i++) {
        res[i] = res_c[i];
      }
    } else {
      // repeat the code, but subsetting names at the same time:
      const auto& names = Rcpp::CharacterVector(v.names());
      auto names_c = std::vector<const char*>();
      static const char* NA_STRING_LOCAL = "dummy"; // so we can keep track of NA
      for (R_xlen_t i = 0; i < pindx.size(); i++) {
        auto ii = pindx[i];
        if (ii < 0) {
          Rcpp::stop("only 0's may be mixed with negative subscripts"); // #nocov
        }
        if (0 < ii && ii <= v.length()) {
          ii--;
          res_c.push_back(v[ii]);
          names_c.push_back(Rcpp::CharacterVector::is_na(names[ii]) ? NA_STRING_LOCAL : names[ii]);
        } else if (ii != 0) { /* out of bounds or NA */
           res_c.push_back(fna());
           names_c.push_back(NA_STRING_LOCAL);
        }
      }
      res = Rcpp::Vector<T>(res_c.size());
      auto res_names = Rcpp::CharacterVector(res.size());
      for (uint32_t i = 0; i < res_c.size(); i++) {
        res[i] = res_c[i];
        if (names_c[i] == NA_STRING_LOCAL) {
          res_names[i] = NA_STRING;
        } else {
          res_names[i] = names_c[i];
        }
      }
      res.names() = res_names;
    }
  }
  
  template<int T, class TT, class I, typename getNA>
  void subset_logical(const Rcpp::Vector<T>& v, const I& pindx, Rcpp::Vector<T>& res, std::vector<TT>& res_c, getNA fna) {
   if (!v.hasAttribute("names")) {
     for (R_xlen_t i = 0; i < v.size(); i++) {        
        if (pindx[i] == NA_LOGICAL) {
          res_c.push_back(fna());
        } else if (pindx[i]) {
          res_c.push_back(v[i]);
        }
      }
      res = Rcpp::Vector<T>(res_c.size());
      for (uint64_t i = 0; i < res_c.size(); i++) {
        res[i] = res_c[i];
      }
    } else {
      // repeat the code, but also subsetting names:
      const auto& names = Rcpp::CharacterVector(v.names());
      auto names_c = std::vector<const char*>();
      static const char* NA_STRING_LOCAL = "dummy"; // so we can keep track of NA
      for (R_xlen_t i = 0; i < v.size(); i++) {
        if (pindx[i] == NA_LOGICAL) {
          res_c.push_back(fna());
          names_c.push_back(NA_STRING_LOCAL);
        } else if (pindx[i]) {
          res_c.push_back(v[i]);
          names_c.push_back(names[i]);
        }
      }
      res = Rcpp::Vector<T>(res_c.size());
      auto res_names = Rcpp::CharacterVector(res.size());
      for (uint32_t i = 0; i < res_c.size(); i++) {
        res[i] = res_c[i];
        if (names_c[i] == NA_STRING_LOCAL) {
          res_names[i] = NA_STRING;
        } else {
          res_names[i] = names_c[i];
        }
      }
      res.names() = res_names;
    }
  }

  const int64_t NA_INTEGER64 = std::numeric_limits<int64_t>::min();

  const int MAX_TZ_STR_LENGTH = 1000;
  
} // end namespace nanotime


namespace nanotime_ops {

  /// This set of functors, to the contrary of the stdlib, allow
  /// operations where the operands and the results are of different
  /// types. This is handy for example is adding duration to time
  /// points, etc.
  template<typename T, typename U, typename R>
  struct plus {
    inline R operator()(const T& t, const U& u) const {
      return t + u;
    }
  };  

  template<typename T, typename U, typename R>
  struct minus {
    inline R operator()(const T& t, const U& u) const {
      return t - u;
    }
  };  

  template<typename T, typename U, typename R>
  struct multiplies {
    inline R operator()(const T& t, const U& u) const {
      return t * u;
    }
  };  

  template<typename T, typename U, typename R>
  struct divides {
    inline R operator()(const T& t, const U& u) const {
      return t / u;
    }
  }; 

}

#endif

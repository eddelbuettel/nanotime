#ifndef GLOBALS_HPP
#define GLOBALS_HPP

#include <chrono>
#include <cctype>

namespace Global {


  using dtime = std::chrono::system_clock::time_point;
  using duration = dtime::duration;

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

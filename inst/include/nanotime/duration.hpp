#ifndef NANOTIME_DURATION_HPP
#define NANOTIME_DURATION_HPP

#include <string>
#include "globals.hpp"


namespace nanotime {

  duration from_string(const std::string& str);
  std::string to_string(duration d);
  bool is_na(duration d);

}

#endif

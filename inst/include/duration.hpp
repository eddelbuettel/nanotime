#ifndef DURATION_HPP
#define DURATION_HPP

#include <string>
#include "globals.hpp"


Global::duration from_string(const std::string& str);
std::string to_string(Global::duration d);


#endif

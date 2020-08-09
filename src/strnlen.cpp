
// SunOS has no strnlen
#include <string.h>

namespace nanotime {

size_t strnlen_(const char *s, size_t maxlen) {
  size_t len;
  for (len = 0; len < maxlen; len++, s++) {
    if (!*s)
      break;
  }
  return (len);
}

}

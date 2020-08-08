#ifndef NANOTIME_UTILITIES_HPP
#define NANOTIME_UTILITIES_HPP


namespace nanotime {

  // helper functions to manage name copying in binary operations:

  inline Rcpp::CharacterVector copyNamesOut(const Rcpp::CharacterVector& nm) {
    if (nm.size() == 0) return nm;
    else {
      Rcpp::CharacterVector res =  clone(nm);
      return res;
    }
  }


  inline Rcpp::CharacterVector getNames(const Rcpp::CharacterVector& nm1, bool scalar1, 
                                        const Rcpp::CharacterVector& nm2, bool scalar2) {
    // these are the R rules for name handling in binary operations:
    if      (nm1.size() == 0)      return copyNamesOut(nm2);
    else if (nm2.size() == 0)      return copyNamesOut(nm1);
    else if (scalar1 && !scalar2)  return copyNamesOut(nm2);
    else if (scalar2 && !scalar1)  return copyNamesOut(nm1);
    else                           return copyNamesOut(nm1);
  }


  template <int T1, int T2, int T3>
  void copyNames(const Rcpp::Vector<T1>& e1_cv,
                 const Rcpp::Vector<T2>& e2_cv,
                 Rcpp::Vector<T3>& res) {
    auto nm1 = e1_cv.hasAttribute("names") ?
      Rcpp::CharacterVector(e1_cv.names()) : Rcpp::CharacterVector(0);
    auto nm2 = e2_cv.hasAttribute("names") ?
      Rcpp::CharacterVector(e2_cv.names()) : Rcpp::CharacterVector(0);
    auto resnames = getNames(nm1, e1_cv.size() == 1, nm2, e2_cv.size() == 1);
    if (resnames.size()) {
      res.names() = resnames;
    }
  }



  // helper functions to set the class and the S4 bit on R objects:

  template<int R>
  SEXP assignS4(const char* classname, Rcpp::Vector<R>& res) {
    Rcpp::CharacterVector cl = Rcpp::CharacterVector::create(classname);
    cl.attr("package") = "nanotime";
    res.attr("class") = cl;
    SET_S4_OBJECT(res);
    return Rcpp::S4(res);
  }


  // .S3Class is needed for S4 classes that are based on S3 classes:

  template<int R>
  SEXP assignS4(const char* classname, Rcpp::Vector<R>& res, const char* oldClass) {
    Rcpp::CharacterVector cl = Rcpp::CharacterVector::create(classname);
    cl.attr("package") = "nanotime";
    res.attr("class") = cl;
    // use 'install' here as in R source code attrib.c: LLL
    Rcpp::CharacterVector oc = Rcpp::CharacterVector::create(oldClass);
    res.attr(".S3Class") = oc;
    SET_S4_OBJECT(res);
    return Rcpp::S4(res);
  }


  inline void checkVectorsLengths(SEXP x, SEXP y) {
    auto nx = XLENGTH(x);
    auto ny = XLENGTH(y);
    // the following as in R source code: 'arithmetic.c' function 'R_binary':
    if (nx > 0 && ny > 0 && ((nx > ny) ? nx % ny : ny % nx) != 0) {
      Rf_warning("longer object length is not a multiple of shorter object length");
    }
  }

  inline void checkVectorsLengths(SEXP x, SEXP y, SEXP z) {
    checkVectorsLengths(x, y);
    checkVectorsLengths(x, z);
    checkVectorsLengths(y, z);
  }

  inline void checkVectorsLengths(SEXP x, SEXP y, SEXP z, SEXP u) {
    checkVectorsLengths(x, y, z);
    checkVectorsLengths(x, y, u);
    checkVectorsLengths(y, z, u);
  }

  inline ssize_t getVectorLengths(SEXP x, SEXP y) {
    if (XLENGTH(x) == 0 || XLENGTH(y) == 0) {
      return 0;
    } else {
      return std::max(XLENGTH(x), XLENGTH(y));
    }
  }

  inline ssize_t getVectorLengths(SEXP x, SEXP y, SEXP z) {
    if (XLENGTH(x) == 0 || XLENGTH(y) == 0 || XLENGTH(z) == 0) {
      return 0;
    } else {
      return std::max(XLENGTH(x), std::max(XLENGTH(y), XLENGTH(z)));
    }
  }

  inline ssize_t getVectorLengths(SEXP x, SEXP y, SEXP z, SEXP u) {
    if (XLENGTH(x) == 0 || XLENGTH(y) == 0 || XLENGTH(z) == 0 || XLENGTH(u)==0) {
      return 0;
    } else {
      return std::max(std::max(XLENGTH(x), XLENGTH(y)), std::max(XLENGTH(z), XLENGTH(u)));
    }
  }

  // SunOS has no strnlen_; definition in src/strnlen.cpp
  size_t strnlen_(const char *s, size_t maxlen);

} // end namespace nanotime

#endif

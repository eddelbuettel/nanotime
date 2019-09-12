#ifndef UTILITIES_HPP
#define UTILITIES_HPP


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





#endif

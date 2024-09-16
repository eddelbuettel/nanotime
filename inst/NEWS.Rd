\name{NEWS}
\title{News for Package \pkg{nanotime}}
\newcommand{\ghpr}{\href{https://github.com/eddelbuettel/nanotime/pull/#1}{##1}}
\newcommand{\ghit}{\href{https://github.com/eddelbuettel/nanotime/issues/#1}{##1}}

\section{Changes in version 0.3.10 (2024-09-16)}{
  \itemize{
    \item Retire several checks for Solaris in test suite (Dirk in \ghpr{130})
    \item Switch to Authors@R in DESCRIPTION as now required by CRAN 
    \item Accommodate R-devel change for setdiff (Dirk in \ghpr{133} fixing
    \ghit{132})
    \item No longer ship defunction ggplot2 demo (Dirk fixing \ghit{131})
  }
}

\section{Changes in version 0.3.9 (2024-06-21)}{
  \itemize{
    \item Condition two tests to not run on arm64 (Dirk in \ghpr{129} fixing
    \ghit{128}) 
  }
}

\section{Changes in version 0.3.8 (2024-06-19)}{
  \itemize{
    \item Time format documentation now has a reference to \pkg{RcppCCTZ}
    \item The package no longer sets a default C++ compilation standard of
    C++11 (Dirk initially in \ghpr{114}, and later switched to C++17)
    \item New \code{accurate} parameter for conversion from \code{POSIXct} to
    \code{nanotime} (Davor Josipovic and Leonardo in \ghpr{116} closing
    \ghit{115})
    \item The \code{as.Date()} function is now vectorized and can take a TZ
    argument (Leonardo and Dirk in \ghpr{119} closing \ghit{118})
    \item Use of internal function \code{SET_S4_OBJECT} has been replaced by
    API function \code{Rf_asS4} (Leonardo in \ghpr{121} closing \ghit{120})
    \item An \code{nanoduration} / \code{nanoduration} expression now returns
    a double (Leonardo in \ghpr{122} closing \ghit{117})
    \item Bitfield calculations no longer require an Windows-only compiler
    switch (Leonardo in \ghpr{124})
    \item A simple manual page format nag involving has been addressed (Dirk
    in \ghpr{126} fixing \ghit{125})
    \item An set of tests tickling an UBSAN issue via \pkg{Rcpp} code no
    longer run unless \code{CI} is set (Dirk in \ghpr{127} fixing \ghit{123})
  }
}

\section{Changes in version 0.3.7 (2022-10-23)}{
  \itemize{
    \item Update mkdocs for material docs generator (Dirk in \ghpr{102})
    \item Use \code{inherits()} instead comparing to \code{class()}
    (Trevor Davis in \ghpr{104})
    \item Set default arguments in \code{nanoduration()} (Trevor Davis
    in \ghpr{105})
    \item Add \code{as.nanoduration.difftime()} support (Trevor Davis
    in \ghpr{106})
    \item Add +/- methods for \code{nanotime} and \code{difftime}
    objects (Trevor Davis in \ghpr{110} closing \ghit{108}, \ghit{107}) 
  }
}

\section{Changes in version 0.3.6 (2022-03-06)}{
  \itemize{
    \item Fix incorrect subsetting with operator \code{\%in\%} (Leonardo
    in \ghpr{100} fixing \ghit{99}).
    \item Fix incorrect parsing for negative nanoperiod (Leonardo in
    \ghpr{100} fixing \ghit{96}).
    \item Test for \code{class} via \code{inherits()} (Dirk).
  }
}

\section{Changes in version 0.3.5 (2021-12-14)}{
  \itemize{
    \item Applied patch by Tomas Kalibera for Windows UCRT under the
    upcoming R 4.2.0 expected for April.
  }
}

\section{Changes in version 0.3.4 (2021-11-24)}{
  \itemize{
    \item Added a few more \code{as.character} conversion function (Dirk)
    \item Expose \code{nanoperiod} functionality via header file for use
    by other packages (Leonardo in \ghpr{95} fixing \ghit{94}).
  }
}

\section{Changes in version 0.3.3 (2021-08-09)}{
  \itemize{
    \item New demo \code{ggplot2Example.R} (Leonardo and Dirk). 
    \item New documentation website using mkdocs-material (Dirk).
    \item Updated unit test to account for r-devel POSIXct changes,
    and re-enable full testing under r-devel (Dirk).
    \item Additional \code{nanoduration} and \code{character} ops plus
    tests (Colin Umansky in \ghpr{88} addressing \ghit{87}).
    \item New \code{plus} and \code{minus} functions for periods
    (Leonardo in \ghpr{91}). 
  }
}

\section{Changes in version 0.3.2 (2020-09-03)}{
  \itemize{
    \item Correct for big endian (Elliott Sales de Andrade in \ghpr{81}). 
    \item Use the \code{RcppCCTZ_API.h} header (Dirk in \ghpr{82}).
    \item Conditionally reduce test coverage (Dirk in \ghpr{83}).
  }
}

\section{Changes in version 0.3.1 (2020-08-09)}{
  \itemize{
    \item Several small cleanups to ensure a more robust compilation
    (Leonardo and Dirk in \ghpr{75} fixing \ghit{74}).
    \item Show Solaris some extra love by skipping tests and examples
    with a timezone (Dirk in \ghpr{76}).
  }
}

\section{Changes in version 0.3.0 (2020-08-06)}{
  \itemize{
    \item Use \code{tzstr=} instead of \code{tz=} in call to
    \pkg{RcppCCTZ::parseDouble()}) (Matt Dowle in \ghpr{49}). 
    \item Add new comparison operators for \code{nanotime} and
    \code{charcters} (Dirk in \ghpr{54} fixing \ghit{52}).
    \item Switch from \pkg{RUnit} to \pkg{tinytest} (Dirk in \ghpr{55})
    \item Substantial functionality extension in with new types
    \code{nanoduration}, \code{nanoival} and \code{nanoperiod} (Leonardo
    in \ghpr{58}, \ghpr{60}, \ghpr{62}, \ghpr{63}, \ghpr{65}, \ghpr{67},
    \ghpr{70} fixing \ghit{47}, \ghit{51}, \ghit{57}, \ghit{61},
    \ghit{64} with assistance from Dirk). 
    \item A new (yet still draft-ish) vignette was added describing the
    four core types (Leonardo and Dirk in \ghpr{71}).
    \item A required compilation flag for Windows was added (Leonardo in
    \ghpr{72}).
    \item \pkg{RcppCCTZ} function are called in new 'non-throwing'
    variants to not trigger exeception errors (Leonardo in \ghpr{73}).
  }
}

\section{Changes in version 0.2.4 (2019-05-25)}{
  \itemize{
    \item Define [[ method (Dirk in \ghpr{45} fixing \ghit{44}).
  }
}

\section{Changes in version 0.2.3 (2018-09-30)}{
  \itemize{
    \item Skip some tests on Solaris which seems borked with timezones.
    As we have no real access, no real fix possible (Dirk in \ghpr{42}).
    \item Update Travis setup
  }
}

\section{Changes in version 0.2.2 (2018-07-18)}{
  \itemize{
    \item Unit tests depending on future \CRANpkg{xts} behaviour remain
    disabled (Dirk in \ghpr{41}).
  }
}

\section{Changes in version 0.2.1 (2018-07-01)}{
  \itemize{
    \item Added attribute-preserving comparison (Leonardo in \ghpr{33}).
    \item Added two \code{integer64} casts in constructors (Dirk in
    \ghpr{36}).
    \item Added two checks for empty arguments (Dirk in \ghpr{37}).
  }
}

\section{Changes in version 0.2.0 (2017-06-22)}{
  \itemize{
    \item Rewritten in S4 to provide more robust operations (\ghpr{17}
    by Leonardo)
    \item Ensure \code{tz=""} is treated as unset (Leonardo in \ghpr{20})
    \item Added \code{format} and \code{tz} arguments to
    \code{nanotime}, \code{format}, \code{print} (\ghpr{22} by Leonardo
    and Dirk)
    \item Ensure printing respect \code{options()$max.print}, ensure
    names are kept with vector (\ghpr{23} by Leonardo)
    \item Correct \code{summary()} by defining \code{names<-} (Leonardo
    in \ghpr{25} fixing \ghit{24})
    \item Report error on operations that are meaningful for type;
    handled NA, NaN, Inf, -Inf correctly (Leonardo in \ghpr{27} fixing
    \ghit{26})   
  }
}
  
\section{Changes in version 0.1.2 (2017-03-27)}{
  \itemize{
    \item The \code{as.integer64} function is now exported as well.
  }
}

\section{Changes in version 0.1.1 (2017-02-04)}{
  \itemize{
    \item The default display format now always shows nine digits
    (\ghpr{10} closing \ghpr{9})
    \item The default print method was updated to use formated output,
    and a new new converter \code{as.integer64} was added
    \item Several 'Ops' method are now explicitly defined allowing
    casting of results (rather than falling back on bit64 behaviour)
    \item The format routine is now more careful about not loosing
    precision (\ghit{13} closing \ghit{12})
  }
}

\section{Changes in version 0.1.0 (2017-01-10)}{
  \itemize{
    \item Added Windows support thanks to expanded \CRANpkg{RcppCCTZ}
    (closes \ghit{6})
    \item Added "mocked up" demo with nanosecond delay networking analysis
    \item Added 'fmt' and 'tz' options to output functions, expanded
    \code{format.nanotime} (closing \ghit{2} and \ghit{3})
    \item Added data.frame support
    \item Expanded tests
  }
}
  
\section{Changes in version 0.0.1 (2016-12-15)}{
  \itemize{
    \item Initial CRAN upload.
    \item Package is functional and provides examples.
  }
}

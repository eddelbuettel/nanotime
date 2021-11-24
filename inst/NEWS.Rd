\name{NEWS}
\title{News for Package \pkg{nanotime}}
\newcommand{\ghpr}{\href{https://github.com/eddelbuettel/nanotime/pull/#1}{##1}}
\newcommand{\ghit}{\href{https://github.com/eddelbuettel/nanotime/issues/#1}{##1}}

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

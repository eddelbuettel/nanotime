\name{NEWS}
\title{News for Package \pkg{nanotime}}
\newcommand{\ghpr}{\href{https://github.com/eddelbuettel/nanotime/pull/#1}{##1}}
\newcommand{\ghit}{\href{https://github.com/eddelbuettel/nanotime/issues/#1}{##1}}

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

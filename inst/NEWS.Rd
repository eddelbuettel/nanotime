\name{NEWS}
\title{News for Package \pkg{RcppCCTZ}}
\newcommand{\ghpr}{\href{https://github.com/eddelbuettel/nanotime/pull/#1}{##1}}
\newcommand{\ghit}{\href{https://github.com/eddelbuettel/nanotime/issues/#1}{##1}}

\section{Changes in version 0.1.1 (2017-xx-yy)}{
  \itemize{
    \item The default display format now always shows nine digits
    (\ghpr{10} closing \ghpr{9})
    \item The default print method was updated to use formated output
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

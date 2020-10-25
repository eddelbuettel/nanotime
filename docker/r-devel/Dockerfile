## Emacs, make this -*- mode: sh; -*-

## we use drd as we want to test under r-devel
FROM rocker/drd

LABEL org.label-schema.license="GPL-2.0" \
      org.label-schema.vcs-url="https://github.com/rvw-org" \
      maintainer="Dirk Eddelbuettel <edd@debian.org>"

## This may not be needed but helps at times
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update \
        && apt-get upgrade -y \
        && apt-get install -y --no-install-recommends \
               git \
        && RDscript -e 'install.packages(c("bit64", \
                                           "RcppCCTZ", \
                                           "tinytest", \
                                           "data.table", \
                                           "xts", \
                                           "RcppDate"))' \
        && git clone --single-branch --branch master \
		https://github.com/eddelbuettel/nanotime nanotime \
    && cd nanotime \
    && RD CMD INSTALL .

## then test under R-devel
## RDscript -e 'tinytest::run_test_dir("inst/tinytest")'

CMD ["bash"]

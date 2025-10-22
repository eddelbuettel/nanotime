

# News for Package <span class="pkg">nanotime</span>

## Changes in version 0.3.12.1 (unreleased)

<ul>
<li>

The <code>methods</code> package is now a Depends as WRE recommends
(Michael Chirico in
<a href="https://github.com/eddelbuettel/nanotime/pull/141">[#141](https://github.com/eddelbuettel/nanotime/issues/141)</a>
based on suggestion by Dirk in
<a href="https://github.com/eddelbuettel/nanotime/issues/140">[#140](https://github.com/eddelbuettel/nanotime/issues/140)</a>)

</li>
<li>

The mkdocs-material documentation site is now generated via
<span class="pkg">altdoc</span>

</li>
</ul>

## Changes in version 0.3.12 (2025-04-02)

<ul>
<li>

Update continuous integration to use r-ci action with bootstrap

</li>
<li>

Add ggplot2 to Suggests due to use in demo/ which is now scanned

</li>
<li>

Refine a version comparison for R 4.5.0 to be greater or equal

</li>
</ul>

## Changes in version 0.3.11 (2025-01-10)

<ul>
<li>

Explicit <code>Rcomplex</code> assignment accommodates pickier compilers
over newer R struct (Michael Chirico in
<a href="https://github.com/eddelbuettel/nanotime/pull/135">[#135](https://github.com/eddelbuettel/nanotime/issues/135)</a>
fixing
<a href="https://github.com/eddelbuettel/nanotime/issues/134">[#134](https://github.com/eddelbuettel/nanotime/issues/134)</a>)

</li>
<li>

When formatting, <code>NA</code> are flagged before <code>CCTZ</code>
call to to not trigger santizier, and set to NA after call (Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/136">[#136](https://github.com/eddelbuettel/nanotime/issues/136)</a>)

</li>
</ul>

## Changes in version 0.3.10 (2024-09-16)

<ul>
<li>

Retire several checks for Solaris in test suite (Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/130">[#130](https://github.com/eddelbuettel/nanotime/issues/130)</a>)

</li>
<li>

Switch to Authors@R in DESCRIPTION as now required by CRAN

</li>
<li>

Accommodate R-devel change for setdiff (Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/133">[#133](https://github.com/eddelbuettel/nanotime/issues/133)</a>
fixing
<a href="https://github.com/eddelbuettel/nanotime/issues/132">[#132](https://github.com/eddelbuettel/nanotime/issues/132)</a>)

</li>
<li>

No longer ship defunction ggplot2 demo (Dirk fixing
<a href="https://github.com/eddelbuettel/nanotime/issues/131">[#131](https://github.com/eddelbuettel/nanotime/issues/131)</a>)

</li>
</ul>

## Changes in version 0.3.9 (2024-06-21)

<ul>
<li>

Condition two tests to not run on arm64 (Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/129">[#129](https://github.com/eddelbuettel/nanotime/issues/129)</a>
fixing
<a href="https://github.com/eddelbuettel/nanotime/issues/128">[#128](https://github.com/eddelbuettel/nanotime/issues/128)</a>)

</li>
</ul>

## Changes in version 0.3.8 (2024-06-19)

<ul>
<li>

Time format documentation now has a reference to
<span class="pkg">RcppCCTZ</span>

</li>
<li>

The package no longer sets a default C++ compilation standard of C++11
(Dirk initially in
<a href="https://github.com/eddelbuettel/nanotime/pull/114">[#114](https://github.com/eddelbuettel/nanotime/issues/114)</a>,
and later switched to C++17)

</li>
<li>

New <code>accurate</code> parameter for conversion from
<code>POSIXct</code> to <code>nanotime</code> (Davor Josipovic and
Leonardo in
<a href="https://github.com/eddelbuettel/nanotime/pull/116">[#116](https://github.com/eddelbuettel/nanotime/issues/116)</a>
closing
<a href="https://github.com/eddelbuettel/nanotime/issues/115">[#115](https://github.com/eddelbuettel/nanotime/issues/115)</a>)

</li>
<li>

The <code>as.Date()</code> function is now vectorized and can take a TZ
argument (Leonardo and Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/119">[#119](https://github.com/eddelbuettel/nanotime/issues/119)</a>
closing
<a href="https://github.com/eddelbuettel/nanotime/issues/118">[#118](https://github.com/eddelbuettel/nanotime/issues/118)</a>)

</li>
<li>

Use of internal function <code>SET_S4_OBJECT</code> has been replaced by
API function <code>Rf_asS4</code> (Leonardo in
<a href="https://github.com/eddelbuettel/nanotime/pull/121">[#121](https://github.com/eddelbuettel/nanotime/issues/121)</a>
closing
<a href="https://github.com/eddelbuettel/nanotime/issues/120">[#120](https://github.com/eddelbuettel/nanotime/issues/120)</a>)

</li>
<li>

An <code>nanoduration</code> / <code>nanoduration</code> expression now
returns a double (Leonardo in
<a href="https://github.com/eddelbuettel/nanotime/pull/122">[#122](https://github.com/eddelbuettel/nanotime/issues/122)</a>
closing
<a href="https://github.com/eddelbuettel/nanotime/issues/117">[#117](https://github.com/eddelbuettel/nanotime/issues/117)</a>)

</li>
<li>

Bitfield calculations no longer require an Windows-only compiler switch
(Leonardo in
<a href="https://github.com/eddelbuettel/nanotime/pull/124">[#124](https://github.com/eddelbuettel/nanotime/issues/124)</a>)

</li>
<li>

A simple manual page format nag involving has been addressed (Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/126">[#126](https://github.com/eddelbuettel/nanotime/issues/126)</a>
fixing
<a href="https://github.com/eddelbuettel/nanotime/issues/125">[#125](https://github.com/eddelbuettel/nanotime/issues/125)</a>)

</li>
<li>

An set of tests tickling an UBSAN issue via
<span class="pkg">Rcpp</span> code no longer run unless <code>CI</code>
is set (Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/127">[#127](https://github.com/eddelbuettel/nanotime/issues/127)</a>
fixing
<a href="https://github.com/eddelbuettel/nanotime/issues/123">[#123](https://github.com/eddelbuettel/nanotime/issues/123)</a>)

</li>
</ul>

## Changes in version 0.3.7 (2022-10-23)

<ul>
<li>

Update mkdocs for material docs generator (Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/102">[#102](https://github.com/eddelbuettel/nanotime/issues/102)</a>)

</li>
<li>

Use <code>inherits()</code> instead comparing to <code>class()</code>
(Trevor Davis in
<a href="https://github.com/eddelbuettel/nanotime/pull/104">[#104](https://github.com/eddelbuettel/nanotime/issues/104)</a>)

</li>
<li>

Set default arguments in <code>nanoduration()</code> (Trevor Davis in
<a href="https://github.com/eddelbuettel/nanotime/pull/105">[#105](https://github.com/eddelbuettel/nanotime/issues/105)</a>)

</li>
<li>

Add <code>as.nanoduration.difftime()</code> support (Trevor Davis in
<a href="https://github.com/eddelbuettel/nanotime/pull/106">[#106](https://github.com/eddelbuettel/nanotime/issues/106)</a>)

</li>
<li>

Add +/- methods for <code>nanotime</code> and <code>difftime</code>
objects (Trevor Davis in
<a href="https://github.com/eddelbuettel/nanotime/pull/110">[#110](https://github.com/eddelbuettel/nanotime/issues/110)</a>
closing
<a href="https://github.com/eddelbuettel/nanotime/issues/108">[#108](https://github.com/eddelbuettel/nanotime/issues/108)</a>,
<a href="https://github.com/eddelbuettel/nanotime/issues/107">[#107](https://github.com/eddelbuettel/nanotime/issues/107)</a>)

</li>
</ul>

## Changes in version 0.3.6 (2022-03-06)

<ul>
<li>

Fix incorrect subsetting with operator <code>%in%</code> (Leonardo in
<a href="https://github.com/eddelbuettel/nanotime/pull/100">[#100](https://github.com/eddelbuettel/nanotime/issues/100)</a>
fixing
<a href="https://github.com/eddelbuettel/nanotime/issues/99">[#99](https://github.com/eddelbuettel/nanotime/issues/99)</a>).

</li>
<li>

Fix incorrect parsing for negative nanoperiod (Leonardo in
<a href="https://github.com/eddelbuettel/nanotime/pull/100">[#100](https://github.com/eddelbuettel/nanotime/issues/100)</a>
fixing
<a href="https://github.com/eddelbuettel/nanotime/issues/96">[#96](https://github.com/eddelbuettel/nanotime/issues/96)</a>).

</li>
<li>

Test for <code>class</code> via <code>inherits()</code> (Dirk).

</li>
</ul>

## Changes in version 0.3.5 (2021-12-14)

<ul>
<li>

Applied patch by Tomas Kalibera for Windows UCRT under the upcoming R
4.2.0 expected for April.

</li>
</ul>

## Changes in version 0.3.4 (2021-11-24)

<ul>
<li>

Added a few more <code>as.character</code> conversion function (Dirk)

</li>
<li>

Expose <code>nanoperiod</code> functionality via header file for use by
other packages (Leonardo in
<a href="https://github.com/eddelbuettel/nanotime/pull/95">[#95](https://github.com/eddelbuettel/nanotime/issues/95)</a>
fixing
<a href="https://github.com/eddelbuettel/nanotime/issues/94">[#94](https://github.com/eddelbuettel/nanotime/issues/94)</a>).

</li>
</ul>

## Changes in version 0.3.3 (2021-08-09)

<ul>
<li>

New demo <code>ggplot2Example.R</code> (Leonardo and Dirk).

</li>
<li>

New documentation website using mkdocs-material (Dirk).

</li>
<li>

Updated unit test to account for r-devel POSIXct changes, and re-enable
full testing under r-devel (Dirk).

</li>
<li>

Additional <code>nanoduration</code> and <code>character</code> ops plus
tests (Colin Umansky in
<a href="https://github.com/eddelbuettel/nanotime/pull/88">[#88](https://github.com/eddelbuettel/nanotime/issues/88)</a>
addressing
<a href="https://github.com/eddelbuettel/nanotime/issues/87">[#87](https://github.com/eddelbuettel/nanotime/issues/87)</a>).

</li>
<li>

New <code>plus</code> and <code>minus</code> functions for periods
(Leonardo in
<a href="https://github.com/eddelbuettel/nanotime/pull/91">[#91](https://github.com/eddelbuettel/nanotime/issues/91)</a>).

</li>
</ul>

## Changes in version 0.3.2 (2020-09-03)

<ul>
<li>

Correct for big endian (Elliott Sales de Andrade in
<a href="https://github.com/eddelbuettel/nanotime/pull/81">[#81](https://github.com/eddelbuettel/nanotime/issues/81)</a>).

</li>
<li>

Use the <code>RcppCCTZ_API.h</code> header (Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/82">[#82](https://github.com/eddelbuettel/nanotime/issues/82)</a>).

</li>
<li>

Conditionally reduce test coverage (Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/83">[#83](https://github.com/eddelbuettel/nanotime/issues/83)</a>).

</li>
</ul>

## Changes in version 0.3.1 (2020-08-09)

<ul>
<li>

Several small cleanups to ensure a more robust compilation (Leonardo and
Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/75">[#75](https://github.com/eddelbuettel/nanotime/issues/75)</a>
fixing
<a href="https://github.com/eddelbuettel/nanotime/issues/74">[#74](https://github.com/eddelbuettel/nanotime/issues/74)</a>).

</li>
<li>

Show Solaris some extra love by skipping tests and examples with a
timezone (Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/76">[#76](https://github.com/eddelbuettel/nanotime/issues/76)</a>).

</li>
</ul>

## Changes in version 0.3.0 (2020-08-06)

<ul>
<li>

Use <code>tzstr=</code> instead of <code>tz=</code> in call to
<span class="pkg">RcppCCTZ::parseDouble()</span>) (Matt Dowle in
<a href="https://github.com/eddelbuettel/nanotime/pull/49">[#49](https://github.com/eddelbuettel/nanotime/issues/49)</a>).

</li>
<li>

Add new comparison operators for <code>nanotime</code> and
<code>charcters</code> (Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/54">[#54](https://github.com/eddelbuettel/nanotime/issues/54)</a>
fixing
<a href="https://github.com/eddelbuettel/nanotime/issues/52">[#52](https://github.com/eddelbuettel/nanotime/issues/52)</a>).

</li>
<li>

Switch from <span class="pkg">RUnit</span> to
<span class="pkg">tinytest</span> (Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/55">[#55](https://github.com/eddelbuettel/nanotime/issues/55)</a>)

</li>
<li>

Substantial functionality extension in with new types
<code>nanoduration</code>, <code>nanoival</code> and
<code>nanoperiod</code> (Leonardo in
<a href="https://github.com/eddelbuettel/nanotime/pull/58">[#58](https://github.com/eddelbuettel/nanotime/issues/58)</a>,
<a href="https://github.com/eddelbuettel/nanotime/pull/60">[#60](https://github.com/eddelbuettel/nanotime/issues/60)</a>,
<a href="https://github.com/eddelbuettel/nanotime/pull/62">[#62](https://github.com/eddelbuettel/nanotime/issues/62)</a>,
<a href="https://github.com/eddelbuettel/nanotime/pull/63">[#63](https://github.com/eddelbuettel/nanotime/issues/63)</a>,
<a href="https://github.com/eddelbuettel/nanotime/pull/65">[#65](https://github.com/eddelbuettel/nanotime/issues/65)</a>,
<a href="https://github.com/eddelbuettel/nanotime/pull/67">[#67](https://github.com/eddelbuettel/nanotime/issues/67)</a>,
<a href="https://github.com/eddelbuettel/nanotime/pull/70">[#70](https://github.com/eddelbuettel/nanotime/issues/70)</a>
fixing
<a href="https://github.com/eddelbuettel/nanotime/issues/47">[#47](https://github.com/eddelbuettel/nanotime/issues/47)</a>,
<a href="https://github.com/eddelbuettel/nanotime/issues/51">[#51](https://github.com/eddelbuettel/nanotime/issues/51)</a>,
<a href="https://github.com/eddelbuettel/nanotime/issues/57">[#57](https://github.com/eddelbuettel/nanotime/issues/57)</a>,
<a href="https://github.com/eddelbuettel/nanotime/issues/61">[#61](https://github.com/eddelbuettel/nanotime/issues/61)</a>,
<a href="https://github.com/eddelbuettel/nanotime/issues/64">[#64](https://github.com/eddelbuettel/nanotime/issues/64)</a>
with assistance from Dirk).

</li>
<li>

A new (yet still draft-ish) vignette was added describing the four core
types (Leonardo and Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/71">[#71](https://github.com/eddelbuettel/nanotime/issues/71)</a>).

</li>
<li>

A required compilation flag for Windows was added (Leonardo in
<a href="https://github.com/eddelbuettel/nanotime/pull/72">[#72](https://github.com/eddelbuettel/nanotime/issues/72)</a>).

</li>
<li>

<span class="pkg">RcppCCTZ</span> function are called in new
‘non-throwing’ variants to not trigger exeception errors (Leonardo in
<a href="https://github.com/eddelbuettel/nanotime/pull/73">[#73](https://github.com/eddelbuettel/nanotime/issues/73)</a>).

</li>
</ul>

## Changes in version 0.2.4 (2019-05-25)

<ul>
<li>

Define \[\[ method (Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/45">[#45](https://github.com/eddelbuettel/nanotime/issues/45)</a>
fixing
<a href="https://github.com/eddelbuettel/nanotime/issues/44">[#44](https://github.com/eddelbuettel/nanotime/issues/44)</a>).

</li>
</ul>

## Changes in version 0.2.3 (2018-09-30)

<ul>
<li>

Skip some tests on Solaris which seems borked with timezones. As we have
no real access, no real fix possible (Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/42">[#42](https://github.com/eddelbuettel/nanotime/issues/42)</a>).

</li>
<li>

Update Travis setup

</li>
</ul>

## Changes in version 0.2.2 (2018-07-18)

<ul>
<li>

Unit tests depending on future
<a href="https://CRAN.R-project.org/package=xts"><span class="pkg">xts</span></a>
behaviour remain disabled (Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/41">[#41](https://github.com/eddelbuettel/nanotime/issues/41)</a>).

</li>
</ul>

## Changes in version 0.2.1 (2018-07-01)

<ul>
<li>

Added attribute-preserving comparison (Leonardo in
<a href="https://github.com/eddelbuettel/nanotime/pull/33">[#33](https://github.com/eddelbuettel/nanotime/issues/33)</a>).

</li>
<li>

Added two <code>integer64</code> casts in constructors (Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/36">[#36](https://github.com/eddelbuettel/nanotime/issues/36)</a>).

</li>
<li>

Added two checks for empty arguments (Dirk in
<a href="https://github.com/eddelbuettel/nanotime/pull/37">[#37](https://github.com/eddelbuettel/nanotime/issues/37)</a>).

</li>
</ul>

## Changes in version 0.2.0 (2017-06-22)

<ul>
<li>

Rewritten in S4 to provide more robust operations
(<a href="https://github.com/eddelbuettel/nanotime/pull/17">[#17](https://github.com/eddelbuettel/nanotime/issues/17)</a> by
Leonardo)

</li>
<li>

Ensure <code>tz=““</code> is treated as unset (Leonardo in
<a href="https://github.com/eddelbuettel/nanotime/pull/20">[#20](https://github.com/eddelbuettel/nanotime/issues/20)</a>)

</li>
<li>

Added <code>format</code> and <code>tz</code> arguments to
<code>nanotime</code>, <code>format</code>, <code>print</code>
(<a href="https://github.com/eddelbuettel/nanotime/pull/22">[#22](https://github.com/eddelbuettel/nanotime/issues/22)</a> by
Leonardo and Dirk)

</li>
<li>

Ensure printing respect <code>options()$max.print</code>, ensure names
are kept with vector
(<a href="https://github.com/eddelbuettel/nanotime/pull/23">[#23](https://github.com/eddelbuettel/nanotime/issues/23)</a> by
Leonardo)

</li>
<li>

Correct <code>summary()</code> by defining <code>names\<-</code>
(Leonardo in
<a href="https://github.com/eddelbuettel/nanotime/pull/25">[#25](https://github.com/eddelbuettel/nanotime/issues/25)</a>
fixing
<a href="https://github.com/eddelbuettel/nanotime/issues/24">[#24](https://github.com/eddelbuettel/nanotime/issues/24)</a>)

</li>
<li>

Report error on operations that are meaningful for type; handled NA,
NaN, Inf, -Inf correctly (Leonardo in
<a href="https://github.com/eddelbuettel/nanotime/pull/27">[#27](https://github.com/eddelbuettel/nanotime/issues/27)</a>
fixing
<a href="https://github.com/eddelbuettel/nanotime/issues/26">[#26](https://github.com/eddelbuettel/nanotime/issues/26)</a>)

</li>
</ul>

## Changes in version 0.1.2 (2017-03-27)

<ul>
<li>

The <code>as.integer64</code> function is now exported as well.

</li>
</ul>

## Changes in version 0.1.1 (2017-02-04)

<ul>
<li>

The default display format now always shows nine digits
(<a href="https://github.com/eddelbuettel/nanotime/pull/10">[#10](https://github.com/eddelbuettel/nanotime/issues/10)</a>
closing
<a href="https://github.com/eddelbuettel/nanotime/pull/9">[#9](https://github.com/eddelbuettel/nanotime/issues/9)</a>)

</li>
<li>

The default print method was updated to use formated output, and a new
new converter <code>as.integer64</code> was added

</li>
<li>

Several ‘Ops’ method are now explicitly defined allowing casting of
results (rather than falling back on bit64 behaviour)

</li>
<li>

The format routine is now more careful about not loosing precision
(<a href="https://github.com/eddelbuettel/nanotime/issues/13">[#13](https://github.com/eddelbuettel/nanotime/issues/13)</a>
closing
<a href="https://github.com/eddelbuettel/nanotime/issues/12">[#12](https://github.com/eddelbuettel/nanotime/issues/12)</a>)

</li>
</ul>

## Changes in version 0.1.0 (2017-01-10)

<ul>
<li>

Added Windows support thanks to expanded
<a href="https://CRAN.R-project.org/package=RcppCCTZ"><span class="pkg">RcppCCTZ</span></a>
(closes
<a href="https://github.com/eddelbuettel/nanotime/issues/6">[#6](https://github.com/eddelbuettel/nanotime/issues/6)</a>)

</li>
<li>

Added "mocked up" demo with nanosecond delay networking analysis

</li>
<li>

Added ‘fmt’ and ‘tz’ options to output functions, expanded
<code>format.nanotime</code> (closing
<a href="https://github.com/eddelbuettel/nanotime/issues/2">[#2](https://github.com/eddelbuettel/nanotime/issues/2)</a> and
<a href="https://github.com/eddelbuettel/nanotime/issues/3">[#3](https://github.com/eddelbuettel/nanotime/issues/3)</a>)

</li>
<li>

Added data.frame support

</li>
<li>

Expanded tests

</li>
</ul>

## Changes in version 0.0.1 (2016-12-15)

<ul>
<li>

Initial CRAN upload.

</li>
<li>

Package is functional and provides examples.

</li>
</ul>
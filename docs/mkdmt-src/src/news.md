<div class="container">
<div role="main">
<h3 id="version-0.3.8-2024-06-19">Version 0.3.8 (2024-06-19)</h3>
<ul>
<li><p>Time format documentation now has a reference to
<code>RcppCCTZ</code></p></li>
<li><p>The package no longer sets a default C++ compilation standard of
C++11 (Dirk initially in <a
href="https://github.com/eddelbuettel/nanotime/pull/114">#114</a>, and
later switched to C++17)</p></li>
<li><p>New <code>accurate</code> parameter for conversion from
<code>POSIXct</code> to <code>nanotime</code> (Davor Josipovic and
Leonardo in <a
href="https://github.com/eddelbuettel/nanotime/pull/116">#116</a>
closing <a
href="https://github.com/eddelbuettel/nanotime/issues/115">#115</a>)</p></li>
<li><p>The <code>as.Date()</code> function is now vectorized and can
take a TZ argument (Leonardo and Dirk in <a
href="https://github.com/eddelbuettel/nanotime/pull/119">#119</a>
closing <a
href="https://github.com/eddelbuettel/nanotime/issues/118">#118</a>)</p></li>
<li><p>Use of internal function <code>SET_S4_OBJECT</code> has been
replaced by API function <code>Rf_asS4</code> (Leonardo in <a
href="https://github.com/eddelbuettel/nanotime/pull/121">#121</a>
closing <a
href="https://github.com/eddelbuettel/nanotime/issues/120">#120</a>)</p></li>
<li><p>An <code>nanoduration</code> / <code>nanoduration</code>
expression now returns a double (Leonardo in <a
href="https://github.com/eddelbuettel/nanotime/pull/122">#122</a>
closing <a
href="https://github.com/eddelbuettel/nanotime/issues/117">#117</a>)</p></li>
<li><p>Bitfield calculations no longer require an Windows-only compiler
switch (Leonardo in <a
href="https://github.com/eddelbuettel/nanotime/pull/124">#124</a>)</p></li>
<li><p>A simple manual page format nag involving has been addressed
(Dirk in <a
href="https://github.com/eddelbuettel/nanotime/pull/126">#126</a> fixing
<a
href="https://github.com/eddelbuettel/nanotime/issues/125">#125</a>)</p></li>
<li><p>An set of tests tickling an UBSAN issue via <code>Rcpp</code>
code no longer run unless <code>CI</code> is set (Dirk in <a
href="https://github.com/eddelbuettel/nanotime/pull/127">#127</a> fixing
<a
href="https://github.com/eddelbuettel/nanotime/issues/123">#123</a>)</p></li>
</ul>
<h3 id="version-0.3.7-2022-10-23">Version 0.3.7 (2022-10-23)</h3>
<ul>
<li><p>Update mkdocs for material docs generator (Dirk in <a
href="https://github.com/eddelbuettel/nanotime/pull/102">#102</a>)</p></li>
<li><p>Use <code>inherits()</code> instead comparing to
<code>class()</code> (Trevor Davis in <a
href="https://github.com/eddelbuettel/nanotime/pull/104">#104</a>)</p></li>
<li><p>Set default arguments in <code>nanoduration()</code> (Trevor
Davis in <a
href="https://github.com/eddelbuettel/nanotime/pull/105">#105</a>)</p></li>
<li><p>Add <code>as.nanoduration.difftime()</code> support (Trevor Davis
in <a
href="https://github.com/eddelbuettel/nanotime/pull/106">#106</a>)</p></li>
<li><p>Add +/- methods for <code>nanotime</code> and
<code>difftime</code> objects (Trevor Davis in <a
href="https://github.com/eddelbuettel/nanotime/pull/110">#110</a>
closing <a
href="https://github.com/eddelbuettel/nanotime/issues/108">#108</a>, <a
href="https://github.com/eddelbuettel/nanotime/issues/107">#107</a>)</p></li>
</ul>
<h3 id="version-0.3.6-2022-03-06">Version 0.3.6 (2022-03-06)</h3>
<ul>
<li><p>Fix incorrect subsetting with operator <code>%in%</code>
(Leonardo in <a
href="https://github.com/eddelbuettel/nanotime/pull/100">#100</a> fixing
<a
href="https://github.com/eddelbuettel/nanotime/issues/99">#99</a>).</p></li>
<li><p>Fix incorrect parsing for negative nanoperiod (Leonardo in <a
href="https://github.com/eddelbuettel/nanotime/pull/100">#100</a> fixing
<a
href="https://github.com/eddelbuettel/nanotime/issues/96">#96</a>).</p></li>
<li><p>Test for <code>class</code> via <code>inherits()</code>
(Dirk).</p></li>
</ul>
<h3 id="version-0.3.5-2021-12-14">Version 0.3.5 (2021-12-14)</h3>
<ul>
<li><p>Applied patch by Tomas Kalibera for Windows UCRT under the
upcoming R 4.2.0 expected for April.</p></li>
</ul>
<h3 id="version-0.3.4-2021-11-24">Version 0.3.4 (2021-11-24)</h3>
<ul>
<li><p>Added a few more <code>as.character</code> conversion function
(Dirk)</p></li>
<li><p>Expose <code>nanoperiod</code> functionality via header file for
use by other packages (Leonardo in <a
href="https://github.com/eddelbuettel/nanotime/pull/95">#95</a> fixing
<a
href="https://github.com/eddelbuettel/nanotime/issues/94">#94</a>).</p></li>
</ul>
<h3 id="version-0.3.3-2021-08-09">Version 0.3.3 (2021-08-09)</h3>
<ul>
<li><p>New demo <code>ggplot2Example.R</code> (Leonardo and
Dirk).</p></li>
<li><p>New documentation website using mkdocs-material (Dirk).</p></li>
<li><p>Updated unit test to account for r-devel POSIXct changes, and
re-enable full testing under r-devel (Dirk).</p></li>
<li><p>Additional <code>nanoduration</code> and <code>character</code>
ops plus tests (Colin Umansky in <a
href="https://github.com/eddelbuettel/nanotime/pull/88">#88</a>
addressing <a
href="https://github.com/eddelbuettel/nanotime/issues/87">#87</a>).</p></li>
<li><p>New <code>plus</code> and <code>minus</code> functions for
periods (Leonardo in <a
href="https://github.com/eddelbuettel/nanotime/pull/91">#91</a>).</p></li>
</ul>
<h3 id="version-0.3.2-2020-09-03">Version 0.3.2 (2020-09-03)</h3>
<ul>
<li><p>Correct for big endian (Elliott Sales de Andrade in <a
href="https://github.com/eddelbuettel/nanotime/pull/81">#81</a>).</p></li>
<li><p>Use the <code>RcppCCTZ_API.h</code> header (Dirk in <a
href="https://github.com/eddelbuettel/nanotime/pull/82">#82</a>).</p></li>
<li><p>Conditionally reduce test coverage (Dirk in <a
href="https://github.com/eddelbuettel/nanotime/pull/83">#83</a>).</p></li>
</ul>
<h3 id="version-0.3.1-2020-08-09">Version 0.3.1 (2020-08-09)</h3>
<ul>
<li><p>Several small cleanups to ensure a more robust compilation
(Leonardo and Dirk in <a
href="https://github.com/eddelbuettel/nanotime/pull/75">#75</a> fixing
<a
href="https://github.com/eddelbuettel/nanotime/issues/74">#74</a>).</p></li>
<li><p>Show Solaris some extra love by skipping tests and examples with
a timezone (Dirk in <a
href="https://github.com/eddelbuettel/nanotime/pull/76">#76</a>).</p></li>
</ul>
<h3 id="version-0.3.0-2020-08-06">Version 0.3.0 (2020-08-06)</h3>
<ul>
<li><p>Use <code>tzstr=</code> instead of <code>tz=</code> in call to
<code>RcppCCTZ::parseDouble()</code>) (Matt Dowle in <a
href="https://github.com/eddelbuettel/nanotime/pull/49">#49</a>).</p></li>
<li><p>Add new comparison operators for <code>nanotime</code> and
<code>charcters</code> (Dirk in <a
href="https://github.com/eddelbuettel/nanotime/pull/54">#54</a> fixing
<a
href="https://github.com/eddelbuettel/nanotime/issues/52">#52</a>).</p></li>
<li><p>Switch from <code>RUnit</code> to <span
class="pkg">tinytest</span> (Dirk in <a
href="https://github.com/eddelbuettel/nanotime/pull/55">#55</a>)</p></li>
<li><p>Substantial functionality extension in with new types
<code>nanoduration</code>, <code>nanoival</code> and
<code>nanoperiod</code> (Leonardo in <a
href="https://github.com/eddelbuettel/nanotime/pull/58">#58</a>, <a
href="https://github.com/eddelbuettel/nanotime/pull/60">#60</a>, <a
href="https://github.com/eddelbuettel/nanotime/pull/62">#62</a>, <a
href="https://github.com/eddelbuettel/nanotime/pull/63">#63</a>, <a
href="https://github.com/eddelbuettel/nanotime/pull/65">#65</a>, <a
href="https://github.com/eddelbuettel/nanotime/pull/67">#67</a>, <a
href="https://github.com/eddelbuettel/nanotime/pull/70">#70</a> fixing
<a href="https://github.com/eddelbuettel/nanotime/issues/47">#47</a>, <a
href="https://github.com/eddelbuettel/nanotime/issues/51">#51</a>, <a
href="https://github.com/eddelbuettel/nanotime/issues/57">#57</a>, <a
href="https://github.com/eddelbuettel/nanotime/issues/61">#61</a>, <a
href="https://github.com/eddelbuettel/nanotime/issues/64">#64</a> with
assistance from Dirk).</p></li>
<li><p>A new (yet still draft-ish) vignette was added describing the
four core types (Leonardo and Dirk in <a
href="https://github.com/eddelbuettel/nanotime/pull/71">#71</a>).</p></li>
<li><p>A required compilation flag for Windows was added (Leonardo in <a
href="https://github.com/eddelbuettel/nanotime/pull/72">#72</a>).</p></li>
<li><p><code>RcppCCTZ</code> function are called in new 'non-throwing'
variants to not trigger exeception errors (Leonardo in <a
href="https://github.com/eddelbuettel/nanotime/pull/73">#73</a>).</p></li>
</ul>
<h3 id="version-0.2.4-2019-05-25">Version 0.2.4 (2019-05-25)</h3>
<ul>
<li><p>Define [[ method (Dirk in <a
href="https://github.com/eddelbuettel/nanotime/pull/45">#45</a> fixing
<a
href="https://github.com/eddelbuettel/nanotime/issues/44">#44</a>).</p></li>
</ul>
<h3 id="version-0.2.3-2018-09-30">Version 0.2.3 (2018-09-30)</h3>
<ul>
<li><p>Skip some tests on Solaris which seems borked with timezones. As
we have no real access, no real fix possible (Dirk in <a
href="https://github.com/eddelbuettel/nanotime/pull/42">#42</a>).</p></li>
<li><p>Update Travis setup</p></li>
</ul>
<h3 id="version-0.2.2-2018-07-18">Version 0.2.2 (2018-07-18)</h3>
<ul>
<li><p>Unit tests depending on future <a
href="https://CRAN.R-project.org/package=xts"><span
class="pkg">xts</span></a> behaviour remain disabled (Dirk in <a
href="https://github.com/eddelbuettel/nanotime/pull/41">#41</a>).</p></li>
</ul>
<h3 id="version-0.2.1-2018-07-01">Version 0.2.1 (2018-07-01)</h3>
<ul>
<li><p>Added attribute-preserving comparison (Leonardo in <a
href="https://github.com/eddelbuettel/nanotime/pull/33">#33</a>).</p></li>
<li><p>Added two <code>integer64</code> casts in constructors (Dirk in
<a
href="https://github.com/eddelbuettel/nanotime/pull/36">#36</a>).</p></li>
<li><p>Added two checks for empty arguments (Dirk in <a
href="https://github.com/eddelbuettel/nanotime/pull/37">#37</a>).</p></li>
</ul>
<h3 id="version-0.2.0-2017-06-22">Version 0.2.0 (2017-06-22)</h3>
<ul>
<li><p>Rewritten in S4 to provide more robust operations (<a
href="https://github.com/eddelbuettel/nanotime/pull/17">#17</a> by
Leonardo)</p></li>
<li><p>Ensure <code>tz=""</code> is treated as unset (Leonardo in <a
href="https://github.com/eddelbuettel/nanotime/pull/20">#20</a>)</p></li>
<li><p>Added <code>format</code> and <code>tz</code> arguments to
<code>nanotime</code>, <code>format</code>, <code>print</code> (<a
href="https://github.com/eddelbuettel/nanotime/pull/22">#22</a> by
Leonardo and Dirk)</p></li>
<li><p>Ensure printing respect <code>options()$max.print</code>, ensure
names are kept with vector (<a
href="https://github.com/eddelbuettel/nanotime/pull/23">#23</a> by
Leonardo)</p></li>
<li><p>Correct <code>summary()</code> by defining
<code>names&lt;-</code> (Leonardo in <a
href="https://github.com/eddelbuettel/nanotime/pull/25">#25</a> fixing
<a
href="https://github.com/eddelbuettel/nanotime/issues/24">#24</a>)</p></li>
<li><p>Report error on operations that are meaningful for type; handled
NA, NaN, Inf, -Inf correctly (Leonardo in <a
href="https://github.com/eddelbuettel/nanotime/pull/27">#27</a> fixing
<a
href="https://github.com/eddelbuettel/nanotime/issues/26">#26</a>)</p></li>
</ul>
<h3 id="version-0.1.2-2017-03-27">Version 0.1.2 (2017-03-27)</h3>
<ul>
<li><p>The <code>as.integer64</code> function is now exported as
well.</p></li>
</ul>
<h3 id="version-0.1.1-2017-02-04">Version 0.1.1 (2017-02-04)</h3>
<ul>
<li><p>The default display format now always shows nine digits (<a
href="https://github.com/eddelbuettel/nanotime/pull/10">#10</a> closing
<a
href="https://github.com/eddelbuettel/nanotime/pull/9">#9</a>)</p></li>
<li><p>The default print method was updated to use formated output, and
a new new converter <code>as.integer64</code> was added</p></li>
<li><p>Several 'Ops' method are now explicitly defined allowing casting
of results (rather than falling back on bit64 behaviour)</p></li>
<li><p>The format routine is now more careful about not loosing
precision (<a
href="https://github.com/eddelbuettel/nanotime/issues/13">#13</a>
closing <a
href="https://github.com/eddelbuettel/nanotime/issues/12">#12</a>)</p></li>
</ul>
<h3 id="version-0.1.0-2017-01-10">Version 0.1.0 (2017-01-10)</h3>
<ul>
<li><p>Added Windows support thanks to expanded <a
href="https://CRAN.R-project.org/package=RcppCCTZ"><span
class="pkg">RcppCCTZ</span></a> (closes <a
href="https://github.com/eddelbuettel/nanotime/issues/6">#6</a>)</p></li>
<li><p>Added "mocked up" demo with nanosecond delay networking
analysis</p></li>
<li><p>Added 'fmt' and 'tz' options to output functions, expanded
<code>format.nanotime</code> (closing <a
href="https://github.com/eddelbuettel/nanotime/issues/2">#2</a> and <a
href="https://github.com/eddelbuettel/nanotime/issues/3">#3</a>)</p></li>
<li><p>Added data.frame support</p></li>
<li><p>Expanded tests</p></li>
</ul>
<h3 id="version-0.0.1-2016-12-15">Version 0.0.1 (2016-12-15)</h3>
<ul>
<li><p>Initial CRAN upload.</p></li>
<li><p>Package is functional and provides examples.</p></li>
</ul>
</div>
</div>

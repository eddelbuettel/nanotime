
### Version 0.3.4 (2021-11-24)

-   Added a few more `as.character` conversion function (Dirk)

-   Expose `nanoperiod` functionality via header file for use by other
    packages (Leonardo in
    [\#95](https://github.com/eddelbuettel/nanotime/pull/95) fixing
    [\#94](https://github.com/eddelbuettel/nanotime/issues/94)).

### Version 0.3.3 (2021-08-09)

-   New demo `ggplot2Example.R` (Leonardo and Dirk).

-   New documentation website using mkdocs-material (Dirk).

-   Updated unit test to account for r-devel POSIXct changes, and
    re-enable full testing under r-devel (Dirk).

-   Additional `nanoduration` and `character` ops plus tests (Colin
    Umansky in [\#88](https://github.com/eddelbuettel/nanotime/pull/88)
    addressing
    [\#87](https://github.com/eddelbuettel/nanotime/issues/87)).

-   New `plus` and `minus` functions for periods (Leonardo in
    [\#91](https://github.com/eddelbuettel/nanotime/pull/91)).

### Version 0.3.2 (2020-09-03)

-   Correct for big endian (Elliott Sales de Andrade in
    [\#81](https://github.com/eddelbuettel/nanotime/pull/81)).

-   Use the `RcppCCTZ_API.h` header (Dirk in
    [\#82](https://github.com/eddelbuettel/nanotime/pull/82)).

-   Conditionally reduce test coverage (Dirk in
    [\#83](https://github.com/eddelbuettel/nanotime/pull/83)).

### Version 0.3.1 (2020-08-09)

-   Several small cleanups to ensure a more robust compilation (Leonardo
    and Dirk in [\#75](https://github.com/eddelbuettel/nanotime/pull/75)
    fixing [\#74](https://github.com/eddelbuettel/nanotime/issues/74)).

-   Show Solaris some extra love by skipping tests and examples with a
    timezone (Dirk in
    [\#76](https://github.com/eddelbuettel/nanotime/pull/76)).

### Version 0.3.0 (2020-08-06)

-   Use `tzstr=` instead of `tz=` in call to `RcppCCTZ::parseDouble()`)
    (Matt Dowle in
    [\#49](https://github.com/eddelbuettel/nanotime/pull/49)).

-   Add new comparison operators for `nanotime` and `charcters` (Dirk in
    [\#54](https://github.com/eddelbuettel/nanotime/pull/54) fixing
    [\#52](https://github.com/eddelbuettel/nanotime/issues/52)).

-   Switch from `RUnit` to [tinytest]{.pkg} (Dirk in
    [\#55](https://github.com/eddelbuettel/nanotime/pull/55))

-   Substantial functionality extension in with new types
    `nanoduration`, `nanoival` and `nanoperiod` (Leonardo in
    [\#58](https://github.com/eddelbuettel/nanotime/pull/58),
    [\#60](https://github.com/eddelbuettel/nanotime/pull/60),
    [\#62](https://github.com/eddelbuettel/nanotime/pull/62),
    [\#63](https://github.com/eddelbuettel/nanotime/pull/63),
    [\#65](https://github.com/eddelbuettel/nanotime/pull/65),
    [\#67](https://github.com/eddelbuettel/nanotime/pull/67),
    [\#70](https://github.com/eddelbuettel/nanotime/pull/70) fixing
    [\#47](https://github.com/eddelbuettel/nanotime/issues/47),
    [\#51](https://github.com/eddelbuettel/nanotime/issues/51),
    [\#57](https://github.com/eddelbuettel/nanotime/issues/57),
    [\#61](https://github.com/eddelbuettel/nanotime/issues/61),
    [\#64](https://github.com/eddelbuettel/nanotime/issues/64) with
    assistance from Dirk).

-   A new (yet still draft-ish) vignette was added describing the four
    core types (Leonardo and Dirk in
    [\#71](https://github.com/eddelbuettel/nanotime/pull/71)).

-   A required compilation flag for Windows was added (Leonardo in
    [\#72](https://github.com/eddelbuettel/nanotime/pull/72)).

-   `RcppCCTZ` function are called in new \'non-throwing\' variants to
    not trigger exeception errors (Leonardo in
    [\#73](https://github.com/eddelbuettel/nanotime/pull/73)).

### Version 0.2.4 (2019-05-25)

-   Define \[\[ method (Dirk in
    [\#45](https://github.com/eddelbuettel/nanotime/pull/45) fixing
    [\#44](https://github.com/eddelbuettel/nanotime/issues/44)).

### Version 0.2.3 (2018-09-30)

-   Skip some tests on Solaris which seems borked with timezones. As we
    have no real access, no real fix possible (Dirk in
    [\#42](https://github.com/eddelbuettel/nanotime/pull/42)).

-   Update Travis setup

### Version 0.2.2 (2018-07-18)

-   Unit tests depending on future
    [[xts]{.pkg}](https://CRAN.R-project.org/package=xts) behaviour
    remain disabled (Dirk in
    [\#41](https://github.com/eddelbuettel/nanotime/pull/41)).

### Version 0.2.1 (2018-07-01)

-   Added attribute-preserving comparison (Leonardo in
    [\#33](https://github.com/eddelbuettel/nanotime/pull/33)).

-   Added two `integer64` casts in constructors (Dirk in
    [\#36](https://github.com/eddelbuettel/nanotime/pull/36)).

-   Added two checks for empty arguments (Dirk in
    [\#37](https://github.com/eddelbuettel/nanotime/pull/37)).

### Version 0.2.0 (2017-06-22)

-   Rewritten in S4 to provide more robust operations
    ([\#17](https://github.com/eddelbuettel/nanotime/pull/17) by
    Leonardo)

-   Ensure `tz=""` is treated as unset (Leonardo in
    [\#20](https://github.com/eddelbuettel/nanotime/pull/20))

-   Added `format` and `tz` arguments to `nanotime`, `format`, `print`
    ([\#22](https://github.com/eddelbuettel/nanotime/pull/22) by
    Leonardo and Dirk)

-   Ensure printing respect `options()$max.print`, ensure names are kept
    with vector
    ([\#23](https://github.com/eddelbuettel/nanotime/pull/23) by
    Leonardo)

-   Correct `summary()` by defining `names<-` (Leonardo in
    [\#25](https://github.com/eddelbuettel/nanotime/pull/25) fixing
    [\#24](https://github.com/eddelbuettel/nanotime/issues/24))

-   Report error on operations that are meaningful for type; handled NA,
    NaN, Inf, -Inf correctly (Leonardo in
    [\#27](https://github.com/eddelbuettel/nanotime/pull/27) fixing
    [\#26](https://github.com/eddelbuettel/nanotime/issues/26))

### Version 0.1.2 (2017-03-27)

-   The `as.integer64` function is now exported as well.

### Version 0.1.1 (2017-02-04)

-   The default display format now always shows nine digits
    ([\#10](https://github.com/eddelbuettel/nanotime/pull/10) closing
    [\#9](https://github.com/eddelbuettel/nanotime/pull/9))

-   The default print method was updated to use formated output, and a
    new new converter `as.integer64` was added

-   Several \'Ops\' method are now explicitly defined allowing casting
    of results (rather than falling back on bit64 behaviour)

-   The format routine is now more careful about not loosing precision
    ([\#13](https://github.com/eddelbuettel/nanotime/issues/13) closing
    [\#12](https://github.com/eddelbuettel/nanotime/issues/12))

### Version 0.1.0 (2017-01-10)

-   Added Windows support thanks to expanded
    [[RcppCCTZ]{.pkg}](https://CRAN.R-project.org/package=RcppCCTZ)
    (closes [\#6](https://github.com/eddelbuettel/nanotime/issues/6))

-   Added \"mocked up\" demo with nanosecond delay networking analysis

-   Added \'fmt\' and \'tz\' options to output functions, expanded
    `format.nanotime` (closing
    [\#2](https://github.com/eddelbuettel/nanotime/issues/2) and
    [\#3](https://github.com/eddelbuettel/nanotime/issues/3))

-   Added data.frame support

-   Expanded tests

### Version 0.0.1 (2016-12-15)

-   Initial CRAN upload.

-   Package is functional and provides examples.

# Changelog

## dpbuild 0.4.0

- Remove qs dependency and tooling

## dpbuild 0.3.1

- Fixed the bug related to the daap pin version not being captured
  properly in the log file

## dpbuild 0.3.0

- Enable saving data objects in qs format via the “type” param in
  [`dp_write()`](https://amashadihossein.github.io/dpbuild/reference/dp_write.md)

## dpbuild 0.2.1

- Make targets default when using
  [`dpcode_add()`](https://amashadihossein.github.io/dpbuild/reference/dpcode_add.md)
  as drake is superseded
  ([\#90](https://github.com/amashadihossein/dpbuild/issues/90))
- Address [\#95](https://github.com/amashadihossein/dpbuild/issues/95)
  to allow
  [`dpconf_get()`](https://amashadihossein.github.io/dpbuild/reference/dpconf_get.md)
  to be called outside of project directory
- Fixed windows bug related to
  [`file.path()`](https://rdrr.io/r/base/file.path.html) call in
  `dp_connect()`

## dpbuild 0.2.0

- Added back support for LabKey boards
  ([\#86](https://github.com/amashadihossein/dpbuild/issues/86)).
  `pinsLabkey` is now required to work with LabKey boards
- Update default gitignore used in
  [`dp_init()`](https://amashadihossein.github.io/dpbuild/reference/dp_init.md)
  to include .RData as well as other common files
  ([\#84](https://github.com/amashadihossein/dpbuild/issues/84)).

## dpbuild 0.1.0

### Breaking changes

- dpbuild now requires pins \>= v1.2.0. This means that data products
  will now use the v1 api and older data products are incompatible with
  dpbuild \>= 0.1.0. Quite a few changes under the hood, but users will
  see minimal changes to the workflow.
- LabKey functionality has been temporarily removed until pins v1 can be
  extended to support LabKey boards

### Other improvments

- Added a `NEWS.md` file to track changes to the package.

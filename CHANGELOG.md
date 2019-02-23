Unreleased
----------
- Added badge showing the license to `README.md`


0.2.1
-----
- Added support for test function attributes, e.g., `#[ignore]`
- Added support for mutable `device` arguments to test functions
- Removed restriction of expecting an argument specifically named
  `device`
- Introduced `nitrokey-test-state` crate containing state needed to
  achieve mutual exclusion of tests


0.2.0
-----
- Adjusted error handling to be compatible with `nitrokey` version `0.4`


0.1.1
-----
- Properly use absolutely qualified macros in generated code
- Enabled CI pipeline comprising building, testing, and linting of the
  project
  - Added badge indicating pipeline status


0.1.0
-----
- Initial release

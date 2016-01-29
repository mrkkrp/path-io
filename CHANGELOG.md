## Path IO 0.3.0

* Added `forgivingAbsence`, `resolveFile`, and `resolveDir` functions, so
  the package now provides all functionality that `Path.IO` module in Stack
  has.

* Added closed type family `RelPath`, `makeRelative`, and
  `makeRelativeToCurrentDir` functions.

* Fixed signature of `getAppUserDataDir`.

## Path IO 0.2.0

* Added functions from `temporary`: `withTempFile`, `withTempDir`,
  `withSystemTempFile`, `withSystemTempDir`, `openTempFile`,
  `openBinaryTempFile`, and `createTempDir`. `temporary` is a lightweight
  and ubiquitous package, so depending on it should be OK.

## Path IO 0.1.1

* Fixed type signatures of `renameFile` and `copyFile`.

## Path IO 0.1.0

* Initial release.

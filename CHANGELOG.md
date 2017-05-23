## Path IO 1.3.0

* Move the type functions `AbsPath` and `RelPath` to the `AnyPath` type
  class.

## Path IO 1.2.3

* Allowed `time-1.7`.

## Path IO 1.2.2

* Fixed a bug in `setModificationTime` function that previously called
  `setAccessTime` instead of `setModificationTime` from `directory`.

* Added notes to all pieces of API that are conditional (some functions are
  only available if `directory-1.2.3.0` or later is used, now it's mentioned
  for every such function explicitely).

## Path IO 1.2.1

* Allowed `directory-1.3.0.0`.

* Added `getXdgDir`. Only available of `directory-1.2.3.0` or later is used.

* Various cosmetic improvements.

## Path IO 1.2.0

* Added `walkDir` function to traverse a directory tree with a handler.

* Added `walkDirAccum` function which is like `walkDir` but also accepts an
  output writer and returns the accumulated output.

* All recursive traversal functions (existing and new) now safely handle
  directory loops due to symbolic or hard links.

* Added “since” notes to public functions in API.

## Path IO 1.1.0

* Fixed bug in `copyDirRecur` when it was unable to fully copy read-only
  directories.

* Added `copyDirRecur'` function that works just like `copyDirRecur`, but
  does not preserve directory permissions.

## Path IO 1.0.1

* Fixed bug in `copyDirRecur` for non-existing destination paths when
  directory to copy does not contain sub-directories.

* Made `copyDirRecur` try to copy permissions for destination directory too
  (previously it only tried to copy them for sub-directories).

## Path IO 1.0.0

* Changed signature of `getAppUserDataDir`, so it takes `String` as the
  first argument.

* Removed deprecated `forgivingAbsence'` function.

* Made `findFile` lazier, it stops searching as soon as a file is found.

* Added some tests.

## Path IO 0.3.1

* Introduced synonym for `forgivingAbsence'` —
  `ignoringAbsence`. `forgivingAbsence'` is deprecated now, but it's still
  there.

* Added a handy shortcut `ensureDir` that is defined as
  `ensureDir = createDirIfMissing True`.

* Made `getHomeDir` and `getTempDir` more robust when they are influenced by
  values of environment variables.

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

# Revision history for cli-git

## 0.2.0.0

* [#2](https://github.com/obsidiansystems/cli-git/pull/2) `ensureCleanGitRepo` and `checkGitCleanStatus` now have `MonadMask` constraints
* [#2](https://github.com/obsidiansystems/cli-git/pull/2) Add `initGitRepo`
* [#2](https://github.com/obsidiansystems/cli-git/pull/2) Bump version bounds for `cli-extras` and add dependency on `which`. **IMPORTANT:** `cli-git` will fail to compile if your build environment does not have `cp` and `git` in its `PATH` variable.

## 0.1.0.2

* Update megaparsec version bounds

## 0.1.0.1

* Add readme
* Support GHC 8.8.4. Update version bounds.

## 0.1.0.0
* Initial release

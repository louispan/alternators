[![Hackage](https://img.shields.io/hackage/v/alternators.svg)](https://hackage.haskell.org/package/alternators)
[![Build Status](https://secure.travis-ci.org/louispan/alternators.png?branch=master)](http://travis-ci.org/louispan/alternators)

Handy functions when using transformers

# Changelog

* 0.2.0.0
  - Added `Readr` which is a ReaderT that uses the Monoid/Semigroup
    instance from `(->) r`
  - Added `Delegate`, a specialization of `ContT () m r`
  - Removed `runReaderM` as it can be replaced with `lift`, `hoist` and `>>=`
    See [gist](https://gist.github.com/louispan/1c7792d45ebe5559ffc45aa9db461c35)
  - Replace `fromMaybeT` with `evalMaybeT`.

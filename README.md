[![Hackage](https://img.shields.io/hackage/v/alternators.svg)](https://hackage.haskell.org/package/alternators)
[![Build Status](https://secure.travis-ci.org/louispan/alternators.png?branch=master)](http://travis-ci.org/louispan/alternators)

Handy functions when using transformers

# Changelog

* 1.0.0.0
  - Removed `runReaderM` as it can be replaced with `lift`, `hoist` and `>>=`
    See [gist](https://gist.github.com/louispan/1c7792d45ebe5559ffc45aa9db461c35)
  - Replace `fromMaybeT` with `evalMaybeT`.
  - Added `AReaderT`, `AStateT`, `AContT`, `ARWS` newtype wrappers with `Monoid` instances.
  - Added `MonadDelegate` for accessing the continuation in `ContT` transformer stacks.
  - Added `MonadDefer` for a more accessing a continuation in the same monad. This is more handy but has fewer instances.

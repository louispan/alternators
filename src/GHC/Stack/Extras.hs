{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Stack.Extras
( module GHC.Stack
, withoutCallStack
) where

import GHC.Stack
import Data.String
import qualified Data.List as L

-- Based on 'GHC.Stack.prettyCallstack'
prettyCallStack' :: (Semigroup str, IsString str) => str -> [(String, SrcLoc)] -> Maybe str
prettyCallStack' delim cs = case cs of
    [] -> Nothing
    xs -> Just . foldr (<>) "" . L.intersperse delim $ prettyCallSite' <$> xs

trimmedCallstack :: Maybe Int -> [(String, SrcLoc)] -> [(String, SrcLoc)]
trimmedCallstack depth cs = maybe cs (`take` cs) depth

prettyCallSite' :: (Semigroup str, IsString str) => (String, SrcLoc) -> str
prettyCallSite' (f, loc) = fromString f <> "@" <> prettySrcLoc' loc

prettySrcLoc' :: (Semigroup str, IsString str) => SrcLoc -> str
prettySrcLoc' SrcLoc {..}
    = foldr (<>) "" $ L.intersperse ":"
        [ fromString srcLocModule
        , fromString $ show srcLocStartLine
        , fromString $ show srcLocStartCol
        ]

-- | Perform some computation excluding the caller's 'CallStack'
-- This function actually pops two entries from the callstack
-- `withoutCallStack` and the caller of `withoutCallStack`.
-- Do not use this more than once in one function,
-- otherwise it will pop off too much.
withoutCallStack :: HasCallStack
                    => ( HasCallStack => a )
                    -> a
withoutCallStack do_this =
  -- we pop the stack before freezing it to remove
  -- withoutCallStack's call-site
  let ?callStack = freezeCallStack (popCallStack (popCallStack callStack))
  in do_this

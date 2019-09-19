{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}


module GHC.Stack.Extras
( module GHC.Stack
, withoutCallStack
) where

import GHC.Stack

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

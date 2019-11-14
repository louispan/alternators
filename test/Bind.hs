{-# LANGUAGE DataKinds #-}

module Bind where

import Control.Monad.Bind
import Data.Proxy

m1 :: IO (Proxy "1")
m1 = pure Proxy

m2 :: IO (Proxy "2")
m2 = pure Proxy

m3 :: IO (Proxy "3")
m3 = pure Proxy

m4 :: IO (Proxy "4")
m4 = pure Proxy

m5 :: IO (Proxy "5")
m5 = pure Proxy

m6 :: IO (Proxy "6")
m6 = pure Proxy

m7 :: IO (Proxy "7")
m7 = pure Proxy

m8 :: IO (Proxy "8")
m8 = pure Proxy

m9 :: IO (Proxy "9")
m9 = pure Proxy

m10 :: IO (Proxy "10")
m10 = pure Proxy

f1 :: Proxy "1" -> IO ()
f1 _ = pure ()

f2 :: Proxy "1" -> Proxy "2" -> IO ()
f2 _ _ = pure ()

f3 :: Proxy "1" -> Proxy "2" -> Proxy "3" -> IO ()
f3 _ _ _ = pure ()

f4 :: Proxy "1" -> Proxy "2" -> Proxy "3" -> Proxy "4" -> IO ()
f4 _ _ _ _ = pure ()

f5 :: Proxy "1" -> Proxy "2" -> Proxy "3" -> Proxy "4" -> Proxy "5" -> IO ()
f5 _ _ _ _ _ = pure ()

f6 :: Proxy "1" -> Proxy "2" -> Proxy "3" -> Proxy "4" -> Proxy "5" -> Proxy "6" -> IO ()
f6 _ _ _ _ _ _ = pure ()

f7 :: Proxy "1" -> Proxy "2" -> Proxy "3" -> Proxy "4" -> Proxy "5" -> Proxy "6" -> Proxy "7" -> IO ()
f7 _ _ _ _ _ _ _ = pure ()

f8 :: Proxy "1" -> Proxy "2" -> Proxy "3" -> Proxy "4" -> Proxy "5" -> Proxy "6" -> Proxy "7" -> Proxy "8" -> IO ()
f8 _ _ _ _ _ _ _ _ = pure ()

f9 :: Proxy "1" -> Proxy "2" -> Proxy "3" -> Proxy "4" -> Proxy "5" -> Proxy "6" -> Proxy "7" -> Proxy "8" -> Proxy "9" -> IO ()
f9 _ _ _ _ _ _ _ _ _ = pure ()

f10 :: Proxy "1" -> Proxy "2" -> Proxy "3" -> Proxy "4" -> Proxy "5" -> Proxy "6" -> Proxy "7" -> Proxy "8" -> Proxy "9" -> Proxy "10" -> IO ()
f10 _ _ _ _ _ _ _ _ _ _ = pure ()

f11 :: Proxy "1" -> Proxy "2" -> Proxy "3" -> Proxy "4" -> Proxy "5" -> Proxy "6" -> Proxy "7" -> Proxy "8" -> Proxy "9" -> Proxy "10" -> Proxy "11" -> IO ()
f11 _ _ _ _ _ _ _ _ _ _ _ = pure ()

g1_1 :: IO ()
g1_1 = f1 `bind1` m1

g2_1 :: Proxy "2" -> IO ()
g2_1 = f2 `bind1` m1

g2_1_1 :: IO ()
g2_1_1 = f2 `bind1` m1 `bind1` m2

g2_2 :: Proxy "1" -> IO ()
g2_2 = f2 `bind2` m2

g2_2_1 :: IO ()
g2_2_1 = f2 `bind2` m2 `bind1` m1

g3 :: Proxy "1" -> Proxy "2" -> IO ()
g3 = f3 `bind3` m3

g4 :: Proxy "1" -> Proxy "2" -> Proxy "3" -> IO ()
g4 = f4 `bind4` m4

g5 :: Proxy "1" -> Proxy "2" -> Proxy "3" -> Proxy "4" -> IO ()
g5 = f5 `bind5` m5

g6 :: Proxy "1" -> Proxy "2" -> Proxy "3" -> Proxy "4" -> Proxy "5" -> IO ()
g6 = f6 `bind6` m6

g7 :: Proxy "1" -> Proxy "2" -> Proxy "3" -> Proxy "4" -> Proxy "5" -> Proxy "6" -> IO ()
g7 = f7 `bind7` m7

g8 :: Proxy "1" -> Proxy "2" -> Proxy "3" -> Proxy "4" -> Proxy "5" -> Proxy "6" -> Proxy "7" -> IO ()
g8 = f8 `bind8` m8

g9 :: Proxy "1" -> Proxy "2" -> Proxy "3" -> Proxy "4" -> Proxy "5" -> Proxy "6" -> Proxy "7" -> Proxy "8" -> IO ()
g9 = f9 `bind9` m9

g10 :: Proxy "1" -> Proxy "2" -> Proxy "3" -> Proxy "4" -> Proxy "5" -> Proxy "6" -> Proxy "7" -> Proxy "8" -> Proxy "9" -> IO ()
g10 = f10 `bind10` m10

g_x1 :: Proxy "1" -> Proxy "3" -> Proxy "8" -> IO ()
g_x1 = f10 `bind10` m10 `bind5` m5 `bind6` m7 `bind2` m2 `bind3` m4 `bind5` m9 `bind3` m6

g_x2 :: Proxy "9" -> Proxy "10" -> IO ()
g_x2 = f10 `bind1` m1 `bind1` m2 `bind1` m3 `bind1` m4 `bind1` m5 `bind1` m6 `bind1` m7 `bind1` m8

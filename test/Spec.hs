module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.ST.Class
import Control.Monad.Trans.ACont
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Extras
import Data.IORef
import Data.STRef
import Safe
import Test.Hspec

main :: IO ()
main = hspec spec

-- | a variation of 'discharge' that also catches 'empty', resulting
-- in a monad that is guaranteed to fire unit @()@ once.
-- This is no longer an inverse of 'delegate'
-- delegate $ discharge' m = m without empty
dischargeA :: (MonadDischarge m, Alternative m) => m a -> (a -> m ()) -> m ()
dischargeA m f = (discharge m f) <|> pure ()
infixl 1 `dischargeA` -- like `(>>=)`

-- | Variation of 'dischargeHead' guaranteed to fire once by catching 'empty'.
-- Even if the input monad result in empty, if it fires at least once
-- then 'dischargeHead'' will result in a Just
dischargeHeadA :: (MonadST m, Alternative m, MonadDischarge m) => m a -> m (Maybe a)
dischargeHeadA m = do
    v <- liftST $ newSTRef Nothing
    dischargeA m $ go v
    liftST $ readSTRef v
  where
    go v a = do
        v' <- liftST $ readSTRef v
        case v' of
            Nothing -> liftST $ writeSTRef v (Just a)
            Just _ -> pure ()

empty' :: (Alternative m, MonadDelegate m) => m String
empty' = empty

-- | convert any monad (whether terminated with 'finish' or 'empty') its events as a Just
-- followed by a final Nothing.
terminally' :: (MonadDischarge m, Alternative m) => m a -> m (Maybe a)
terminally' m = (Just <$> (m `also` empty)) <|> pure Nothing

start :: (MonadDelegate m, MonadIO m) => IORef [String] -> m String -> m String
start v m = do
    liftIO $ modifyIORef v ("start" :)
    m

basic :: (MonadDelegate m, MonadIO m) => IORef [String] -> m String
basic v = start v $
    delegate $ \fire -> do
        fire "hello"
        fire "world"
        fire "bye"
        liftIO $ modifyIORef v ("done" :)

basicWithEmpty :: (Alternative m, MonadDelegate m, MonadIO m) => IORef [String] -> m String
basicWithEmpty v = start v $
    delegate $ \fire -> do
        fire "hello"
        fire "world"
        fire "bye"
        liftIO $ modifyIORef v ("done" :)
        void empty
        liftIO $ modifyIORef v ("ignored" :)

basicWithFinish :: (MonadDelegate m, MonadIO m) => IORef [String] -> m String
basicWithFinish v = start v $
    delegate $ \fire -> do
        fire "hello"
        fire "world"
        fire "bye"
        liftIO $ modifyIORef v ("done" :)
        void finish
        liftIO $ modifyIORef v ("ignored" :)


basicWithFinishEmpty :: (Alternative m, MonadDelegate m, MonadIO m) => IORef [String] -> m String
basicWithFinishEmpty v = start v $
    delegate $ \fire -> do
        fire "hello"
        fire "world"
        fire "bye"
        liftIO $ modifyIORef v ("done" :)
        void $ delegate $ \_ -> empty
        liftIO $ modifyIORef v ("ignored" :)

spec :: Spec
spec = do
    let testBasic runM m expected = do
            it "delegate allows firing multiple times" $ do
                v <- newIORef []
                void $ runM $ do
                    a <- m v
                    -- This happens once for every @a@
                    liftIO $ modifyIORef v (a :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ("end" : expected)

            it "callCC get called multiple times as well" $ do
                v <- newIORef []
                void $ runM $ do
                    a <- callCC $ \k -> m v >>= k
                    liftIO $ modifyIORef v (a :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ("end" : expected)

        testDischarge hasEmpty runM m expected = do
            it "discharge reduces it back to only firing once, even if it does not fire" $ do
                v <- newIORef []
                void $ runM $ do
                    let f a = liftIO $ modifyIORef v (a :)
                    discharge (delegate $ \_ -> pure ()) f
                    -- This only happens once
                    liftIO $ modifyIORef v ("test" :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ["end", "test"]

            it "discharge reduces it back to only firing once, except for empty" $ do
                v <- newIORef []
                void $ runM $ do
                    let f a = liftIO $ modifyIORef v (a :)
                    discharge (m v) f
                    -- This only happens once
                    liftIO $ modifyIORef v ("test" :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                if hasEmpty
                    then as `shouldBe` ["end"] <> expected
                    else as `shouldBe` ["end", "test"] <> expected

            it "delegate + discharge = id" $ do
                v <- newIORef []
                void $ runM $ do
                    a <- delegate $ discharge (m v)
                    -- This happens once for every @a@
                    liftIO $ modifyIORef v (a :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ["end"] <> expected

            it "delegate + discharge = id (2)" $ do
                v <- newIORef []
                void $ runM $ do
                    a <- delegate $ discharge (delegate $ \_ -> pure ())
                    -- This happens once for every @a@
                    liftIO $ modifyIORef v ("never" :)
                    liftIO $ modifyIORef v (a :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ["end"]

        testDischargeA hasEmpty runM m expected = do
            it "delegate + discharge = id (empty preserved)" $ do
                v <- newIORef []
                void $ runM $ do
                    a <- (delegate $ discharge (m v)) <|> pure "foo"
                    -- This happens once for every @a@
                    liftIO $ modifyIORef v (a :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                if hasEmpty
                    then as `shouldBe` ["end", "foo"] <> expected
                    else as `shouldBe` ["end"] <> expected

            it "delegate + dischargeA != id (empty not preserved)" $ do
                v <- newIORef []
                void $ runM $ do
                    a <- (delegate $ dischargeA (m v)) <|> pure "foo"
                    -- This happens once for every @a@
                    liftIO $ modifyIORef v (a :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ["end"] <> expected

            it "dischargeA reduces it back to only firing once, even for empty" $ do
                v <- newIORef []
                void $ runM $ do
                    let f a = liftIO $ modifyIORef v (a :)
                    dischargeA (m v) f
                    -- This only happens once
                    liftIO $ modifyIORef v ("test" :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ["end", "test"] <> expected

        testDischargeHead hasEmpty runM m expected starting = do
            it "dischargeHead" $ do
                v <- newIORef []
                void $ runM $ do
                    a <- show <$> dischargeHead (m v)
                    liftIO $ modifyIORef v (a :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                let xs = if hasEmpty then [] else [show $ headMay expected]
                as `shouldBe` ["end"] <> xs <> starting

        testDischargeHeadA runM m expected starting = do
            it "dischargeHeadA" $ do
                v <- newIORef []
                void $ runM $ do
                    a <- show <$> dischargeHeadA (m v)
                    liftIO $ modifyIORef v (a :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                let xs = [show $ headMay expected]
                as `shouldBe` ["end"] <> xs <> starting

        testDischargeList hasEmpty runM m expected starting = do
            it "dischargeList" $ do
                v <- newIORef []
                void $ runM $ do
                    as <- dischargeList (m v)
                    liftIO $ modifyIORef v (as <>)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                let xs = if hasEmpty then [] else expected
                as `shouldBe` ["end"] <> xs <> starting

    describe "Alternative: empty" $ do
        testBasic ((`evalMaybeT` ()) . evalAContT) (const empty) []

    describe "Alternative: m <|> empty = m" $ do
        let expected = ["done", "bye", "world", "hello", "start"]
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> basic v <|> empty) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> basicWithFinish v <|> empty) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> basicWithEmpty v <|> empty) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> basicWithFinishEmpty v <|> empty) expected

    describe "Alternative: empty <|> m = m" $ do
        let expected = ["done", "bye", "world", "hello", "start"]
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> empty <|> basic v) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> empty <|> basicWithFinish v) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> empty <|> basicWithEmpty v) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> empty <|> basicWithFinishEmpty v) expected

    describe "Alternative: left catch: pure foo <|> m = pure foo" $ do
        let expected = ["foo"]
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> pure "foo" <|> basic v) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> pure "foo" <|> basicWithFinish v) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> pure "foo" <|> basicWithEmpty v) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> pure "foo" <|> basicWithFinishEmpty v) expected

    describe "Alternative: basic <|> pure foo = basic" $ do
        let expected =  ["done", "bye", "world", "hello", "start"]
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> basic v <|> pure "foo") expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> basicWithFinish v <|> pure "foo") expected

    describe "Alternative: basicWithEmpty <|> pure foo" $ do
        let expected = ["foo", "done", "bye", "world", "hello", "start"]
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> basicWithEmpty v <|> pure "foo") expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> basicWithFinishEmpty v <|> pure "foo") expected

    describe "Alternative: NOT right Zero. m *> empty != empty. bind to empty teminates, but includes effects up to termination" $ do
        let expected = ["start"]
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> basic v *> empty) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> basicWithFinish v *> empty) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> basicWithEmpty v *> empty) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> basicWithFinishEmpty v *> empty) expected

    describe "Alternative: left distribution: a <|> b >>= k = (a >>= k) <|> (b >>= k)" $ do
        -- m <|> empty
        let expected = ["done", "byebye", "worldworld", "hellohello", "start"]
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> (basic v <|> empty) >>= \a -> pure (a <> a)) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> (basicWithFinish v <|> empty) >>= \a -> pure (a <> a)) expected

        -- no empty <|> pure foo
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> (basicWithFinish v <|> pure "foo") >>= \a -> pure (a <> a)) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> (basic v <|> pure "foo") >>= \a -> pure (a <> a)) expected

        -- empty <|> m
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> (empty <|> basic v) >>= \a -> pure (a <> a)) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> (empty <|> basicWithFinish v) >>= \a -> pure (a <> a)) expected

        -- m <|> n
        let expected' = ["foofoo", "done", "byebye", "worldworld", "hellohello", "start"]
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> (basicWithEmpty v <|> pure "foo") >>= \a -> pure (a <> a)) expected'
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> (basicWithFinishEmpty v <|> pure "foo") >>= \a -> pure (a <> a)) expected'

    -------------------------------------------------------------
    describe "Alternative: discharge " $ do
        do
            let expected = ["done", "bye", "world", "hello", "start"]
            testDischarge False ((`evalMaybeT` ()) . evalAContT) basic expected
            testDischarge False ((`evalMaybeT` ()) . evalAContT) basicWithFinish expected
            testDischarge True ((`evalMaybeT` ()) . evalAContT) (const empty) []
            testDischarge True ((`evalMaybeT` ()) . evalAContT) basicWithEmpty expected
            testDischarge True ((`evalMaybeT` ()) . evalAContT) basicWithFinishEmpty expected

            testDischargeA False ((`evalMaybeT` ()) . evalAContT) basic expected
            testDischargeA False ((`evalMaybeT` ()) . evalAContT) basicWithFinish expected
            testDischargeA True ((`evalMaybeT` ()) . evalAContT) (const empty) []
            testDischargeA True ((`evalMaybeT` ()) . evalAContT) basicWithEmpty expected
            testDischargeA True ((`evalMaybeT` ()) . evalAContT) basicWithFinishEmpty expected

            testDischargeHead True ((`evalMaybeT` ()) . evalAContT) (\v -> start v empty') ([] :: [String]) ["start"]
            testDischargeHead False ((`evalMaybeT` ()) . evalAContT) basic ["hello"] ["done", "start"]
            testDischargeHead False ((`evalMaybeT` ()) . evalAContT) basicWithFinish ["hello"] ["done", "start"]
            testDischargeHead True ((`evalMaybeT` ()) . evalAContT) basicWithEmpty ([] :: [String]) ["done", "start"]
            testDischargeHead True ((`evalMaybeT` ()) . evalAContT) basicWithFinishEmpty ([] :: [String]) ["done", "start"]

        -- dischargeHead' catchs failures
        do
            let expected = ["hello"]
                starting = ["done", "start"]
            testDischargeHeadA ((`evalMaybeT` ()) . evalAContT) (\v -> start v empty') ([] :: [String]) ["start"]
            testDischargeHeadA ((`evalMaybeT` ()) . evalAContT) basic expected starting
            testDischargeHeadA ((`evalMaybeT` ()) . evalAContT) basicWithFinish expected starting
            testDischargeHeadA ((`evalMaybeT` ()) . evalAContT) basicWithEmpty expected starting
            testDischargeHeadA ((`evalMaybeT` ()) . evalAContT) basicWithFinishEmpty expected starting

        do
            let expected = ["hello", "world", "bye"]
                starting = ["done", "start"]
            testDischargeList True ((`evalMaybeT` ()) . evalAContT) (\v -> start v empty') ([] :: [String]) ["start"]
            testDischargeList False ((`evalMaybeT` ()) . evalAContT) basic expected starting
            testDischargeList False ((`evalMaybeT` ()) . evalAContT) basicWithFinish expected starting
            testDischargeList True ((`evalMaybeT` ()) . evalAContT) basicWithEmpty ([] :: [String]) starting
            testDischargeList True ((`evalMaybeT` ()) . evalAContT) basicWithFinishEmpty ([] :: [String]) starting

    describe "Alternative: terminally' " $ do
        let expected = ["Nothing","done","Just \"bye\"","Just \"world\"","Just \"hello\"","start"]
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> show <$> terminally' (basic v)) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> show <$> terminally' (basicWithFinish v)) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> show <$> terminally' (basicWithEmpty v)) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> show <$> terminally' (basicWithFinishEmpty v)) expected

        -- terminally removes empty and finish effects
        testDischarge False ((`evalMaybeT` ()) . evalAContT) (\v -> show <$> terminally' (basic v)) expected
        testDischarge False ((`evalMaybeT` ()) . evalAContT) (\v -> show <$> terminally' (basicWithFinish v)) expected
        testDischarge False ((`evalMaybeT` ()) . evalAContT) (\v -> show <$> terminally' (basicWithEmpty v)) expected
        testDischarge False ((`evalMaybeT` ()) . evalAContT) (\v -> show <$> terminally' (basicWithFinishEmpty v)) expected

    describe "Alternative: also " $ do
        let expected = ["foo", "done", "bye", "world", "hello", "start"]
        testBasic evalAContT (\v -> (basic v `also` empty `also` empty) <|> pure "foo") expected
        testBasic evalAContT (\v -> (finish `also` basic v `also` empty `also` empty) <|> pure "foo") expected

    describe "ContT" $ do
        let expected = ["done", "bye", "world", "hello", "start"]
        testBasic evalContT basic expected
        testBasic evalContT basicWithFinish expected
        testDischarge False evalContT basic expected
        testDischarge False evalContT basicWithFinish expected

        -- also
        testBasic evalContT (\v -> basic v `also` finish) expected
        testBasic evalContT (\v -> finish `also` basic v) expected
        testBasic evalContT (\v -> finish `also` basic v `also` pure "foo") ("foo" : expected)

    describe "AContT" $ do
        let expected = ["done", "bye", "world", "hello", "start"]
        testBasic evalAContT basic expected
        testBasic evalAContT basicWithFinish expected
        testDischarge False evalAContT basic expected
        testDischarge False evalAContT basicWithFinish expected


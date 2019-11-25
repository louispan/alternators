module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Trans.ACont
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Extras
import Data.IORef
import Safe
import Test.Hspec

main :: IO ()
main = hspec spec

empty' :: (Alternative m, MonadDelegate m) => m String
empty' = empty

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
                    discharge f (delegate $ \_ -> pure ())
                    -- This only happens once
                    liftIO $ modifyIORef v ("test" :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ["end", "test"]

            it "discharge reduces it back to only firing once, except for empty" $ do
                v <- newIORef []
                void $ runM $ do
                    let f a = liftIO $ modifyIORef v (a :)
                    discharge f (m v)
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
                    a <- delegate $ \fire -> discharge fire (m v)
                    -- This happens once for every @a@
                    liftIO $ modifyIORef v (a :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ["end"] <> expected

            it "delegate + discharge = id (2)" $ do
                v <- newIORef []
                void $ runM $ do
                    a <- delegate $ \fire -> discharge fire (delegate $ \_ -> pure ())
                    -- This happens once for every @a@
                    liftIO $ modifyIORef v ("never" :)
                    liftIO $ modifyIORef v (a :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ["end"]

        testDischarge' hasEmpty runM m expected = do
            it "delegate + discharge = id (empty preserved)" $ do
                v <- newIORef []
                void $ runM $ do
                    a <- (delegate $ \fire -> discharge fire (m v)) <|> pure "foo"
                    -- This happens once for every @a@
                    liftIO $ modifyIORef v (a :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                if hasEmpty
                    then as `shouldBe` ["end", "foo"] <> expected
                    else as `shouldBe` ["end"] <> expected

            it "delegate + discharge != id (empty not preserved)" $ do
                v <- newIORef []
                void $ runM $ do
                    a <- (delegate $ \fire -> discharge' fire (m v)) <|> pure "foo"
                    -- This happens once for every @a@
                    liftIO $ modifyIORef v (a :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ["end"] <> expected

            it "discharge' reduces it back to only firing once, even for empty" $ do
                v <- newIORef []
                void $ runM $ do
                    let f a = liftIO $ modifyIORef v (a :)
                    discharge' f (m v)
                    -- This only happens once
                    liftIO $ modifyIORef v ("test" :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ["end", "test"] <> expected

        testDischargeHead hasEmpty runM m expected = do
            it "dischargeHead" $ do
                v <- newIORef []
                void $ runM $ do
                    a <- show <$> dischargeHead (m v)
                    liftIO $ modifyIORef v (a :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                let xs = [ show $ do
                        bs <- initMay expected -- skip start
                        headMay bs
                        ]
                    xs' = if hasEmpty then [] else xs
                as `shouldBe` ["end"] <> xs' <> (maybe [] pure (headMay expected))

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
        let expected = ["done", "bye", "world", "hello", "start"]
        testDischarge False ((`evalMaybeT` ()) . evalAContT) basic expected
        testDischarge False ((`evalMaybeT` ()) . evalAContT) basicWithFinish expected
        testDischarge True ((`evalMaybeT` ()) . evalAContT) (const empty) []
        testDischarge True ((`evalMaybeT` ()) . evalAContT) basicWithEmpty expected
        testDischarge True ((`evalMaybeT` ()) . evalAContT) basicWithFinishEmpty expected

        testDischarge' False ((`evalMaybeT` ()) . evalAContT) basic expected
        testDischarge' False ((`evalMaybeT` ()) . evalAContT) basicWithFinish expected
        testDischarge' True ((`evalMaybeT` ()) . evalAContT) (const empty) []
        testDischarge' True ((`evalMaybeT` ()) . evalAContT) basicWithEmpty expected
        testDischarge' True ((`evalMaybeT` ()) . evalAContT) basicWithFinishEmpty expected

        testDischargeHead True ((`evalMaybeT` ()) . evalAContT) (const empty') []

    describe "Alternative: terminally " $ do
        let expected = ["Nothing","done","Just \"bye\"","Just \"world\"","Just \"hello\"","start"]
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> show <$> terminally (basic v)) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> show <$> terminally (basicWithFinish v)) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> show <$> terminally (basicWithEmpty v)) expected
        testBasic ((`evalMaybeT` ()) . evalAContT) (\v -> show <$> terminally (basicWithFinishEmpty v)) expected
        -- temrinally removes empty and finish effects
        testDischarge False ((`evalMaybeT` ()) . evalAContT) (\v -> show <$> terminally (basic v)) expected
        testDischarge False ((`evalMaybeT` ()) . evalAContT) (\v -> show <$> terminally (basicWithFinish v)) expected
        testDischarge False ((`evalMaybeT` ()) . evalAContT) (\v -> show <$> terminally (basicWithEmpty v)) expected
        testDischarge False ((`evalMaybeT` ()) . evalAContT) (\v -> show <$> terminally (basicWithFinishEmpty v)) expected

    describe "ContT" $ do
        let expected = ["done", "bye", "world", "hello", "start"]
        testBasic evalContT basic expected
        testBasic evalContT basicWithFinish expected
        testDischarge False evalContT basic expected
        testDischarge False evalContT basicWithFinish expected

    describe "AContT" $ do
        let expected = ["done", "bye", "world", "hello", "start"]
        testBasic evalAContT basic expected
        testBasic evalAContT basicWithFinish expected
        testDischarge False evalAContT basic expected
        testDischarge False evalAContT basicWithFinish expected


    -- describe "AContT ()" $ do
    --     -- testDelegate evalAContT basic
    --     -- testDischarge evalAContT basic
    --     it "delegateHead" $ do
    --         v <- newIORef []
    --         ((`evalMaybeT` ()) . evalAContT) $ do
    --             a <- delegateHeadIO (basic v)
    --             liftIO $ modifyIORef v (a :)
    --         liftIO $ modifyIORef v ("end" :)
    --         as <- readIORef v
    --         as `shouldBe` ["end", "hello", "start"]


    -- describe "AContT ()" $ do
    --     -- testDelegate evalAContT basic
    --     -- testDischarge evalAContT basic
    --     it "delegateHead" $ do
    --         v <- newIORef []
    --         ((`evalMaybeT` ()) . evalAContT) $ do
    --             a <- delegateHeadIO (basic v <|> pure "hi")
    --             liftIO $ modifyIORef v (a :)
    --         liftIO $ modifyIORef v ("end" :)
    --         as <- readIORef v
    --         as `shouldBe` ["end", "hello", "start"]


    describe "MaybeT (ContT ())" $ do
        -- testDelegate evalAContT basic
        -- testDischarge evalAContT basic
        it "finish also" $ do
            v <- newIORef []
            ((`evalMaybeT` ()) . evalAContT) $ do
                a <- (finish `also` empty `also` empty) <|> pure "foo"
                liftIO $ modifyIORef v (a :)
            liftIO $ modifyIORef v ("end" :)
            as <- readIORef v
            as `shouldBe` ["end", "foo"]

    describe "MaybeT (ContT ())" $ do
        -- testDelegate evalAContT basic
        -- testDischarge evalAContT basic
        it "also is definitely after" $ do
            v <- newIORef []
            ((`evalMaybeT` ()) . evalAContT) $ do
                a <- (basic v <|> pure "hi") `also` empty `also` empty <|> pure "foo"
                liftIO $ modifyIORef v (a :)
            liftIO $ modifyIORef v ("end" :)
            as <- readIORef v
            as `shouldBe` ["end", "foo", "done", "bye", "world", "hello", "start"]

    describe "MaybeT (ContT ())" $ do
        -- testDelegate evalAContT basic
        -- testDischarge evalAContT basic
        it "dischargeHead" $ do
            v <- newIORef []
            -- (void . runTerminateT . evalAContT) $ do
            (void . evalContT) $ do
                a <- dischargeHead (basic v)
                case a of
                    Nothing -> liftIO $ modifyIORef v ("Nothing" :)
                    Just a' -> liftIO $ modifyIORef v (a' :)
            liftIO $ modifyIORef v ("end" :)
            as <- readIORef v
            as `shouldBe` ["end", "hello", "done", "start"]

    -- describe "MaybeT (ContT ()" $ do
    --     -- testDelegate evalAContT basic
    --     -- testDischarge evalAContT basic
    --     it "delegateHead2" $ do
    --         v <- newIORef []
    --         -- (void . runTerminateT . evalAContT) $ do
    --         (void . evalAContT) $ do
    --             a <- delegateHead (basic v <|> pure "hi") -- `also` empty
    --             liftIO $ modifyIORef v (a :)
    --         liftIO $ modifyIORef v ("end" :)
    --         as <- readIORef v
    --         as `shouldBe` ["end", "hello", "start"]


    describe "MaybeT (ContT ())" $ do
        -- testDelegate evalAContT basic
        -- testDischarge evalAContT basic
        it "alternative" $ do
            v <- newIORef []
            ((`evalMaybeT` ()) . evalAContT) $ do
                let m1 = pure "foo" `also` empty
                    m2 = pure "bar" `also` empty
                a <- m1 <|> m2 <|> (pure "hi")
                liftIO $ modifyIORef v (a :)
            liftIO $ modifyIORef v ("end" :)
            as <- readIORef v
            as `shouldBe` ["end", "hi", "bar", "foo"]

        it "alternative2 also" $ do
            v <- newIORef []
            ((`evalMaybeT` ()) . evalAContT) $ do
                let m2 = delegate $ \fire -> do
                        fire "bar"
                        empty
                    m1 = m0 `also` empty -- FIXME: `also` doesn't work, doesn't respect empty
                    m0 = delegate $ \fire -> do
                        fire "foo"
                        liftIO $ modifyIORef v ("wack" :)
                        empty
                        -- empty
                a <- m1 <|> m2 <|> (pure "hi")
                liftIO $ modifyIORef v (a :)
            liftIO $ modifyIORef v ("end" :)
            as <- readIORef v
            as `shouldBe` ["end", "hi", "bar", "wack", "foo"]

    -- describe "AContT () MaybeT" $ do
    --     testDelegateAlternative ((`evalMaybeT` ()) . evalAContT) basic
    --     testDischarge ((`evalMaybeT` ()) . evalAContT) basic

    -- describe "MaybeT ContT" $ do
    -- --     testDelegate (evalContT . (`evalMaybeT` ())) basic
    -- --     testDelegateAlternative (evalContT . (`evalMaybeT` ())) basic
    --     testDischarge ((`evalMaybeT` ()) . evalAContT) basic

    --     it "discharge reduces it back to only firing once2" $ do
    --         v <- newIORef []
    --         void $ ((`evalMaybeT` ()) . evalAContT) $ do
    --             let f a = do
    --                     liftIO $ modifyIORef v (a :)
    --                     empty -- NB. This empty doesn't get caught as it's in discharge
    --                 m v' = do
    --                     liftIO $ modifyIORef v' ("start" :)
    --                     delegate $ \fire -> do
    --                         fire "hello"
    --                         fire "world"
    --                         fire "bye"
    --                         void $ empty
    --                         empty
    --             discharge f (m v <|> pure "extra" <|> pure "none")
    --             -- This only happens once, but doesn't happen because of empty in f
    --             liftIO $ modifyIORef v ("test" :)
    --         liftIO $ modifyIORef v ("end" :)
    --         as <- readIORef v
    --         as `shouldBe` ["end"] <> ["none", "extra", "hello", "start"]

    --     it "dischargeHead" $ do
    --         v <- newIORef []
    --         ((`evalMaybeT` ()) . evalAContT) $ do
    --             a <- delegate $ \fire -> do
    --                 -- for every event, fire it, then stop
    --                 -- which means stop after first event
    --                 a <- basic
    --                 fire a
    --                 lift empty

    --             liftIO $ modifyIORef v (a :)
    --         liftIO $ modifyIORef v ("end" :)
    --         as <- readIORef v
    --         as `shouldBe` ["end", "hello"]

    -- describe "ExceptT AContT MaybeT" $ do
    --     testDischarge ((`evalMaybeT` ()) . evalAContT . void . runExceptT) basic

    --     it "dischargeHead" $ do
    --         v <- newIORef []
    --         ((`evalMaybeT` ()) . evalAContT . void . runExceptT) $ do
    --             a <- delegate $ \fire -> do
    --                 -- for every event, fire it, then stop
    --                 -- which means stop after first event
    --                 a <- basic
    --                 fire a
    --                 lift empty

    --             liftIO $ modifyIORef v (a :)
    --         liftIO $ modifyIORef v ("end" :)
    --         as <- readIORef v
    --         as `shouldBe` ["end", "hello"]

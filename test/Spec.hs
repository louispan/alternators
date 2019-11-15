module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Except
import Control.Monad.Trans.ACont
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Extras
import Data.IORef
import Test.Hspec

main :: IO ()
main = hspec spec

basic :: MonadDelegate m => m String
basic = delegate $ \fire -> do
        fire "hello"
        fire "world"
        fire "bye"

spec :: Spec
spec = do
    let testBasic runM m expected = do
            v <- newIORef []
            void $ runM $ do
                a <- m
                -- This happens once for every @a@
                liftIO $ modifyIORef v (a :)
            liftIO $ modifyIORef v ("end" :)
            as <- readIORef v
            as `shouldBe` "end": expected

        testDelegate runM m = do
            it "delegate allows firing multiple times" $ do
                testBasic runM m ["bye", "world", "hello"]

            it "callCC get call multiple times" $ do
                v <- newIORef []
                void $ runM $ do
                    a <- callCC $ \k -> m >>= k
                    liftIO $ modifyIORef v (a :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ["end", "bye", "world", "hello"]

        testDischarge runM m = do
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

            it "discharge reduces it back to only firing once" $ do
                v <- newIORef []
                void $ runM $ do
                    let f a = liftIO $ modifyIORef v (a :)
                    discharge f m
                    -- This only happens once
                    liftIO $ modifyIORef v ("test" :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ["end", "test"] <> ["bye", "world", "hello"]

            it "delegate + discharge = id" $ do
                v <- newIORef []
                void $ runM $ do
                    a <- delegate $ \fire -> discharge fire m
                    -- This happens once for every @a@
                    liftIO $ modifyIORef v (a :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ["end"] <> ["bye", "world", "hello"]

            it "delegate + discharge = id (2)" $ do
                v <- newIORef []
                void $ runM $ do
                    a <- delegate $ \fire -> discharge fire (delegate $ \_ -> pure ())
                    -- This happens once for every @a@
                    liftIO $ modifyIORef v (a :)
                    liftIO $ modifyIORef v ("never" :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ["end"]

        testDelegateAlternative runM m = do
            it "delegate allows firing with failure1" $ do
                v <- newIORef []
                void $ runM $ do
                    a <- delegate $ \fire -> do
                        fire "a"
                        fire "b"
                        void $ empty
                        m >>= fire
                    -- This happens once for every @a@
                    liftIO $ modifyIORef v (a :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ["end", "b", "a"]

            it "delegate allows firing with failure0" $ do
                v <- newIORef []
                void $ runM $ do
                    a <- delegate $ \fire -> do
                        void $ empty
                        m >>= fire
                    -- This happens once for every @a@
                    liftIO $ modifyIORef v (a :)
                liftIO $ modifyIORef v ("end" :)
                as <- readIORef v
                as `shouldBe` ["end"]


    describe "Alternative: Monoid: empty" $ do
        it "empty" $ do
            testBasic ((`evalMaybeT` ()) . evalAContT) empty []

    describe "Alternative: Monoid: right empty" $ do
        testDelegate ((`evalMaybeT` ()) . evalAContT) (basic <|> empty)

    describe "Alternative: Monoid: left empty" $ do
        testDelegate ((`evalMaybeT` ()) . evalAContT) (empty <|> basic)

    describe "Alternative" $ do
        it "pure foo <|> basic = pure foo" $ do
            testBasic ((`evalMaybeT` ()) . evalAContT) (pure "foo" <|> basic) ["foo"]

    describe "Alternative: basic <|> pure foo = basic" $ do
        testDelegate ((`evalMaybeT` ()) . evalAContT) (basic <|> pure "foo")

    describe "Alternative:  Right Zero. m *> empty = empty" $ do
        it "right empty" $ do
            testBasic ((`evalMaybeT` ()) . evalAContT) (basic *> empty) []

    describe "Alternative: left distribution: a <|> b >>= k = (a >>= k) <|> (b >>= k)" $ do
        it "right mzero" $ do
            testBasic ((`evalMaybeT` ()) . evalAContT) ((basic <|> empty) >>= \a -> pure (a <> a))
                ["byebye", "worldworld", "hellohello"]

        it "left mzero" $ do
            testBasic ((`evalMaybeT` ()) . evalAContT) ((empty <|> basic) >>= \a -> pure (a <> a))
                ["byebye", "worldworld", "hellohello"]

        it "no mzero" $ do
            testBasic ((`evalMaybeT` ()) . evalAContT) ((empty <|> basic) >>= \a -> pure (a <> a))
                ["byebye", "worldworld", "hellohello"]

    describe "Alternative: left catch: (pure a) <|> b = pure a" $ do
        it "right mzero" $ do
            testBasic ((`evalMaybeT` ()) . evalAContT) (pure "foo" <|> basic)
                ["foo"]

    -------------------------------------------------------------

    describe "MonadPlus: Monoid: mzero" $ do
        it "mzero" $ do
            testBasic ((`evalMaybeT` ()) . evalAContT) mzero []

    describe "MonadPlus: Monoid: right mzero" $ do
        testDelegate ((`evalMaybeT` ()) . evalAContT) (basic `mplus` empty)

    describe "MonadPlus: Monoid: left mzero" $ do
        testDelegate ((`evalMaybeT` ()) . evalAContT) (empty `mplus` basic)

    describe "MonadPlus" $ do
        it "pure foo `mplus` basic = pure foo" $ do
            testBasic ((`evalMaybeT` ()) . evalAContT) (pure "foo" `mplus` basic) ["foo"]

    describe "MonadPlus: basic `mplus` pure foo = basic" $ do
        testDelegate ((`evalMaybeT` ()) . evalAContT) (basic `mplus` pure "foo")

    describe "MonadPlus: Left Zero: mzero >>= k = mzero" $ do
        it "MonadPlus: Left Zero: mzero >>= k = mzero" $ do
            let m = mzero >>= \_ -> error "crash"
            v <- newIORef []
            (`evalMaybeT` ()) $ evalAContT $ do
                a <- m
                -- This happens once for every @a@
                liftIO $ modifyIORef v (a :)
            liftIO $ modifyIORef v ("end" :)
            as <- readIORef v
            as `shouldBe` ["end"]

    describe "MonadPlus: m *> mzero = mzero" $ do
        it "right mzero" $ do
            testBasic ((`evalMaybeT` ()) . evalAContT) (basic *> mzero) []

    describe "MonadPlus: left distribution: a `mplus` b >>= k = (a >>= k) `mplus` (b >>= k)" $ do
        it "right mzero" $ do
            testBasic ((`evalMaybeT` ()) . evalAContT) ((basic `mplus` mzero) >>= \a -> pure (a <> a))
                ["byebye", "worldworld", "hellohello"]

        it "left mzero" $ do
            testBasic ((`evalMaybeT` ()) . evalAContT) ((mzero `mplus` basic) >>= \a -> pure (a <> a))
                ["byebye", "worldworld", "hellohello"]

        it "no mzero" $ do
            testBasic ((`evalMaybeT` ()) . evalAContT) ((basic `mplus` basic) >>= \a -> pure (a <> a))
                ["byebye", "worldworld", "hellohello"]

    describe "MonadPlus: left catch: (return a) `mplus` b = return a" $ do
        it "right mzero" $ do
            testBasic ((`evalMaybeT` ()) . evalAContT) (pure "foo" `mplus` basic)
                ["foo"]

    -------------------------------------------------------------
    describe "ContT" $ do
        testDelegate evalContT basic
        testDischarge evalContT basic

    describe "AContT ()" $ do
        testDelegate evalAContT basic
        testDischarge evalAContT basic
        it "delegateHead" $ do
            v <- newIORef []
            ((`evalMaybeT` ()) . evalAContT) $ do
                a <- delegateHead basic
                liftIO $ modifyIORef v (a :)
            liftIO $ modifyIORef v ("end" :)
            as <- readIORef v
            as `shouldBe` ["end", "hello"]

    describe "AContT () MaybeT" $ do
        testDelegateAlternative ((`evalMaybeT` ()) . evalAContT) basic
        testDischarge ((`evalMaybeT` ()) . evalAContT) basic

    describe "MaybeT ContT" $ do
        testDelegate (evalContT . (`evalMaybeT` ())) basic
        testDelegateAlternative (evalContT . (`evalMaybeT` ())) basic
        testDischarge ((`evalMaybeT` ()) . evalContT) basic

        it "dischargeHead" $ do
            v <- newIORef []
            ((`evalMaybeT` ()) . evalAContT) $ do
                a <- delegate $ \fire -> do
                    -- for every event, fire it, then stop
                    -- which means stop after first event
                    a <- basic
                    fire a
                    lift empty

                liftIO $ modifyIORef v (a :)
            liftIO $ modifyIORef v ("end" :)
            as <- readIORef v
            as `shouldBe` ["end", "hello"]

    describe "ExceptT AContT MaybeT" $ do
        testDischarge ((`evalMaybeT` ()) . evalAContT . void . runExceptT) basic

        it "dischargeHead" $ do
            v <- newIORef []
            ((`evalMaybeT` ()) . evalAContT . void . runExceptT) $ do
                a <- delegate $ \fire -> do
                    -- for every event, fire it, then stop
                    -- which means stop after first event
                    a <- basic
                    fire a
                    lift empty

                liftIO $ modifyIORef v (a :)
            liftIO $ modifyIORef v ("end" :)
            as <- readIORef v
            as `shouldBe` ["end", "hello"]

    describe "ExceptT ContT" $ do
        testDelegate (evalContT . void . runExceptT) basic

        let processError v m = do
                e <- m
                case e of
                    Left a -> liftIO $ modifyIORef v (a :)
                    Right () -> pure ()

        it "delegate allows firing with failure1" $ do
            v <- newIORef []
            (void . evalContT . processError v . runExceptT) $ do
                a <- delegate $ \fire -> do
                    fire "a"
                    fire "b"
                    void $ throwError "error"
                    basic >>= fire
                -- This happens once for every @a@
                liftIO $ modifyIORef v (a :)
            liftIO $ modifyIORef v ("end" :)
            as <- readIORef v
            as `shouldBe` ["end", "error", "b", "a"]

        it "delegate allows firing with failure0" $ do
            v <- newIORef []
            (void . evalContT . processError v . runExceptT) $ do
                a <- delegate $ \fire -> do
                    void $ throwError "error"
                    basic >>= fire
                -- This happens once for every @a@
                liftIO $ modifyIORef v (a :)
            liftIO $ modifyIORef v ("end" :)
            as <- readIORef v
            as `shouldBe` ["end", "error"]

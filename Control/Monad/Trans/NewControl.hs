{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.NewControl
    ( -- * Introduction
      -- $intro

      -- ** Nesting calls to the base monad
      -- $nesting

      -- ** A generic solution
      -- $generic

      -- ** Using monad-control and liftBaseWith
      -- $usage

      -- * MonadBaseControl
      MonadBaseControl (..), defaultLiftBaseWith

      -- * MonadBaseControl
    , MonadTransControl (..), defaultLiftWith

      -- * Utility functions
    , control, liftBaseOp, liftBaseOp_, liftBaseDiscard
    ) where

import           Control.Monad ((>=>), liftM)
import           Control.Monad.ST.Lazy (ST)
import qualified Control.Monad.ST.Strict as Strict (ST)
import           Control.Monad.Trans.Error (ErrorT(ErrorT), runErrorT, Error)
import           Control.Monad.Trans.Identity (IdentityT(IdentityT),
                                               runIdentityT)
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.Monad.Trans.List (ListT(ListT), runListT)
import           Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import           Control.Monad.Trans.RWS (RWST(RWST), runRWST)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST(RWST), runRWST)
import           Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import           Control.Monad.Trans.State (StateT(StateT), runStateT)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT (StateT),
                                                             runStateT)
import           Control.Monad.Trans.Writer (WriterT(WriterT), runWriterT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT(WriterT),
                                                              runWriterT)
import           Data.Functor.Identity (Identity)
import           Data.Monoid (Monoid, mempty)
import           GHC.Conc.Sync (STM)

{- $intro

What is the problem that 'monad-control' aims to solve?  To answer that, let's
back up a bit.  We know that a monad represents some kind of "computational
context".  The question is, can we separate this context from the monad, and
reconstitute it later?  If we know the monadic types involved, then for some
monads we can.  Consider the 'State' monad: it's essentially a function from
an existing state, to a pair of some new state and a value.  It's fairly easy
then to extract its state and later use it to "resume" that monad:

@
import Control.Applicative
import Control.Monad.Trans.State

main = do
    let f = do { modify (+1); show <$> get } :: StateT Int IO String

    (x,y) <- runStateT f 0
    print $ "x = " ++ show x   -- x = "1"

    (x',y') <- runStateT f y
    print $ "x = " ++ show x'  -- x = "2"
@

In this way, we interleave between @StateT Int IO@ and 'IO', by completing the
'StateT' invocation, obtaining its state as a value, and starting a new
'StateT' block from the prior state.  We've effectively resumed the earlier
'StateT' block.
-}

{- $nesting

But what if we didn't, or couldn't, exit the 'StateT' block to run our 'IO'
computation?  In that case we'd need to use 'liftIO' to enter 'IO' and make a
nested call to 'runStateT' inside that 'IO' block.  Further, we'd want to
restore any changes made to the inner 'StateT' within the outer 'StateT',
after returning from the 'IO' action:

@
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.IO.Class

main = do
    let f = do { modify (+1); show <$> get } :: StateT Int IO String

    flip runStateT 0 $ do
        x <- f
        y <- get
        y' <- liftIO $ do
            print $ "x = " ++ show x   -- x = "1"

            (x',y') <- runStateT f y
            print $ "x = " ++ show x'  -- x = "2"
            return y'
        put y'
@
-}

{- $generic

This works fine for 'StateT', but how can we write it so that it works for any
monad tranformer over IO?  We'd need a function that might look like this:

@
foo :: MonadIO m => m String -> m String
foo f = do
    x <- f
    y <- getTheState
    y' <- liftIO $ do
        print $ "x = " ++ show x

        (x',y') <- runTheMonad f y
        print $ "x = " ++ show x'
        return y'
    putTheState y'
@

But this is impossible, since we only know that 'm' is a 'Monad'.  Even with a
'MonadState' constraint, we would not know about a function like
'runTheMonad'.  This indicates we need a type class with at least three
capabilities: getting the current monad tranformer's state, executing a new
transformer within the base monad, and restoring the enclosing transformer's
state upon returning from the base monad.  This is exactly what
'MonadBaseControl' provides, from 'monad-control':

@
class MonadBase b m => MonadBaseControl b m | m -> b where
    data StM m :: * -> *
    liftBaseWith :: (RunInBase m b -> b a) -> m a
    restoreM :: StM m a -> m a
@

Taking this definition apart piece by piece:

1. The 'MonadBase' constraint exists so that 'MonadBaseControl' can be used
   over multiple base monads: 'IO', 'ST', 'STM', etc.

2. 'liftBaseWith' combines three things from our last example into one: it
   gets the current state from the monad transformer, wraps it an 'StM' type,
   lifts the given action into the base monad, and provides that action with a
   function which can be used to resume the enclosing monad within the base
   monad.  When such a function exits, it returns a new 'StM' value.

3. 'restoreM' takes the encapsulated tranformer state as an 'StM' value, and
   applies it to the parent monad transformer so that any changes which may
   have occurred within the "inner" transformer are propagated out.  (This
   also has the effect that later, repeated calls to 'restoreM' can "reset"
   the transformer state back to what it was previously.)
-}

{- $usage

With that said, here's the same example from above, but now generic for any
transformer supporting @MonadBaseControl IO@:

@
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans.Control

foo :: MonadBaseControl IO m => m String -> m String
foo f = do
    x <- f
    y' <- liftBaseWith $ \runInIO -> do
        print $ "x = " ++ show x   -- x = "1"

        x' <- runInIO f
        -- print $ "x = " ++ show x'

        return x'
    restoreM y'

main = do
    let f = do { modify (+1); show <$> get } :: StateT Int IO String

    (x',y') <- flip runStateT 0 $ foo f
    print $ "x = " ++ show x'   -- x = "2"
@

One notable difference in this example is that the second 'print' statement in
'foo' becomes impossible, since the "monadic value" returned from the inner
call to 'f' must be restored and executed within the outer monad.  That is,
@runInIO f@ is executed in IO, but it's result is an @StM m String@ rather
than @IO String@, since the computation carries monadic context from the inner
transformer.  Converting this to a plain 'IO' computation would require
calling a function like 'runStateT', which we cannot do without knowing which
transformer is being used.

As a convenience, since calling 'restoreM' after exiting 'liftBaseWith' is so
common, you can use 'control' instead of @restoreM =<< liftBaseWith@:

@
y' <- restoreM =<< liftBaseWith (\runInIO -> runInIO f)

becomes...

y' <- control $ \runInIO -> runInIO f
@

Another common pattern is when you don't need to restore the inner
transformer's state to the outer transformer, you just want to pass it down as
an argument to some function in the base monad:

@
foo :: MonadBaseControl IO m => m String -> m String
foo f = do
    x <- f
    liftBaseDiscard forkIO $ f
@

In this example, the first call to 'f' affects the state of 'm', while the
inner call to 'f', though inheriting the state of 'm' in the new thread, but
does not restore its effects to the parent monad transformer when it returns.

Now that we have this machinery, we can use it to make any function in 'IO'
directly usable from any supporting transformer.  Take 'catch' for example:

@
catch :: Exception e => IO a -> (e -> IO a) -> IO a
@

What we'd like is a function that works for any @MonadBaseControl IO m@,
rather than just 'IO'.  With the 'control' function this is easy:

@
catch :: (MonadBaseControl IO m, Exception e) => m a -> (e -> m a) -> m a
catch f h = control $ \runInIO -> catch (runInIO f) (runInIO . h)
@

You can find many function which are generalized like this in the packages
'lifted-base' and 'lifted-async'.
-}

--------------------------------------------------------------------------------
-- * MonadBaseControl
--------------------------------------------------------------------------------

class (Monad m, Monad b) => MonadBaseControl b m | m -> b where
    -- | Monadic state of @m@.
    data StM m a :: *

    -- | @liftBaseWith@ is similar to 'liftIO' and 'liftBase' in that it
    -- lifts a base computation to the constructed monad.
    --
    -- Instances should satisfy similar laws as the 'MonadIO' and 'MonadBase'
    -- laws:
    --
    -- @liftBaseWith . const . return = return@
    --
    -- @liftBaseWith (const (m >>= f)) =
    --      liftBaseWith (const m) >>= liftBaseWith . const . f@
    --
    -- The difference with 'liftBase' is that before lifting the base
    -- computation @liftBaseWith@ captures the state of @m@. It then provides
    -- the base computation with a 'RunInBase' function that allows running
    -- @m@ computations in the base monad on the captured state.
    captureM :: m (StM m ())

    runWithM :: StM m () -> m a -> b (StM m a)

    -- | Construct a @m@ computation from the monadic state of @m@ that is
    -- returned from a 'RunInBase' function.
    --
    -- Instances should satisfy:
    --
    -- @liftBaseWith (\\runInBase -> runInBase m) >>= restoreM = m@
    restoreM :: StM m a -> m a

-- | Default implementation of 'liftBaseWith'.  Below is a prototypical
--   instance of 'MonadBaseControl' for 'StateT':
--
-- @
--instance MonadBaseControl b m => MonadBaseControl b (StateT s m) where
--    newtype StM (StateT s m) a = StateST (StM m (a, s))
--    liftBaseWith f = StateT $ \\s ->
--        defaultLiftBaseWith  f (`runStateT` s) StateST (,s)
--    restoreM (StateST m) = StateT . const . restoreM $ m
-- @
defaultLiftBaseWith
    :: MonadBaseControl b m
    => ((t m x -> b (StM (t m) x)) -> b s) -- ^ Passed through from liftBaseWith
    -> (t m x -> m a)                     -- ^ Run the monad transformer
    -> (StM m a -> StM (t m) x)           -- ^ Constructor for instance's StM
    -> (s -> r)                           -- ^ Transform returned state value
    -> m r
defaultLiftBaseWith f h u g =
    liftM g $ liftBaseWith $ \runInBase -> f $ \k ->
        liftM u $ runInBase $ h k
{-# INLINE defaultLiftBaseWith #-}

--------------------------------------------------------------------------------
-- * MonadTransControl
--------------------------------------------------------------------------------

class MonadTrans t => MonadTransControl t where
  -- | Monadic state of @t@.
  data StT t :: * -> *

  -- | @liftWith@ is similar to 'lift' in that it lifts a computation from
  -- the argument monad to the constructed monad.
  --
  -- Instances should satisfy similar laws as the 'MonadTrans' laws:
  --
  -- @liftWith . const . return = return@
  --
  -- @liftWith (const (m >>= f)) = liftWith (const m) >>= liftWith . const . f@
  --
  -- The difference with 'lift' is that before lifting the @m@ computation
  -- @liftWith@ captures the state of @t@. It then provides the @m@
  -- computation with a 'Run' function that allows running @t n@ computations in
  -- @n@ (for all @n@) on the captured state.
  liftWith :: (Monad m, Monad n) => ((t n x -> n (StT t x)) -> m a) -> t m a

  -- | Construct a @t@ computation from the monadic state of @t@ that is
  -- returned from a 'Run' function.
  --
  -- Instances should satisfy:
  --
  -- @liftWith (\\run -> run t) >>= restoreT . return = t@
  restoreT :: Monad m => StT t a -> t m a

-- | Default implementation of 'liftWith'.  Below is a prototypical instance
--   of 'MonadTransControl' for 'StateT':
--
-- @
--instance MonadTransControl (StateT s) where
--    newtype StT (StateT s) a = StateST { unStateST :: (a, s) }
--    liftWith f = StateT $ \\s ->
--        defaultLiftWith  f (`runStateT` s) StateST (,s)
--    restoreT = StateT . const . return . unStateST
-- @
defaultLiftWith :: (Monad m, Monad n)
                => ((x -> n y) -> m s) -- ^ Passed through from liftWith
                -> (x -> n a)         -- ^ Run the monad transformer
                -> (a -> y)           -- ^ Constructor for instance's StT
                -> (s -> r)           -- ^ Transform returned state value
                -> m r
defaultLiftWith f h u g = liftM g $ f $ liftM u . h
{-# INLINE defaultLiftWith #-}

--------------------------------------------------------------------------------
-- * Base Instances
--------------------------------------------------------------------------------

-- instance MonadBaseControl IO IO where
--      newtype StM IO a = StIO a
--      liftBaseWith f = f $ liftM StIO
--      restoreM (StIO x) = return x
--      {-# INLINE liftBaseWith #-}
--      {-# INLINE restoreM #-}

-- instance MonadBaseControl Maybe Maybe where
--      newtype StM Maybe a = St a
--      liftBaseWith f = f $ liftM St
--      restoreM (St x) = return x
--      {-# INLINE liftBaseWith #-}
--      {-# INLINE restoreM #-}

-- instance MonadBaseControl (Either e) (Either e) where
--      newtype StM (Either e) a = StE a
--      liftBaseWith f = f $ liftM StE
--      restoreM (StE x) = return x
--      {-# INLINE liftBaseWith #-}
--      {-# INLINE restoreM #-}

-- instance MonadBaseControl [] [] where
--      newtype StM [] a = StL a
--      liftBaseWith f = f $ liftM StL
--      restoreM (StL x) = return x
--      {-# INLINE liftBaseWith #-}
--      {-# INLINE restoreM #-}

-- instance MonadBaseControl ((->) r) ((->) r) where
--      newtype StM ((->) r) a = StF a
--      liftBaseWith f = f $ liftM StF
--      restoreM (StF x) = return x
--      {-# INLINE liftBaseWith #-}
--      {-# INLINE restoreM #-}

-- instance MonadBaseControl Identity Identity where
--      newtype StM Identity a = StI a
--      liftBaseWith f = f $ liftM StI
--      restoreM (StI x) = return x
--      {-# INLINE liftBaseWith #-}
--      {-# INLINE restoreM #-}

-- instance MonadBaseControl STM STM where
--      newtype StM STM a = StSTM a
--      liftBaseWith f = f $ liftM StSTM
--      restoreM (StSTM x) = return x
--      {-# INLINE liftBaseWith #-}
--      {-# INLINE restoreM #-}

-- instance MonadBaseControl (Strict.ST s) (Strict.ST s) where
--      newtype StM (Strict.ST s) a = StSTS a
--      liftBaseWith f = f $ liftM StSTS
--      restoreM (StSTS x) = return x
--      {-# INLINE liftBaseWith #-}
--      {-# INLINE restoreM #-}

-- instance MonadBaseControl (ST s) (ST s) where
--      newtype StM (ST s) a = StST a
--      liftBaseWith f = f $ liftM StST
--      restoreM (StST x) = return x
--      {-# INLINE liftBaseWith #-}
--      {-# INLINE restoreM #-}

--------------------------------------------------------------------------------
-- * Transformer Instances
--------------------------------------------------------------------------------

-- -- ListT

-- instance MonadBaseControl b m => MonadBaseControl b (ListT m) where
--     newtype StM (ListT m) a = ListTStM (StM m [a])
--     liftBaseWith f = ListT $ defaultLiftBaseWith f runListT ListTStM return
--     restoreM (ListTStM m) = ListT . restoreM $ m
--     {-# INLINE liftBaseWith #-}
--     {-# INLINE restoreM #-}

-- instance  MonadTransControl (ListT) where
--     newtype StT ListT a = ListTStT { unListTStT :: [a] }
--     liftWith f = ListT $ defaultLiftWith f runListT ListTStT return
--     restoreT = ListT . return . unListTStT
--     {-# INLINE liftWith #-}
--     {-# INLINE restoreT #-}

-- -- MaybeT

-- instance MonadBaseControl b m => MonadBaseControl b (MaybeT m) where
--     newtype StM (MaybeT m) a = MaybeTStM (StM m (Maybe a))
--     liftBaseWith f = MaybeT $ defaultLiftBaseWith f runMaybeT MaybeTStM return
--     restoreM (MaybeTStM m) = MaybeT . restoreM $ m
--     {-# INLINE liftBaseWith #-}
--     {-# INLINE restoreM #-}

-- instance  MonadTransControl (MaybeT) where
--     newtype StT MaybeT a = MaybeTStT { unMaybeTStT :: Maybe a }
--     liftWith f = MaybeT $ defaultLiftWith f runMaybeT MaybeTStT return
--     restoreT = MaybeT . return . unMaybeTStT
--     {-# INLINE liftWith #-}
--     {-# INLINE restoreT #-}

-- -- IdentityT

-- instance MonadBaseControl b m
--          => MonadBaseControl b (IdentityT m) where
--     newtype StM (IdentityT m) a = IdentityTStM (StM m a)
--     liftBaseWith f =
--         IdentityT $ defaultLiftBaseWith f runIdentityT IdentityTStM id
--     restoreM (IdentityTStM m) = IdentityT . restoreM $ m
--     {-# INLINE liftBaseWith #-}
--     {-# INLINE restoreM #-}

-- instance  MonadTransControl (IdentityT) where
--     newtype StT IdentityT a = IdentityTStT { unIdentityTStT :: a }
--     liftWith f = IdentityT $ defaultLiftWith f runIdentityT IdentityTStT id
--     restoreT = IdentityT . return . unIdentityTStT
--     {-# INLINE liftWith #-}
--     {-# INLINE restoreT #-}

-- -- WriterT

-- instance (MonadBaseControl b m, Monoid w)
--          => MonadBaseControl b (WriterT w m) where
--     newtype StM (WriterT w m) a = WriterTStM (StM m   (a, w))
--     liftBaseWith f = WriterT $
--         defaultLiftBaseWith f runWriterT WriterTStM (\x -> (x, mempty))
--     restoreM (WriterTStM m) = WriterT . restoreM $ m
--     {-# INLINE liftBaseWith #-}
--     {-# INLINE restoreM #-}

-- instance  Monoid w => MonadTransControl (WriterT w) where
--     newtype StT (WriterT w) a = WriterTStT { unWriterTStT :: (a, w) }
--     liftWith f = WriterT $
--         defaultLiftWith f runWriterT WriterTStT (\x -> (x, mempty))
--     restoreT = WriterT . return . unWriterTStT
--     {-# INLINE liftWith #-}
--     {-# INLINE restoreT #-}

-- instance (MonadBaseControl b m, Monoid w)
--          => MonadBaseControl b (Strict.WriterT w m) where
--     newtype StM (Strict.WriterT w m) a = StWriterTStM (StM m (a, w))
--     liftBaseWith f = Strict.WriterT $
--         defaultLiftBaseWith f Strict.runWriterT StWriterTStM (\x -> (x, mempty))
--     restoreM (StWriterTStM m) = Strict.WriterT . restoreM $ m
--     {-# INLINE liftBaseWith #-}
--     {-# INLINE restoreM #-}

-- instance Monoid w => MonadTransControl (Strict.WriterT w) where
--     newtype StT (Strict.WriterT w) a = StWriterTStT { unStWriterTStT :: (a, w) }
--     liftWith f = Strict.WriterT $
--         defaultLiftWith f Strict.runWriterT StWriterTStT (\x -> (x, mempty))
--     restoreT = Strict.WriterT . return . unStWriterTStT
--     {-# INLINE liftWith #-}
--     {-# INLINE restoreT #-}

-- -- ErrorT

-- instance (MonadBaseControl b m, Error e)
--          => MonadBaseControl b (ErrorT e m) where
--     newtype StM (ErrorT e m) a = ErrorTStM (StM m (Either e a))
--     liftBaseWith f = ErrorT $ defaultLiftBaseWith f runErrorT ErrorTStM return
--     restoreM (ErrorTStM m) = ErrorT . restoreM $ m
--     {-# INLINE liftBaseWith #-}
--     {-# INLINE restoreM #-}

-- instance Error e => MonadTransControl (ErrorT e) where
--     newtype StT (ErrorT e) a = ErrorTStT { unErrorTStT :: Either e a }
--     liftWith f = ErrorT $ defaultLiftWith f runErrorT ErrorTStT return
--     restoreT = ErrorT . return . unErrorTStT
--     {-# INLINE liftWith #-}
--     {-# INLINE restoreT #-}

-- StateT

instance MonadBaseControl b m => MonadBaseControl b (StateT s m) where
    newtype StM (StateT s m) a = StateTStM (StM m (a, s))
    captureM = StateT $ \s -> liftM StateTStM $ liftM (\x -> (x,s)) $ captureM
    runWithM (StateTStM s) m = runWithM s (runStateT m)
    restoreM (StateTStM m) = StateT . const . restoreM $ m
    {-# INLINE captureM #-}
    {-# INLINE runWithM #-}
    {-# INLINE restoreM #-}

-- instance MonadTransControl (StateT s) where
--     newtype StT (StateT s) a = StateTStT { unStateTStT :: (a, s) }
--     liftWith f = StateT $ \s ->
--         defaultLiftWith f (`runStateT` s) StateTStT (\x -> (x,s))
--     restoreT = StateT . const . return . unStateTStT
--     {-# INLINE liftWith #-}
--     {-# INLINE restoreT #-}

-- instance MonadBaseControl b m => MonadBaseControl b (Strict.StateT s m) where
--     newtype StM (Strict.StateT s m) a = StStateTStM (StM m (a, s))
--     liftBaseWith f = Strict.StateT $ \s ->
--         defaultLiftBaseWith f (`Strict.runStateT` s) StStateTStM (\x -> (x,s))
--     restoreM (StStateTStM m) = Strict.StateT . const . restoreM $ m
--     {-# INLINE liftBaseWith #-}
--     {-# INLINE restoreM #-}

-- instance MonadTransControl (Strict.StateT s) where
--     newtype StT (Strict.StateT s) a = StStateTStT { unStStateTStT :: (a, s) }
--     liftWith f = Strict.StateT $ \s ->
--         defaultLiftWith f (`Strict.runStateT` s) StStateTStT (\x -> (x,s))
--     restoreT = Strict.StateT . const . return . unStStateTStT
--     {-# INLINE liftWith #-}
--     {-# INLINE restoreT #-}

-- -- ReaderT

-- instance MonadBaseControl b m => MonadBaseControl b (ReaderT r m) where
--     newtype StM (ReaderT r m) a = ReaderTStM (StM m a)
--     liftBaseWith f = ReaderT $ \r ->
--         defaultLiftBaseWith f (`runReaderT` r) ReaderTStM id
--     restoreM (ReaderTStM m) = ReaderT . const . restoreM $ m
--     {-# INLINE liftBaseWith #-}
--     {-# INLINE restoreM #-}

-- instance MonadTransControl (ReaderT r) where
--     newtype StT (ReaderT r) a = ReaderTStT { unReaderTStT :: a }
--     liftWith f = ReaderT $ \r ->
--         defaultLiftWith f (`runReaderT` r) ReaderTStT id
--     restoreT = ReaderT . const . return . unReaderTStT
--     {-# INLINE liftWith #-}
--     {-# INLINE restoreT #-}

-- -- RWST

-- instance (MonadBaseControl b m, Monoid w)
--          => MonadBaseControl b (RWST r w s m) where
--     newtype StM (RWST r w s m) a = RWSTStM (StM m (a, s ,w))
--     liftBaseWith f = RWST $ \r s ->
--         defaultLiftBaseWith f (flip (`runRWST` r) s) RWSTStM
--             (\x -> (x,s,mempty))
--     restoreM (RWSTStM m) = RWST . const . const . restoreM $ m
--     {-# INLINE liftBaseWith #-}
--     {-# INLINE restoreM #-}

-- instance Monoid w => MonadTransControl (RWST r w s) where
--     newtype StT (RWST r w s) a = RWSTStT { unRWSTStT :: (a, s ,w) }
--     liftWith f = RWST $ \r s ->
--         defaultLiftWith f (flip (`runRWST` r) s) RWSTStT (\x -> (x,s,mempty))
--     restoreT = RWST . const . const . return . unRWSTStT
--     {-# INLINE liftWith #-}
--     {-# INLINE restoreT #-}

-- instance (MonadBaseControl b m, Monoid w)
--          => MonadBaseControl b (Strict.RWST r w s m) where
--     newtype StM (Strict.RWST r w s m) a = StRWSTStM (StM m (a, s, w))
--     liftBaseWith f = Strict.RWST $ \r s ->
--         defaultLiftBaseWith f (flip (`Strict.runRWST` r) s) StRWSTStM
--             (\x -> (x,s,mempty))
--     restoreM (StRWSTStM m) = Strict.RWST . const . const . restoreM $ m
--     {-# INLINE liftBaseWith #-}
--     {-# INLINE restoreM #-}

-- instance Monoid w => MonadTransControl (Strict.RWST r w s) where
--     newtype StT (Strict.RWST r w s) a = StRWSTStT { unStRWSTStT :: (a, s, w) }
--     liftWith f = Strict.RWST $ \r s ->
--         defaultLiftWith f (flip (`Strict.runRWST` r) s) StRWSTStT
--             (\x -> (x,s,mempty))
--     restoreT = Strict.RWST . const . const . return . unStRWSTStT
--     {-# INLINE liftWith #-}
--     {-# INLINE restoreT #-}

--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

liftBaseWith :: MonadBaseControl b m => ((m x -> b (StM m x)) -> b a) -> m a
liftBaseWith = undefined
    -- liftBaseWith f = StateT $ \s ->
    --     defaultLiftBaseWith f (`runStateT` s) StateTStM (\x -> (x,s))

-- | An often used composition: @control = 'liftBaseWith' >>= 'restoreM'@
control :: MonadBaseControl b m => ((m x -> b (StM m x)) -> b (StM m a)) -> m a
control = liftBaseWith >=> restoreM
{-# INLINE control #-}

-- | @liftBaseOp@ is a particular application of 'liftBaseWith' that allows
-- lifting control operations of type:
--
-- @((a -> b c) -> b c)@ to: @('MonadBaseControl' b m => (a -> m c) -> m c)@.
--
-- For example:
--
-- @liftBaseOp alloca :: 'MonadBaseControl' 'IO' m => (Ptr a -> m c) -> m c@
liftBaseOp :: MonadBaseControl b m
           => ((a -> b (StM m c)) -> b (StM m d)) -> (a -> m c)  -> m d
liftBaseOp f g = control $ \runInBase -> f $ runInBase . g
{-# INLINE liftBaseOp #-}

-- | @liftBaseOp_@ is a particular application of 'liftBaseWith' that allows
-- lifting control operations of type:
--
-- @(b a -> b a)@ to: @('MonadBaseControl' b m => m a -> m a)@.
--
-- For example:
--
-- @liftBaseOp_ mask_ :: 'MonadBaseControl' 'IO' m => m a -> m a@
liftBaseOp_ :: MonadBaseControl b m
            => (b (StM m a) -> b (StM m c)) -> m a -> m c
liftBaseOp_ f m = control $ \runInBase -> f $ runInBase m
{-# INLINE liftBaseOp_ #-}

-- | @liftBaseDiscard@ is a particular application of 'liftBaseWith' that allows
-- lifting control operations of type:
--
-- @(b () -> b a)@ to: @('MonadBaseControl' b m => m () -> m a)@.
--
-- Note that, while the argument computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in the base monad @b@.
--
-- For example:
--
-- @liftBaseDiscard forkIO :: 'MonadBaseControl' 'IO' m => m () -> m ThreadId@
liftBaseDiscard :: MonadBaseControl b m => (b (StM m c) -> b a) -> m c -> m a
liftBaseDiscard f m = liftBaseWith $ \runInBase -> f $ runInBase m
{-# INLINE liftBaseDiscard #-}

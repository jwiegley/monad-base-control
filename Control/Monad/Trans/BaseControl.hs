{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.BaseControl
    ( -- * MonadBaseControl
      MonadBaseControl (..), defaultLiftBaseWith
      -- * Utility functions
    , control, liftBaseOp, liftBaseOp_, liftBaseDiscard
    ) where

import           Control.Monad ((>=>), liftM)
import           Control.Monad.ST.Lazy (ST)
import qualified Control.Monad.ST.Strict as Strict (ST)
import           Control.Monad.Trans.Error (ErrorT(ErrorT), runErrorT, Error)
import           Control.Monad.Trans.Identity (IdentityT(IdentityT),
                                               runIdentityT)
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
    -- @liftBaseWith (const (m >>= f)) = liftBaseWith (const m) >>= liftBaseWith . const . f@
    --
    -- The difference with 'liftBase' is that before lifting the base
    -- computation @liftBaseWith@ captures the state of @m@. It then provides
    -- the base computation with a 'RunInBase' function that allows running
    -- @m@ computations in the base monad on the captured state.
    liftBaseWith :: ((m x -> b (StM m x)) -> b a) -> m a

    -- | Construct a @m@ computation from the monadic state of @m@ that is
    -- returned from a 'RunInBase' function.
    --
    -- Instances should satisfy:
    --
    -- @liftBaseWith (\\runInBase -> runInBase m) >>= restoreM = m@
    restoreM :: StM m a -> m a

#define BASE(M, ST)               \
instance MonadBaseControl M M where {     \
    newtype StM M a = ST a;               \
    liftBaseWith f = f $ liftM ST;        \
    restoreM (ST x) = return x;           \
    {-# INLINE liftBaseWith #-};          \
    {-# INLINE restoreM #-}}

BASE(IO,            StIO)
BASE(Maybe,         St)
BASE((Either e),    StE)
BASE([],            StL)
BASE(((->) r),       StF)
BASE(Identity,      StI)
BASE(STM,           StSTM)
BASE((Strict.ST s), StSTS)
BASE(       (ST s), StST)

#undef BASE

-- | Default implementation of 'liftBaseWith'.  Below is a prototypical instance
--   for 'MonadBaseControl' for 'StateT':
--
-- @
--instance MonadBaseControl b m => MonadBaseControl b (StateT s m) where
--    newtype StM (StateT s m) a = StateST (StM m (a, s))
--    liftBaseWith f = StateT $ \\s ->
--        defaultLiftBaseWith (,s) (`runStateT` s) StateST f
--    restoreM (StateST m) = StateT . const . restoreM $ m
-- @
defaultLiftBaseWith
    :: MonadBaseControl b m
    => (z -> r)                   -- ^ Transform returned state value
    -> (t -> m x)                 -- ^ Run the monad transformer
    -> (StM m x -> y)             -- ^ Constructor for instance's StM
    -> ((t -> b y) -> b z)         -- ^ Passed through from liftBaseWith
    -> m r
defaultLiftBaseWith g h u f =
    liftM g $ liftBaseWith $ \runInBase -> f $ \k ->
        liftM u $ runInBase $ h k

#define TRANS(CTX, M, C1, C2, R, ST, TY, F)                             \
instance (MonadBaseControl b m, CTX) => MonadBaseControl b (M m) where { \
    newtype StM (M m) a = ST (StM m TY);                                \
    liftBaseWith f  = C1 defaultLiftBaseWith F R ST f;                  \
    restoreM (ST m) = C2 . restoreM $ m;                                \
    {-# INLINE liftBaseWith #-};                                        \
    {-# INLINE restoreM #-}}

TRANS(Monad m, ListT, ListT $, ListT,
      runListT, ListTStM, [a], return)
TRANS(Monad m, MaybeT, MaybeT $, MaybeT,
      runMaybeT, MaybeTStM, (Maybe a), return)
TRANS(Monad m, IdentityT, IdentityT $, IdentityT,
      runIdentityT, IdentityTStM, a, id)
TRANS(Monoid w, WriterT w, WriterT $, WriterT,
      runWriterT, WriterTStM, (a, w), (\x -> (x, mempty)))
TRANS(Monoid w, Strict.WriterT w, Strict.WriterT $, Strict.WriterT,
      Strict.runWriterT, StWriterTStM, (a, w), (\x -> (x, mempty)))
TRANS(Error e, ErrorT e, ErrorT $, ErrorT,
      runErrorT, ErrorTStM, (Either e a), return)
TRANS(Monad m, StateT s, StateT $ \s ->, StateT . const,
      (`runStateT` s), StateTStM, (a, s), (\x -> (x,s)))
TRANS(Monad m, Strict.StateT s, Strict.StateT $ \s ->, Strict.StateT . const,
      (`Strict.runStateT` s), StStateTStM, (a, s), (\x -> (x,s)))
TRANS(Monad m, ReaderT r, ReaderT $ \r ->, ReaderT . const,
      (`runReaderT` r), ReaderTStM, a, id)
TRANS(Monoid w, RWST r w s, RWST $ \r s ->, RWST . const . const,
      (flip (`runRWST` r) s), RWSTStM, (a, s ,w), (\x -> (x,s,mempty)))
TRANS(Monoid w, Strict.RWST r w s, Strict.RWST $ \r s ->,
      Strict.RWST . const . const,
      (flip (`Strict.runRWST` r) s), StRWSTStM, (a, s, w),
      (\x -> (x,s,mempty)))

#undef TRANS

--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

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
liftBaseDiscard :: MonadBaseControl b m => (b () -> b a) -> m () -> m a
liftBaseDiscard f m = liftBaseWith $ \runInBase -> f $ runInBase m >> return ()
{-# INLINE liftBaseDiscard #-}

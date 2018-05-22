module Control.Monad.Trans.Control where

import Control.Monad.Trans.Accum
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy as L
import Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.Writer.Lazy as L
import Control.Monad.Trans.Writer.Strict as S

class MonadTrans t => MonadTransControl t where
    type StT t a
    liftWith :: Monad m => (Run t -> m a) -> t m a
    restoreT :: Monad m => m (StT t a) -> t m a

type Run t = ∀ n b . Monad n => t n b -> n (StT t b)

instance MonadTransControl IdentityT where
    type StT IdentityT a = a
    liftWith f = IdentityT $ f runIdentityT
    restoreT = IdentityT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadTransControl MaybeT where
    type StT MaybeT a = Maybe a
    liftWith f = MaybeT $ fmap pure $ f runMaybeT
    restoreT = MaybeT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadTransControl (ExceptT e) where
    type StT (ExceptT e) a = Either e a
    liftWith f = ExceptT $ fmap pure $ f runExceptT
    restoreT = ExceptT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadTransControl (ReaderT r) where
    type StT (ReaderT r) a = a
    liftWith f = ReaderT $ \ r -> f (flip runReaderT r)
    restoreT = ReaderT . pure
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadTransControl (L.StateT s) where
    type StT (L.StateT s) a = (a, s)
    liftWith f = L.StateT $ \ s -> flip (,) s <$> f (flip L.runStateT s)
    restoreT = L.StateT . pure
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadTransControl (S.StateT s) where
    type StT (S.StateT s) a = (a, s)
    liftWith f = S.StateT $ \ s -> flip (,) s <$> f (flip S.runStateT s)
    restoreT = S.StateT . pure
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance Monoid w => MonadTransControl (L.WriterT w) where
    type StT (L.WriterT w) a = (a, w)
    liftWith f = L.WriterT $ flip (,) mempty <$> f L.runWriterT
    restoreT = L.WriterT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance Monoid w => MonadTransControl (S.WriterT w) where
    type StT (S.WriterT w) a = (a, w)
    liftWith f = S.WriterT $ flip (,) mempty <$> f S.runWriterT
    restoreT = S.WriterT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance Monoid c => MonadTransControl (AccumT c) where
    type StT (AccumT c) a = (a, c)
    liftWith f = AccumT $ \ c -> flip (,) c <$> f (flip runAccumT c)
    restoreT = AccumT . pure
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

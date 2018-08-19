{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.Base.Control where

import Control.Concurrent.STM (STM)
import Control.Monad (replicateM)
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy as L
import Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.Writer.Lazy as L
import Control.Monad.Trans.Writer.Strict as S
import Control.Monad.ST.Lazy as L (ST)
import Control.Monad.ST.Strict as S (ST)
import Data.Basic
import Data.Foldable
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Language.Haskell.TH
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadPrec (ReadPrec)

class (Basic1 m, Monad m, Monad (Base m)) => MonadBaseControl m where
    type StM m a
    liftBaseWith :: (RunInBase m -> Base m a) -> m a
    restoreM :: StM m a -> m a

type RunInBase m = ∀ a . m a -> Base m (StM m a)

newtype ComposeSt t m a = ComposeSt { unComposeSt :: StM m (StT t a) }

type RunInBaseDefault t m = ∀ a . t m a -> Base m (ComposeSt t m a)

defaultLiftBaseWith :: (MonadTransControl t, MonadBaseControl m) => (RunInBaseDefault t m -> Base m a) -> t m a
defaultLiftBaseWith = \ f -> liftWith $ \ run -> liftBaseWith $ \ runInBase -> f $ fmap ComposeSt . runInBase . run
{-# INLINABLE defaultLiftBaseWith #-}

defaultRestoreM :: (MonadTransControl t, MonadBaseControl m) => ComposeSt t m a -> t m a
defaultRestoreM = restoreT . restoreM . unComposeSt
{-# INLINABLE defaultRestoreM #-}

$(let f :: Int -> Name -> Q Dec
      f n name =
          [InstanceD Nothing [] (ConT ''MonadBaseControl `AppT` t)
           [TySynInstD ''StM $ TySynEqn [t, VarT u] (VarT u),
            FunD 'liftBaseWith [Clause [VarP v] (NormalB (VarE v `AppE` VarE 'id)) []],
            ValD (VarP 'restoreM) (NormalB (VarE 'pure)) []]
            | t <- foldl' AppT (ConT name) <$> replicateM n (VarT <$> newName "a")
            , u <- newName "a"
            , v <- newName "f"]
  in traverse (uncurry f) [(0, ''IO), (1, ''L.ST), (1, ''S.ST), (0, ''STM),
                           (0, ''Maybe), (1, ''Either), (0, ''Identity),
                           (0, ''[]), (0, ''NonEmpty), (0, ''Proxy),
                           (0, ''ReadP), (0, ''ReadPrec)])

instance Monoid a => MonadBaseControl ((,) a) where
    type StM ((,) a) b = b
    liftBaseWith f = f id
    restoreM = pure

instance MonadBaseControl ((->) a) where
    type StM ((->) a) b = b
    liftBaseWith f = f id
    restoreM = pure

$(let f :: Int -> Name -> Q Dec
      f n name = 
          [InstanceD Nothing [ConT ''MonadBaseControl `AppT` f] (ConT ''MonadBaseControl `AppT` AppT t f)
           [TySynInstD ''StM $ TySynEqn [AppT t f, a] (foldl' AppT (ConT ''ComposeSt) [t, f, a]),
            ValD (VarP 'liftBaseWith) (NormalB (VarE 'defaultLiftBaseWith)) [],
            ValD (VarP 'restoreM) (NormalB (VarE 'defaultRestoreM)) []]
            | t <- foldl' AppT (ConT name) <$> replicateM n (VarT <$> newName "a")
            , f <- VarT <$> newName "f"
            , a <- VarT <$> newName "a"]
  in traverse (uncurry f) [(0, ''IdentityT), (0, ''MaybeT), (1, ''ReaderT), (1, ''ExceptT),
                           (1, ''L.StateT), (1, ''S.StateT)])

instance (MonadBaseControl f, Monoid a) => MonadBaseControl (L.WriterT a f) where
    type StM (L.WriterT a f) b = ComposeSt (L.WriterT a) f b
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance (MonadBaseControl f, Monoid a) => MonadBaseControl (S.WriterT a f) where
    type StM (S.WriterT a f) b = ComposeSt (S.WriterT a) f b
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance (MonadBaseControl f, Monoid a) => MonadBaseControl (AccumT a f) where
    type StM (AccumT a f) b = ComposeSt (AccumT a) f b
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

control :: MonadBaseControl m => (RunInBase m -> Base m (StM m a)) -> m a
control f = liftBaseWith f >>= restoreM

liftBaseOp :: MonadBaseControl m => ((a -> Base m (StM m b)) -> Base m (StM m c)) -> (a -> m b) -> m c
liftBaseOp f = \ g -> control (\ r -> f (r . g))

liftBaseOp_ :: MonadBaseControl m => (Base m (StM m a) -> Base m (StM m b)) -> m a -> m b
liftBaseOp_ f = liftBaseOp (f . ($ ())) . pure

liftBaseDiscard :: MonadBaseControl m => (Base m () -> Base m a) -> m () -> m a
liftBaseDiscard f = \ x -> liftBaseWith (\ r -> f (() <$ r x))

liftBaseOpDiscard :: MonadBaseControl m => ((a -> Base m ()) -> Base m b) -> (a -> m ()) -> m b
liftBaseOpDiscard f = \ g -> liftBaseWith (\ r -> f ((() <$) . r . g))

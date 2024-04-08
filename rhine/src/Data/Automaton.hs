{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Automaton where

-- base
import Control.Applicative (Alternative (..), liftA2)
import Data.Monoid (Ap (..))

-- mmorph
import Control.Monad.Morph (MFunctor (hoist))

-- simple-affine-space
import Data.VectorSpace (VectorSpace (..))

-- rhine

import Control.Monad ((<$!>))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT, runExceptT, withExceptT)
import Data.Automaton.Result

data AutomatonT m b = forall s.
  AutomatonT
  { state :: s
  , step :: s -> m (Result s b)
  }

instance (Functor m) => Functor (AutomatonT m) where
  fmap f AutomatonT {state, step} = AutomatonT state $! fmap (fmap f) <$> step
  {-# INLINE fmap #-}

-- FIXME maybe I should remove this and instead one has to go via final
stepAutomaton :: (Functor m) => AutomatonT m a -> m (Result (AutomatonT m a) a)
stepAutomaton AutomatonT {state, step} = mapResultState (`AutomatonT` step) <$> step state
{-# INLINE stepAutomaton #-}

data JointState a b = JointState a b

apResult :: Result s1 (a -> b) -> Result s2 a -> Result (JointState s1 s2) b
apResult (Result resultStateA outputF) (Result resultStateB outputA) = Result (JointState resultStateA resultStateB) $! outputF outputA
{-# INLINE apResult #-}

-- FIXME Use par & pseq in places
instance (Applicative m) => Applicative (AutomatonT m) where
  pure a = AutomatonT () $ const $ pure $ Result () a
  {-# INLINE pure #-}

  AutomatonT stateF0 stepF <*> AutomatonT stateA0 stepA =
    AutomatonT (JointState stateF0 stateA0) (\(JointState stateF stateA) -> apResult <$> stepF stateF <*> stepA stateA)
  {-# INLINE (<*>) #-}

deriving via Ap (AutomatonT m) a instance (Applicative m, Num a) => Num (AutomatonT m a)

instance (Applicative m, Fractional a) => Fractional (AutomatonT m a) where
  fromRational = pure . fromRational
  recip = fmap recip

instance (Applicative m, Floating a) => Floating (AutomatonT m a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sin = fmap sin
  cos = fmap cos
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

instance (VectorSpace v s, Eq s, Floating s, Applicative m) => VectorSpace (AutomatonT m v) (AutomatonT m s) where
  zeroVector = pure zeroVector
  (*^) = liftA2 (*^)
  (^+^) = liftA2 (^+^)
  dot = liftA2 dot
  normalize = fmap normalize

instance (Alternative m) => Alternative (AutomatonT m) where
  empty = constM empty
  {-# INLINE empty #-}

  AutomatonT stateL0 stepL <|> AutomatonT stateR0 stepR =
    AutomatonT
      { state = Undecided
      , step = \case
          Undecided -> (mapResultState DecideL <$> stepL stateL0) <|> (mapResultState DecideR <$> stepR stateR0)
          DecideL stateL -> mapResultState DecideL <$> stepL stateL
          DecideR stateR -> mapResultState DecideR <$> stepR stateR
      }
  {-# INLINE (<|>) #-}

data Alternatively stateL stateR = Undecided | DecideL stateL | DecideR stateR

instance MFunctor AutomatonT where
  hoist f AutomatonT {state, step} = AutomatonT {state, step = f <$> step}
  {-# INLINE hoist #-}

mapAutomatonT :: (Functor m, Functor n) => (forall s. m (Result s a) -> n (Result s b)) -> AutomatonT m a -> AutomatonT n b
mapAutomatonT f AutomatonT {state, step} = AutomatonT state $ fmap f step
{-# INLINE mapAutomatonT #-}

constM :: (Functor m) => m a -> AutomatonT m a
constM ma = AutomatonT () $ const $ Result () <$> ma
{-# INLINE constM #-}

reactimate :: (Monad m) => AutomatonT m () -> m void
reactimate AutomatonT {state, step} = go state
  where
    go s = do
      Result s' () <- step s
      go s'
{-# INLINE reactimate #-}

concatS :: (Monad m) => AutomatonT m [a] -> AutomatonT m a
concatS AutomatonT {state, step} =
  AutomatonT
    { state = (state, [])
    , step = go
    }
  where
    go (s, []) = do
      Result s' as <- step s
      go (s', as)
    go (s, a : as) = return $ Result (s, as) a
{-# INLINE concatS #-}

-- FIXME selective should also be easy
applyExcept :: (Monad m) => AutomatonT (ExceptT (e1 -> e2) m) a -> AutomatonT (ExceptT e1 m) a -> AutomatonT (ExceptT e2 m) a
applyExcept (AutomatonT state1 step1) (AutomatonT state2 step2) =
  AutomatonT
    { state = Left state1
    , step
    }
  where
    step (Left s1) = do
      resultOrException <- lift $ runExceptT $ step1 s1
      case resultOrException of
        Right result -> return $! mapResultState Left result
        Left f -> step (Right (state2, f))
    step (Right (s2, f)) = mapResultState (Right . (,f)) <$!> withExceptT f (step2 s2)
{-# INLINE applyExcept #-}

exceptS :: (Monad m) => AutomatonT (ExceptT e m) b -> AutomatonT m (Either e b)
exceptS AutomatonT {state, step} =
  AutomatonT
    { step = \case
        Left state -> do
          throwOrResult <- runExceptT $ step state
          case throwOrResult of
            Left e -> return $ Result (Right e) (Left e)
            Right result -> return $ Right <$> mapResultState Left result
        er@(Right e) -> return $ Result er (Left e) -- FIXME a bit wasteful to create a new Left every time?
    , state = Left state
    }
{-# INLINE exceptS #-}

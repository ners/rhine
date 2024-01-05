{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Automaton.MSF.Final where

import Data.Automaton.MSF
import qualified Data.Automaton.Final as AutomatonFinal
import Control.Applicative (Alternative)
import Control.Monad.Trans.Reader
import Control.Category
import Prelude hiding (id, (.))
import Control.Arrow
import qualified Data.Automaton.Optimized as AutomatonOptimized

newtype Final m a b = Final {getFinal :: AutomatonFinal.Final (ReaderT a m) b}
  deriving newtype (Functor, Applicative, Alternative)

instance (Monad m) => Category (Final m) where
  id = toFinal id
  f1 . f2 = toFinal $ fromFinal f1 . fromFinal f2

instance (Monad m) => Arrow (Final m) where
  arr = toFinal . arr
  first = toFinal . first . fromFinal

toFinal :: (Functor m) => MSF m a b -> Final m a b
toFinal (MSF automaton) = Final $ AutomatonOptimized.toFinal automaton

fromFinal :: Final m a b -> MSF m a b
fromFinal Final {getFinal} = MSF $ AutomatonOptimized.fromFinal getFinal

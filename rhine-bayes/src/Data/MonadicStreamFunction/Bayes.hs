-- FIXME or should I indeed define everything for automata first and add MSFs separately?
{-# LANGUAGE NamedFieldPuns #-}

module Data.MonadicStreamFunction.Bayes where

-- base
import Control.Arrow

-- transformers
import Control.Monad.Trans.Reader (ReaderT (..))

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Population (PopulationT (..), fromWeightedList, runPopulationT)

-- mmorph
import Control.Monad.Morph (hoist)

-- rhine
import Data.Automaton (AutomatonT (..))
import Data.Automaton.MSF (MSF (..), handleS)
import Data.Automaton.Result (Result (..))

-- | Run the Sequential Monte Carlo algorithm continuously on an 'MSF'
runPopulationS ::
  forall m a b.
  (Monad m) =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x. PopulationT m x -> PopulationT m x) ->
  MSF (PopulationT m) a b ->
  -- FIXME Why not MSF m a (PopulationT b)
  MSF m a [(b, Log Double)]
runPopulationS nParticles resampler =
  handleS
    ( runPopulationAutomaton
        (commuteReaderPopulation . hoist resampler . commuteReaderPopulationBack)
        . hoist commuteReaderPopulation
    )
  where
    commuteReaderPopulation :: forall m r a. (Monad m) => ReaderT r (PopulationT m) a -> PopulationT (ReaderT r m) a
    commuteReaderPopulation = fromWeightedList . ReaderT . fmap runPopulationT . runReaderT

    commuteReaderPopulationBack :: forall m r a. (Monad m) => PopulationT (ReaderT r m) a -> ReaderT r (PopulationT m) a
    commuteReaderPopulationBack = ReaderT . fmap fromWeightedList . runReaderT . runPopulationT

    runPopulationAutomaton ::
      forall m b.
      (Monad m) =>
      (forall x. PopulationT m x -> PopulationT m x) ->
      AutomatonT (PopulationT m) b ->
      AutomatonT m [(b, Log Double)]
    runPopulationAutomaton resampler AutomatonT {step, state} =
      AutomatonT
        { state = replicate nParticles (state, 1 / fromIntegral nParticles)
        , step = \states -> do
            resultsAndProbabilities <- runPopulationT $ normalize $ resampler $ do
              state <- fromWeightedList $ pure states
              step state
            return $! Result (first resultState <$> resultsAndProbabilities) (first output <$> resultsAndProbabilities)
        }

-- FIXME see PR re-adding this to monad-bayes
normalize :: (Monad m) => PopulationT m a -> PopulationT m a
normalize = fromWeightedList . fmap (\particles -> second (/ (sum $ snd <$> particles)) <$> particles) . runPopulationT

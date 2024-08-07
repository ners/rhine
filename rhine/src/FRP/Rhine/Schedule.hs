{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
The 'MonadSchedule' class from the @monad-schedule@ package is the compatibility mechanism between two different clocks.
It implements a concurrency abstraction that allows the clocks to run at the same time, independently.
Several such clocks running together form composite clocks, such as 'ParallelClock' and 'SequentialClock'.
This module defines these composite clocks,
and utilities to work with them.
-}
module FRP.Rhine.Schedule where

-- base
import Control.Arrow
import Data.List.NonEmpty as N

-- transformers
import Control.Monad.Trans.Reader

-- monad-schedule
import Control.Monad.Schedule.Class

-- automaton
import Data.Automaton
import Data.Automaton.Recursive (getRecursive, toRecursive)
import Data.Stream
import Data.Stream.Optimized (OptimizedStreamT (..), toStreamT)
import Data.Stream.Recursive qualified as StreamRecursive
import Data.Stream.Result

-- rhine
import FRP.Rhine.Clock

-- * Scheduling

{- | Run several automata concurrently.

Whenever one automaton outputs a value,
it is returned together with all other values that happen to be output at the same time.
-}
scheduleList :: (Monad m, MonadSchedule m) => NonEmpty (Automaton m a b) -> Automaton m a (NonEmpty b)
scheduleList automatons0 =
  Automaton $
    Stateful $
      StreamT
        { state = (getRecursive . toRecursive <$> automatons0, [])
        , step = \(automatons, running) -> ReaderT $ \a -> do
            let bsAndConts = flip (runReaderT . StreamRecursive.getRecursive) a <$> automatons
            (done, running') <- schedule (N.head bsAndConts :| N.tail bsAndConts ++ running)
            return $ Result (resultState <$> done, running') $ output <$> done
        }

{- | Run two automata concurrently.

Whenever one automaton returns a value, it is returned.

This is similar to 'scheduleList', but more efficient.
-}
schedulePair :: (Monad m, MonadSchedule m) => Automaton m a b -> Automaton m a b -> Automaton m a b
schedulePair (Automaton automatonL) (Automaton automatonR) = Automaton $! Stateful $! scheduleStreams (toStreamT automatonL) (toStreamT automatonR)
  where
    scheduleStreams :: (Monad m, MonadSchedule m) => StreamT m b -> StreamT m b -> StreamT m b
    scheduleStreams (StreamT stateL0 stepL) (StreamT stateR0 stepR) =
      StreamT
        { state = (stepL stateL0, stepR stateR0)
        , step
        }
      where
        step (runningL, runningR) = do
          result <- race runningL runningR
          case result of
            Left (Result stateL' b, runningR') -> return $ Result (stepL stateL', runningR') b
            Right (runningL', Result stateR' b) -> return $ Result (runningL', stepR stateR') b

-- | Run two running clocks concurrently.
runningSchedule ::
  ( Monad m
  , MonadSchedule m
  , Clock m cl1
  , Clock m cl2
  , Time cl1 ~ Time cl2
  ) =>
  cl1 ->
  cl2 ->
  RunningClock m (Time cl1) (Tag cl1) ->
  RunningClock m (Time cl2) (Tag cl2) ->
  RunningClock m (Time cl1) (Either (Tag cl1) (Tag cl2))
runningSchedule _ _ rc1 rc2 = schedulePair (rc1 >>> arr (second Left)) (rc2 >>> arr (second Right))

{- | A schedule implements a combination of two clocks.
   It outputs a time stamp and an 'Either' value,
   which specifies which of the two subclocks has ticked.
-}
initSchedule ::
  ( Time cl1 ~ Time cl2
  , Monad m
  , MonadSchedule m
  , Clock m cl1
  , Clock m cl2
  ) =>
  cl1 ->
  cl2 ->
  RunningClockInit m (Time cl1) (Either (Tag cl1) (Tag cl2))
initSchedule cl1 cl2 = do
  (runningClock1, initTime) <- initClock cl1
  (runningClock2, _) <- initClock cl2
  return
    ( runningSchedule cl1 cl2 runningClock1 runningClock2
    , initTime
    )

-- * Composite clocks

-- ** Sequentially combined clocks

{- | Two clocks can be combined with a schedule as a clock
   for an asynchronous sequential composition of signal networks.
-}
data SequentialClock cl1 cl2 = (Time cl1 ~ Time cl2) =>
  SequentialClock
  { sequentialCl1 :: cl1
  , sequentialCl2 :: cl2
  }

-- | Abbrevation synonym.
type SeqClock cl1 cl2 = SequentialClock cl1 cl2

instance
  (Monad m, MonadSchedule m, Clock m cl1, Clock m cl2) =>
  Clock m (SequentialClock cl1 cl2)
  where
  type Time (SequentialClock cl1 cl2) = Time cl1
  type Tag (SequentialClock cl1 cl2) = Either (Tag cl1) (Tag cl2)
  initClock SequentialClock {..} =
    initSchedule sequentialCl1 sequentialCl2

-- ** Parallelly combined clocks

{- | Two clocks can be combined with a schedule as a clock
   for an asynchronous parallel composition of signal networks.
-}
data ParallelClock cl1 cl2 = (Time cl1 ~ Time cl2) =>
  ParallelClock
  { parallelCl1 :: cl1
  , parallelCl2 :: cl2
  }

-- | Abbrevation synonym.
type ParClock cl1 cl2 = ParallelClock cl1 cl2

instance
  (Monad m, MonadSchedule m, Clock m cl1, Clock m cl2) =>
  Clock m (ParallelClock cl1 cl2)
  where
  type Time (ParallelClock cl1 cl2) = Time cl1
  type Tag (ParallelClock cl1 cl2) = Either (Tag cl1) (Tag cl2)
  initClock ParallelClock {..} =
    initSchedule parallelCl1 parallelCl2

-- * Navigating the clock tree

-- | The clock that represents the rate at which data enters the system.
type family In cl where
  In (SequentialClock cl1 cl2) = In cl1
  In (ParallelClock cl1 cl2) = ParallelClock (In cl1) (In cl2)
  In cl = cl

-- | The clock that represents the rate at which data leaves the system.
type family Out cl where
  Out (SequentialClock cl1 cl2) = Out cl2
  Out (ParallelClock cl1 cl2) = ParallelClock (Out cl1) (Out cl2)
  Out cl = cl

{- | A tree representing possible last times to which
   the constituents of a clock may have ticked.
-}
data LastTime cl where
  SequentialLastTime ::
    LastTime cl1 ->
    LastTime cl2 ->
    LastTime (SequentialClock cl1 cl2)
  ParallelLastTime ::
    LastTime cl1 ->
    LastTime cl2 ->
    LastTime (ParallelClock cl1 cl2)
  LeafLastTime :: Time cl -> LastTime cl

-- | An inclusion of a clock into a tree of parallel compositions of clocks.
data ParClockInclusion clS cl where
  ParClockInL ::
    ParClockInclusion (ParallelClock clL clR) cl ->
    ParClockInclusion clL cl
  ParClockInR ::
    ParClockInclusion (ParallelClock clL clR) cl ->
    ParClockInclusion clR cl
  ParClockRefl :: ParClockInclusion cl cl

{- | Generates a tag for the composite clock from a tag of a leaf clock,
   given a parallel clock inclusion.
-}
parClockTagInclusion :: ParClockInclusion clS cl -> Tag clS -> Tag cl
parClockTagInclusion (ParClockInL parClockInL) tag = parClockTagInclusion parClockInL $ Left tag
parClockTagInclusion (ParClockInR parClockInR) tag = parClockTagInclusion parClockInR $ Right tag
parClockTagInclusion ParClockRefl tag = tag

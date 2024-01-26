{-# LANGUAGE RecordWildCards #-}

{- |
Collect and process all incoming values statefully and with time stamps.
-}
module FRP.Rhine.ResamplingBuffer.ClSF where

-- transformers
import Control.Monad.Trans.Reader (runReaderT)

-- dunai
import Data.MonadicStreamFunction.InternalCore (unMSF)

-- rhine
import FRP.Rhine.ClSF.Core
import FRP.Rhine.ResamplingBuffer

{- | Given a clocked signal function that accepts
   a varying number of timestamped inputs (a list),
   a `ResamplingBuffer` can be formed
   that collects all this input and steps the signal function
   whenever output is requested.
-}
clsfBuffer ::
  (Monad m) =>
  -- | The clocked signal function that consumes
  --   and a list of timestamped inputs,
  --   and outputs a single value.
  --   The list will contain the /newest/ element in the head.
  ClSF m cl2 [(TimeInfo cl1, a)] b ->
  ResamplingBuffer m cl1 cl2 a b
clsfBuffer = clsfBuffer' []
  where
    clsfBuffer' ::
      (Monad m) =>
      [(TimeInfo cl1, a)] ->
      ClSF m cl2 [(TimeInfo cl1, a)] b ->
      ResamplingBuffer m cl1 cl2 a b
    clsfBuffer' as msf = ResamplingBuffer {..}
      where
        put ti1 a = return $ clsfBuffer' ((ti1, a) : as) msf
        get ti2 = do
          (b, msf') <- runReaderT (unMSF msf as) ti2
          return (b, clsfBuffer msf')

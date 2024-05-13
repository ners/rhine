module Stream where

-- base
import Control.Monad.Identity (Identity (..))

-- selective
import Control.Selective

-- tasty
import Test.Tasty (testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (testCase, (@?=))

-- automaton
import Automaton
import Data.Stream (streamToList, unfold)
import Data.Stream.Result

tests =
  testGroup
    "Stream"
    [ Automaton.tests
    , testGroup
        "Selective"
        [ testCase "Selects second stream based on first stream" $
            let automaton1 = unfold 0 (\n -> Result (n + 1) (if even n then Right n else Left n))
                automaton2 = pure (* 10)
             in take 10 (runIdentity (streamToList (automaton1 <*? automaton2))) @?= [0, 10, 2, 30, 4, 50, 6, 70, 8, 90]
        ]
    ]

{-# LANGUAGE ScopedTypeVariables #-}

module Automaton.MSF where

-- base
import Control.Applicative (Alternative (..))
import Data.Functor.Identity (runIdentity)

-- tasty
import Test.Tasty (testGroup)

-- tasty-quickcheck
import Test.Tasty.QuickCheck

-- rhine
import Data.Automaton.MSF.Final
import Data.Automaton.MSF.Trans.Maybe
import FRP.Rhine

tests =
  testGroup
    "Automaton.MSF"
    [ testGroup
        "Alternative"
        [ testGroup
            "<|>"
            [ testProperty "has same semantics as final" $
                \(input :: [(Maybe Int, Maybe Int)]) ->
                  embed ((arr fst >>> inMaybe) <|> (arr snd >>> inMaybe)) input
                    === embed (fromFinal $ (arr fst >>> toFinal inMaybe) <|> (arr snd >>> toFinal inMaybe)) input
            ]
        , testGroup
            "some"
            [ testProperty "has same semantics as final" $
                \(input :: [Maybe Int]) ->
                  embed (some inMaybe) input
                    === embed (fromFinal $ some $ toFinal inMaybe) input
            ]
        ]
    ]

inMaybe :: MSF Maybe (Maybe a) a
inMaybe = hoistS (runIdentity . runMaybeT) inMaybeT

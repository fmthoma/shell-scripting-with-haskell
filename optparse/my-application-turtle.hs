#!/usr/bin/env stack
-- stack runhaskell --resolver=lts-8.0 --package=turtle

{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Turtle

data Options = Options
    { foo :: Bool
    , bar :: Maybe Text
    , baz :: Text }
    deriving (Show)

optionsParser :: Parser Options
optionsParser = liftA3 Options
    (switch "foo" 'f' "To foo or not to foo")
    (optional (optText "bar" 'b' "A bar option"))
    (argText "BAZ" "Some baz input")

main :: IO ()
main = do
    opt <- options "Parse some options" optionsParser
    print opt

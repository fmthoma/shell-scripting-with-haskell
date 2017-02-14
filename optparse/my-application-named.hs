#!/usr/bin/env stack
-- stack runhaskell --resolver=lts-7.19 --package=optparse-generic

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Options.Generic

data Options
    = Options { foo :: Maybe Text, bar :: Maybe Int, baz :: Bool }
    deriving (Show, Generic)

instance ParseRecord Options

main = do
    command <- getRecord "My Application" :: IO Options
    print command

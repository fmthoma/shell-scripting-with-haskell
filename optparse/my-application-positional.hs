#!/usr/bin/env stack
-- stack runhaskell --resolver=lts-7.19 --package=optparse-generic

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Options.Generic

data Positional = Positional Text Int (Maybe Text)
    deriving (Show, Generic)

instance ParseRecord Positional

main = do
    command <- getRecord "My Application" :: IO Positional
    print command

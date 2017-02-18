#!/usr/bin/env stack
-- stack runhaskell --resolver=lts-8.0 --package=optparse-generic

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Options.Generic

data Command
    = Foo
    | Bar Text
    | Baz { option :: Bool }
    deriving (Show, Generic)

instance ParseRecord Command

main = do
    command <- getRecord "My Application" :: IO Command
    print command

#!/usr/bin/env stack
-- stack runhaskell --resolver=lts-7.19 --package=optparse-generic

{-# LANGUAGE DataKinds, DeriveGeneric, OverloadedStrings, TypeOperators #-}

import Options.Generic

data Command
    = Foo
    | Bar (Text       <?> "Enter text here.")
          (Maybe Text <?> "Some more text, if you want")
    | Baz { switch :: Bool <?> "Turn it on!"
          , volume :: Int  <?> "Goes to eleven!" }
    deriving (Show, Generic)

instance ParseRecord Command

main = do
    command <- getRecord "My Application" :: IO Command
    print command

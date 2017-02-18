#!/usr/bin/env stack
-- stack runhaskell --resolver=lts-8.0 --package=turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main = do
    command <- options "My Application" (pure ())
    print command

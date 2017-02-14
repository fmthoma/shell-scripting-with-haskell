#!/usr/bin/env stack
-- stack runhaskell --resolver=lts-7.19 --package=turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main = do
    command <- options "My Application" (pure ())
    print command

#!/usr/bin/env stack
-- stack runhaskell --resolver=lts-7.19 --package=turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Data.Text as Text

main = sh $ do
  file <- ls "."
  echo ""
  echo (Text.replicate 80 "-")
  liftIO (print file)

  isFile <- liftIO (testfile file)
  guard isFile
  line <- input file
  echo line

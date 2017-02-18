#!/usr/bin/env stack
-- stack runhaskell --resolver=lts-8.0 --package=turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main = sh $ do
  file <- ls "."

  echo ""
  echo "--------------------------------------------------------------------------------"
  printf fp file

  isFile <- liftIO (testfile file)
  guard isFile
  line <- input file
  echo line

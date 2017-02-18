#!/usr/bin/env stack
-- stack runhaskell --resolver=lts-8.0 --package=optparse-applicative

{-# LANGUAGE LambdaCase #-}
import           Control.Applicative
import qualified Options.Applicative as Opt

data Options = Options
    { name   :: String
    , switch :: Bool
    , volume :: Maybe Int }
    deriving (Show)

optionsParser :: Opt.Parser Options
optionsParser = Opt.helper
    <*> liftA3 Options nameParser switchParser volumeParser
  where nameParser = Opt.strArgument $ mconcat
            [ Opt.metavar "NAME"
            , Opt.help "Your name here." ]
        switchParser = Opt.switch $ mconcat
            [ Opt.long "switch"
            , Opt.short 's'
            , Opt.help "Turn it on!" ]
        volumeParser = optional . Opt.option Opt.auto $ mconcat
            [ Opt.long "volume"
            , Opt.short 'v'
            , Opt.metavar "VOL"
            , Opt.help "Goes to eleven!" ]

getOptions :: String -> Opt.Parser a -> IO a
getOptions description parser
    = Opt.execParser (Opt.info parser (Opt.header description))

main = getOptions "My application" optionsParser >>= \case
    Options name False _
        -> putStrLn (name ++ " turns his amps off.")
    Options name True Nothing
        -> putStrLn (name ++ " turns his amps on.")
    Options name True (Just vol)
        -> putStrLn (name ++ " turns his amps to " ++ show vol ++ "!")

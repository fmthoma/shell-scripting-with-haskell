Shell scripting with Haskell
============================

Slides: [Shell scripting with Haskell.pdf](Shell scripting with Haskell.pdf)


Shell scripting with high-level languages
=========================================

## Why use a high-level language for scripting

A simple script is easily written in your favourite shell's language. As scripts
tend to grow larger, eventually becoming small command line applications,
advantages of higher-level languages pay off compared to bare shell scripts.

* **Abstraction**: Support for data structures, types and encapsulation helps
allow cleaner semantics.
* **Flexibility**: High-level languages provide a rich set of both high-level
and low-level libraries.
* **Scalability**: Module systems keep growing applications organized.
* **Robustness**: All of these make refactoring easier and applications more
resilient.

## Why use a statically typed language for scripting

* Dynamically typed languages are pretty popular in the scripting world as they
are easy to hack away with.
* However, they share a number of problems with bare shell scripts:
As scripts grow larger, the initial flexibility now makes the application
increasingly harder to reason about.
* Statically typed programs are easy to refactor and extend

## Why use Haskell for scripting

* Concise syntax, virtually no boilerplate
* Good library support, e.g. command line option parsers, `ncurses` bindings
* Can be interpreted using `runhaskell` or `stack runhaskell`


The `turtle` library
====================

## The `turtle` library

* `turtle` is an implementation of the UNIX command line environment in Haskell.
* The idea is to provide a set of recognizeable functions for accessing the file
system, streaming data, and job control.

Its purpose is mainly educational, but it is surprisingly handy when it comes to
writing small shell-like scripts.

## Demo

Start a `ghci` session (`stack ghci`) in the project directory and try some
Shell commands. Stack makes sure that the dependencies are built (might take a
while the first time), the `.ghci` file makes sure that the relevant modules are
imported. The `-XOverloadedStrings` language extension is enabled to make use of
`Text` and `FilePath` easier.

```haskell
:set -XOverloadedStrings
import Turtle
import qualified Data.Text as Text
import qualified Filesystem.Path.CurrentOS as Path
```


```haskell
projectDir <- pwd
print projectDir
cd =<< home
pwd
cd projectDir
pwd
view (ls ".")
let vi file = proc "vi" [file] empty
vi "README.md"
vi ".ghci"
```

## Shell commands and their types

Turtle exposes some default shell commands:

* `echo :: Line -> IO ()`
* `cd   :: FilePath -> IO ()`
* `mv   :: FilePath -> FilePath -> IO ()`
* `cp   :: FilePath -> FilePath -> IO ()`
* `rm   :: FilePath -> IO ()`
* `pwd  :: IO FilePath`

For a complete reference see the [turtle manual](https://hackage.haskell.org/package/turtle-1.2.8/docs/Turtle-Prelude.html).

## Building your own commands

The `proc` function allows calling external commands:

```haskell
proc :: Text        -- Command
     -> [Text]      -- Arguments
     -> Shell Line  -- Lines of standard input
     -> IO ExitCode
```

Example:

```haskell
vi :: FilePath -> IO ExitCode
vi file = proc "vi" [format fp file] empty
```

## Shell streams

What about piping standard output to `less`?

```haskell
less :: Shell Line -> IO ExitCode
less txt = proc "less" [] txt
```

## The `Shell` type

The above commands all had side-effects, but did not have any standard
input/output. Besides browsing and manipulating the file system, the main use of
shell scripting is to stream and manipulate lines of text using pipes.
In `turtle` this is the realm of the `Shell` type.

`Shell a` is a stream of items of type `a`, with the possibility to execute `IO`
actions.

* `stdin  :: Shell Line`
* `input  :: FilePath -> Shell Line`
* `yes    :: Shell Line`
* `select :: [a] -> Shell a`
* `ls     :: FilePath -> Shell FilePath`
* `cat    :: [Shell a] -> Shell a`
* `view   :: Show a => Shell a -> IO ()`

## `Shell` composition

Function application/composition can be used to compose shell actions: `(.)` and
`($)` act like unix pipes (but backwards):

```haskell
less' :: FilePath -> IO ExitCode
less' = less . input
-- »cat <file> | less«
```

The bind operator `(>>=)` is the equivalent to `xargs`:

```haskell
dircat :: FilePath -> Shell Line
dircat dir = ls dir >>= input
-- »ls <dir> | xargs cat«
```

A small demo can be found in
[`shell/print-all-files.hs`](shell/print-all-files.hs).


Scripts & Dependency Management
===============================

## `runhaskell`

GHC has a script interpreter that can be used in a shebang line:

```
#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main = echo "Hello, World"
```

However, this fails unless `turtle` is installed globally in the user
environment.

## `stack runhaskell`

Stack has a remedy for the dependency problem:

```
#!/usr/bin/env stack
-- stack runhaskell --resolver=lts-8.0 --package=turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main = echo "Hello, World"
```

All examples in this repo use the Stack script interpreter.

Upcoming versions of `stack` will feature a `stack script` command that will
even improve on encapsulation and reproducibility of standalone scripts.


Parsing command line options
============================

## Auto-generated CLIs

Even the most trivial command line applications should have a `--help` option
providing a short description of the application and its usage:

```
> optparse/my-application.hs --help
My Application

Usage: my-application.hs

Available options:
  -h,--help                Show this help text
```

Turtle can generate this CLI for us:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Turtle

main = do
    command <- options "My Application" (pure ())
    print command
```

([Source](optparse/my-application.hs))

## Parameters and options

`turtle` provides an API for parsing parameters and options:

```haskell
data Options = Options
    { foo :: Bool
    , bar :: Maybe Text
    , baz :: Text }
    deriving (Show)

optionsParser :: Parser Options
optionsParser = liftA3 Options
    (switch "foo" 'f' "To foo or not to foo")
    (optional (optText "bar" 'b' "A bar option"))
    (argText "BAZ" "Some baz args")
```

([Source](optparse/my-application-turtle.hs))

```
> optparse/my-application-turtle.hs --help
Parse some options

Usage: my-application-turtle.hs [-f|--foo] [-b|--bar BAR] BAZ

Available options:
  -h,--help                Show this help text
  -f,--foo                 To foo or not to foo
  -b,--bar BAR             A bar option
  BAZ                      Some baz args
```

## Simple CLIs

Sometimes only one or two simple parameters need to be passed. The
[`optparse-generic`](https://hackage.haskell.org/package/optparse-generic-1.1.0)
library requires even less boilerplate to generate a CLI.

```haskell
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Options.Generic

data Positional = Positional Text Int (Maybe Text)
    deriving (Show, Generic)

instance ParseRecord Positional

main = do
    command <- getRecord "My Application" :: IO Positional
    print command
```

([Source](optparse/my-application-positional.hs))

```
> optparse/my-application-positional.hs --help
My Application

Usage: my-application-positional.hs TEXT INT [TEXT]

Available options:
  -h,--help                Show this help text
```

`optparse-generic` uses GHC's Generics feature to auto-generate the help text
and even the command line argument parser. All you need is to enable the
`-XDeriveGenerics` language extension, derive a `Generic` and declare a
`ParseRecord` instance for your application configuration type.

The library most trivially supports positional parameters: Product types are
parsed each argument in order. A `Maybe` argument becomes an optional positional
argument. If a given argument cannot be parsed with the expected type, a useful
hint along with the usage description is printed.

Named parameters are also supported: Just use a record instead of an
ordinary product type, and the record fields become named parameters. `Bool`ean
record fields are automatically converted to switches, i.e named parameters
without arguments. `Maybe` parameters are optional again.

Product types are converted to subcommands, the (lowercased) constructors are
used for the command names. Of course each constructor can again take parameters
(positional arguments), or have record fields (named options).

Subcommands have their own `--help` option that prints a usage summary.

## `bash` auto-completion

... is provided out of the box:

```
source <( my-application --bash-completion-script $(which my-application) )
```

`zsh` is not supported (your chance to contribute!), but luckily `bash`
auto-completion compatibility can be enabled:

```
autoload -U bashcompinit
bashcompinit
source <( my-application --bash-completion-script $(which my-application) )
```


To globally install the completion script, copy it to the bash-completion
directory:

```
my-application --bash-completion-script $(which my-application) \
    > /etc/bash_completion.d/my-application
```


A small application
===================

## Demo

[`brick/select-file.hs`](brick/select-file.hs)

[select-file](brick/select-file.hs) is a small demo program that lists files in
a directory in a menu and prints the selected file to stdout.


Conclusion
==========

## Conclusion

Haskell has a rich ecosystem for scripting and small CLI applications:

* `turtle` for shell-like file-system access, external processes, and streaming
* `optparse-applicative` for declarative command line option parsing
* `brick` (and `vty`) as a lightweight `ncurses` textual interface
* `stack` with `stack runhaskell` for ad-hoc dependency management

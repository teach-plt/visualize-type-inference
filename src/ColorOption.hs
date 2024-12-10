{-# LANGUAGE DeriveDataTypeable #-}

{- Originally written by chatgpt.com 2024-12-10 from the following prompt:

> Please write me an options parser in Haskell using the optparse-applicative package that can parse the option given as follows:

> ... from https://www.gnu.org/software/gettext/manual/html_node/The-_002d_002dcolor-option.html

Cut down to my purposes.

-}

module ColorOption where

import Options.Applicative
  ( Parser, ReadM, eitherReader, help, long, metavar, option, value
  )

-- | Values of the GNU option @--color@.

data ColorOption = Always | Never | Auto
  deriving (Eq)

-- | Parser for the option @--color@.

colorOption :: Parser ColorOption
colorOption =
  option readColorOption
    $  long "color"
    <> metavar "when"
    <> value Auto
    <> help "Specify when colorized output should be generated. Options: always, never, auto. Default: auto."

-- | Read string input into 'ColorOption'.

readColorOption :: ReadM ColorOption
readColorOption = eitherReader $ \case
  "always" -> Right Always
  "yes"    -> Right Always
  "never"  -> Right Never
  "no"     -> Right Never
  "auto"   -> Right Auto
  "tty"    -> Right Auto
  invalid  -> Left $ "Invalid color option: " ++ invalid

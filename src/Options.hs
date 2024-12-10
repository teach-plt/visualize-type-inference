module Options where

import Data.List
  ( intercalate )
import Data.Version
  ( Version(versionBranch) )

import Options.Applicative
  ( Parser, (<**>)
  , action, execParser, footerDoc, headerDoc, help, helper, hidden
  , info, infoOption, long, metavar, optional, short, strArgument, switch, value
  )
import Options.Applicative.Help.Pretty
  ( pretty, nest, vcat )

import System.IO
  ( stdout )
import System.Console.ANSI
  ( hSupportsANSI )

import License
import qualified Paths_visualize_type_inference as Paths

-- | Program options.

data Options = Options
  { optBatch      :: Bool
      -- ^ Run in batch mode (rather than interactively step-by-step).
  , optJ          :: Bool
      -- ^ Algorithm J (immediate substitution) rather than C (constraint collection).
  , optNoColors   :: Bool
      -- ^ Turn off colors.
  , optFile       :: Maybe FilePath
      -- ^ The file with the input (optional).
  } deriving Show


self :: String
self = "visualize-type-inference"

homepage :: String
homepage = concat [ "https://github.com/teach-plt/", self ]

-- | The program version obtained from the cabal file.

version :: String
version = intercalate "." $ map show $ versionBranch Paths.version

-- | Option parsing and handling

options :: IO Options
options = do
  opts <- execParser $ info parser infoMod
  hSupportsANSI stdout >>= \case
    True  -> return opts
    False -> return opts{ optNoColors = True }
  where
  parser = programOptions <**>
    (versionOption <*> numericVersionOption <*> licenseOption <*> helper)
  infoMod = headerDoc header <> footerDoc footer

  versionOption =
    infoOption versionLong
      $  long "version"
      <> short 'V'
      <> help "Show version info."
      <> hidden
  versionText = unwords [ self, "version", version ]
  versionLong = intercalate "\n" $
    [ versionText
    , copyright
    , "This is free software under the BSD-3-clause license."
    ]

  numericVersionOption =
    infoOption version
      $  long "numeric-version"
      <> help "Show just version number."
      <> hidden
      -- Newline at the end:
      -- <> helpDoc (Just $ text "Show just version number." <$$> text "")

  licenseOption =
    infoOption license
      $  long "license"
      -- <> long "licence"
      <> help "Show the license text."
      <> hidden

  -- Obs: match the order with Options.Options!
  programOptions = pure Options
    <*> oBatch
    <*> oJ
    <*> oNoColors
    <*> oFile

  oBatch =
    switch
      $  long "batch"
      <> short 'b'
      <> help "Run in batch mode (rather than interactively step-by-step)."

  oJ =
    switch
      $  short 'J'
      <> help "Algorithm J: solve and substitute eagerly."

  -- oVerbose =
  --   switch
  --     $  long "verbose"
  --     <> short 'v'
  --     <> help "Comment on what is happening."

  oNoColors =
    switch
      $  long "no-colors"
      <> help "Disable colorized output.  Automatic if terminal does not support colors."

  oFile :: Parser (Maybe FilePath)
  oFile = optional $
    strArgument
      $  metavar "FILE"
      <> action "file"
      <> help "The text file containing the lambda-term for type inference."

  -- Note: @header@ does not respect line breaks, so we need @headerDoc@.
  header = Just $ vcat
    [ pretty $ unwords [ versionText, homepage ]
    , ""
    , "Visualize type inference for the simply-typed lambda-calculus"
    , "based on type meta variables and unification."
    ]
  footer = Just $ vcat
    [ "By default, constraints are collected first and then solved one-by-one."
    , "With option -J, each new constraint is solved right away."
    , ""
    , "Type inference is performed on a single term in Haskell syntax read from FILE."
    , "If no FILE is given, the term is read from standard input and processed in batch mode."
    , ""
    , "Example terms:"
    , nest 2 $ "\\ f -> \\ g -> \\ x -> f (g x)"
    , nest 2 $ "λ x → x x"
    ]

{-# LANGUAGE GADTs #-}

module Cli where

import Data.Version (showVersion)
import Options.Applicative
import Paths_haskboard (version)

data CliOptions where
  CliOptions :: {project :: Maybe String} -> CliOptions

cliParser :: Parser CliOptions
cliParser =
  CliOptions
    <$> optional (argument str (metavar "PROJECT"))

versionOption :: Parser (a -> a)
versionOption = infoOption ("haskboard " ++ showVersion version) (long "version" <> short 'v' <> help "Show version")

parseToCli :: IO CliOptions
parseToCli = execParser opts
  where
    opts =
      info
        (cliParser <**> helper <**> versionOption)
        ( fullDesc
            <> header "Haskboard - A terminal-based Kanban Task Manager"
        )

module Main where

import Options.Applicative
import System.FilePath
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Sud.Rdg as S

data Params = Params { fname :: FilePath, verbose :: Bool }

mkparams :: Parser Params
mkparams = Params 
    <$> strArgument(metavar "File" <> help "CSV file name")
    <*> switch ( long "verbose output" <> short 'q')

cmdLineParser :: IO Params
cmdLineParser = execParser opts
    where
        opts = info (mkparams <**> helper)
                    (fullDesc <> progDesc "Sudoku solver by Willem Melis")

work :: Params -> IO ()
work params = do
    csvData <- TIO.readFile $ fname params
    pure ()

main :: IO ()
main = cmdLineParser >>= work

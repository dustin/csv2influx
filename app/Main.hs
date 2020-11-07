module Main where

import qualified Data.ByteString.Lazy as BL
import           Options.Applicative  (Parser, argument, eitherReader, execParser, fullDesc, help, helper, info, long,
                                       metavar, option, progDesc, short, showDefault, some, str, strOption, value,
                                       (<**>))

import           CSV2Influx


options :: Parser Options
options = Options
  <$> strOption (long "measurement" <> short 'm' <> showDefault <> value "measurement"
                 <> help "measurement column name")
  <*> option commaParser (long "tags" <> short 't' <> value [] <> help "tags column names (comma separated)")
  <*> option commaParser (long "fields" <> short 'f' <> value [] <> help "field column names (comma separated)")
  <*> strOption (long "time" <> showDefault <> value "time" <> help "timestamp column name")
  <*> some (argument str (metavar "FILES..."))

  where
    commaParser = eitherReader split

main :: IO ()
main = mapM_ BL.putStr =<< run =<< execParser opts
  where opts = info (options <**> helper)
          (fullDesc <> progDesc "Convert CSV to influxdb wire protocol")

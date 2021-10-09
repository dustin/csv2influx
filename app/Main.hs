module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty   as NE
import           Options.Applicative  (Parser, argument, eitherReader, execParser, fullDesc, help, helper, info, long,
                                       metavar, option, progDesc, short, showDefault, some, str, strOption, value,
                                       (<**>))

import           CSV2Influx


options :: Parser Options
options = Options
  <$> strOption (long "measurement" <> short 'm' <> showDefault <> value "measurement"
                 <> help "measurement column name")
  <*> option commaParser (long "tags" <> short 't' <> value [] <> help "tags column names (comma separated)")
  <*> option commaParser' (long "fields" <> short 'f' <> help "field column names (comma separated)")
  <*> option commaParser (long "intfields" <> short 'i' <> help "fields whose values are type int")
  <*> strOption (long "time" <> showDefault <> value "time" <> help "timestamp column name")
  <*> some (argument str (metavar "FILES..."))

  where
    commaParser = eitherReader split
    commaParser' = eitherReader (fmap NE.fromList <$> split)

main :: IO ()
main = mapM_ BL.putStr =<< run =<< execParser opts
  where opts = info (options <**> helper)
          (fullDesc <> progDesc "Convert CSV to influxdb wire protocol")

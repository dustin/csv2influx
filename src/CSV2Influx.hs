module CSV2Influx where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Csv              as CSV
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Data.Vector           as V

data Row = Row
  { measurement :: !Text
  , tags        :: ![Text]
  , fields      :: ![Text]
  , timestamp   :: !Text
  } deriving Show

data Options = Options
  { optMeasurement :: BC.ByteString
  , optTags        :: [BC.ByteString]
  , optFields      :: [BC.ByteString]
  , optTimestamp   :: BC.ByteString
  , optInputs      :: [FilePath]
  } deriving Show

-- use the csv library to parse csv parameters.  LOL
split :: String -> Either String [BC.ByteString]
split = fmap (head . V.toList) . CSV.decode CSV.NoHeader . BL.fromStrict . BC.pack

run :: Options -> IO [BL.ByteString]
run Options{..} = traverse doOne optInputs
  where
    doOne fp = CSV.decodeByNameWithP parser CSV.defaultDecodeOptions <$> BL.readFile fp >>= \case
      Right (_, csvd) -> pure $ foldMap (BL.fromStrict . TE.encodeUtf8 . toProtocol) csvd
      Left x -> fail x

    parser h = Row
               <$> CSV.lookup h optMeasurement
               <*> traverse (CSV.lookup h) optTags
               <*> traverse (CSV.lookup h) optFields
               <*> CSV.lookup h optTimestamp

    toProtocol Row{..} = mconcat [ join (measurement:mtags), " ", join mfields, " ", timestamp, "\n"]
      where
        mtags = zipe optTags tags
        mfields = zipe optFields fields
        join = T.intercalate ","
        zipe ks = zipWith (\k v -> (T.pack . BC.unpack) k <> "=" <> v) ks

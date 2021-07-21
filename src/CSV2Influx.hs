module CSV2Influx where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Csv              as CSV
import           Data.Foldable         (toList, fold)
import           Data.List.NonEmpty    (NonEmpty(..))
import qualified Data.List.NonEmpty    as NE
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE

data Row = Row
  { measurement :: !Text
  , tags        :: ![Text]
  , fields      :: !(NonEmpty Text)
  , timestamp   :: !Text
  } deriving Show

data Options = Options
  { optMeasurement :: BC.ByteString
  , optTags        :: [BC.ByteString]
  , optFields      :: NonEmpty BC.ByteString
  , optTimestamp   :: BC.ByteString
  , optInputs      :: [FilePath]
  } deriving Show

-- use the csv library to parse csv parameters.  LOL
split :: String -> Either String [BC.ByteString]
split = fmap (head . toList) . CSV.decode CSV.NoHeader . BL.fromStrict . BC.pack

run :: Options -> IO [BL.ByteString]
run Options{..} = traverse doOne optInputs
  where
    doOne fp = either fail (pure . foldMap (BL.fromStrict . TE.encodeUtf8 . toProtocol) . snd)
               . CSV.decodeByNameWithP parser CSV.defaultDecodeOptions =<< BL.readFile fp

    parser h = Row
               <$> CSV.lookup h optMeasurement
               <*> traverse (CSV.lookup h) optTags
               <*> traverse (CSV.lookup h) optFields
               <*> CSV.lookup h optTimestamp

    toProtocol Row{..} = fold [join (measurement:|mtags), " ", join mfields, " ", timestamp, "\n"]
      where
        mtags = zipWith zf optTags tags
        mfields = NE.zipWith zf optFields fields
        join = T.intercalate "," . toList
        zf k v = (T.pack . BC.unpack) k <> "=" <> v

import qualified Data.ByteString.Char8 as BC
import           Data.Char
import           Data.Coerce           (coerce)
import           Data.Foldable         (fold)
import           Data.List             (intercalate)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.QuickCheck

import           CSV2Influx

newtype PlausibleFieldName = PlausibleFieldName String deriving (Show, Eq)

instance Arbitrary PlausibleFieldName where
  arbitrary = do
    n <- choose (1, 11)
    PlausibleFieldName <$> vectorOf n chars

      where chars = choose (minBound, '~') `suchThat` (\c -> isAscii c && isAlphaNum c)

-- Split is really limited because the input is a string from the CLI,
-- but the output is an 8-bit bytestring, so we only look at ascii
-- chars for this test (and only in non-empty inputs).  It might be
-- nice to make this a little less strict in inputs, but not if it
-- violates this.
prop_Splits :: NonEmptyList PlausibleFieldName -> Property
prop_Splits (NonEmpty pfns) = Right joins === pjoins
  where
    joins = intercalate "," (coerce pfns)
    pjoins = intercalate "," . fmap BC.unpack <$> split joins

tests :: [TestTree]
tests = [
  goldenVsStringDiff "some freezers" diff "test/freezers.out"
    (dofiles base),

  goldenVsStringDiff "some freezers without tags" diff "test/freezers-tags.out"
    (dofiles $ base { optTags = [] }),

  goldenVsStringDiff "some freezers with more fields" diff "test/freezers+fields.out"
    (dofiles $ base { optFields = ["temperature", "sensor"] }),

  goldenVsStringDiff "some fridges" diff "test/fridges.out"
    (dofiles $ base { optInputs = ["test/two.csv"] }),

  goldenVsStringDiff "freezers and fridges" diff "test/fandf.out"
    (dofiles $ base { optInputs = ["test/one.csv", "test/two.csv"]} ),

    testProperty "can split" prop_Splits

    ]

  where dofiles = (fmap.fmap) fold run
        base = Options "name" ["site", "sensor"] ["temperature"] "time" ["test/one.csv"]
        diff ref new = ["diff", "-u", ref, new]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests

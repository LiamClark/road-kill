import ParserTests
import Test.Hspec

main :: IO ()
main = hspec $ do
  parserTests
  lexerTests


import Test.Hspec
import Control.Exception (evaluate)
import Pars
import Lib

main :: IO ()
main = hspec $ do
  describe "parseExpr" $ do
    it "LOL" $ do
      parse parseExpr "(cons (1 2))" `shouldBe` ("(cons (1 2))" :: String)
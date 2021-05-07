import Test.Hspec

import Ast

main :: IO ()
main = hspec $ do 
  describe "paser" $ do 
    describe "expressions" exprParse_t

exprParse_t = do
  it "fuck" $ do
    id "fuck you" `shouldBe` "fuck you"


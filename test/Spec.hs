import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "someFunction" $ do
    it "gets correct result" $ do
      True `shouldBe` False

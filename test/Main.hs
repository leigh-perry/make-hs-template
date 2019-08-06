import Program
import qualified Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  test <- testSpec "make-hs-template" spec
  Test.Tasty.defaultMain test

spec :: Spec
spec =
  parallel $ do
    it "matches directory to be included" $ Program.isRelevantDir "/srcFilepath" "/srcFilepath/path" `shouldBe` True
    it "matches directory .git to be excluded" $
      Program.isRelevantDir "/srcFilepath" "/srcFilepath/.git" `shouldBe` False
    it "matches directory .stack-work to be excluded" $
      Program.isRelevantDir "/srcFilepath" "/srcFilepath/.stack-work" `shouldBe` False
    it "is not fooled by directory .stack-workxxx" $
      Program.isRelevantDir "/srcFilepath" "/srcFilepath/.stack-workxxx" `shouldBe` True
    it "matches stack.yaml.lock to be excluded" $
      Program.isRelevantFile "/srcFilepath" "/srcFilepath/stack.yaml.lock" "stack.yaml.lock" `shouldBe` False
    it "is not fooled by stack.yaml.lockxxx" $
      Program.isRelevantFile "/srcFilepath" "/srcFilepath/stack.yaml.lockxxx" "stack.yaml.lockxxx" `shouldBe` True

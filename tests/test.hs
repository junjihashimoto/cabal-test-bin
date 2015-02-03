{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec
import Test.Cabal.Path
import Text.Regex.Posix
import Control.Exception

shouldReturnRegex :: IO String -> String -> IO ()
shouldReturnRegex exe pattern = do
  v <- exe
  (v =~ pattern) `shouldBe` True

main :: IO ()
main = hspec $ do
  describe "library-test" $ do
    it "getExePath" $ do
      (getExePath "." "cabal-test-bin" `shouldReturn` "./dist/build/cabal-test-bin/cabal-test-bin")
      `catch`
      (\(_::SomeException) -> (getExePath "." "cabal-test-bin" `shouldReturnRegex` "./dist/dist-sandbox-(.*)/cabal-test-bin/cabal-test-bin"))
    it "getExeDir" $ do
      (getExeDir "." "cabal-test-bin" `shouldReturn` "./dist/build/cabal-test-bin")
      `catch`
      (\(_::SomeException) -> (getExeDir "." "cabal-test-bin" `shouldReturnRegex` "./dist/dist-sandbox-(.*)/cabal-test-bin"))

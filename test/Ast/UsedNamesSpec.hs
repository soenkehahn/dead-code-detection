
module Ast.UsedNamesSpec where

import           Control.Exception
import           Data.Foldable
import           Data.List
import           Test.Hspec

import           Ast.UsedNames

spec :: Spec
spec = do
  let errs :: [(String, ())]
      errs =
        ("errorNyiData", errorNyiData ()) :
        ("errorNyiOutputable", errorNyiOutputable ()) :
        []
  forM_ errs $ \ (name, err) -> do
    describe name $ do
      it "explains that this is not yet implemented" $ do
        seq err (return ()) `shouldThrow` \ (ErrorCall message) ->
          "not yet implemented" `isInfixOf` message

      it "points to the issue tracker" $ do
        seq err (return ()) `shouldThrow` \ (ErrorCall message) ->
          "https://github.com/soenkehahn/dead-code-detection/issues" `isInfixOf`
            message

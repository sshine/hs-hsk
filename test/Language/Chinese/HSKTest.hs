{-# LANGUAGE OverloadedStrings #-}

module Language.Chinese.HSKTest where

import Test.Tasty
import Test.Tasty.Hspec

import Language.Chinese.HSK (HSKLevel(..), process, plecoTrie)

spec_HSK :: Spec
spec_HSK =
  describe "process" $ do
    it "handles single characters" $ do
      process "你" plecoTrie `shouldBe` [(HSK1, ["你"])]
      process "好" plecoTrie `shouldBe` [(HSK1, ["好"])]

    it "handles multiple single characters" $ do
      process "你，们" plecoTrie `shouldBe` [(HSK1, ["你"]), (HSK1, ["们"])]
      process "好们" plecoTrie `shouldBe` [(HSK1, ["好"]), (HSK1, ["们"])]

    it "handles combined words by choosing longest prefix" $ do
      process "你们" plecoTrie `shouldBe` [(HSK1, ["你们"])]

    it "handles combined words with missing shorter prefix" $ do
      process "朋友" plecoTrie `shouldBe` [(HSK1, ["朋友"])]

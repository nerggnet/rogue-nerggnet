-- test/Spec.hs
module Main (main) where

import Test.Hspec

import qualified File.MapIOSpec
import qualified Game.GridUtilsSpec
import qualified Game.LogicSpec
import qualified Game.StateSpec
import qualified UI.DrawSpec
import qualified UI.LayoutSpec
import qualified UI.ScreenSpec

main :: IO ()
main = hspec $ do
  describe "Game.GridUtils" Game.GridUtilsSpec.spec
  describe "Game.State"     Game.StateSpec.spec
  describe "Game.Logic"     Game.LogicSpec.spec
  describe "File.MapIO"     File.MapIOSpec.spec
  describe "UI.Draw"        UI.DrawSpec.spec
  describe "UI.Layout"      UI.LayoutSpec.spec
  describe "UI.Screen"      UI.ScreenSpec.spec

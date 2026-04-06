module Main (main) where

import Test.Hspec
import Practica04
import Test.Hspec.Runner
import Data.List (sort)

main :: IO ()
main = hspecWith defaultConfig specs


specs :: Spec
specs = do

    describe "Tests: Conflicto" $ do 
        it "Caso: Con cláusula vacía" $ do
            conflict ([], [[Var "p", Var "q"], [Var "r", Var "s"], [], [Var "p", Var "r", Var "t"]]) `shouldBe` True
        it "Caso: Sin cláusula vacía" $ do 
            conflict ([], [[Var "p", Var "q"], [Var "r", Var "s"], [Var "p", Var "r", Var "t"]]) `shouldBe` False
    
    describe "Tests: Éxito" $ do
        it "Caso: Conjunto de cláusulas vacío" $ do 
            success ([("p", True), ("q", False)], []) `shouldBe` True
        it "Caso: Conjunto de cláusulas no vacío" $ do 
            success ([("p", True), ("q", False)], [[Var "p", Var "q"]]) `shouldBe` False

    describe "Tests: Cláusula unitaria" $ do 
        it "Caso: Literal True" $ do
            unit ([], [[Var "p", Var "q"], [Var "r", Var "s"], [Var "t"]]) `shouldBe` ([("t", True)], [[Var "p", Var "q"], [Var "r", Var "s"]])
        it "Caso: Literal False" $ do
            unit ([], [[Var "p", Var "q"], [Var "r", Var "s"], [Not (Var "t")]]) `shouldBe` ([("t", False)], [[Var "p", Var "q"], [Var "r", Var "s"]])
        it "Caso: Sin Literales" $ do
            unit ([], [[Var "p", Var "q"], [Var "r", Var "s"]]) `shouldBe` ([], [[Var "p", Var "q"], [Var "r", Var "s"]])

    describe "Tests: Eliminación" $ do
        it "Caso: Literal True" $ do
            elim ([("p", True)], [[Var "p", Var "q"], [Var "r", Var "s"], [Not (Var "p"), Var "r", Var "t"]]) `shouldBe` ([("p", True)], [[Var "r", Var "s"], [Not (Var "p"), Var "r", Var "t"]])
        it "Caso: Literal False" $ do
            elim ([("p", False)], [[Var "p", Var "q"], [Var "r", Var "s"], [Not (Var "p"), Var "r", Var "t"]]) `shouldBe` ([("p", False)], [[Var "p", Var "q"], [Var "r", Var "s"]])
        it "Caso: Sin Literales" $ do
            elim ([], [[Var "p", Var "q"], [Var "r", Var "s"], [Not (Var "p"), Var "r", Var "t"]]) `shouldBe` ([], [[Var "p", Var "q"], [Var "r", Var "s"], [Not (Var "p"), Var "r", Var "t"]])
    
    describe "Tests: Reducción" $ do 
        it "Caso: Literal True" $ do
            red ([("p", True)], [[Var "p", Var "q"], [Var "r", Var "s"], [Not (Var "p"), Var "r", Var "t"]]) `shouldBe` ([("p", True)], [[Var "p", Var "q"], [Var "r", Var "s"], [Var "r", Var "t"]])
        it "Caso: Literal False" $ do
            red ([("p", False)], [[Var "p", Var "q"], [Var "r", Var "s"], [Not (Var "p"), Var "r", Var "t"]]) `shouldBe` ([("p", False)], [[Var "q"], [Var "r", Var "s"], [Not (Var "p"), Var "r", Var "t"]])
        it "Caso: Sin Literales" $ do
            red ([], [[Var "p", Var "q"], [Var "r", Var "s"], [Not (Var "p"), Var "r", Var "t"]]) `shouldBe` ([], [[Var "p", Var "q"], [Var "r", Var "s"], [Not (Var "p"), Var "r", Var "t"]])

    describe "Tests: Separación" $ do
        it "Caso: Literal True" $ do
            sep (Var "p") ([], [[Var "p", Var "q"]]) `shouldBe` (([("p", True)], [[Var "p", Var "q"]]), ([("p", False)], [[Var "p", Var "q"]]))
        it "Caso: Literal True" $ do
            sep (Not (Var "p")) ([], [[Var "p", Var "q"]]) `shouldBe` (([("p", True)], [[Var "p", Var "q"]]), ([("p", False)], [[Var "p", Var "q"]]))
    
    describe "Tests: Heurística" $ do 
        it "Caso: Un solo máximo" $ do 
            heuristicsLiteral [[Var "p", Var "q"], [Var "p", Var "r"], [Var "p", Var "q", Var "s"]] `shouldBe` Var "p"
        it "Caso: Dos máximos" $ do 
            heuristicsLiteral [[Var "p", Var "q"], [Var "p", Var "r"], [Var "q", Var "s"]] `shouldSatisfy` (`elem` [Var "p", Var "q"])

    let clausulasOne = [[Var "p"], [Not (Var "r"), Var "q"], [Not (Var "r"), Var "p"], [Not (Var "q")]]
    let clausualasTt =  [[Var "p", Var "q", Not (Var "p")], [Var "p", Var "q", Not (Var "q")], [Var "p", Var "q", Not (Var "p")], [Var "p", Var "q", Not (Var "q")]]
    let clausulasCr = [[Not (Var "p")], [Not (Var "q"), Not (Var "p")], [Not (Var "p"), Not (Var "q")], [Not (Var "q")], [Var "p", Var "q"]]

    describe "Tests: dpll" $ do
        it "Caso: Un solo modelo" $ do
            dpll clausulasOne `shouldMatchList` [("p", True), ("q", False), ("r", False)]
        it "Caso: Tautología" $ do
            sort (dpll clausualasTt) `shouldSatisfy` (`elem` map sort [[("p", True), ("q", True)], [("p", True), ("q", False)], [("p", False), ("q", True)], [("p", False), ("q", False)], [("p", True)], [("q", True)], [("p", False)], [("q", False)]])
        it "Caso: Contradicción" $ do
            dpll clausulasCr `shouldBe` []

    let formOne = And (Not (Impl (Var "p") (And (Var "r") (Not (Var "q"))))) (And (Impl (Var "r") (Var "p")) (Not (Var "q")))
    let formTt =  Syss (Not (Or (Var "p") (Var "q"))) (And (Not (Var "p")) (Not (Var "q")))
    let formCr = Syss (Or (Var "p") (Var "q")) (And (Not (Var "p")) (Not (Var "q")))
    let formFNC = Or (Or (And (Var "p") (Var "r")) (Var "s")) (Var "q")

    describe "Tests: dpll2" $ do
        it "Caso: Un solo modelo" $ do
            dpll2 formOne `shouldMatchList` [("p", True), ("q", False), ("r", False)]
        it "Caso: Tautología" $ do
            sort (dpll2 formTt) `shouldSatisfy` (`elem` map sort [[("p", True), ("q", True)], [("p", True), ("q", False)], [("p", False), ("q", True)], [("p", False), ("q", False)], [("p", True)], [("q", True)], [("p", False)], [("q", False)]])
        it "Caso: Contradicción" $ do
            dpll2 formCr `shouldBe` []
        it "Caso: Truco" $ do
            sort (dpll2 formFNC) `shouldSatisfy` (`elem` map sort [[("p", True), ("q", True), ("r", True), ("s", True)], [("p", True), ("r", True), ("s", True)], [("p", True), ("q", True), ("r", True)], [("p", True), ("r", True)], [("p", True), ("q", True), ("s", True)], [("p", True), ("s", True)], [("p", True), ("q", True)], [("q", True), ("r", True), ("s", True)], [("r", True), ("s", True)], [("q", True), ("r", True)], [("q", True), ("s", True)], [("s", True)], [("q", True)]])
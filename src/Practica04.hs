module Practica04 where

import Data.List (partition, maximumBy)

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

type Literal = Prop
type Clausula = [Literal]

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

--Definicion de los tipos para la practica
type Interpretacion = [( String , Bool ) ]
type Estado = ( Interpretacion , [Clausula])
data ArbolDPLL = Node Estado ArbolDPLL | Branch Estado ArbolDPLL ArbolDPLL | Void deriving Show

--IMPLEMENTACION PARTE 1
--Ejercicio 1
conflict :: Estado -> Bool
conflict (_, cs) = [] `elem` cs

--Ejercicio 2
success :: Estado -> Bool
success (_, cs) = null cs

--Ejercicio 3
unit :: Estado -> Estado
unit (i, cs) =
  case [l | [l] <- cs, not (contradic l i)] of
    [] -> (i, cs)
    (l:_) ->
      let i' = addLit l i
          cs' = filter (/= [l]) cs
      in (i', cs')

--Funciones auxiilares para unit
addLit :: Literal -> Interpretacion -> Interpretacion
addLit (Var x) i = (x, True) : filter (\(y,_) -> y /= x) i
addLit (Not (Var x)) i = (x, False) : filter (\(y,_) -> y /= x) i
addLit _ i = i

contradic :: Literal -> Interpretacion -> Bool
contradic (Var x) i = lookup x i == Just False
contradic (Not (Var x)) i = lookup x i == Just True
contradic _ _ = False


--Ejercicio 4
elim :: Estado -> Estado
elim (i, cs) = (i, filter (not . satClause i) cs)

--Funciones auxiliares
satClause :: Interpretacion -> Clausula -> Bool
satClause i = any (satLit i)

satLit :: Interpretacion -> Literal -> Bool
satLit i (Var x) = lookup x i == Just True
satLit i (Not (Var x)) = lookup x i == Just False
satLit _ _ = False

--Ejercicio 5
red :: Estado -> Estado
red (i, cs) = (i, map (filter (not . falsLit i)) cs)

--Funcion auxiliar
falsLit :: Interpretacion -> Literal -> Bool
falsLit i (Var x) = lookup x i == Just False
falsLit i (Not (Var x)) = lookup x i == Just True
falsLit _ _ = False

--Ejercicio 6
sep :: Literal -> Estado -> (Estado, Estado)
sep l (i, cs) =
  let v = case l of
        Var x -> x
        Not (Var x) -> x
        _ -> error "Literal invalida"
  in ( ( (v, True) : i , cs )
     , ( (v, False) : i , cs )
     )
--Auxiliar
neg :: Literal -> Literal
neg (Var x) = Not (Var x)
neg (Not (Var x)) = Var x
neg l = l


--IMPLEMENTACION PARTE 2
buildDPLL :: Estado -> ArbolDPLL
buildDPLL st
  | conflict st = Node st Void
  | success st = Node st Void
  | otherwise =
    let st' = fix (unit . elim . red) st
    in if conflict st' then Node st' Void
        else if success st' then Node st' Void
        else
            let l = heuristicsLiteral (snd st')
                (stL, stR) = sep l st'
            in Branch st'
               (buildDPLL stL)
               (buildDPLL stR)

fix :: Eq a => (a -> a) -> a -> a
fix f x = if f x == x then x else fix f (f x)

--Ejercicio 1
heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral cs =
  fst $ maximumBy cmp (countLits (concat cs))
  where
    cmp (_,a) (_,b) = compare a b

countLits :: [Literal] -> [(Literal, Int)]
countLits [] = []
countLits (l:ls) =
  let (same, rest) = partition (== l) ls
      n = 1 + length same
  in (l, n) : countLits rest

--EJERCICIO 2
dpll :: [Clausula] -> Interpretacion
dpll cs = solve (buildDPLL ([], cs))

solve :: ArbolDPLL -> Interpretacion
solve Void = []
solve (Node (i, cs) _)
  | null cs = i
  | [] `elem` cs = []
  | otherwise = []
solve (Branch _ l r) =
  let left = solve l
  in if null left then solve r else left

--EXTRA
dpll2 :: Prop -> Interpretacion
dpll2 p = dpll (clausulas (fnc p))

--De la práctica anterior

--Ejercicio 1fnn :: Prop -> Prop
fnn (Not (Cons a)) = Cons (not a)
fnn (Not (Var p)) = Not (Var p)
fnn (Not (Not p)) =  fnn p
fnn (Not (And p q)) = Or (fnn (Not p)) (fnn (Not q))
fnn (Not (Or p q)) = And (fnn (Not p)) (fnn (Not q))
fnn (Not (Impl p q)) = And (fnn p) (fnn (Not q))
fnn (Not (Syss p q)) = Or (And (fnn p) (fnn (Not q))) (And (fnn q) (fnn (Not p)))
fnn (Cons a) = Cons a
fnn (Var p) = Var p
fnn (Not p) = Not (fnn p)
fnn (And p q) = And (fnn p) (fnn q)
fnn (Or p q) = Or (fnn p) (fnn q)
fnn (Impl p q) = Or (fnn (Not p)) (fnn q)
fnn (Syss p q) = And (Or (fnn (Not p)) (fnn q)) (Or (fnn (Not q)) (fnn p))

--Ejercicio 2
fnc :: Prop -> Prop
fnc (Not (Var p)) = Not (Var p)
fnc (Not (Cons a)) = Cons (not a)
fnc (Cons a) = Cons a
fnc (Var p) = Var p
fnc (Not p) = fnc (fnn (Not p))
fnc (And p q) = And (fnc p) (fnc q)
fnc (Or p q) = distributividad (fnc p) (fnc q)
fnc (Impl p q) = fnc (fnn (Impl p q))
fnc (Syss p q) = fnc (fnn (Syss p q))

--Función auxiliar que nos ayuda a la distributividad
distributividad :: Prop -> Prop -> Prop
distributividad p (And q r) = And (distributividad p q) (distributividad p r)
distributividad (And p q) r = And (distributividad p r) (distributividad q r)
distributividad p q = Or p q

--Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas (And p q) = clausulas p ++ clausulas q
clausulas (Or p q) =  [literales (Or p q)]
clausulas p = [literales p]

--Función auxiliar que saca los literales
literales :: Prop -> Clausula
literales (Or p q) = sinDuplicados (literales p ++ literales q)
literales p = [p]

sinDuplicados :: Eq a => [a] -> [a]
sinDuplicados [] = []
sinDuplicados (x:xs) = x : sinDuplicados (filter (/= x) xs)

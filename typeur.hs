import Control.Monad.State(State)
import qualified Control.Monad.State as ST 
import Data.Map(Map)
import qualified Data.Map as M

import qualified Data.List as L

-- Termes
data PTerm = Variable String | App PTerm PTerm | Abs String PTerm 
    | N Integer | Add PTerm PTerm

instance Show PTerm where
    show (Variable s)   = s
    show (N i)          = show i
    show (App pt1 pt2)  = "(" ++ show pt1 ++ ", " ++ show pt2 ++ ")"
    show (Abs s pt)     = "(fun " ++ s ++ " -> " ++ show pt ++ ")"
    show (Add pt1 pt2)  = "(" ++ show pt1 ++ " + " ++ show pt2 ++ ")"

-- Types
data PType = Type String | Arrow PType PType | Product PType PType | Nat 

instance Show PType where
    show (Type s)           = s
    show (Product pt1 pt2)  = "(" ++ show pt1 ++ " x " ++ show pt2 ++ ")"
    show (Arrow pt1 pt2)    = "(" ++ show pt1 ++ " -> " ++ show pt2 ++ ")"
    show Nat                = "Nat"

-- Environnement
type Environnement = Map String PType

-- Listes d'équation
type Equation = [(PType, PType)]

type Compteur = State Int

nouvelle_variable :: Compteur String
nouvelle_variable = do
    n <- ST.get
    ST.put (n + 1)
    return ("T" ++ show n)

cherche_type :: String -> Environnement -> PType
cherche_type s e = maybe Nat id (M.lookup s e)

appartient_type :: String -> PType -> Bool
appartient_type s1 (Type s2)        = s1 == s2
appartient_type s (Arrow pt1 pt2)   = appartient_type s pt1 || appartient_type s pt2
appartient_type _ _ = False

substitue_type :: PType -> String -> PType -> PType
substitue_type pt@(Type s1) s2 newPt
    | s1 == s2  = newPt
    | otherwise = pt
substitue_type (Arrow pt1 pt2) s newPt  = Arrow (substitue_type pt1 s newPt) (substitue_type pt2 s newPt)
substitue_type Nat _ _                  = Nat

substitue_type_partout :: Equation -> String -> PType -> Equation
substitue_type_partout eq s pt = L.map (\(pt1, pt2) -> (substitue_type pt1 s pt, substitue_type pt2 s pt)) eq

genere_equation :: PTerm -> PType -> Environnement -> Equation
genere_equation (Variable v) pty env    = [(pty, cherche_type v env)]
--genere_equation (App pt1 pt2) pty env   = 


-- Unification

-- T1 = T1 -> T2 : echec
-- T1 = T2 -> T3 : [T1 -> T2 -> T3]
-- T1 -> T2 = T3 -> T4 : T1 = T3 ; T2 = T4
-- N = N : []
-- N = T1 -> T2 : echec

main :: IO ()
main = do
    putStrLn "xd"
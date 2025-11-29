{-# LANGUAGE DeriveAnyClass #-}
{- HLINT ignore "Use camelCase" -}

import Control.Monad.State(State)
import Control.Exception(Exception)
import qualified Control.Exception as E
import qualified Control.Monad.State as ST 
import Data.Map(Map)
import qualified Data.Maybe as MB
import qualified Data.Map as M
import qualified Data.List as L
import Data.Either(Either)

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

type Equation_Z = (Equation, Equation)

type Compteur = State Int

data VarPasTrouve = VarPasTrouve deriving (Show, Exception)

newtype Echec_unif = Echec_unif String
    deriving (Show, Exception)

nouvelle_variable :: Compteur String
nouvelle_variable = do
    n <- ST.get
    ST.put (n + 1)
    return ("T" ++ show n)

cherche_type :: String -> Environnement -> Either VarPasTrouve PType
cherche_type s e = case M.lookup s e of
    Just t  -> Right t
    Nothing -> Left VarPasTrouve

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
genere_equation pte pty env = ST.evalState (aux pte pty env) 1
  where
    aux :: PTerm -> PType -> Environnement -> State Int Equation
    aux (Variable v) pty env = case cherche_type v env of
        Left _     -> error $ "variable " ++ v ++ " non trouvée dans l'environnement"
        Right tvar -> return [(pty, tvar)]

    aux (App pt1 pt2) pty env = do
      nv  <- nouvelle_variable
      eq1 <- aux pt1 (Arrow (Type nv) pty) env
      eq2 <- aux pt2 (Type nv) env
      return $ eq1 ++ eq2

    aux (Abs x pt) pty env = do
        nv1  <- nouvelle_variable
        nv2  <- nouvelle_variable
        eq   <- aux pt (Type nv2) (M.insert x (Type nv1) env)
        return $ (pty, Arrow (Type nv1) (Type nv2)) : eq

    aux (N _) pty env = return [(pty, Nat)]

    aux (Add pt1 pt2) pty env = do
        eq1 <- aux pt1 Nat env
        eq2 <- aux pt2 Nat env
        return $ (pty, Nat) : (eq1 ++ eq2)

rembobine :: Equation_Z -> Equation_Z
rembobine e@([], _) = e
rembobine (c:e1, e2) = (e1, c:e2)  

substitue_type_zip :: Equation_Z -> String -> PType -> Equation_Z
substitue_type_zip (e1, e2) v pty = 
    (substitue_type_partout e1 v pty, substitue_type_partout e2 v pty)

trouve_but :: Equation_Z -> String -> Either VarPasTrouve PType
trouve_but (_, []) _ = Left VarPasTrouve
trouve_but (e1, (Type v, t) : e2) but
    | v == but = Right t
    | otherwise = trouve_but ( (Type v, t) : e1 , e2) but
trouve_but (e1, (t, Type v) : e2) but 
    | v == but = Right t
    | otherwise = trouve_but ((t, Type v) : e1 , e2) but
trouve_but (e1, c : e2) but = trouve_but (c : e1, e2) but

unification :: Equation_Z -> String -> Either Echec_unif PType
unification e@(_, []) but =
    case trouve_but (rembobine e) but of
        Left _  -> Left (Echec_unif "but pas trouvé")
        Right t -> Right t
unification (e1, (Type v1, t2):e2) but
    | v1 == but =
        unification ((Type v1, t2):e1, e2) but
unification (e1, (Type v1, Type v2):e2) but =
    unification (substitue_type_zip (rembobine (e1,e2)) v2 (Type v1)) but
unification (e1, (Type v1, t2):e2) but
    | appartient_type v1 t2 =
        Left (Echec_unif ("occurence de " ++ v1 ++ " dans " ++ show t2))
    | otherwise =
        unification (substitue_type_zip (rembobine (e1,e2)) v1 t2) but
unification (e1, (t1, Type v2):e2) but
    | appartient_type v2 t1 =
        Left (Echec_unif ("occurence de " ++ v2 ++ " dans " ++ show t1))
    | otherwise =
        unification (substitue_type_zip (rembobine (e1,e2)) v2 t1) but
unification (e1, (Arrow t1 t2, Arrow t3 t4):e2) but =
    unification (e1, (t1,t3):(t2,t4):e2) but
unification (_, (Arrow{}, t3):_) _ =
    Left (Echec_unif ("type fleche non-unifiable avec " ++ show t3))
unification (_, (t3, Arrow{}):_) _ =
    Left (Echec_unif ("type fleche non-unifiable avec " ++ show t3))
unification (e1, (Nat,Nat):e2) but =
    unification (e1, e2) but
unification (_, (Nat,t3):_) _ =
    Left (Echec_unif ("type entier non-unifiable avec " ++ show t3))
unification (_, (t3,Nat):_) _ =
    Left (Echec_unif ("type entier non-unifiable avec " ++ show t3))


inference :: PTerm -> String
inference pte = 
    let eq = genere_equation pte (Type "but") M.empty
        eqZ = ([], eq) in
    case unification eqZ "but" of
        Left msg      -> show pte ++ " *** PAS TYPABLE *** :" ++ show msg
        Right res     -> show pte ++ " *** TYPABLE *** avec le type " ++ show res

ex_id :: PTerm
ex_id = Abs "x" (Variable "x")
inf_ex_id :: String
inf_ex_id = inference ex_id
ex_s :: PTerm
ex_s =
    Abs "x" (
        Abs "y" (
            Abs "z" (
                App
                    (App (Variable "x") (Variable "z"))
                    (App (Variable "y") (Variable "z"))
            )
        )
    )
inf_ex_s :: String
inf_ex_s = inference ex_s
ex_nat1 :: PTerm
ex_nat1 = App (Abs "x" (Add (Variable "x") (N 1))) (N 3)
inf_ex_nat1 :: String
inf_ex_nat1 = inference ex_nat1
ex_nat2 :: PTerm
ex_nat2 = Abs "x" (Add (Variable "x") (Variable "x"))
inf_ex_nat2 :: String
inf_ex_nat2 = inference ex_nat2
ex_omega :: PTerm
ex_omega = App
    (Abs "x" (App (Variable "x") (Variable "x")))
    (Abs "y" (App (Variable "y") (Variable "y")))
inf_ex_omega :: String
inf_ex_omega = inference ex_omega
ex_nat3 :: PTerm
ex_nat3 = App ex_nat2 ex_id
inf_ex_nat3 :: String
inf_ex_nat3 = inference ex_nat3
ex_fail :: PTerm
ex_fail = Add (Abs "x" (Variable "x")) (N 3)
inf_ex_fail :: String
inf_ex_fail = inference ex_fail

main :: IO ()
main = do
    putStrLn "inf_ex_id:"
    putStrLn inf_ex_id
    putStrLn "inf_ex_s:"
    putStrLn inf_ex_s
    putStrLn "inf_ex_fail:"
    putStrLn inf_ex_fail
    putStrLn "inf_ex_nat1:"
    putStrLn inf_ex_nat1
    putStrLn "inf_ex_nat2:"
    putStrLn inf_ex_nat2
    putStrLn "inf_ex_nat3:"
    putStrLn inf_ex_nat3
    putStrLn "inf_ex_omega:"
    putStrLn inf_ex_omega
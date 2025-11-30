{-# LANGUAGE DeriveAnyClass #-}

import Control.Monad.State(State)
import Control.Exception(Exception)
import qualified Control.Exception as E
import qualified Control.Monad.State as ST
import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Set as S
import qualified Data.Maybe as MB
import qualified Data.Map as M
import qualified Data.List as L
import Data.Either(Either)

data PTerm = Variable String | App PTerm PTerm | Abs String PTerm
    | N Integer | Add PTerm PTerm | Sub PTerm PTerm
    | Cons PTerm PTerm | Nil | Head PTerm | Tail PTerm
    | IfZero PTerm PTerm PTerm | IfEmpty PTerm PTerm PTerm
    | Rec PTerm | Let String PTerm PTerm

instance Show PTerm where
    show (Variable s)       = s
    show (N i)              = show i
    show (App pt1 pt2)      = "(" ++ show pt1 ++ ", " ++ show pt2 ++ ")"
    show (Abs s pt)         = "(fun " ++ s ++ " -> " ++ show pt ++ ")"
    show (Add pt1 pt2)      = "(" ++ show pt1 ++ " + " ++ show pt2 ++ ")"
    show (Sub pt1 pt2)      = "(" ++ show pt1 ++ " - " ++ show pt2 ++ ")"
    show (Cons pt1 pt2)     = "(" ++ show pt1 ++ " :: " ++ show pt2 ++ ")"
    show Nil                = "[]"
    show (Head pt)          = "(head " ++ show pt ++ ")"
    show (Tail pt)          = "(tail " ++ show pt ++ ")"
    show (IfZero c pt1 pt2) =  "(if0 " ++ show c ++ " then " ++ show pt1 ++ " else " ++ show pt2 ++ ")"
    show (IfEmpty c pt1 pt2)= "(ifEmpty " ++ show c ++ " then " ++ show pt1 ++ " else " ++ show pt2 ++ ")"
    show (Rec pt)           = "(rec " ++ show pt ++ ")"
    show (Let x pt1 pt2)    = "(let " ++ x ++ " = " ++ show pt1 ++ " in " ++ show pt2 ++ ")"

data PType = TVar String | Arrow PType PType | Nat
    | ListT PType | Forall String PType
--    | Product PType PType

instance Show PType where
    show (TVar s)           = s
    --show (Product pt1 pt2)  = "(" ++ show pt1 ++ " x " ++ show pt2 ++ ")"
    show (Arrow pt1 pt2)    = "(" ++ show pt1 ++ " -> " ++ show pt2 ++ ")"
    show Nat                = "Nat"
    show (ListT pt)         = "[" ++ show pt ++ "]"
    show (Forall x pt)      = "(forall " ++ show x ++ ". " ++ show pt ++ ")"

type Environnement = Map String PType

type Equation = [(PType, PType)]

type EquationZ = (Equation, Equation)

type Compteur = State Int

type VarGen = State Int

data VarPasTrouve = VarPasTrouve deriving (Show, Exception)

newtype EchecUnif = Echec_unif String
    deriving (Show, Exception)

maxStepUnification :: Int
maxStepUnification = 1000

maxStepEvaluation :: Int
maxStepEvaluation = 100

-- Typeur

nouvelleVariableTypage :: Compteur String
nouvelleVariableTypage = do
    n <- ST.get
    ST.put (n + 1)
    return ("T" ++ show n)

variablesLibre :: PTerm -> Set String
variablesLibre (Variable v) = S.singleton v
variablesLibre (Abs v t)    = S.delete v (variablesLibre t)
variablesLibre (App t1 t2)  = S.union (variablesLibre t1) (variablesLibre t2)
variablesLibre (Add t1 t2)  = S.union (variablesLibre t1) (variablesLibre t2)
variablesLibre (N _)        = S.empty
variablesLibre (Sub t1 t2)  = S.union (variablesLibre t1) (variablesLibre t2)
variablesLibre (Cons t1 t2) = S.union (variablesLibre t1) (variablesLibre t2)
variablesLibre Nil          = S.empty
variablesLibre (Head t)     = variablesLibre t
variablesLibre (Tail t)     = variablesLibre t
variablesLibre (IfZero c t1 t2) = 
    S.unions [variablesLibre c, variablesLibre t1, variablesLibre t2]
variablesLibre (IfEmpty c t1 t2) = 
    S.unions [variablesLibre c, variablesLibre t1, variablesLibre t2]
variablesLibre (Rec t)     = variablesLibre t
variablesLibre (Let x e1 e2) = S.union (variablesLibre e1) (S.delete x (variablesLibre e2))

estValeur :: PTerm -> Bool
estValeur (Abs _ _) = True
estValeur (N _)     = True
estValeur Nil       = True
estValeur (Cons v1 v2) = estValeur v1 && estValeur v2
estValeur _         = False

typerVariableLibre :: PType -> Set String
typerVariableLibre (TVar x)          = S.singleton x
typerVariableLibre (Arrow t1 t2)     = typerVariableLibre t1 `S.union` typerVariableLibre t2
--typerVariableLibre (Product t1 t2)   = typerVariableLibre t1 `S.union` typerVariableLibre t2
typerVariableLibre (ListT t)         = typerVariableLibre t
typerVariableLibre Nat               = S.empty
typerVariableLibre (Forall x t)      = S.delete x (typerVariableLibre t)

-- calcule les variables libres dans tous les types de l'environnement
freeVarsEnv :: Environnement -> Set String
freeVarsEnv env = foldr (S.union . typerVariableLibre) S.empty (M.elems env)

-- généralise le type t par rapport à l'environnement env
generalise :: Environnement -> PType -> PType
generalise env t =
    let fvEnv = freeVarsEnv env
        fvT   = typerVariableLibre t
        polys = fvT S.\\ fvEnv
    in foldr Forall t polys

-- cherche le type de la variable s dans l'environnement e
chercheType :: String -> Environnement -> Either VarPasTrouve PType
chercheType s e = case M.lookup s e of
    Just t  -> Right t
    Nothing -> Left VarPasTrouve

-- vérifie si la variable s apparaît dans le type pt
appartientType :: String -> PType -> Bool
appartientType s1 (TVar s2)        = s1 == s2
appartientType s (Arrow pt1 pt2)   = appartientType s pt1 || appartientType s pt2
appartientType _ _ = False

-- remplace toutes les occurrences de la variable s par le type newPt dans le type pt
substitueType :: PType -> String -> PType -> PType
substitueType pt@(TVar s1) s2 newPt
    | s1 == s2  = newPt
    | otherwise = pt
substitueType (Arrow pt1 pt2) s newPt  = Arrow (substitueType pt1 s newPt) (substitueType pt2 s newPt)
substitueType Nat _ _                  = Nat
substitueType (ListT pt) s newPt = ListT (substitueType pt s newPt)
substitueType (Forall v pt) s newPt
    | v == s    = Forall v pt 
    | otherwise = Forall v (substitueType pt s newPt)

-- remplace toutes les occurrences de la variable s par le type pt dans toutes les équations
substitueTypePartout :: Equation -> String -> PType -> Equation
substitueTypePartout eq s pt = L.map (\(pt1, pt2) -> (substitueType pt1 s pt, substitueType pt2 s pt)) eq

-- genère les équations de typage pour un terme et un type donnés dans un environnement donné
genereEquation :: PTerm -> PType -> Environnement -> Equation
genereEquation pte pty env = ST.evalState (aux pte pty env) 1
  where
    aux :: PTerm -> PType -> Environnement -> State Int Equation
    aux (Variable v) pty env = case chercheType v env of
        Left _     -> error $ "variable " ++ v ++ " non trouvée dans l'environnement"
        Right tvar -> return [(pty, tvar)]

    aux (App pt1 pt2) pty env = do
      nv  <- nouvelleVariableTypage
      eq1 <- aux pt1 (Arrow (TVar nv) pty) env
      eq2 <- aux pt2 (TVar nv) env
      return $ eq1 ++ eq2

    aux (Abs x pt) pty env = do
        nv1  <- nouvelleVariableTypage
        nv2  <- nouvelleVariableTypage
        eq   <- aux pt (TVar nv2) (M.insert x (TVar nv1) env)
        return $ (pty, Arrow (TVar nv1) (TVar nv2)) : eq

    aux (N _) pty env = return [(pty, Nat)]

    aux (Add pt1 pt2) pty env = do
        eq1 <- aux pt1 Nat env
        eq2 <- aux pt2 Nat env
        return $ (pty, Nat) : (eq1 ++ eq2)

    aux (Sub pt1 pt2) pty env = do
        eq1 <- aux pt1 Nat env
        eq2 <- aux pt2 Nat env
        return $ (pty, Nat) : (eq1 ++ eq2)

    aux (Cons pt1 pt2) pty env = do
        x <- nouvelleVariableTypage
        eqh <- aux pt1 (TVar x) env
        eqt <- aux pt2 (ListT (TVar x)) env
        return $ (pty, ListT (TVar x)) : (eqh ++ eqt)
    
    aux Nil pty env = do
        x <- nouvelleVariableTypage
        return [(pty, ListT (TVar x))]

    aux (Head pt) pty env = do
        x <- nouvelleVariableTypage
        eql <- aux pt (ListT (TVar x)) env
        return $ (pty, TVar x) : eql

    aux (Tail pt) pty env = do
        x <- nouvelleVariableTypage
        eql <- aux pt (ListT (TVar x)) env
        return $ (pty, ListT (TVar x)) : eql

    aux (IfZero ptc pt1 pt2) pty env = do
        eqc <- aux ptc Nat env
        eq1 <- aux pt1 pty env
        eq2 <- aux pt2 pty env
        return $ eqc ++ eq1 ++ eq2

    aux (IfEmpty ptc pt1 pt2) pty env = do
        x <- nouvelleVariableTypage
        eqc <- aux ptc (ListT (TVar x)) env
        eq1 <- aux pt1 pty env
        eq2 <- aux pt2 pty env
        return $ eqc ++ eq1 ++ eq2

    aux (Rec pt) pty env = do
        aux pt (Arrow pty pty) env

    aux (Let x e1 e2) pty env = do
        let t0 = case inference env e1 of
                    Left err -> error ("Echec du typage dans let : " ++ show err)
                    Right t  -> t
            sigma = generalise env t0
        aux e2 pty (M.insert x sigma env)

-- rembobine les équations pour traiter la prochaine équation à résoudre
rembobine :: EquationZ -> EquationZ
rembobine e@([], _) = e
rembobine (c:e1, e2) = (e1, c:e2)

-- remplace toutes les occurrences de la variable v par le type pty dans toutes les équations
substitueTypeZip :: EquationZ -> String -> PType -> EquationZ
substitueTypeZip (e1, e2) v pty =
    (substitueTypePartout e1 v pty, substitueTypePartout e2 v pty)

-- cherche le type associé à la variable but dans les équations
trouveBut :: EquationZ -> String -> Either VarPasTrouve PType
trouveBut (_, []) _ = Left VarPasTrouve
trouveBut (e1, (TVar v, t) : e2) but
    | v == but = Right t
    | otherwise = trouveBut ( (TVar v, t) : e1 , e2) but
trouveBut (e1, (t, TVar v) : e2) but
    | v == but = Right t
    | otherwise = trouveBut ((t, TVar v) : e1 , e2) but
trouveBut (e1, c : e2) but = trouveBut (c : e1, e2) but

-- vérifie si les constructeurs des deux types sont compatibles
constructeursCompatibles :: PType -> PType -> Bool
constructeursCompatibles (TVar _) _ = True
constructeursCompatibles _ (TVar _) = True
constructeursCompatibles (Forall _ _) _ = True
constructeursCompatibles _ (Forall _ _) = True
constructeursCompatibles Nat Nat = True
constructeursCompatibles (Arrow _ _) (Arrow _ _) = True
--constructeursCompatibles (Product _ _) (Product _ _) = True
constructeursCompatibles (ListT _) (ListT _) = True
constructeursCompatibles _ _ = False

-- effectue l'unification des équations pour trouver le type du but
unification :: Int -> EquationZ -> String -> Either EchecUnif PType
unification 0 _ _ = Left $ Echec_unif "timeout"
unification _ e@(_, []) but =
    case trouveBut (rembobine e) but of
        Left _  -> Left $ Echec_unif "but pas trouvé"
        Right t -> Right t
unification n (e1, (TVar v1, t2):e2) but
    | v1 == but =
        unification (n - 1) ((TVar v1, t2):e1, e2) but
unification n (e1, (TVar v1, TVar v2):e2) but =
    unification (n - 1) (substitueTypeZip (rembobine (e1,e2)) v2 (TVar v1)) but
unification n (e1, (TVar v1, t2):e2) but
    | appartientType v1 t2 =
        Left $ Echec_unif ("occurence de " ++ v1 ++ " dans " ++ show t2)
    | otherwise =
        unification (n - 1) (substitueTypeZip (rembobine (e1,e2)) v1 t2) but
unification n (e1, (t1, TVar v2):e2) but
    | appartientType v2 t1 =
        Left $ Echec_unif ("occurence de " ++ v2 ++ " dans " ++ show t1)
    | otherwise =
        unification (n - 1) (substitueTypeZip (rembobine (e1,e2)) v2 t1) but
unification n (e1, (Forall x t1, t2):e2) but =
    let x'  = x ++ "'" 
        t1' = substitueType t1 x (TVar x')
    in unification n (e1, (t1', t2):e2) but
unification n (e1, (t1, Forall x t2):e2) but =
    let x'  = x ++ "'"
        t2' = substitueType t2 x (TVar x')
    in unification n (e1, (t1, t2'):e2) but
unification n (e1, (ListT t1, ListT t2):e2) but =
    unification n (e1, (t1, t2):e2) but
unification _ (_, (t1, t2):_) _
    | not (constructeursCompatibles t1 t2)
    = Left $ Echec_unif ("constructeurs incompatibles : " ++ show t1 ++ " et " ++ show t2)
unification n (e1, (Arrow t1 t2, Arrow t3 t4):e2) but =
    unification (n - 1) (e1, (t1,t3):(t2,t4):e2) but
unification _ (_, (Arrow{}, t3):_) _ =
    Left $ Echec_unif ("type fleche non-unifiable avec " ++ show t3)
unification _ (_, (t3, Arrow{}):_) _ =
    Left $ Echec_unif ("type fleche non-unifiable avec " ++ show t3)
unification n (e1, (Nat,Nat):e2) but =
    unification (n - 1) (e1, e2) but
unification _ (_, (Nat,t3):_) _ =
    Left $ Echec_unif ("type entier non-unifiable avec " ++ show t3)
unification _ (_, (t3,Nat):_) _ =
    Left $ Echec_unif ("type entier non-unifiable avec " ++ show t3)

-- Type chcker affichant si le pterm est typable ou non
inferencePrinter :: PTerm -> String
inferencePrinter pte =
    let eq = genereEquation pte (TVar "but") M.empty
        eqZ = ([], eq) in
    case unification maxStepUnification eqZ "but" of
        Left msg      -> show pte ++ " *** PAS TYPABLE *** :" ++ show msg
        Right res     -> show pte ++ " *** TYPABLE *** avec le type " ++ show res

-- infère le type d'un pterm dans un environnement donné, utilisé dans pour le let-polymorphique
inference :: Environnement -> PTerm -> Either EchecUnif PType
inference env pte =
    let eq  = genereEquation pte (TVar "but") env
        eqZ = ([], eq)
    in unification maxStepUnification eqZ "but"

-- Evaluateur

-- génère une nouvelle variable qui n'est pas dans l'ensemble des variables
nouvelleVariableEval :: Set String -> VarGen String
nouvelleVariableEval variables = do
    n <- ST.get
    ST.put (n + 1)
    let v = "x" ++ show n
    if v `elem` variables
        then nouvelleVariableEval variables
        else return v

-- renomme toutes les occurrences de la variable old par new dans le terme
renommerVariable :: String -> String -> PTerm -> PTerm
renommerVariable courant nouveau (Variable v) =
    if v == courant then Variable nouveau 
    else Variable v
renommerVariable courant nouveau (Abs v t1) =
    if v == courant then Abs v t1
    else Abs v (renommerVariable courant nouveau t1)
renommerVariable courant nouveau (App t1 t2) = 
    App (renommerVariable courant nouveau t1) (renommerVariable courant nouveau t2)
renommerVariable courant nouveau (Add t1 t2) = 
    Add (renommerVariable courant nouveau t1) (renommerVariable courant nouveau t2)
renommerVariable courant nouveau (N n) = N n
renommerVariable courant nouveau (Sub t1 t2) =
    Sub (renommerVariable courant nouveau t1) (renommerVariable courant nouveau t2)
renommerVariable courant nouveau (Cons t1 t2) =
    Cons (renommerVariable courant nouveau t1) (renommerVariable courant nouveau t2)
renommerVariable courant nouveau Nil = Nil
renommerVariable courant nouveau (Head t) =
    Head (renommerVariable courant nouveau t)
renommerVariable courant nouveau (Tail t) =
    Tail (renommerVariable courant nouveau t)
renommerVariable courant nouveau (IfZero c t1 t2) =
    IfZero (renommerVariable courant nouveau c)
           (renommerVariable courant nouveau t1)
           (renommerVariable courant nouveau t2)
renommerVariable courant nouveau (IfEmpty c t1 t2) =
    IfEmpty (renommerVariable courant nouveau c)
            (renommerVariable courant nouveau t1)
            (renommerVariable courant nouveau t2)
renommerVariable courant nouveau (Rec t) =
    Rec (renommerVariable courant nouveau t)
renommerVariable courant nouveau (Let x e1 e2) =
    Let x (renommerVariable courant nouveau e1)
          (if x == courant then e2 else renommerVariable courant nouveau e2)

-- renomme les variables liées pour éviter les conflits
alphaConversion :: PTerm -> State Int PTerm
alphaConversion = aux S.empty
    where
        aux _ (Variable s) = return $ Variable s
        aux _ (N i) = return $ N i
        aux variables (Add pt1 pt2) = do
            pt1' <- aux variables pt1
            pt2' <- aux variables pt2
            return $ Add pt1' pt2'
        aux variables (App pt1 pt2) = do
            pt1' <- aux variables pt1
            pt2' <- aux variables pt2
            return $ App pt1' pt2'
        aux variables (Abs x pt) = do
            x' <- nouvelleVariableEval variables
            t' <- aux (S.insert x' variables) (renommerVariable x x' pt)
            return $ Abs x' t'

-- substitution de n à la place de x dans t
substitution :: String -> PTerm -> PTerm -> State Int PTerm
substitution x n (Variable v) = return $ if v == x then n else Variable v
substitution x n (N k) = return $ N k
substitution x n (Add t1 t2) = do
    t1' <- substitution x n t1
    t2' <- substitution x n t2
    return $ Add t1' t2'
substitution x n (Sub t1 t2) = do
    t1' <- substitution x n t1
    t2' <- substitution x n t2
    return $ Sub t1' t2'
substitution x n Nil = return Nil
substitution x n (Cons t1 t2) = do
    t1' <- substitution x n t1
    t2' <- substitution x n t2
    return $ Cons t1' t2'
substitution x n (Head t) = do
    t' <- substitution x n t
    return $ Head t'
substitution x n (Tail t) = do
    t' <- substitution x n t
    return $ Tail t'
substitution x n (IfZero c t1 t2) = do
    c'  <- substitution x n c
    t1' <- substitution x n t1
    t2' <- substitution x n t2
    return $ IfZero c' t1' t2'
substitution x n (IfEmpty c t1 t2) = do
    c'  <- substitution x n c
    t1' <- substitution x n t1
    t2' <- substitution x n t2
    return $ IfEmpty c' t1' t2'
substitution x n (Rec t) = do
    t' <- substitution x n t
    return $ Rec t'
substitution x n (Let v e1 e2) 
    | v == x = do
        e1' <- substitution x n e1
        return $ Let v e1' e2
    | S.member v (variablesLibre n) = do
        v' <- nouvelleVariableEval (S.union (variablesLibre e2) (variablesLibre n))
        e1' <- substitution x n e1
        e2' <- substitution x n (renommerVariable v v' e2)
        return $ Let v' e1' e2'
    | otherwise = do
        e1' <- substitution x n e1
        e2' <- substitution x n e2
        return $ Let v e1' e2'
substitution x n (App t1 t2) = do
    t1' <- substitution x n t1
    t2' <- substitution x n t2
    return $ App t1' t2'
substitution x n (Abs v t1) 
    | v == x = return $ Abs v t1
    | S.member v (variablesLibre n) = do
        v' <- nouvelleVariableEval (S.union (variablesLibre t1) (variablesLibre n))
        t1' <- substitution x n (renommerVariable v v' t1)
        return $ Abs v' t1'
    | otherwise = do
        t1' <- substitution x n t1
        return $ Abs v t1'

etapeCallByValue :: PTerm -> State Int (Maybe PTerm)
etapeCallByValue (App (Abs x t) v) | estValeur v = do
    t' <- substitution x v t
    return $ Just t'
etapeCallByValue (App t1 t2) | not (estValeur t1) = do
    mt1 <- etapeCallByValue t1
    return $ fmap (`App` t2) mt1
etapeCallByValue (App v1 t2) | estValeur v1 && not (estValeur t2) = do
    mt2 <- etapeCallByValue t2
    return $ fmap (App v1) mt2
etapeCallByValue (Add (N n1) (N n2)) = return $ Just (N (n1 + n2))
etapeCallByValue (Add t1 t2) | not (estValeur t1) = do
    mt1 <- etapeCallByValue t1
    return $ fmap (`Add` t2) mt1
etapeCallByValue (Add v1 t2) | estValeur v1 && not (estValeur t2) = do
    mt2 <- etapeCallByValue t2
    return $ fmap (Add v1) mt2
etapeCallByValue _ = return Nothing

evalCallByValue :: Int -> PTerm -> State Int [PTerm]
evalCallByValue 0 term = return [term]
evalCallByValue n term = do
    mterm <- etapeCallByValue term
    case mterm of
        Nothing -> return [term]
        Just t' -> do
            rest <- evalCallByValue (n - 1) t'
            return (term : rest)

evaluationTerm :: PTerm -> [PTerm]
evaluationTerm term = ST.evalState (evalCallByValue maxStepEvaluation term) 0

exId :: PTerm
exId = Abs "x" (Variable "x")
infExId :: String
infExId = inferencePrinter exId
exS :: PTerm
exS =
    Abs "x" (
        Abs "y" (
            Abs "z" (
                App
                    (App (Variable "x") (Variable "z"))
                    (App (Variable "y") (Variable "z"))
            )
        )
    )
infExS :: String
infExS = inferencePrinter exS
exNat1 :: PTerm
exNat1 = App (Abs "x" (Add (Variable "x") (N 1))) (N 3)
infExNat1 :: String
infExNat1 = inferencePrinter exNat1
exNat2 :: PTerm
exNat2 = Abs "x" (Add (Variable "x") (Variable "x"))
infExNat2 :: String
infExNat2 = inferencePrinter exNat2
exOmega :: PTerm
exOmega = App
    (Abs "x" (App (Variable "x") (Variable "x")))
    (Abs "y" (App (Variable "y") (Variable "y")))
infExOmega :: String
infExOmega = inferencePrinter exOmega
ex_nat3 :: PTerm
ex_nat3 = App exNat2 exId
infExNat3 :: String
infExNat3 = inferencePrinter ex_nat3
exFail :: PTerm
exFail = Add (Abs "x" (Variable "x")) (N 3)
infExFail :: String
infExFail = inferencePrinter exFail
exLetList :: PTerm
exLetList = 
    Let "id" (Abs "x" (Variable "x"))
        (Cons
            (App (Variable "id") (Cons (N 1) (Cons (N 2) Nil)))
            (Cons (App (Variable "id") (Cons (N 3) Nil)) Nil)
        )
infExLetList :: String
infExLetList = inferencePrinter exLetList
exLetFail :: PTerm
exLetFail =
    Let "id" (Abs "x" (Variable "x"))
        (Cons
            (App (Variable "id") (N 3))
            (Cons (App (Variable "id") (Abs "y" (Variable "y"))) Nil)
        )
infExLetFail :: String
infExLetFail = inferencePrinter exLetFail
exIfZero1 :: PTerm
exIfZero1 = 
    App (Abs "x" (IfZero (Variable "x") (N 10) (Add (Variable "x") (N 1)))) (N 0)
infExIfZero1 :: String
infExIfZero1 = inferencePrinter exIfZero1
exIfEmptyCons :: PTerm
exIfEmptyCons =
    App (Abs "xs" (IfEmpty (Variable "xs")
                      Nil
                      (Cons (Head (Variable "xs")) (Tail (Variable "xs"))))) (Cons (N 1) Nil)
infExIfEmptyCons :: String
infExIfEmptyCons = inferencePrinter exIfEmptyCons

main :: IO ()
main = do
    let stepsExNat1 = evaluationTerm exNat1
    let stepsExOmega  = evaluationTerm exOmega
    let stepsIfEmptyCons = evaluationTerm exIfEmptyCons
    putStrLn "Inférence typable"
    putStrLn "infExId:"
    putStrLn infExId
    putStrLn "infExS:"
    putStrLn infExS
    putStrLn "infExNat1:"
    putStrLn infExNat1
    putStrLn "infExNat2:"
    putStrLn infExNat2
    putStrLn "infExLetList"
    putStrLn infExLetList
    putStrLn "infExIfZero1"
    putStrLn infExIfZero1
    putStrLn "infExIfEmptyCons"
    putStrLn infExIfEmptyCons
    putStrLn "\nInférence non typable"
    putStrLn "infExFail:"
    putStrLn infExFail
    putStrLn "infExNat3:"
    putStrLn infExNat3
    putStrLn "infExOmega:"
    putStrLn infExOmega
    putStrLn "infExLetFail:"
    putStrLn infExLetFail
    putStrLn "\n==========================="
    putStrLn "======= Evaluation ========"
    putStrLn "==========================="
    putStrLn "exOmega"
    mapM_ print stepsExOmega
    putStrLn "exNat1"
    mapM_ print stepsExNat1
    putStrLn "exIfEmptyCons"
    mapM_ print stepsIfEmptyCons

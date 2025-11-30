# Projet de TAS: Typeur-Evaluateur de lambda-calcul

Contributeurs: Mickael VILAYVANH

## Pré-requis:

- GHC
- Haskell

## Installation:

Lien vers l'installation:

https://www.haskell.org/downloads/


## Exécuter le programme

Depuis le répertoire racine du projet, il est possible d'exécuter le programme de deux façons.

### Via compilation

Compiler le code avec

```sh
ghc typeur.hs
```

Cela génère un exécutable. Utilisable en tapant:

```sh
./typeur
```

Sur Windows

```sh
./typeur.exe
```

### Via l'interpréteur GHC

Ouvrir l'interpréteur GHCI en fournissant le fichier en argument

```sh
ghci typeur.hs
```

Depuis l'interpréteur, appeler la fonction main du programme.

```
ghci> main
```

### Créer ses propres lambda expression

Définir une fonction

```
lambda_expr_name :: PTerm
lambda_expr_name = ... 
```
Et utiliser le type PTerm pour le construire

Il n'y a pas de listener et parser, donc il faut définir les expressions en dur.

Elle est maintenant utilisable dans le main pour unification et évaluation.

Il faut la passer en paramètre des fonctions respectivement de la tâche recherchée.

### Ce qui a été fait

La partie lambda calcul simplifié et let, entiers et listes ont été fait
L'unification est bien défini pour les différents termes.
La génération d'équation est aussi définie.

Quelques tests ont été fait à la fin du fichier vers le main. On définit d'abord une fonction
constante contenant le PTerm envoloppé puis on doit utiliser la fonction
inferencePrinter pour type checker. Pour évaluer une expression, 
il faut nécessairement utiliser une expression contenant le terme App pour l'application.

Ensuite, on exploite la puissance de la monade State pour faire l'évaluation d'étape en étape donc dans le main,
pour exécuter un PTerm bien typé, il faut utiliser
ST.evalState avec la fonction evalCallByValue comme

```hs
let stepsExNat1 = ST.evalState (evalCallByValue maxStepEvaluation exNat1) 0
```


###

Ce programme est libre de droit
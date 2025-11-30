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
ghc evaluateur-typeur.hs
```

Cela génère un exécutable. Utilisable en tapant:

```sh
./evaluateur-typeur
```

Sur Windows

```sh
./evaluateur-typeur.exe
```

### Via l'interpréteur GHC

Ouvrir l'interpréteur GHCI en fournissant le fichier en argument

```sh
ghci evaluateur-typeur.hs
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
L'évaluation pour le lambda calcul simplifié est complet. Celui pour le let, entiers et listes est incomplet.

Quelques tests ont été fait à la fin du fichier vers le main. On définit d'abord une fonction
constante contenant le PTerm envoloppé puis on doit utiliser la fonction
inferencePrinter pour type checker. Pour évaluer une expression, 
il faut nécessairement utiliser une expression contenant le terme App pour l'application.

Ensuite, on exploite la puissance de la monade State pour faire l'évaluation d'étape en étape donc dans le main,
pour exécuter un PTerm bien typé, il faut utiliser evaluationTerm

```hs
let stepsExNat1 = evaluationTerm exNat1
```

### Ce qui n'a pas été fait

L'évaluation pour les let, entiers et listes n'est pas disponible.
La partie trait impérative n'a pas été traitée ainsi que les extensions.

Ce programme est libre de droit et d'usage.

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

```
ghc typeur.hs
```

Cela génère un exécutable. Utilisable en tapant:

```
./typeur
```

### Via l'interpréteur GHC

Ouvrir l'interpréteur GHCI en fournissant le fichier en argument

```
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


Ce programme est libre de droit
# Apps pour le cours d'introduction à la dynamique des populations et des communautés

Ce dépôt contient des applications **Shiny** d’introduction à la dynamique des populations.  
Suivez les étapes ci‑dessous pour exécuter une app en local avec **RStudio**.

---
## 0. Prérequis

1. Installer **R** (version ≥ 4.3.3 impératif) → <https://cran.r-project.org>  
2. Installer **RStudio Desktop** (dernière version 2025.05.1+513 recommandée) → <https://posit.co/download/rstudio-desktop/>
---

## 1. Télécharger l’archive ZIP du dépot (*repository*)

1. Sur la page GitHub: <https://github.com/florianorgeret/IntroPopDyn>
2. Cliquez sur le bouton vert **\<\> Code** (en haut à droite) puis cliquer sur **Download ZIP**.  
2. Décompressez l’archive dans le dossier de votre choix (par ex. `Documents/IntroPopDyn`).  
3. Dans ce dossier, double cliquer sur **IntroPopDyn.Rproj** pour lancer RStudio avec ce projet
4. Vérifier que le chemin par défault dans R est bien ce dossier :
    - il est indiqué juste en dessous de l'onglet Console, en bas dans RStudio
    - vous pouvez aussi lancer `gtwd()` (*get working directory*) directement dans la Console

## 2. Lancer une des applications

### 1. Version "Flux constants"

Directement dans la **Console** copier 

```r
source("1_bide_flux_constant.R") # permet de vérifier l'installation des packages R nécessaires
```
Puis appuyer sur "Entrée".

Une liste de packages va s'installer, cela peut prendre quelques minutes la première fois.

Il se peut que la console vous demander de continuer avec posant une question à choix Y/N, taper Y dans la Console, puis "Entrée".

*ATTENTION !*
*Normalement il ne devrait pas y avoir d'erreurs, sinon essayez d'installer la dernière version de* [RTools](https://cran.r-project.org/bin/windows/Rtools/) 
*Puis essayer encore d'installer les packages. Si une erreur persiste, essaye de la comprendre, et de voir quel package ne s'installe pas correctement (i.e. demandez a google)*

Une fois les packages installés, copier :

```r
shiny::runApp("1_bide_flux_constant.R")
```

puis taper entrée. L'application devrait se lancer !

### 2. Version "Taux constants"

Idem pour cette version, toujours dans la console, copier la ligne suivante puis lancer la en tapant "Entrée" :

```r
source("2_bide_taux_constant.R")
```

Normalement ca devrait être beaucoup plus rapide, si cela a fonctionner pour la première application

et ensuite, toujours dans la console copier cette ligne puis lancer en tapant "Entrée" :

```r
shiny::runApp("2_bide_taux_constant.R")
```

La deuxième app, pour le deuxième exercice devrait s'ouvrir !

## 3. Attention !

RStudio va ouvrir une nouvelle fenêtre Shiny avec l'appli mais si appuyer sur le bouton "Run Simulation" ne déclenche rien, essayer de l'ouvrir 
dans votre navigateur internet, en cliquant sur "Open in Browser" en haut de l'appli (logo avec une petite flèche et fenêtre).
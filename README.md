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
3. Charger **IntroPopDyn.Rproj** dans le dossier décompressé.     

## 2. Lancer une des applications

Directement dans l’onglet **Console** (ou dans un nouveau **Script** en lançant les commandes avec `CTRL`+ `ENTER`):

```r
# pour la version “flux constants”
source("1_bide_flux_constant.R") #permet de vérifier l'installation des packages R nécessaires
shiny::runApp("1_bide_flux_constant.R")#lance l'appli !
```

```r
# pour la version “taux constants”
source("2_bide_taux_constant.R") #permet de vérifier l'installation des packages R nécessaires
shiny::runApp("2_bide_taux_constant.R") #lance l'appli !
```
## 3. Attention !

RStudio va ouvrir une nouvelle fenetre Shiny avec l'appli mais si appuyer sur le bouton "Run Simulation" ne déclenche rien, essayer de l'ouvrir 
dans votre navigateur internet, en cliquant sur "Open in Browser" en haut de l'appli (logo avec une petite flèche et fenêtre).
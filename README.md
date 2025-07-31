# Apps pour le cours d'introduction à la dynamique des populations et des communautés

Ce dépôt contient des applications **Shiny** d’introduction à la dynamique des populations.  
Suivez les étapes ci‑dessous pour exécuter l’app en local avec **RStudio**.

---

## 0. Prérequis

1. Installer **R** (≥ 4.3.3) → <https://cran.r-project.org>  
2. Installer **RStudio Desktop** → <https://posit.co/download/rstudio-desktop/>

---

## 1. Télécharger l’archive ZIP du dépot

1. Sur la page GitHub: <https://github.com/florianorgeret/IntroPopDyn>
2. Cliquez sur le bouton vert **\<\> Code** puis **Download ZIP**.  
2. Décompressez l’archive dans le dossier de votre choix (par ex. `Documents/IntroPopDyn`).  
3. Charger **IntroPopDyn.Rproj** dans le dossier décompressé.     
  

### (Alternative avec Git : cloner le dépôt dans un projet RStudio)

1. Installation de **Git** – nécessaire pour cloner le dépôt depuis RStudio  
   * Windows : <https://git-scm.com/download/win>  
   * macOS  : `brew install git` ou installeur <https://git-scm.com/download/mac>  
   * Linux : gestionnaire de paquets (ex. `sudo apt install git`)
2. *File ▸ New Project…*  
3. *Version Control ▸ Git*  
4. Dans **Repository URL**, collez&nbsp;:  
   `https://github.com/florianorgeret/IntroPopDyn`  
5. Choisissez un dossier local puis **Create Project**.  
   RStudio ouvre automatiquement le projet.

---

## 2. Lancer une des applications

Directement dans l’onglet **Console** ou dans un nouveau **Script**:

```r
# pour la version “flux constants”
source("1_bide_flux_constant.R")
shiny::runApp("1_bide_flux_constant.R")
```

```r
# pour la version “taux constants”
source("2_bide_taux_constant.R")
shiny::runApp("2_bide_taux_constant.R")
```

# Apps pour le cours d'introduction à la dynamique des populations et des communautés

Ce dépôt contient une application **Shiny** d’introduction à la dynamique des populations (`first_sim.R`).  
Suivez les étapes ci‑dessous pour exécuter l’app en local avec **RStudio**.

---

## 0. Prérequis

1. Avoir R installé (version >4.3.3 conseillé) ▸ https://cran.r-project.org/

2. Avoir RStudio installé ▸ https://posit.co/download/rstudio-desktop/

## 1. Créer un nouveau projet RStudio depuis GitHub

1. **File ▸ New Project…**  

2. **Version Control ▸ Git**  

3. Dans *Repository URL* collez :  https://github.com/florianorgeret/IntroPopDyn

4. Choisissez le dossier où cloner le dépôt, puis **Create Project**.  
RStudio ouvre automatiquement le projet.

---

## 2. Restaurer l’environnement de travail (packages)

Dans l’onglet **Console** (lancer les lignes de commande une par une en tapant `Entrée` <kbd>↵</kbd> à chaque fois) :

```r
install.packages("renv")      # une seule fois sur votre machine
```
```r
renv::activate()              # active la bibliothèque du projet
```
```r
renv::restore()               # installe les versions de packages listées dans renv.lock
```

Et à la question :

```console
Do you want to proceed? [Y/n]
```
=> il suffit de taper <kbd>Y</kbd> dans la console et de lancer les installations avec la touche `Entrée` <kbd>↵</kbd> 

L'installation de tous les packages peut alors prendre plusieurs minutes (<5 min), mais uniquement pour la première fois...

## 3. Lancer l’application Shiny

```r
shiny::runApp("first_sim.R")
```

Un onglet s’ouvre alors dans votre navigateur (ou le panneau Viewer) avec l’interface interactive.
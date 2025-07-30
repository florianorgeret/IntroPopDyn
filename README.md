# IntroPopDyn

Ce dépôt contient une application **Shiny** d’introduction à la dynamique des populations (`first_sim.R`).  
Suivez les étapes ci‑dessous pour exécuter l’app en local avec **RStudio**.

---

## 1 Créer un nouveau projet RStudio depuis GitHub

1. **File ▸ New Project…**  

2. **Version Control ▸ Git**  

3. Dans *Repository URL* collez :  https://github.com/florianorgeret/IntroPopDyn

4. Choisissez le dossier où cloner le dépôt, puis **Create Project**.  
RStudio ouvre automatiquement le projet.

---

## 2 Restaurer l’environnement de travail (packages)

Dans l’onglet **Console** :

```r
install.packages("renv")      # une seule fois sur votre machine
renv::restore()               # installe les versions de packages listées dans renv.lock
```

## 3 Lancer l’application Shiny

```r
shiny::runApp("first_sim.R")
```

Un onglet s’ouvre alors dans votre navigateur (ou le panneau Viewer) avec l’interface interactive.
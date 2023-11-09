
### Auteur: FRANCK MALEVE TAKUETE

# Documentation - Tester le positionnement des dames sur un échiquier en R

## Introduction
Cette documentation explique comment écrire un script en langage R pour tester le positionnement de plusieurs dames sur un échiquier sans qu'elles se mangent mutuellement. Le problème des huit dames est un problème classique en informatique et en mathématiques.

## Prérequis
Avant de commencer, assurez-vous d'avoir les éléments suivants :
- Une installation de R sur votre système.
- Des connaissances de base en programmation R.

**Installation de R sur Windows :**

1. Téléchargemer R : Allez sur [le site officiel de R](https://cran.r-project.org/mirrors.html), Choisissez un miroir près de votre emplacement géographique, puis téléchargez la version la plus récente de R pour Windows.

2. Installation de R : Exécutez le fichier d'installation téléchargé. Suivez les instructions du programme d'installation en laissant les options par défaut à moins que vous ne souhaitiez personnaliser certains paramètres.

3. Télécharger et installation de RTools (pour certains packages) : Pour certains packages, vous pourriez avoir besoin de [RTools](https://cran.r-project.org/bin/windows/Rtools/). Suivez les instructions d'installation.

4. Télécharger et installation de RStudio : Exécutez le fichier d'installation téléchargé. Suivez les instructions du programme d'installation en laissant les options par défaut à moins que vous ne souhaitiez personnaliser certains paramètres.


**Installation de R sur Mac :**

1. Télécharger de R : Allez sur [le site officiel de R](https://cran.r-project.org/mirrors.html), choisissez un miroir proche, puis téléchargez la version la plus récente de R pour macOS.

2. Installer R : Exécutez le fichier d'installation téléchargé. Suivez les instructions du programme d'installation en laissant les options par défaut à moins que vous ne souhaitiez personnaliser certains paramètres.

3. Télécharger et installation de RStudio : Exécutez le fichier d'installation téléchargé. Suivez les instructions du programme d'installation en laissant les options par défaut à moins que vous ne souhaitiez personnaliser certains paramètres.

## Objectif
L'objectif de ce script est de déterminer toutes les combinaisons possibles pour placer plusieurs dames sur un échiquier de manière à ce qu'elles ne menacent pas mutuellement.

## Étapes
Nous allons suivre les étapes suivantes pour résoudre le problème des dames :
1. Création d'un échiquier virtuel en R.
2. Génération de toutes les combinaisons possibles de placement de dames.
3. Vérification de chaque combinaison pour s'assurer qu'aucune dame ne menace les autres.
4. Affichage des solutions valides.

## Code R
Voici un exemple de code en R pour résoudre le problème des x dames :

```R
TrouverSolutions <- function(x, y) {
  solutions <- list()  # Créer une liste vide pour stocker les solutions
  
  EstValide <- function(placement, colonne, ligne) {
    # Vérifier si une dame peut être placée dans la colonne/ligne spécifiée
    i <- 1
    while (i < colonne) {
      if (placement[i] == ligne || 
          placement[i] - i == ligne - colonne || 
          placement[i] + i == ligne + colonne) {
        return(FALSE)
      }
      i <- i + 1
    }
    return(TRUE)
  }
  
  PlacerDames <- function(placement, colonne = 1) {
    if (colonne > x) {
      solutions <<- c(solutions, list(placement - 1))  # Ajouter la solution trouvée de la liste
    } else {
      ligne <- 1
      while (ligne <= y) {  # Utiliser y pour le nombre de colonnes
        if (EstValide(placement, colonne, ligne)) {
          placement[colonne] <- ligne
          PlacerDames(placement, colonne + 1)
        }
        ligne <- ligne + 1
      }
    }
  }
  
  placementInitial <- integer(x)
  PlacerDames(placementInitial)
  cat("Nombre de solutions trouvées:", length(solutions), "\n")
  
  return(solutions)  # Retourner la liste complète des solutions
}

TrouverSolutions(10,10) #...ici...

# Charger le package tidyverse (ggplot2)
library(tidyverse)

# Utiliser la fonction avec le nombre de lignes/colonnes de votre choix
x <- 10 #...ici...
y <- 10 #...ici...
solutions <- TrouverSolutions(x, y)

# Créer un dataframe contenant les solutions
df_Solutions <- as.data.frame(do.call(rbind, solutions))

# Compter le nombre de fois que chaque position est occupée
df_HeatmapData <- df_Solutions %>% pivot_longer(everything(), names_to = "Colonne", values_to = "Ligne") %>% group_by(Colonne, Ligne) %>% summarise(Freq = n())

# Convertisser les colonnes en facteurs et spécifier l'ordre
df_HeatmapData$Colonne <- factor(df_HeatmapData$Colonne, levels = paste0("V", 1:x))
df_HeatmapData$Ligne <- factor(df_HeatmapData$Ligne, levels = as.character(0:(y-1)))

# Créer le heatmap avec ggplot2
ggplot(data = df_HeatmapData, aes(x = Colonne, y = Ligne, fill = Freq)) + geom_tile() + scale_fill_gradient(low = "white", high = "red") + labs(title = "Heatmap des solutions possibles", x = "Colonne", y = "Ligne")

```
## Développement
1. La fonction TrouverSolutions prend deux arguments, x et y, qui déterminent la taille de l'échiquier (x lignes et y colonnes).

2. La fonction principale TrouverSolutions crée une liste vide appelée "solutions" pour stocker les solutions trouvées.

3. La fonction EstVlide vérifie si une dame peut être placée dans une certaine colonne et ligne sans menacer les autres dames en fonction des règles des échecs.

4. La fonction PlacerDames est récursive et essaie de placer les dames dans chaque colonne. Si une solution est trouvée, elle est ajoutée à la liste "solutions".

5. La fonction renvoie le nombre de solutions trouvées et la liste complète des solutions.

6. Ensuite, le code utilise le package "tidyverse" pour créer un heatmap (carte thermique) qui représente les solutions possibles. Il crée un dataframe à partir des solutions, compte combien de fois chaque position est occupée, puis crée un heatmap en utilisant "ggplot2".

## Exécution
1. Lancer R Studio
2. Créer un nouveau fichier 
3. Copier/Coller le code sur le fichier 
4. Donner une valeur à x et à y  pour tester les solutions suivant la taille de l'échiquier qui vous convient e.g: TrouverSolutions(10,10) pour une grille de 10 fois 10 
```R
TrouverSolutions(10,10) #...ici...


# Charger le package tidyverse (ggplot2)
library(tidyverse)

# Utiliser la fonction avec le nombre de lignes/colonnes de votre choix
x <- 10 #...ici...
y <- 10 #...ici...
```
5. En haut à droite de la page, cliquez sur 'Run' pour déboguer le programme

## Résultats
Résultat de l'exécution pour une grille de 10 fois 10

...

[[722]]
 [1] 9 7 4 1 3 0 6 8 2 5

[[723]]
 [1] 9 7 4 1 3 0 6 8 5 2

[[724]]
 [1] 9 7 4 2 0 5 1 8 6 3

 Le total des combinaisons possibles est de 724

## Conclusion
La documentation décrit comment écrire un script en R pour résoudre le problème du positionnement des dames sur un échiquier. Elle explique les étapes, fournit un exemple de code, et guide l'exécution du script.

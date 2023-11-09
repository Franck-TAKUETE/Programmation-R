TrouverSolutions <- function(x, y) {
  
  if(x <= 0 || y <= 0){
    stop("Entrez des valeurs positives")
  }
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
      print(placement - 1)
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

# Utiliser la fonction avec le nombre de lignes/colonnes de votre choix
x <- 10 #...ici...
y <- 10 #...ici...

nombreDeSolutions = TrouverSolutions(10,10) #...ici...
nombreDeSolutions


# Charger le package tidyverse (ggplot2)
library(tidyverse)


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

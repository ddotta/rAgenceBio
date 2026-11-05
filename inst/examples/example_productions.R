#!/usr/bin/env Rscript
# Exemple : analyse des productions et activités
# Usage : Rscript example_productions.R

suppressPackageStartupMessages({
  library(rAgenceBio)
  library(dplyr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

cat("==========================================\n")
cat(" Analyse des productions et activités bio\n")
cat("==========================================\n")

# 1. Rechercher des opérateurs et analyser leurs productions
cat("\n--- Opérateurs bio viticoles (recherche: 'vigne') ---\n")
vigneron_ops <- ab_search("vigne", limit = 20)
cat("Nombre total d'opérateurs trouvés:", attr(vigneron_ops, "nb_total"), "\n")

flat_prods <- ab_flatten(vigneron_ops, level = "productions")
if (nrow(flat_prods) > 0) {
  cat("\nProductions les plus courantes :\n")
  print(flat_prods %>%
    group_by(production_nom) %>%
    summarise(
      nb_operateurs = n_distinct(numeroBio),
      .groups = "drop"
    ) %>%
    arrange(desc(nb_operateurs)) %>%
    head(10))

  cat("\nÉtat des productions (AB, C1, C2, C3…) :\n")
  print(flat_prods %>%
    group_by(etat) %>%
    summarise(nb = n(), .groups = "drop") %>%
    arrange(desc(nb)))
}

# 2. Opérateurs par activité
cat("\n--- Opérateurs par activité ---\n")
flat_summary <- ab_flatten(vigneron_ops, level = "summary")
if (nrow(flat_summary) > 0) {
  print(flat_summary %>%
    filter(!is.na(activites) & activites != "") %>%
    group_by(activites) %>%
    summarise(nb = n(), .groups = "drop") %>%
    arrange(desc(nb)) %>%
    head(10))
}

# 3. Coordonnées géographiques
cat("\n--- Répartition géographique (latitude / longitude) ---\n")
with_coords <- flat_summary %>%
  filter(!is.na(lat), !is.na(long))
if (nrow(with_coords) > 0) {
  cat("Opérateurs avec coordonnées:", nrow(with_coords), "\n")
  print(with_coords %>%
    summarise(
      lat_min = min(lat), lat_max = max(lat),
      long_min = min(long), long_max = max(long)
    ))
}

cat("\nTerminé.\n")

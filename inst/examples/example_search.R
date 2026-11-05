#!/usr/bin/env Rscript
# Exemple : recherche et analyse d'opérateurs bio
# Usage : Rscript example_search.R

suppressPackageStartupMessages({
  library(rAgenceBio)
  library(dplyr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

cat("==========================================\n")
cat(" Exemple 1 : Recherche par mot-clé\n")
cat("==========================================\n")

brasseries <- ab_search("brasserie", limit = 5)
print(brasseries)

cat("\n==========================================\n")
cat(" Exemple 2 : Recherche par code postal\n")
cat("==========================================\n")

paris_ops <- ab_search(code_postal = "75001", limit = 5)
print(paris_ops)

cat("\n==========================================\n")
cat(" Exemple 3 : Recherche par SIRET\n")
cat("==========================================\n")

siret_ops <- ab_by_siret("51438127600013")
print(siret_ops)

cat("\n==========================================\n")
cat(" Exemple 4 : Fiche complète (numeroBio)\n")
cat("==========================================\n")

op <- ab_get(126851)
if (!is.null(op)) {
  cat("Nom:        ", op$nomAnnuaire %||% op$raisonSociale, "\n")
  cat("NumeroBio:  ", op$numeroBio, "\n")
  cat("SIRET:      ", op$siret, "\n")
  cat("Adresse:    ", op$adressesOperateurs_lieu, ", ",
      op$adressesOperateurs_codePostal, " ", op$adressesOperateurs_ville,
      "\n", sep = "")
  cat("Coordonnées: lat=", op$lat, ", long=", op$long, "\n", sep = "")
  cat("Productions: ", op$productions, "\n")
} else {
  cat("Opérateur 126851 introuvable.\n")
}

cat("\n==========================================\n")
cat(" Exemple 5 : Données aplaties (tidy)\n")
cat("==========================================\n")

ops <- ab_search("producteur", limit = 10)
flat <- ab_flatten(ops, level = "summary")
print(head(flat, 5))

cat("\n==========================================\n")
cat(" Exemple 6 : Suggestions de recherche\n")
cat("==========================================\n")

sugg <- ab_suggestions("vigneron")
print(head(sugg, 5))

cat("\n\nTerminé avec succès.\n")

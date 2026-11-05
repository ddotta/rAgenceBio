#!/usr/bin/env Rscript
# Exemple : analyse géographique des opérateurs bio
# Usage : Rscript example_regions.R

suppressPackageStartupMessages({
  library(rAgenceBio)
  library(dplyr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

cat("==========================================\n")
cat(" Analyse géographique des opérateurs bio\n")
cat("==========================================\n")

# 1. Recherche dans un département (via code postal)
cat("\n--- Opérateurs bio à Lyon (69001) ---\n")
lyon_ops <- ab_search(code_postal = "69001", limit = 10)
flat <- ab_flatten(lyon_ops, level = "summary")
print(flat %>% select(nom, codePostal, ville, activites, cert_etat))

# 2. Analyse des productions
cat("\n--- Productions des opérateurs trouvés ---\n")
prods_flat <- ab_flatten(lyon_ops, level = "productions")
if (nrow(prods_flat) > 0) {
  print(prods_flat %>%
    group_by(production_nom) %>%
    summarise(nb_operateurs = n_distinct(numeroBio), .groups = "drop") %>%
    arrange(desc(nb_operateurs)) %>%
    head(10))
}

# 3. Certificats actifs vs arrêtés
cat("\n--- État des certificats ---\n")
certs_flat <- ab_flatten(lyon_ops, level = "certificates")
if (nrow(certs_flat) > 0) {
  print(certs_flat %>%
    group_by(etatCertification) %>%
    summarise(nb = n(), .groups = "drop") %>%
    arrange(desc(nb)))
}

cat("\nTerminé.\n")

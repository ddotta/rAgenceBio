#' Déplier des colonnes "nom=valeur" aplaties en colonnes séparées
#'
#' Prend un tibble déjà aplati (p. ex. renvoyé par \code{\link{ab_by_siret}}())
#' dont certaines colonnes contiennent des chaînes
#' \code{"nom=valeur; nom=valeur | nom=valeur; ..."} et crée une colonne par
#' champ : \code{adressesOperateurs_id}, \code{adressesOperateurs_lieu},
#' \code{adressesOperateurs_codePostal}, \code{adressesOperateurs_ville},
#' \code{adressesOperateurs_lat}, etc.
#'
#' Chaque cellule est découpée en enregistrements (séparés par
#' \code{sep_records}) puis en champs \code{nom=valeur} (séparés par
#' \code{sep_fields}). Quand une cellule contient plusieurs enregistrements
#' (p. ex. plusieurs adresses), deux comportements sont possibles : en mode
#' large (défaut), les valeurs d'un même champ sont jointes par ", " ; en
#' mode long (\code{long = TRUE}), la ligne est dupliquée une fois par
#' enregistrement (produit cartésien si plusieurs colonnes sont dépliées) et
#' chaque nouvelle colonne porte la valeur d'un seul enregistrement. Les
#' valeurs vides deviennent \code{NA}. Le type des nouvelles colonnes est
#' inféré automatiquement : entier, numérique, logique, sinon caractère ;
#' les identifiants à conserver comme texte (code postal, code commune,
#' SIRET, téléphones, URL, dates, ...) restent en caractères.
#'
#' @param x Un tibble dont certaines colonnes contiennent des chaînes
#'   \code{"nom=valeur"} (p. ex. le résultat de \code{ab_by_siret()}).
#' @param columns Caractère. Noms des colonnes à déplier (défaut :
#'   \code{"adressesOperateurs"}).
#' @param sep_fields,sep_records Séparateurs employés dans les chaînes
#'   aplaties : entre champs \code{nom=valeur} et entre enregistrements.
#' @param long Logique. Si \code{TRUE}, une ligne par enregistrement :
#'   les lignes dont une cellule contient plusieurs enregistrements sont
#'   dupliquées (défaut : \code{FALSE}, les valeurs sont jointes par ", ").
#' @return Le tibble \code{x} enrichi des nouvelles colonnes
#'   \code{<colonne>_<champ>}, placées juste après chaque colonne dépliée.
#'   En mode \code{long = TRUE}, le nombre de lignes peut augmenter.
#'
#' @examples
#' \dontrun{
#' ops <- ab_by_siret("51438127600013")
#' ops <- ab_expand(ops)
#' print(ops$adressesOperateurs_lieu)
#' print(ops$adressesOperateurs_codePostal)
#' print(ops$adressesOperateurs_lat)
#' }
#' @export
ab_expand <- function(x, columns = "adressesOperateurs",
                      sep_fields = "; ", sep_records = " | ",
                      long = FALSE) {
  if (!is.data.frame(x)) {
    stop("`x` doit être un data.frame (p. ex. le résultat de ab_by_siret()).",
         call. = FALSE)
  }
  columns <- as.character(columns)
  missing <- setdiff(columns, names(x))
  if (length(missing) > 0) {
    stop("Colonne(s) absente(s) de `x` : ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (isTRUE(long)) {
    return(.ab_expand_long(x, columns, sep_fields, sep_records))
  }

  # Déplie chaque colonne demandée en un bloc de colonnes "<col>_<clé>".
  new_cols <- list()
  for (col in columns) {
    vals <- unname(vapply(x[[col]], function(v) {
      if (length(v) == 1L && !is.na(v)) as.character(v) else NA_character_
    }, character(1)))

    parsed <- lapply(vals, .ab_parse_kv,
                     sep_fields = sep_fields, sep_records = sep_records)

    # Union ordonnée des clés rencontrées sur toutes les lignes.
    keys <- unique(unlist(lapply(parsed, function(p) {
      unique(unlist(lapply(p, names), use.names = FALSE))
    }), use.names = FALSE))
    if (length(keys) == 0) {
      next
    }

    for (k in keys) {
      v <- vapply(parsed, function(p) {
        hits <- unlist(lapply(p, function(r) r[[k]]), use.names = FALSE)
        hits <- hits[!is.na(hits)]
        if (length(hits) == 0) {
          NA_character_
        } else {
          paste(unique(hits), collapse = ", ")
        }
      }, character(1))
      new_cols[[paste0(col, "_", k)]] <- .ab_coerce_kv(v, key = k)
    }
  }

  if (length(new_cols) == 0) {
    return(x)
  }

  # Retire d'éventuelles colonnes homonymes (une nouvelle exécution sur le
  # même tibble reste propre), puis ajoute les nouvelles colonnes.
  x <- x[setdiff(names(x), names(new_cols))]
  out <- dplyr::bind_cols(x, dplyr::as_tibble(new_cols))

  # Replace chaque bloc de nouvelles colonnes juste après sa colonne source.
  for (col in rev(columns)) {
    block <- names(new_cols)[startsWith(names(new_cols), paste0(col, "_"))]
    if (length(block) == 0 || !(col %in% names(out))) {
      next
    }
    out <- dplyr::relocate(out, dplyr::all_of(block),
                           .after = dplyr::all_of(col))
  }
  out
}

# Clés qui doivent rester du texte même quand elles ressemblent à des
# nombres : codes à zéros initiaux, identifiants longs, téléphones, URL,
# dates.
.ab_text_keys <- c(
  "codePostal", "codeCommune", "siret", "code", "codeNAF",
  "telephone", "telephoneNational", "telephoneCommerciale",
  "numeroControleEu", "url", "dateMaj", "datePremierEngagement",
  "dateEngagement", "dateArret", "dateSuspension"
)

# Analyse une chaîne "nom=valeur; nom=valeur | nom=valeur; ..." en liste de
# listes nommées (une par enregistrement). Les valeurs vides deviennent NA.
# Retourne list() quand la cellule est vide ou NA.
.ab_parse_kv <- function(x, sep_fields = "; ", sep_records = " | ") {
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    return(list())
  }
  records <- strsplit(x, sep_records, fixed = TRUE)[[1L]]
  records <- trimws(records)
  records <- records[nzchar(records)]
  if (length(records) == 0) {
    return(list())
  }
  lapply(records, function(rec) {
    fields <- strsplit(rec, sep_fields, fixed = TRUE)[[1L]]
    fields <- trimws(fields)
    fields <- fields[nzchar(fields)]
    out <- list()
    for (f in fields) {
      # Découpe sur le PREMIER "=" uniquement : contrairement à
      # strsplit(..., fixed = TRUE), qui supprime le morceau vide final sur
      # certaines versions de R ("dateSuspension=" deviendrait
      # "dateSuspension"), un champ "nom=" produit ici une valeur vide (NA),
      # pas une perte de clé. Une valeur contenant "=" (p. ex. une URL avec
      # paramètres) est conservée entière.
      eq <- regexpr("=", f, fixed = TRUE)[1L]
      if (eq < 1L) {
        next
      }
      nm <- trimws(substr(f, 1L, eq - 1L))
      val <- trimws(substr(f, eq + 1L, nchar(f)))
      if (!nzchar(nm)) {
        next
      }
      out[[nm]] <- if (nzchar(val)) val else NA_character_
    }
    out
  })
}

# Infère le type d'un vecteur de valeurs analysé : entier, numérique,
# logique, sinon caractère. La conversion n'a lieu que si TOUTES les valeurs
# non manquantes l'autorisent (une cellule multi-enregistrements jointe par
# ", " empêche la conversion et la colonne reste en caractères). Les clés
# devant rester du texte (cf. .ab_text_keys) ne sont jamais converties.
.ab_coerce_kv <- function(v, key = NULL) {
  if (!is.null(key) && key %in% .ab_text_keys) {
    return(v)
  }
  vals <- v[!is.na(v)]
  if (length(vals) == 0) {
    return(v)
  }
  # Entier : signe optionnel, aucun zéro initial (sinon "06000" deviendrait
  # 6000) et valeur dans les limites d'un entier R.
  if (all(grepl("^-?(0|[1-9][0-9]*)$", vals))) {
    num <- suppressWarnings(as.numeric(vals))
    if (all(!is.na(num)) && all(abs(num) <= .Machine$integer.max)) {
      return(suppressWarnings(as.integer(v)))
    }
    # Trop grand pour un entier : on garde du texte (identifiant long).
    return(v)
  }
  if (all(grepl("^-?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][-+]?[0-9]+)?$",
                vals))) {
    return(suppressWarnings(as.numeric(v)))
  }
  if (all(vals %in% c("TRUE", "FALSE"))) {
    out <- rep(NA, length(v))
    out[v %in% "TRUE"] <- TRUE
    out[v %in% "FALSE"] <- FALSE
    return(out)
  }
  v
}

# Interne : version "longue" d'ab_expand(). Chaque ligne dont une cellule
# contient plusieurs enregistrements est dupliquée une fois par
# enregistrement ; si plusieurs colonnes sont dépliées, le produit
# cartésien de leurs enregistrements est appliqué (la dernière colonne
# varie le plus vite). Les lignes sans enregistrement sont conservées avec
# des NA.
.ab_expand_long <- function(x, columns, sep_fields, sep_records) {
  parsed <- lapply(columns, function(col) {
    vals <- unname(vapply(x[[col]], function(v) {
      if (length(v) == 1L && !is.na(v)) as.character(v) else NA_character_
    }, character(1)))
    lapply(vals, .ab_parse_kv,
           sep_fields = sep_fields, sep_records = sep_records)
  })
  names(parsed) <- columns

  n_recs <- lapply(parsed, lengths)         # enregistrements par cellule
  safe <- lapply(n_recs, pmax, 1L)          # 1 pour les cellules vides
  reps <- Reduce(`*`, safe)                 # doublons par ligne source
  src_idx <- rep(seq_len(nrow(x)), reps)

  n_cols <- length(columns)
  new_cols <- list()
  for (i in seq_len(n_cols)) {
    col <- columns[[i]]
    # Taille de bloc des colonnes suivantes : la position de
    # l'enregistrement de la colonne i varie toutes les `block` lignes du
    # produit cartésien.
    block <- rep(1L, nrow(x))
    if (i < n_cols) {
      block <- Reduce(`*`, safe[(i + 1L):n_cols])
    }
    rec_idx <- unlist(lapply(seq_len(nrow(x)), function(r) {
      ((seq_len(reps[r]) - 1L) %/% block[r]) %% safe[[i]][r] + 1L
    }), use.names = FALSE)

    keys <- unique(unlist(lapply(parsed[[i]], function(p) {
      unique(unlist(lapply(p, names), use.names = FALSE))
    }), use.names = FALSE))
    if (length(keys) == 0) {
      next
    }

    for (k in keys) {
      v <- unname(vapply(seq_along(rec_idx), function(j) {
        r <- src_idx[j]
        recs <- parsed[[i]][[r]]
        if (length(recs) == 0) {
          return(NA_character_)
        }
        rec <- recs[[min(rec_idx[j], length(recs))]]
        val <- rec[[k]]
        if (is.null(val) || is.na(val)) NA_character_ else as.character(val)
      }, character(1)))
      new_cols[[paste0(col, "_", k)]] <- .ab_coerce_kv(v, key = k)
    }
  }

  if (length(new_cols) == 0) {
    return(x)
  }

  # Duplique les lignes sources puis ajoute les nouvelles colonnes.
  out <- x[src_idx, , drop = FALSE]
  out <- out[setdiff(names(out), names(new_cols))]
  out <- dplyr::bind_cols(out, dplyr::as_tibble(new_cols))

  # Replace chaque bloc de nouvelles colonnes juste après sa colonne source.
  for (col in rev(columns)) {
    block <- names(new_cols)[startsWith(names(new_cols), paste0(col, "_"))]
    if (length(block) == 0 || !(col %in% names(out))) {
      next
    }
    out <- dplyr::relocate(out, dplyr::all_of(block),
                           .after = dplyr::all_of(col))
  }
  out
}


#' Aplatir les enregistrements d'opérateurs imbriqués en tibble tidy
#'
#' Prend le résultat de \code{\link{ab_search}}() ou un enregistrement
#' d'opérateur isolé (p. ex. issu de \code{\link{ab_get}}(flatten = FALSE))
#' et déplie
#' les structures JSON imbriquées (adresses, productions, activités,
#' certificats) en un tibble plat, prêt pour l'analyse.
#'
#' Chaque opérateur peut avoir plusieurs adresses, productions et
#' certificats. La fonction utilise un format long/tidy :
#' une ligne par paire (opérateur, production) ou (opérateur, adresse).
#'
#' @param x Un tibble renvoyé par \code{ab_search()} (avec une
#'   colonne-liste \code{raw} contenant les enregistrements complets des
#'   opérateurs) ou tout tibble d'enregistrements d'opérateurs imbriqués
#'   (champs de l'API répartis en colonnes). Un enregistrement d'opérateur
#'   isolé (p. ex. issu de \code{ab_get(..., flatten = FALSE)}) est
#'   également accepté. Noter que \code{ab_by_siret()} et \code{ab_get()}
#'   renvoient désormais des données déjà aplaties par défaut ;
#'   \code{ab_flatten()} reste utile sur \code{ab_search()} et sur la
#'   liste brute d'\code{ab_get(..., flatten = FALSE)}.
#' @param level Caractère. Niveau de détail de l'aplatissement :
#'   \describe{
#'   \item{summary}{(défaut) une ligne par opérateur, avec les champs
#'     d'adresse et d'activité résumés.}
#'   \item{productions}{une ligne par paire (opérateur, production).}
#'   \item{addresses}{une ligne par paire (opérateur, adresse).}
#'   \item{certificates}{une ligne par paire (opérateur, certificat).}
#'   }
#' @return Un \code{tibble} avec des colonnes plates adaptées au niveau
#'   choisi.
#'
#' @examples
#' \dontrun{
#' # Rechercher puis aplatir au niveau résumé
#' ops <- ab_search("brasserie", limit = 10)
#' summary_flat <- ab_flatten(ops, level = "summary")
#' print(summary_flat)
#'
#' # Une ligne par production
#' prod_flat <- ab_flatten(ops, level = "productions")
#'
#' # Une ligne par adresse
#' addr_flat <- ab_flatten(ops, level = "addresses")
#' }
#' @export
ab_flatten <- function(x, level = c("summary", "productions", "addresses", "certificates")) {
  level <- match.arg(level)

  # Permet de passer directement un enregistrement d'opérateur isolé
  # (p. ex. le résultat d'ab_get()) en l'enveloppant dans un tibble à une
  # ligne avec une colonne "raw".
  if (is.list(x) && !is.data.frame(x) && !("raw" %in% names(x))) {
    x <- dplyr::tibble(raw = list(x))
  }

  if ("raw" %in% names(x)) {
    raw_list <- x$raw
  } else if (is.data.frame(x)) {
    # ab_by_siret() renvoie les champs de l'API directement en colonnes :
    # chaque ligne est un enregistrement d'opérateur complet.
    raw_list <- lapply(seq_len(nrow(x)), function(i) {
      ab_unwrap(as.list(x[i, , drop = FALSE]))
    })
  } else {
    stop("Input must have a 'raw' list-column (use ab_search() or ab_by_siret()).",
         call. = FALSE)
  }

  # Filtre les NULL
  raw_list <- raw_list[!vapply(raw_list, is.null, logical(1))]

  if (length(raw_list) == 0) {
    return(dplyr::tibble())
  }

  switch(level,
    summary = flatten_summary(raw_list),
    productions = flatten_productions(raw_list),
    addresses = flatten_addresses(raw_list),
    certificates = flatten_certificates(raw_list)
  )
}

#' @keywords internal
#' @noRd
# Déplie les enveloppes de liste de longueur 1 créées par `$` sur les
# colonnes-listes d'un data.frame à 1 ligne (p. ex. op$productions renvoie
# list(df), et non df).
ab_unwrap <- function(x) {
  if (is.data.frame(x)) {
    return(x)
  }
  # Ne déplier une liste de longueur 1 que lorsque son unique élément est
  # lui-même une liste (ou data.frame) : c'est l'enveloppe créée par `$`
  # sur les colonnes-listes. Une liste nommée de longueur 1 contenant une
  # valeur atomique est un vrai enregistrement ; on la conserve.
  if (is.list(x) && length(x) == 1 && is.list(x[[1]])) {
    return(ab_unwrap(x[[1]]))
  }
  if (is.list(x)) {
    return(lapply(x, ab_unwrap))
  }
  x
}

#' @keywords internal
#' @noRd
# Convertit un conteneur imbriqué (data.frame, liste ou NULL) en liste de
# listes nommées, une par enregistrement.
ab_records <- function(x) {
  x <- ab_unwrap(x)
  if (is.null(x) || length(x) == 0) {
    return(list())
  }
  if (is.data.frame(x)) {
    if (nrow(x) == 0) {
      return(list())
    }
    return(lapply(seq_len(nrow(x)), function(i) {
      ab_unwrap(as.list(x[i, , drop = FALSE]))
    }))
  }
  if (is.list(x)) {
    # Un enregistrement unique est une liste nommée dont tous les
    # éléments sont atomiques.
    if (all(vapply(x, function(v) !is.list(v), logical(1)))) {
      return(list(x))
    }
    # Une liste d'enregistrements : normaliser chaque élément
    # (data.frame ou liste) en liste nommée par enregistrement.
    return(lapply(x, function(v) {
      v <- ab_unwrap(v)
      if (is.data.frame(v)) {
        if (nrow(v) == 0) {
          return(list())
        }
        return(ab_unwrap(as.list(v[1, , drop = FALSE])))
      }
      v
    }))
  }
  list()
}

#' @keywords internal
#' @noRd
# Premier enregistrement d'un conteneur imbriqué, ou une liste vide s'il
# n'y en a aucun.
ab_first_record <- function(x) {
  recs <- ab_records(x)
  if (length(recs) == 0) list() else recs[[1]]
}

#' @keywords internal
#' @noRd
# Extrait les libellés d'un conteneur imbriqué d'éléments nommés (utilisé
# pour les types d'adresse : data.frame avec une colonne "label", liste ou
# caractère).
ab_labels <- function(x) {
  x <- ab_unwrap(x)
  if (is.null(x)) {
    return(character(0))
  }
  if (is.character(x) || is.factor(x)) {
    return(as.character(x))
  }
  if (is.data.frame(x)) {
    for (f in c("label", "libelle", "nom")) {
      if (f %in% names(x)) {
        return(as.character(x[[f]]))
      }
    }
    return(character(0))
  }
  if (is.list(x)) {
    # Un simple vecteur de libellés stocké sous forme de liste.
    # as.character() sur une liste produirait "c(\"a\", \"b\")" ; on
    # déplie donc les éléments atomiques avant conversion.
    if (all(vapply(x, function(v) is.null(v) || is.atomic(v), logical(1)))) {
      return(as.character(unlist(x)))
    }
    recs <- ab_records(x)
    return(vapply(recs, function(r) {
      if (is.null(r)) {
        return("")
      }
      if (is.list(r)) {
        as.character(r$label %||% r$libelle %||% r$nom %||% "")
      } else {
        as.character(r[1])
      }
    }, character(1)))
  }
  as.character(x)
}

#' @keywords internal
#' @noRd
# Extrait une coordonnée d'un enregistrement d'adresse, en cherchant
# d'abord dans l'enregistrement lui-même puis dans son élément imbriqué
# "location".
ab_coord <- function(rec, field) {
  if (is.null(rec)) {
    return(NA_real_)
  }
  v <- rec[[field]]
  if (!is.null(v) && length(v) > 0) {
    return(as.numeric(v[[1]]))
  }
  loc <- ab_first_record(rec$location)
  v <- loc[[field]]
  if (!is.null(v) && length(v) > 0) {
    return(as.numeric(v[[1]]))
  }
  NA_real_
}

#' @keywords internal
#' @noRd
flatten_summary <- function(raw_list) {
  records <- lapply(raw_list, function(op) {
    addr <- ab_first_record(op$adressesOperateurs)

    activites <- ab_records(op$activites)
    act_noms <- if (length(activites) > 0) {
      paste(vapply(activites, function(a) a$nom %||% a$nomAnnuaire %||% "",
                   character(1)),
            collapse = "; ")
    } else ""

    productions <- ab_records(op$productions)
    prod_noms <- if (length(productions) > 0) {
      paste(vapply(productions, function(p) p$nom %||% "", character(1)),
            collapse = "; ")
    } else ""

    cert <- ab_first_record(op$certificats)
    site <- ab_first_record(op$siteWebs)

    list(
      id = op$id %||% NA_integer_,
      nom = op$nomAnnuaire %||% op$raisonSociale %||% NA_character_,
      nomAnnuaire = op$nomAnnuaire %||% NA_character_,
      raisonSociale = op$raisonSociale %||% NA_character_,
      siret = op$siret %||% NA_character_,
      numeroBio = op$numeroBio %||% NA_integer_,
      codeNAF = op$codeNAF %||% NA_character_,
      gerant = op$gerant %||% NA_character_,
      codePostal = addr$codePostal %||% NA_character_,
      ville = addr$ville %||% NA_character_,
      lat = ab_coord(addr, "lat"),
      long = ab_coord(addr, "long"),
      codeCommune = addr$codeCommune %||% NA_character_,
      pays = addr$pays %||% NA_character_,
      typeAdresse = paste(ab_labels(addr$typeAdresseOperateurs), collapse = "; "),
      activites = act_noms,
      productions = prod_noms,
      cert_organisme = cert$organisme %||% "",
      cert_etat = cert$etatCertification %||% "",
      dateEngagement = cert$dateEngagement %||% cert$dateDemarrage %||% NA_character_,
      siteWeb = site$url %||% "",
      dateMaj = op$dateMaj %||% NA_character_
    )
  })

  dplyr::as_tibble(do.call(rbind, lapply(records, function(r) as.data.frame(r, stringsAsFactors = FALSE))))
}

#' @keywords internal
#' @noRd
flatten_productions <- function(raw_list) {
  rows <- list()
  for (op in raw_list) {
    prods <- ab_records(op$productions)
    if (length(prods) == 0) next

    for (p in prods) {
      etat <- ab_first_record(p$etatProductions)
      rows[[length(rows) + 1]] <- list(
        id = op$id %||% NA_integer_,
        nom = op$nomAnnuaire %||% op$raisonSociale %||% NA_character_,
        numeroBio = op$numeroBio %||% NA_integer_,
        siret = op$siret %||% NA_character_,
        production_code = p$code %||% NA_character_,
        production_nom = p$nom %||% NA_character_,
        etat = etat$etatProduction %||% "",
        anneeReference = as.integer(etat$anneeReferenceControle %||% NA)
      )
    }
  }

  if (length(rows) == 0) return(dplyr::tibble())
  dplyr::as_tibble(do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE))))
}

#' @keywords internal
#' @noRd
flatten_addresses <- function(raw_list) {
  rows <- list()
  for (op in raw_list) {
    addrs <- ab_records(op$adressesOperateurs)
    if (length(addrs) == 0) next

    for (a in addrs) {
      rows[[length(rows) + 1]] <- list(
        id = op$id %||% NA_integer_,
        nom = op$nomAnnuaire %||% op$raisonSociale %||% NA_character_,
        numeroBio = op$numeroBio %||% NA_integer_,
        siret = op$siret %||% NA_character_,
        lieu = a$lieu %||% NA_character_,
        codePostal = a$codePostal %||% NA_character_,
        ville = a$ville %||% NA_character_,
        codeCommune = a$codeCommune %||% NA_character_,
        lat = ab_coord(a, "lat"),
        long = ab_coord(a, "long"),
        pays = a$pays %||% NA_character_,
        typeAdresse = paste(ab_labels(a$typeAdresseOperateurs), collapse = "; "),
        active = a$active %||% NA
      )
    }
  }

  if (length(rows) == 0) return(dplyr::tibble())
  dplyr::as_tibble(do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE))))
}

#' @keywords internal
#' @noRd
flatten_certificates <- function(raw_list) {
  rows <- list()
  for (op in raw_list) {
    certs <- ab_records(op$certificats)
    if (length(certs) == 0) next

    for (c in certs) {
      rows[[length(rows) + 1]] <- list(
        id = op$id %||% NA_integer_,
        nom = op$nomAnnuaire %||% op$raisonSociale %||% NA_character_,
        numeroBio = op$numeroBio %||% NA_integer_,
        siret = op$siret %||% NA_character_,
        organisme = c$organisme %||% NA_character_,
        numeroControleEu = c$numeroControleEu %||% NA_character_,
        etatCertification = c$etatCertification %||% NA_character_,
        dateEngagement = c$dateEngagement %||% c$dateDemarrage %||% NA_character_,
        dateArret = c$dateArret %||% NA_character_,
        dateSuspension = c$dateSuspension %||% NA_character_,
        url = c$url %||% NA_character_
      )
    }
  }

  if (length(rows) == 0) return(dplyr::tibble())
  dplyr::as_tibble(do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE))))
}

#' @keywords internal
#' @noRd
# Convertit récursivement une valeur imbriquée (data.frame, liste, vecteur)
# en une chaîne lisible : champs "nom=valeur" séparés par `sep_fields`,
# enregistrements séparés par `sep_records`. Les valeurs manquantes sont
# rendues vides ("") et les vecteurs sont joints par ", ".
.ab_collapse <- function(x, sep_fields = "; ", sep_records = " | ") {
  x <- ab_unwrap(x)
  if (is.null(x) || length(x) == 0) {
    return("")
  }
  if (is.data.frame(x)) {
    if (nrow(x) == 0) {
      return("")
    }
    rows <- lapply(seq_len(nrow(x)), function(i) {
      r <- as.list(x[i, , drop = FALSE])
      fields <- vapply(names(r), function(nm) {
        v <- r[[nm]]
        val <- if (is.list(v)) {
          .ab_collapse(v, sep_fields = sep_fields, sep_records = sep_records)
        } else {
          paste(.ab_fmt_atomic(v), collapse = ", ")
        }
        paste0(nm, "=", val)
      }, character(1))
      paste(fields, collapse = sep_fields)
    })
    return(paste(rows, collapse = sep_records))
  }
  if (is.list(x)) {
    return(paste(
      vapply(x, function(v) {
        .ab_collapse(v, sep_fields = sep_fields, sep_records = sep_records)
      }, character(1)),
      collapse = sep_records
    ))
  }
  paste(.ab_fmt_atomic(x), collapse = ", ")
}

#' @keywords internal
#' @noRd
# Formate un vecteur atomique en caractère, NA devenant "".
.ab_fmt_atomic <- function(x) {
  vapply(x, function(z) {
    if (length(z) == 1L && is.na(z)) "" else as.character(z)
  }, character(1))
}

#' @keywords internal
#' @noRd
# Aplatit un data.frame d'enregistrements d'opérateurs (une ligne par
# opérateur) en un tibble dont TOUTES les colonnes sont atomiques :
# - les colonnes data.frame (objets imbriqués, p. ex. venteAnnuaire) sont
#   éclatées en colonnes "champ.sousChamp" ;
# - les colonnes-listes (champs multi-valués : adresses, productions,
#   certificats, catégories, ...) sont converties en chaînes lisibles via
#   .ab_collapse().
.ab_flatten_columns <- function(items) {
  out <- list()
  for (nm in names(items)) {
    col <- items[[nm]]
    if (is.data.frame(col)) {
      for (cn in names(col)) {
        sub <- col[[cn]]
        out[[paste0(nm, ".", cn)]] <- if (is.list(sub)) {
          vapply(sub, .ab_collapse, character(1))
        } else {
          sub
        }
      }
    } else if (is.list(col)) {
      out[[nm]] <- vapply(col, .ab_collapse, character(1))
    } else {
      out[[nm]] <- col
    }
  }
  dplyr::as_tibble(out)
}


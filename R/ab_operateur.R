#' Obtenir un opérateur bio par numeroBio
#'
#' Récupère l'enregistrement complet d'un opérateur à l'aide de son
#' identifiant \code{numeroBio} (un entier unique attribué par
#' l'Agence Bio).
#'
#' Utilise l'API open data \url{https://opendata.agencebio.org}.
#'
#' @param numeroBio Entier ou chaîne numérique. Le numéro bio unique
#'   de l'opérateur.
#' @param flatten Logique. Si \code{TRUE} (défaut), renvoie un tibble à une
#'   colonne par champ, entièrement aplati et déplié (comme
#'   \code{\link{ab_by_siret}}(), une ligne par adresse). Si \code{FALSE},
#'   renvoie l'enregistrement brut sous forme de liste.
#' @return Un \code{tibble} entièrement aplati, une ligne par adresse (ou la
#'   liste brute de l'enregistrement si \code{flatten = FALSE}),
#'   ou \code{NULL} si l'opérateur est introuvable.
#'
#' @examples
#' \dontrun{
#' # Obtenir un opérateur : tibble entièrement aplati (défaut)
#' op <- ab_get(125787)
#' print(op$raisonSociale)
#' print(op$adressesOperateurs_codePostal)
#' print(op$productions)
#'
#' # Enregistrement brut sous forme de liste
#' raw <- ab_get(125787, flatten = FALSE)
#' }
#' @export
ab_get <- function(numeroBio, flatten = TRUE) {
  numeroBio <- as.character(numeroBio)
  numeroBio <- trimws(numeroBio[!is.na(numeroBio)])
  numeroBio <- unique(numeroBio[nzchar(numeroBio)])
  if (length(numeroBio) == 0) {
    return(NULL)
  }
  
  pb <- .ab_progress_new(length(numeroBio), "Recherche par numeroBio")
  on.exit(.ab_progress_done(pb), add = TRUE)
  
  results <- lapply(seq_along(numeroBio), function(i) {
    res <- .ab_get_one(numeroBio[[i]])
    .ab_progress_tick(pb, i)
    res
  })
  names(results) <- numeroBio
  results <- results[!vapply(results, is.null, logical(1))]
  
  if (length(results) == 0) {
    return(NULL)
  }
  
  if (isTRUE(flatten)) {
    items <- dplyr::bind_rows(unname(results))
    return(.ab_flatten_items(items, expand = TRUE, long = TRUE))
  }
  
  # un seul numéro : l'enregistrement brut ; plusieurs : liste nommée par numeroBio
  raw <- lapply(results, function(it) ab_unwrap(as.list(it[1, , drop = FALSE])))
  if (length(numeroBio) == 1) raw[[1]] else raw
}

.ab_get_one <- function(nb) {
  data <- .ab_request(
    url = paste0(.ab_base_opendata, "/api/gouv/operateurs/"),
    query = list(numeroBio = nb),
    error_on_404 = FALSE   # un numéro inconnu est ignoré, il n'interrompt pas la boucle
  )
  
  if (is.null(data) || length(data) == 0 ||
      (is.data.frame(data) && nrow(data) == 0)) {
    return(NULL)
  }
  
  if (is.data.frame(data)) {
    return(dplyr::as_tibble(data[1, , drop = FALSE]))
  }
  rec <- ab_unwrap(data[[1]])
  if (is.data.frame(rec)) {
    dplyr::as_tibble(rec[1, , drop = FALSE])
  } else {
    dplyr::as_tibble(lapply(rec, function(v) {
      if (is.data.frame(v) || is.list(v)) list(v) else v
    }))
  }
}

#' Interne : normaliser l'élément `items` de l'API en tibble
#' d'enregistrements
#'
#' Selon le nombre de correspondances, l'API renvoie un élément `items`
#' de formes différentes : \code{NULL} ou une liste vide quand rien ne
#' correspond, un data.frame quand un ou plusieurs enregistrements
#' correspondent, et parfois une simple liste d'enregistrements. Cette
#' fonction normalise toutes ces formes en un tibble avec une ligne par
#' enregistrement d'opérateur (0 ligne en l'absence de correspondance),
#' afin que les appelants n'aient jamais à deviner la forme.
#'
#' @param items L'élément \code{items} d'une réponse API analysée.
#' @return Un \code{tibble} avec une ligne par enregistrement d'opérateur.
#' @keywords internal
#' @noRd
.ab_items_df <- function(items) {
  if (is.null(items) || length(items) == 0) {
    return(dplyr::tibble())
  }
  if (is.data.frame(items)) {
    return(dplyr::as_tibble(items))
  }
  # Une simple liste : soit un enregistrement unique (une liste nommée
  # d'éléments atomiques), soit une liste d'enregistrements.
  # ab_records() normalise ces deux formes en listes nommées, une par
  # enregistrement.
  recs <- ab_records(items)
  recs <- recs[!vapply(recs, function(r) is.null(r) || length(r) == 0,
                       logical(1))]
  if (length(recs) == 0) {
    return(dplyr::tibble())
  }
  dplyr::bind_rows(lapply(recs, dplyr::as_tibble))
}

#' Obtenir un ou plusieurs opérateurs bio par SIRET
#'
#' Recherche un ou plusieurs opérateurs à l'aide de leur numéro SIRET
#' (identifiant d'entreprise français à 14 chiffres).
#'
#' Utilise l'API open data \url{https://opendata.agencebio.org}.
#'
#' @param siret Caractère (ou vecteur de caractères). Un ou plusieurs
#'   numéros SIRET, chacun composé d'exactement 14 chiffres. Les SIRET
#'   invalides (un avertissement est émis) ou inconnus sont ignorés.
#' @param expand Logique. Si \code{TRUE} (défaut), les champs multi-valués
#'   aplatis (\code{adressesOperateurs}, \code{categories},
#'   \code{activites}, \code{certificats}, \code{annuaireActivites},
#'   \code{siteWebs}) sont dépliés en colonnes séparées via
#'   \code{\link{ab_expand}}() : \code{adressesOperateurs_lieu},
#'   \code{adressesOperateurs_codePostal}, \code{categories_nom}, etc.
#'   La colonne \code{productions} reste une chaîne lisible : ses
#'   sous-enregistrements imbriqués (\code{etatProductions}) ne peuvent pas
#'   être dépliés de façon fiable. Pour une ligne par production, utiliser
#'   \code{\link{ab_flatten}}(x, level = "productions").
#' @param long Logique. Si \code{TRUE} (défaut), une ligne par adresse :
#'   les opérateurs possédant plusieurs adresses sont dupliqués, chaque
#'   ligne portant sa propre adresse (\code{codePostal}, \code{ville},
#'   \code{lat}, \code{long} et colonnes \code{adressesOperateurs_*}).
#'   Si \code{FALSE}, une ligne par opérateur et les valeurs des adresses
#'   multiples sont jointes par ", ".
#' @return Un \code{tibble} entièrement aplati : toutes les colonnes sont
#'   atomiques (aucun data.frame imbriqué). Les champs scalaires de l'API
#'   deviennent des colonnes directes ; les objets imbriqués (p. ex.
#'   \code{venteAnnuaire}) sont éclatés en colonnes
#'   \code{champ.sousChamp} ; les champs multi-valués
#'   (\code{adressesOperateurs}, \code{productions}, \code{certificats},
#'   \code{categories}, \code{activites}, ...) sont convertis en chaînes
#'   lisibles, puis dépliés en colonnes \code{<champ>_<sousChamp>} quand
#'   \code{expand = TRUE} (sauf \code{productions}, voir ci-dessus). Les
#'   colonnes pratiques \code{codePostal}, \code{ville}, \code{lat} et
#'   \code{long} sont extraites de la première adresse (ou de chaque adresse
#'   en mode \code{long = TRUE}, qui duplique alors les lignes). Un tibble
#'   de 0 ligne est renvoyé quand aucun SIRET ne correspond.
#'
#' @examples
#' \dontrun{
#' # Rechercher un seul SIRET
#' ops <- ab_by_siret("51438127600013")
#' print(ops)
#'
#' # Rechercher plusieurs SIRET (les inconnus sont ignorés)
#' ops2 <- ab_by_siret(c("51438127600013", "12345678900011"))
#' }
#' @export
ab_by_siret <- function(siret, expand = TRUE, long = TRUE) {
  # ab_by_siret accepte un ou plusieurs SIRET ; l'API n'en traite qu'un à
  # la fois. On boucle dessus et on concatène les résultats, avec une barre
  # de progression en session interactive.
  siret <- as.character(siret)
  pb <- .ab_progress_new(length(siret), "Recherche par SIRET")
  on.exit(.ab_progress_done(pb), add = TRUE)
  results <- lapply(seq_along(siret), function(i) {
    s <- trimws(siret[[i]])
    if (!grepl("^[0-9]{14}$", s)) {
      warning("Ignoring invalid SIRET (must be 14 digits): ", s, call. = FALSE)
      return(NULL)
    }
    data <- .ab_request(
      url = paste0(.ab_base_opendata, "/api/gouv/operateurs/"),
      query = list(siret = s)
    )
    items <- .ab_items_df(data$items)
    if (nrow(items) == 0) {
      return(NULL)
    }
    if (!"siret" %in% names(items)) {
      items$siret <- s
    }
    .ab_progress_tick(pb, i)
    # Chaque champ de l'API devient une colonne : une ligne par
    # opérateur trouvé.
    items
  })

  results <- results[!vapply(results, is.null, logical(1))]
  if (length(results) == 0) {
    return(dplyr::tibble())
  }

  items <- dplyr::bind_rows(results)

  # Aplatit (objets éclatés, champs multi-valués en chaînes lisibles) puis
  # déplie en colonnes séparées ; avec long = TRUE (défaut), une ligne par
  # adresse quand un opérateur en possède plusieurs.
  .ab_flatten_items(items, expand = expand, long = long)
}

# Interne : aplatir un data.frame d'enregistrements bruts (une ligne par
# opérateur) en tibble entièrement plat :
# - les objets imbriqués sont éclatés en colonnes "champ.sousChamp" ;
# - les champs multi-valués sont convertis en chaînes lisibles puis dépliés
#   en colonnes "<champ>_<sousChamp>" (adressesOperateurs, categories,
#   activites, certificats, annuaireActivites, siteWebs ; productions est
#   exclu, voir ab_expand) ;
# - avec long = TRUE, une ligne par adresse (les lignes sont dupliquées) et
#   les colonnes pratiques codePostal/ville/lat/long suivent chaque adresse ;
# - post_cols : colonnes supplémentaires (p. ex. nomAnnuaire, raw) ajoutées
#   AVANT la duplication éventuelle des lignes, donc répétées avec elles.
.ab_flatten_items <- function(items, expand = TRUE, long = TRUE,
                              prefer_cp = NULL, post_cols = NULL) {
  flat <- .ab_flatten_columns(items)

  # Colonnes supplémentaires (nomAnnuaire, raw...) attachées avant la
  # duplication éventuelle en mode long.
  if (!is.null(post_cols)) {
    for (nm in names(post_cols)) {
      flat[[nm]] <- post_cols[[nm]]
    }
  }

  if (isTRUE(expand) && nrow(flat) > 0) {
    if ("adressesOperateurs" %in% names(flat)) {
      flat <- ab_expand(flat, columns = "adressesOperateurs", long = long)
    }
    flat <- ab_expand(flat, columns = intersect(
      c("categories", "siteWebs", "activites", "certificats",
        "annuaireActivites"),
      names(flat)
    ), long = FALSE)
  }

  # Colonnes pratiques codePostal/ville/lat/long. En mode long, chaque ligne
  # porte sa propre adresse : on reprend donc les colonnes dépliées, champ
  # par champ (quand le champ existe). Sinon, on garde les éventuels champs
  # de premier niveau de l'API, ou la première adresse (ou celle qui
  # correspond à prefer_cp).
  long_cp <- isTRUE(long) && "adressesOperateurs_codePostal" %in% names(flat)
  long_ville <- isTRUE(long) && "adressesOperateurs_ville" %in% names(flat)
  long_lat <- isTRUE(long) && "adressesOperateurs_lat" %in% names(flat)
  long_long <- isTRUE(long) && "adressesOperateurs_long" %in% names(flat)

  if (long_cp) {
    flat$codePostal <- flat$adressesOperateurs_codePostal
  } else if (!"codePostal" %in% names(flat)) {
    flat$codePostal <- .ab_first_addr_field(items, "codePostal", prefer_cp)
  }
  if (long_ville) {
    flat$ville <- flat$adressesOperateurs_ville
  } else if (!"ville" %in% names(flat)) {
    flat$ville <- .ab_first_addr_field(items, "ville", prefer_cp)
  }
  if (long_lat) {
    flat$lat <- flat$adressesOperateurs_lat
  } else if (!"lat" %in% names(flat)) {
    flat$lat <- .ab_first_addr_coord(items, "lat")
  }
  if (long_long) {
    flat$long <- flat$adressesOperateurs_long
  } else if (!"long" %in% names(flat)) {
    flat$long <- .ab_first_addr_coord(items, "long")
  }

  # Place les colonnes pratiques après l'identifiant le plus parlant.
  conv <- intersect(c("codePostal", "ville", "lat", "long"), names(flat))
  anchor <- names(flat)[names(flat) %in% c("numeroBio", "siret", "id")][1]
  if (length(conv) > 0 && !is.na(anchor)) {
    flat <- dplyr::relocate(flat, dplyr::all_of(conv),
                            .after = dplyr::all_of(anchor))
  }
  flat
}

# Premier champ de la première adresse de chaque opérateur (NA si aucune
# adresse). Quand prefer_cp est fourni, l'adresse retenue est celle qui
# correspond à ce code postal (quand l'opérateur en a plusieurs).
.ab_first_addr_field <- function(items, field, prefer_cp = NULL) {
  addrs <- items[["adressesOperateurs"]]
  if (is.null(addrs)) {
    return(rep(NA_character_, nrow(items)))
  }
  has_cp <- !is.null(prefer_cp) && !is.na(prefer_cp) && nzchar(prefer_cp)
  vapply(addrs, function(x) {
    recs <- ab_records(x)
    if (length(recs) == 0) {
      return(NA_character_)
    }
    if (has_cp) {
      hit <- which(vapply(recs, function(r) {
        identical(as.character(r$codePostal %||% NA_character_),
                  as.character(prefer_cp))
      }, logical(1)))
      if (length(hit) > 0) {
        recs <- recs[hit[1]]
      }
    }
    v <- recs[[1]][[field]]
    if (is.null(v) || length(v) == 0) NA_character_ else as.character(v[[1]])
  }, character(1))
}

# Première coordonnée de la première adresse de chaque opérateur (NA si
# aucune adresse ou coordonnée absente).
.ab_first_addr_coord <- function(items, field) {
  addrs <- items[["adressesOperateurs"]]
  if (is.null(addrs)) {
    return(rep(NA_real_, nrow(items)))
  }
  vapply(addrs, function(x) ab_coord(ab_first_record(x), field), numeric(1))
}

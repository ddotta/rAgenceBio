# Helper interne : construire le tibble de résultat vide pour ab_search()
#
# @return Un tibble avec les mêmes colonnes qu'un résultat rempli, mais
#   aucune ligne.
# @keywords internal
# @noRd
.ab_empty_search <- function() {
  dplyr::tibble(
    id = integer(0),
    nomAnnuaire = character(0),
    siret = character(0),
    numeroBio = integer(0),
    codePostal = character(0),
    ville = character(0),
    raw = list()
  )
}

# Helper interne : ne conserver que les opérateurs ayant une adresse avec
# le code postal demandé (vérifié sur TOUTES les adresses de l'opérateur).
#
# @param items data.frame d'enregistrements bruts d'opérateurs (une ligne
#   par opérateur).
# @param code_postal character(1).
# @return items filtré sur les lignes correspondantes.
# @keywords internal
# @noRd
.ab_filter_code_postal <- function(items, code_postal) {
  if (!is.data.frame(items) || nrow(items) == 0) {
    return(items)
  }
  keep <- vapply(seq_len(nrow(items)), function(i) {
    addrs <- items$adressesOperateurs[[i]]
    if (is.null(addrs) || length(addrs) == 0) {
      return(FALSE)
    }
    cps <- if (is.data.frame(addrs)) {
      addrs$codePostal
    } else {
      vapply(addrs, function(a) {
        as.character(a$codePostal %||% NA_character_)
      }, character(1))
    }
    any(!is.na(cps) & cps == code_postal)
  }, logical(1))
  items[keep, , drop = FALSE]
}

# Helper interne : construire le tibble tidy renvoyé par ab_search() à
# partir des éléments bruts de l'API.
#
# @param items data.frame d'enregistrements bruts d'opérateurs (une ligne
#   par opérateur).
# @param nb_total entier, nombre total d'opérateurs correspondants
#   (métadonnée de l'API).
# @param prefer_cp code postal optionnel : lorsqu'il est fourni, l'adresse
#   extraite est celle qui correspond à ce code (quand l'opérateur a
#   plusieurs adresses).
# @return Un tibble avec les colonnes id, nomAnnuaire, siret, numeroBio,
#   codePostal, ville, raw et l'attribut nb_total.
# @keywords internal
# @noRd
.ab_items_to_tibble <- function(items, nb_total, prefer_cp = NULL,
                                expand = TRUE, long = FALSE) {
  # nomAnnuaire avec repli sur la dénomination courante, la raison sociale
  # ou le nom (les noms de colonnes diffèrent légèrement entre backends).
  nomAnnuaire <- rep(NA_character_, nrow(items))
  for (candidate in c("nomAnnuaire", "denominationcourante", "raisonSociale", "nom")) {
    if (candidate %in% names(items)) {
      v <- as.character(items[[candidate]])
      fill <- is.na(nomAnnuaire) & !is.na(v)
      nomAnnuaire[fill] <- v[fill]
    }
  }

  # Tibble entièrement aplati et déplié ; `raw` conserve l'enregistrement
  # brut complet (un data.frame à 1 ligne par opérateur) et nomAnnuaire/raw
  # sont attachés avant l'éventuelle duplication des lignes en mode long.
  flat <- .ab_flatten_items(
    items,
    expand = expand,
    long = long,
    prefer_cp = prefer_cp,
    post_cols = list(
      nomAnnuaire = nomAnnuaire,
      raw = unname(lapply(seq_len(nrow(items)), function(i) {
        items[i, , drop = FALSE]
      }))
    )
  )

  # Place nomAnnuaire juste après id pour la lisibilité.
  if ("id" %in% names(flat)) {
    flat <- dplyr::relocate(flat, dplyr::all_of("nomAnnuaire"),
                            .after = dplyr::all_of("id"))
  }

  attr(flat, "nb_total") <- nb_total
  flat
}

# Interne : recherche via l'API open data. La pagination se fait avec nb
# (taille de page, jusqu'à 500 enregistrements) et debut (décalage 0-based).
# Il n'existe pas de filtre exact par code postal : code_postal est filtré
# côté client, les départements resserrant la recherche côté serveur.
#
# @keywords internal
# @noRd
.ab_search_opendata <- function(query, code_postal, limit, offset,
                                expand = TRUE, long = FALSE) {
  has_cp <- !is.null(code_postal) && !is.na(code_postal) && nzchar(code_postal)
  page_size <- min(max(limit, 1L), 500L)
  wanted <- offset + limit
  max_pages <- 50L  # au plus 25 000 enregistrements parcourus par recherche

  base_params <- list(
    q = query,
    nb = as.character(page_size)
  )
  if (has_cp) {
    base_params$departements <- substr(code_postal, 1, 2)
  }

  pages <- list()
  nb_total <- 0L
  debut <- 0L
  pb <- NULL
  target <- 0L
  repeat {
    data <- .ab_request(
      url = paste0(.ab_base_opendata, "/api/gouv/operateurs/"),
      query = c(base_params, list(debut = as.character(debut)))
    )

    nb_total <- as.integer(data$nbTotal)
    items <- data$items
    if (is.null(items) || length(items) == 0 ||
        (is.data.frame(items) && nrow(items) == 0)) {
      break
    }
    raw_n <- nrow(items)

    # Barre de progression basée sur le nombre total annoncé par l'API
    # (connu seulement après la première page), en session interactive.
    if (is.null(pb)) {
      target <- min(nb_total, wanted)
      pb <- .ab_progress_new(target, "Recherche d'opérateurs")
      on.exit(.ab_progress_done(pb), add = TRUE)
    }

    if (has_cp) {
      items <- .ab_filter_code_postal(items, code_postal)
    }

    if (nrow(items) > 0) {
      pages[[length(pages) + 1]] <- items
    }

    got <- sum(vapply(pages, nrow, integer(1)))
    .ab_progress_tick(pb, min(got, max(target, 0L)))
    if (got >= wanted || raw_n < page_size ||
        debut >= page_size * (max_pages - 1L)) {
      break
    }
    debut <- debut + page_size
  }

  if (length(pages) == 0) {
    result <- .ab_empty_search()
  } else {
    items <- dplyr::bind_rows(pages)
    n <- nrow(items)
    start <- offset + 1L
    if (start > n || limit < 1L) {
      result <- .ab_empty_search()
    } else {
      end <- min(offset + limit, n)
      items <- items[start:end, , drop = FALSE]
      result <- .ab_items_to_tibble(
        items, nb_total,
        prefer_cp = if (has_cp) code_postal else NULL,
        expand = expand,
        long = long
      )
    }
  }
  attr(result, "nb_total") <- nb_total
  result
}

#' Rechercher des opérateurs bio
#'
#' Recherche dans l'annuaire de l'Agence Bio des professionnels certifiés
#' biologiques par requête en texte libre, code postal ou autres critères,
#' via l'API open data officielle \url{https://opendata.agencebio.org}
#' (voir \url{https://www.data.gouv.fr/dataservices/api-professionnels-bio}).
#'
#' @param query Caractère. Recherche en texte libre : nom d'opérateur,
#'   activité, produit, SIRET ou raison sociale. Peut être \code{NULL}.
#' @param code_postal Caractère (5 chiffres). Filtre par code postal
#'   français. Peut être \code{NULL}. L'API n'a pas de filtre exact par
#'   code postal : le département (deux premiers chiffres) resserre la
#'   recherche côté serveur et le code postal exact est filtré côté client
#'   sur toutes les adresses de chaque opérateur.
#' @param limit Entier. Nombre maximal de résultats à renvoyer
#'   (défaut : 20). L'API pagine avec \code{nb} (taille de page, jusqu'à
#'   500 enregistrements) et \code{debut} (décalage) ; les pages sont
#'   récupérées jusqu'à obtenir \code{limit} enregistrements.
#' @param offset Entier. Nombre de résultats à sauter pour la pagination
#'   (défaut : 0).
#' @param expand Logique. Si \code{TRUE} (défaut), le tibble renvoyé est
#'   entièrement aplati et déplié : tous les champs de l'API deviennent des
#'   colonnes (objets éclatés en \code{champ.sousChamp}, champs
#'   multi-valués dépliés en \code{<champ>_<sousChamp>} via
#'   \code{\link{ab_expand}}()). Si \code{FALSE}, seules les colonnes de
#'   premier niveau (\code{id}, \code{nomAnnuaire}, \code{siret},
#'   \code{numeroBio}, \code{codePostal}, \code{ville}) et la colonne-liste
#'   \code{raw} sont renvoyées.
#' @param long Logique. Si \code{TRUE}, une ligne par adresse : les
#'   opérateurs possédant plusieurs adresses sont dupliqués (défaut :
#'   \code{FALSE}, une ligne par opérateur avec les valeurs jointes par
#'   ", ").
#' @return Un \code{tibble} avec une ligne par opérateur (ou une ligne par
#'   adresse si \code{long = TRUE}). Chaque ligne contient l'enregistrement
#'   brut de l'opérateur dans une colonne-liste nommée \code{raw}, ainsi
#'   que les colonnes pratiques \code{id}, \code{nomAnnuaire},
#'   \code{codePostal}, \code{ville}. Avec \code{expand = TRUE} (défaut),
#'   tous les champs de l'API sont également présents sous forme de colonnes
#'   plates et dépliées (\code{adressesOperateurs_lieu},
#'   \code{categories_nom}, ...).
#'   Utilisez \code{\link{ab_flatten}}() pour obtenir un format long
#'   (une ligne par production, adresse ou certificat).
#'
#' @examples
#' \dontrun{
#' # Rechercher des brasseries bio
#' results <- ab_search("brasserie", limit = 10)
#' print(results)
#'
#' # Rechercher par code postal
#' paris <- ab_search(code_postal = "75001", limit = 5)
#' print(paris)
#'
#' # Combiner requête et code postal
#' farms <- ab_search(query = "maraichage", code_postal = "33000", limit = 20)
#'
#' # Obtenir un data.frame plat et tidy
#' flat <- ab_flatten(farms)
#' }
#' @export
ab_search <- function(query = NULL,
                      code_postal = NULL,
                      limit = 20L,
                      offset = 0L,
                      expand = TRUE,
                      long = FALSE) {

  limit <- as.integer(limit)
  offset <- as.integer(offset)

  .ab_search_opendata(query, code_postal, limit, offset,
                      expand = expand, long = long)
}

#' Obtenir des suggestions de recherche
#'
#' Renvoie des suggestions de noms pour la recherche d'opérateurs, utiles
#' pour construire des interfaces de recherche interactives ou valider une
#' requête de recherche. L'API open data n'a pas de point d'accès dédié
#' aux suggestions : elles sont donc dérivées des premiers résultats d'une
#' recherche en texte libre.
#'
#' Utilise l'API open data \url{https://opendata.agencebio.org}.
#'
#' @param query Caractère. La chaîne de recherche (au moins 2 caractères).
#' @param limit Entier. Nombre maximal de suggestions (défaut : 50, max : 50).
#' @return Un \code{tibble} avec les colonnes :
#'   \code{nomAnnuaire} (nom d'affichage), \code{nom} (raison sociale),
#'   \code{codePostal} (code postal).
#'
#' @examples
#' \dontrun{
#' # Suggestions pour "brasserie"
#' sugg <- ab_suggestions("brasserie")
#' print(sugg)
#'
#' # Suggestions pendant la saisie
#' sugg2 <- ab_suggestions("la ferme")
#' }
#' @export
ab_suggestions <- function(query, limit = 50L) {
  query <- trimws(query)
  if (nchar(query) < 2) {
    stop("Query must be at least 2 characters.", call. = FALSE)
  }
  limit <- min(max(limit, 1L), 50L)

  ops <- ab_search(query, limit = limit)

  dplyr::tibble(
    nomAnnuaire = ops$nomAnnuaire,
    nom = vapply(ops$raw, function(op) {
      as.character(op$raisonSociale %||% op$denominationcourante %||%
                     NA_character_)
    }, character(1)),
    codePostal = ops$codePostal
  )
}

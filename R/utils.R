#' Interne : opérateur de coalescence NULL
#'
#' Renvoie \code{x} s'il n'est pas \code{NULL}, sinon \code{y}.
#'
#' @param x,y Objets.
#' @return \code{x} ou \code{y}.
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

# Interne : crée une barre de progression pour les boucles longues
# (SIRET multiples, pagination). La barre n'est affichée qu'en session
# interactive et quand l'option rAgenceBio.progress vaut TRUE (défaut) ;
# elle renvoie sinon NULL et les appels suivants sont sans effet.
.ab_progress_new <- function(n, title = "") {
  if (isTRUE(getOption("rAgenceBio.progress", TRUE)) && interactive() &&
      !is.na(n) && n > 1) {
    utils::txtProgressBar(min = 0, max = n, initial = 0, style = 3,
                          title = title)
  } else {
    NULL
  }
}

# Interne : met à jour une barre de progression (sans effet si NULL).
.ab_progress_tick <- function(pb, i) {
  if (!is.null(pb)) {
    utils::setTxtProgressBar(pb, i)
  }
}

# Interne : ferme une barre de progression (sans effet si NULL).
.ab_progress_done <- function(pb) {
  if (!is.null(pb)) {
    close(pb)
  }
}

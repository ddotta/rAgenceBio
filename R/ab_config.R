#' @keywords internal
#' @noRd
# URL de base de l'API open data de l'Agence Bio (seul backend utilisé).
# Voir https://www.data.gouv.fr/dataservices/api-professionnels-bio
.ab_base_opendata <- "https://opendata.agencebio.org"

#' @keywords internal
#' @noRd
# État du limiteur de débit (simple compteur en mémoire)
.ab_rate_limiter <- new.env()
.ab_rate_limiter$last_call <- 0
.ab_rate_limiter$min_interval <- 1 / 50  # 50 req/s -> 20 ms entre deux appels

#' Interne : ralentir une requête pour respecter la limite de 50 req/s
#'
#' @keywords internal
#' @noRd
.ab_throttle <- function() {
  now <- Sys.time()
  elapsed <- as.numeric(difftime(now, .ab_rate_limiter$last_call, units = "secs"))
  wait <- .ab_rate_limiter$min_interval - elapsed
  if (wait > 0) {
    Sys.sleep(wait)
  }
  .ab_rate_limiter$last_call <- Sys.time()
}

#' Interne : effectuer une requête GET vers l'API Agence Bio
#'
#' Gère la limitation de débit, la gestion des erreurs et l'analyse JSON.
#'
#' La vérification du certificat SSL peut être désactivée avec
#' \code{options(rAgenceBio.ssl_verifypeer = FALSE)}. C'est utile derrière
#' les proxys d'entreprise qui présentent des certificats auto-signés.
#'
#' @param url Caractère. URL complète à requêter.
#' @param query Vecteur de caractères nommé de paramètres de requête
#'   (les valeurs NA sont supprimées).
#' @param error_on_404 Logique. Si TRUE (défaut), s'arrête avec un message
#'   sur une erreur HTTP 404. Si FALSE, renvoie \code{NULL} en cas de 404.
#' @return Un objet JSON analysé (liste ou data.frame via jsonlite).
#' @keywords internal
#' @noRd
.ab_request <- function(url, query = NULL, error_on_404 = TRUE) {
  .ab_throttle()

  if (!is.null(query)) {
    # Les paramètres de requête peuvent contenir des NULL ou des NA ; les
    # deux doivent être supprimés avant de construire l'URL. Supprimer
    # d'abord les éléments NULL : is.na(NULL) renvoie logical(0), ce qui
    # casserait vapply(query, is.na, logical(1)).
    query <- Filter(Negate(is.null), query)
    # isTRUE() garantit un logique scalaire même pour les valeurs NULL ou
    # de longueur 0.
    query <- query[!vapply(query, function(x) isTRUE(is.na(x)), logical(1))]
    if (length(query) > 0) {
      url <- httr::modify_url(url, query = query)
    }
  }

  req_headers <- httr::add_headers(
    Accept = "application/json",
    `User-Agent` = "rAgenceBio/0.1.0 (https://github.com/yourname/rAgenceBio)"
  )

  # Certains proxys d'entreprise interceptent le trafic HTTPS et présentent
  # des certificats auto-signés. Permettre aux utilisateurs de désactiver
  # la vérification du certificat via une option.
  verify_peer <- isTRUE(getOption("rAgenceBio.ssl_verifypeer", TRUE))
  ssl_hint <- paste0(
    "\nIf you are behind a corporate proxy that presents a self-signed ",
    "certificate, retry with options(rAgenceBio.ssl_verifypeer = FALSE)."
  )

  # Réessayer quelques fois les erreurs de passerelle transitoires
  # (limitation de débit, délais du proxy) avec un backoff exponentiel
  # avant d'abandonner.
  retries <- as.integer(getOption("rAgenceBio.retries", 2L))

  resp <- NULL
  for (attempt in 0:retries) {
    resp <- tryCatch(
      {
        if (verify_peer) {
          httr::GET(url, req_headers)
        } else {
          httr::GET(url, req_headers, httr::config(ssl_verifypeer = 0L))
        }
      },
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("SSL|certificate", msg)) {
          msg <- paste0(msg, ssl_hint)
        }
        stop(msg, call. = FALSE)
      }
    )

    status <- httr::status_code(resp)
    if (status %in% c(429L, 502L, 503L, 504L) && attempt < retries) {
      Sys.sleep(2^attempt)  # 1s, 2s, ...
      next
    }
    break
  }

  status <- httr::status_code(resp)

  if (status == 404) {
    if (error_on_404) {
      stop("Resource not found (HTTP 404): ", url, call. = FALSE)
    }
    return(NULL)
  }

  if (httr::http_error(resp)) {
    msg <- tryCatch(
      jsonlite::fromJSON(rawToChar(resp$content))$message,
      error = function(e) NULL
    )
    hint <- if (status == 504L) {
      paste0(
        "\nThe gateway timed out. If you are behind a corporate proxy, add ",
        "opendata.agencebio.org to its allowlist."
      )
    } else {
      ""
    }
    stop(
      sprintf("HTTP %d error from %s: %s%s", status, url,
              if (is.null(msg)) "unknown error" else msg, hint),
      call. = FALSE
    )
  }

  tryCatch(
    jsonlite::fromJSON(
      rawToChar(resp$content),
      simplifyVector = TRUE,
      simplifyDataFrame = TRUE,
      simplifyMatrix = TRUE
    ),
    error = function(e) {
      stop(sprintf("Could not parse JSON response from %s: %s",
                   url, conditionMessage(e)),
           call. = FALSE)
    }
  )
}

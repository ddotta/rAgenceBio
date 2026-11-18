# Tests for rAgenceBio
# These tests hit the live API -- they will fail without internet access.
# Run with: devtools::test()
test_that("ab_search returns a tibble with expected columns", {
  skip_if_offline("opendata.agencebio.org")
  
  result <- ab_search("brasserie", limit = 5)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("id", "nomAnnuaire", "siret", "numeroBio",
                    "codePostal", "ville", "raw") %in% names(result)))
  expect_true(nrow(result) >= 1)
  expect_true(nrow(result) <= 5)
})

test_that("ab_search with code_postal filters results", {
  skip_if_offline("opendata.agencebio.org")
  
  result <- ab_search(code_postal = "75001", limit = 5)
  expect_s3_class(result, "tbl_df")
  # All returned operators should have CP 75001
  cps <- result$codePostal
  expect_true(all(cps %in% c("75001", NA_character_)))
})

test_that("ab_search returns empty tibble for no results", {
  skip_if_offline("opendata.agencebio.org")
  
  result <- ab_search(query = "zzzzzzzzz_inexistant_xxx", limit = 5)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("ab_get returns flattened operator data for valid numeroBio", {
  skip_if_offline("opendata.agencebio.org")
  
  # First, find a valid numeroBio
  search_result <- ab_search("brasserie", limit = 1)
  if (nrow(search_result) > 0 && !is.na(search_result$numeroBio[1])) {
    nb <- search_result$numeroBio[1]
    op <- ab_get(nb)
    expect_s3_class(op, "tbl_df")
    expect_true("numeroBio" %in% names(op))
    # Une ligne par adresse : chaque ligne porte le même numeroBio
    expect_true(nrow(op) >= 1)
    expect_true(all(op$numeroBio == nb))
    # Aucune colonne imbriquée (liste ou data.frame) ne doit subsister
    expect_false(any(vapply(op, is.list, logical(1))))
  }
})

test_that("ab_get returns NULL for invalid numeroBio", {
  skip_if_offline("opendata.agencebio.org")
  
  op <- ab_get(999999999)
  expect_null(op)
})

test_that("ab_by_siret validates input format", {
  # Invalid SIRET should produce a warning, not an error
  expect_warning(
    result <- ab_by_siret("123"),
    "invalid SIRET"
  )
  expect_equal(nrow(result), 0)
})

test_that("ab_by_siret skips unknown SIRETs without error", {
  skip_if_offline("opendata.agencebio.org")
  
  # Regression test: the API answers with an empty list in `items` when a
  # SIRET is unknown, which used to crash seq_len(nrow(items)).
  expect_no_error(
    ops_multi <- ab_by_siret(c("51438127600013", "98765432100022"))
  )
  expect_s3_class(ops_multi, "tbl_df")
  expect_true(nrow(ops_multi) >= 1)
  expect_true(all(ops_multi$siret %in% "51438127600013"))
})

test_that("ab_by_siret returns all API fields as columns (no raw column)", {
  skip_if_offline("opendata.agencebio.org")
  
  ops <- ab_by_siret("51438127600013")
  expect_s3_class(ops, "tbl_df")
  expect_true(nrow(ops) >= 1)
  expect_false("raw" %in% names(ops))
  expect_true(all(c("id", "raisonSociale", "siret", "numeroBio", "codeNAF") %in%
                    names(ops)))
})

test_that("ab_by_siret returns a fully flattened tibble (no nested data.frames)", {
  skip_if_offline("opendata.agencebio.org")
  
  ops <- ab_by_siret("51438127600013")
  expect_s3_class(ops, "tbl_df")
  expect_true(nrow(ops) >= 1)
  # Aucune colonne imbriquée (liste ou data.frame) ne doit subsister
  expect_false(any(vapply(ops, is.list, logical(1))))
  # Les objets imbriqués sont éclatés en colonnes champ.sousChamp
  expect_true("venteAnnuaire.venteParticuliers" %in% names(ops))
  # Les champs multi-valués sont convertis en texte lisible
  expect_match(ops$categories, "Magasins spécialisés", fixed = TRUE)
  expect_match(ops$adressesOperateurs, "codePostal=13004", fixed = TRUE)
  expect_match(ops$productions, "etatProduction=AB", fixed = TRUE)
  # La première adresse est extraite en colonnes pratiques
  expect_equal(ops$codePostal, "13004")
  expect_equal(ops$ville, "MARSEILLE")
  expect_equal(ops$lat, 43.316748)
  expect_equal(ops$long, 5.404689)
})

test_that("ab_by_siret flattens nested structures (mocked)", {
  local_mocked_bindings(
    .ab_request = function(url, query, error_on_404 = TRUE) {
      list(items = data.frame(
        id = 28792L, raisonSociale = "CECOBIO", siret = "51438127600013",
        numeroBio = 126851L, codeNAF = "47.29Z",
        categories = I(list(data.frame(
          id = 4L, nom = "Magasins spécialisés", stringsAsFactors = FALSE
        ))),
        siteWebs = I(list(list())),
        adressesOperateurs = I(list(data.frame(
          id = 28835L, lieu = "60 AVENUE SAINT JUST", codePostal = "13004",
          ville = "MARSEILLE", lat = 43.316748, long = 5.404689,
          active = TRUE, pays = "FRANCE",
          typeAdresseOperateurs = I(list(c("Lieux d'activité", "Siège social"))),
          stringsAsFactors = FALSE
        ))),
        productions = I(list(data.frame(
          id = c(1318L, 1319L),
          code = c("47.00.11", "47.00.12"),
          nom = c("Fruits frais", "Fruits en conserve"),
          etatProductions = I(list(
            data.frame(id = 6L, etatProduction = "AB",
                       anneeReferenceControle = 2021L),
            data.frame(id = 6L, etatProduction = "AB",
                       anneeReferenceControle = 2021L)
          )),
          stringsAsFactors = FALSE
        ))),
        venteAnnuaire = data.frame(
          venteParticuliers = FALSE, venteProsGros = FALSE,
          stringsAsFactors = FALSE
        ),
        stringsAsFactors = FALSE
      ))
    },
    .package = "rAgenceBio"
  )
  
  ops <- ab_by_siret("51438127600013")
  expect_s3_class(ops, "tbl_df")
  expect_equal(nrow(ops), 1)
  # Aucune colonne imbriquée (liste ou data.frame) ne doit subsister
  expect_false(any(vapply(ops, is.list, logical(1))))
  # Champs scalaires conservés
  expect_equal(ops$raisonSociale, "CECOBIO")
  # Objet imbriqué éclaté en colonnes
  expect_equal(ops$venteAnnuaire.venteParticuliers, FALSE)
  # Champs multi-valués convertis en texte lisible
  expect_match(ops$categories, "Magasins spécialisés", fixed = TRUE)
  expect_match(ops$adressesOperateurs, "codePostal=13004", fixed = TRUE)
  expect_match(ops$productions, "etatProduction=AB", fixed = TRUE)
  # Première adresse extraite en colonnes pratiques
  expect_equal(ops$codePostal, "13004")
  expect_equal(ops$ville, "MARSEILLE")
  expect_equal(ops$lat, 43.316748)
  expect_equal(ops$long, 5.404689)
})

# Offline (mocked) tests: the API answers with `items` in different shapes
# depending on the number of matches.
test_that("ab_by_siret handles an empty items list", {
  local_mocked_bindings(
    .ab_request = function(url, query, error_on_404 = TRUE) {
      list(items = list())
    },
    .package = "rAgenceBio"
  )
  
  expect_no_error(ops <- ab_by_siret("98765432100022"))
  expect_s3_class(ops, "tbl_df")
  expect_equal(nrow(ops), 0)
})

test_that("ab_by_siret handles mixed known and unknown SIRETs", {
  local_mocked_bindings(
    .ab_request = function(url, query, error_on_404 = TRUE) {
      if (identical(query$siret, "51438127600013")) {
        list(items = data.frame(
          id = 28792L, raisonSociale = "CECOBIO", siret = "51438127600013",
          numeroBio = 126851L, codeNAF = "47.29Z", stringsAsFactors = FALSE
        ))
      } else {
        list(items = list())
      }
    },
    .package = "rAgenceBio"
  )
  
  expect_no_error(
    ops <- ab_by_siret(c("51438127600013", "98765432100022"))
  )
  expect_equal(nrow(ops), 1)
  expect_equal(ops$raisonSociale, "CECOBIO")
})

test_that("ab_by_siret handles a single record returned as a plain list", {
  local_mocked_bindings(
    .ab_request = function(url, query, error_on_404 = TRUE) {
      list(items = list(
        id = 28792L, raisonSociale = "CECOBIO", siret = "51438127600013",
        numeroBio = 126851L, codeNAF = "47.29Z"
      ))
    },
    .package = "rAgenceBio"
  )
  
  ops <- ab_by_siret("51438127600013")
  expect_equal(nrow(ops), 1)
  expect_equal(ops$raisonSociale, "CECOBIO")
  expect_false("raw" %in% names(ops))
})

test_that("ab_flatten accepts ab_by_siret output without a raw column", {
  ops <- dplyr::tibble(
    id = 28792L,
    raisonSociale = "CECOBIO",
    siret = "51438127600013",
    numeroBio = 126851L,
    codeNAF = "47.29Z",
    adressesOperateurs = list(data.frame(
      codePostal = "13004", ville = "MARSEILLE", stringsAsFactors = FALSE
    ))
  )
  
  flat <- ab_flatten(ops, level = "summary")
  expect_s3_class(flat, "tbl_df")
  expect_equal(flat$codePostal, "13004")
  expect_equal(flat$ville, "MARSEILLE")
})

test_that("ab_suggestions returns a tibble", {
  skip_if_offline("opendata.agencebio.org")
  
  result <- ab_suggestions("brasserie")
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("nomAnnuaire", "nom", "codePostal"))
})

test_that("ab_suggestions requires at least 2 characters", {
  expect_error(ab_suggestions("a"), "at least 2 characters")
})

test_that("ab_flatten produces a summary tibble", {
  skip_if_offline("opendata.agencebio.org")
  
  ops <- ab_search("brasserie", limit = 3)
  flat <- ab_flatten(ops, level = "summary")
  expect_s3_class(flat, "tbl_df")
  expect_true(nrow(flat) > 0)
  expect_true("nom" %in% names(flat))
  expect_true("codePostal" %in% names(flat))
})

test_that("ab_flatten productions level works", {
  skip_if_offline("opendata.agencebio.org")
  
  ops <- ab_search("producteur", limit = 3)
  flat <- ab_flatten(ops, level = "productions")
  expect_s3_class(flat, "tbl_df")
  if (nrow(flat) > 0) {
    expect_true("production_nom" %in% names(flat))
    expect_true("etat" %in% names(flat))
  }
})

test_that("ab_flatten addresses level works", {
  skip_if_offline("opendata.agencebio.org")
  
  ops <- ab_search("brasserie", limit = 3)
  flat <- ab_flatten(ops, level = "addresses")
  expect_s3_class(flat, "tbl_df")
  if (nrow(flat) > 0) {
    expect_true("codePostal" %in% names(flat))
    expect_true("ville" %in% names(flat))
  }
})

test_that("ab_expand splits adressesOperateurs into typed columns", {
  ops <- dplyr::tibble(
    id = 28792L,
    raisonSociale = "CECOBIO",
    siret = "51438127600013",
    numeroBio = 126851L,
    adressesOperateurs = paste0(
      "id=28835; lieu=60 AVENUE SAINT JUST; codePostal=13004; ville=MARSEILLE; ",
      "lat=43.316748; long=5.404689; codeCommune=13055; active=TRUE; ",
      "pays=FRANCE; departementId=14; coordUpdatedByUser=FALSE; ",
      "operateurAdrOpId=138014; ",
      "typeAdresseOperateurs=Lieux d'activité, Siège social, Lieux de vente"
    )
  )
  
  res <- ab_expand(ops)
  
  expect_s3_class(res, "tbl_df")
  expect_true(all(c(
    "adressesOperateurs_id", "adressesOperateurs_lieu",
    "adressesOperateurs_codePostal", "adressesOperateurs_ville",
    "adressesOperateurs_lat", "adressesOperateurs_long",
    "adressesOperateurs_codeCommune", "adressesOperateurs_active",
    "adressesOperateurs_pays", "adressesOperateurs_departementId",
    "adressesOperateurs_coordUpdatedByUser",
    "adressesOperateurs_operateurAdrOpId",
    "adressesOperateurs_typeAdresseOperateurs"
  ) %in% names(res)))
  
  expect_equal(res$adressesOperateurs_id, 28835L)
  expect_equal(res$adressesOperateurs_lieu, "60 AVENUE SAINT JUST")
  # Code postal et code commune : toujours du texte (zéros initiaux)
  expect_equal(res$adressesOperateurs_codePostal, "13004")
  expect_type(res$adressesOperateurs_codePostal, "character")
  expect_equal(res$adressesOperateurs_codeCommune, "13055")
  expect_type(res$adressesOperateurs_codeCommune, "character")
  expect_equal(res$adressesOperateurs_ville, "MARSEILLE")
  expect_equal(res$adressesOperateurs_lat, 43.316748)
  expect_equal(res$adressesOperateurs_long, 5.404689)
  expect_true(res$adressesOperateurs_active)
  expect_false(res$adressesOperateurs_coordUpdatedByUser)
  expect_equal(res$adressesOperateurs_departementId, 14L)
  expect_equal(res$adressesOperateurs_typeAdresseOperateurs,
               "Lieux d'activité, Siège social, Lieux de vente")
  # La colonne source est conservée
  expect_match(res$adressesOperateurs, "ville=MARSEILLE", fixed = TRUE)
})

test_that("ab_expand handles multiple records and missing values", {
  ops <- dplyr::tibble(
    id = c(1L, 2L),
    adressesOperateurs = c(
      paste0("id=10; ville=PARIS; active=TRUE | ",
             "id=11; ville=LYON; active=FALSE"),
      NA_character_
    )
  )
  
  res <- ab_expand(ops)
  
  # Deux enregistrements : les valeurs d'un même champ sont jointes par ", "
  expect_equal(res$adressesOperateurs_ville, c("PARIS, LYON", NA))
  expect_equal(res$adressesOperateurs_id, c("10, 11", NA))
  # Cellule multi-enregistrements : la colonne reste du texte
  expect_type(res$adressesOperateurs_ville, "character")
  # Ligne sans adresse : NA partout
  expect_true(is.na(res$adressesOperateurs_active[2]))
})

test_that("ab_expand keeps identifiers as text and validates input", {
  ops <- dplyr::tibble(
    siret = "51438127600013",
    adressesOperateurs = "codePostal=06000; siret=51438127600013; lat=43.5"
  )
  
  res <- ab_expand(ops)
  # Code postal avec zéro initial : doit rester du texte
  expect_equal(res$adressesOperateurs_codePostal, "06000")
  expect_type(res$adressesOperateurs_codePostal, "character")
  # SIRET trop long pour un entier : reste du texte
  expect_equal(res$adressesOperateurs_siret, "51438127600013")
  expect_type(res$adressesOperateurs_siret, "character")
  # lat est numérique
  expect_equal(res$adressesOperateurs_lat, 43.5)
  expect_type(res$adressesOperateurs_lat, "double")
  
  expect_error(ab_expand(ops, columns = "inconnue"), "absente")
  expect_error(ab_expand("pas un tibble"), "data.frame")
})

test_that("ab_expand expands several columns at once", {
  ops <- dplyr::tibble(
    id = 1L,
    categories = "id=4; nom=Magasins spécialisés",
    activites = "id=3; nom=Distribution"
  )
  
  res <- ab_expand(ops, columns = c("categories", "activites"))
  
  expect_equal(res$categories_id, 4L)
  expect_equal(res$categories_nom, "Magasins spécialisés")
  expect_equal(res$activites_id, 3L)
  expect_equal(res$activites_nom, "Distribution")
})

test_that("ab_expand keeps empty-valued fields as NA columns", {
  ops <- dplyr::tibble(
    certificats = paste0(
      "numeroControleEu=FR-BIO-10; organisme=BV; etatCertification=ARRETEE; ",
      "dateSuspension=; url=https://example.org/a?x=1&y=2"
    )
  )
  
  res <- ab_expand(ops, columns = "certificats")
  
  # Champ vide ("nom=") : la colonne existe avec NA, la clé n'est pas perdue
  expect_true("certificats_dateSuspension" %in% names(res))
  expect_true(is.na(res$certificats_dateSuspension))
  # Valeur contenant "=" : conservée entière
  expect_equal(res$certificats_url, "https://example.org/a?x=1&y=2")
})

test_that("ab_by_siret expands multi-valued columns by default (mocked)", {
  local_mocked_bindings(
    .ab_request = function(url, query, error_on_404 = TRUE) {
      list(items = data.frame(
        id = 28792L, raisonSociale = "CECOBIO", siret = "51438127600013",
        numeroBio = 126851L, codeNAF = "47.29Z",
        categories = I(list(data.frame(
          id = 4L, nom = "Magasins spécialisés", stringsAsFactors = FALSE
        ))),
        adressesOperateurs = I(list(data.frame(
          id = 28835L, lieu = "60 AVENUE SAINT JUST", codePostal = "13004",
          ville = "MARSEILLE", lat = 43.316748, long = 5.404689,
          active = TRUE, pays = "FRANCE", stringsAsFactors = FALSE
        ))),
        activites = I(list(data.frame(
          id = 3L, nom = "Distribution", stringsAsFactors = FALSE
        ))),
        productions = I(list(data.frame(
          id = 1318L, code = "47.00.11", nom = "Fruits frais",
          etatProductions = I(list(
            data.frame(id = 6L, etatProduction = "AB",
                       anneeReferenceControle = 2021L)
          )),
          stringsAsFactors = FALSE
        ))),
        stringsAsFactors = FALSE
      ))
    },
    .package = "rAgenceBio"
  )
  
  ops <- ab_by_siret("51438127600013")
  
  # Colonnes dépliées en "<champ>_<sousChamp>"
  expect_equal(ops$adressesOperateurs_lieu, "60 AVENUE SAINT JUST")
  expect_equal(ops$adressesOperateurs_codePostal, "13004")
  expect_equal(ops$adressesOperateurs_lat, 43.316748)
  expect_true(ops$adressesOperateurs_active)
  expect_equal(ops$categories_id, 4L)
  expect_equal(ops$categories_nom, "Magasins spécialisés")
  expect_equal(ops$activites_nom, "Distribution")
  # Colonnes sources conservées
  expect_match(ops$adressesOperateurs, "codePostal=13004", fixed = TRUE)
  # productions n'est pas déplié (sous-enregistrements imbriqués)
  expect_false(any(startsWith(names(ops), "productions_")))
  expect_match(ops$productions, "etatProduction=AB", fixed = TRUE)
  
  # expand = FALSE : aucune colonne dépliée
  ops_raw <- ab_by_siret("51438127600013", expand = FALSE)
  expect_false(any(startsWith(names(ops_raw), "adressesOperateurs_")))
  expect_false(any(startsWith(names(ops_raw), "categories_")))
  expect_equal(ops_raw$codePostal, "13004")
})

test_that("ab_by_siret duplicates rows per address by default (mocked)", {
  local_mocked_bindings(
    .ab_request = function(url, query, error_on_404 = TRUE) {
      list(items = data.frame(
        id = 28792L, raisonSociale = "CECOBIO", siret = "51438127600013",
        numeroBio = 126851L,
        adressesOperateurs = I(list(data.frame(
          id = c(1211596L, 1211211L),
          lieu = c("RUE DU PACIFIQUE", "218 Rue des canadiens"),
          codePostal = c("76240", "76520"),
          ville = c("BELBEUF", "Les Authieux-sur-le-Port-Saint-Ouen"),
          lat = c(49.397646, 49.338649),
          long = c(1.155471, 1.130055),
          active = TRUE, pays = "FRANCE",
          stringsAsFactors = FALSE
        ))),
        stringsAsFactors = FALSE
      ))
    },
    .package = "rAgenceBio"
  )
  
  # Deux adresses : deux lignes, une par adresse (comportement par défaut)
  ops <- ab_by_siret("48380302900048")
  expect_equal(nrow(ops), 2)
  expect_equal(ops$adressesOperateurs_id, c(1211596L, 1211211L))
  expect_equal(ops$adressesOperateurs_lieu,
               c("RUE DU PACIFIQUE", "218 Rue des canadiens"))
  expect_equal(ops$adressesOperateurs_codePostal, c("76240", "76520"))
  # Les colonnes pratiques suivent chaque adresse
  expect_equal(ops$codePostal, c("76240", "76520"))
  expect_equal(ops$ville, c("BELBEUF", "Les Authieux-sur-le-Port-Saint-Ouen"))
  expect_equal(ops$lat, c(49.397646, 49.338649))
  expect_equal(ops$long, c(1.155471, 1.130055))
  expect_true(ops$adressesOperateurs_active[1])
  
  # long = FALSE : une ligne, valeurs jointes
  ops_wide <- ab_by_siret("48380302900048", long = FALSE)
  expect_equal(nrow(ops_wide), 1)
  expect_equal(ops_wide$adressesOperateurs_codePostal, "76240, 76520")
  expect_equal(ops_wide$codePostal, "76240")
})

test_that("ab_expand long duplicates rows per record", {
  ops <- dplyr::tibble(
    id = c(1L, 2L),
    adressesOperateurs = c(
      paste0("id=10; ville=PARIS; active=TRUE | ",
             "id=11; ville=LYON; active=FALSE"),
      NA_character_
    )
  )
  
  res <- ab_expand(ops, long = TRUE)
  
  expect_equal(nrow(res), 3)
  expect_equal(res$id, c(1L, 1L, 2L))
  expect_equal(res$adressesOperateurs_id, c(10L, 11L, NA))
  expect_equal(res$adressesOperateurs_ville, c("PARIS", "LYON", NA))
  expect_equal(res$adressesOperateurs_active, c(TRUE, FALSE, NA))
  # La colonne source est conservée sur les lignes dupliquées
  expect_match(res$adressesOperateurs[1], "PARIS", fixed = TRUE)
  expect_match(res$adressesOperateurs[2], "LYON", fixed = TRUE)
})

test_that("ab_search returns fully expanded columns (mocked)", {
  local_mocked_bindings(
    .ab_request = function(url, query, error_on_404 = TRUE) {
      list(
        nbTotal = 1L,
        items = data.frame(
          id = 28792L, raisonSociale = "CECOBIO", siret = "51438127600013",
          numeroBio = 126851L, codeNAF = "47.29Z",
          categories = I(list(data.frame(
            id = 4L, nom = "Magasins spécialisés", stringsAsFactors = FALSE
          ))),
          adressesOperateurs = I(list(data.frame(
            id = 28835L, lieu = "60 AVENUE SAINT JUST", codePostal = "13004",
            ville = "MARSEILLE", lat = 43.316748, long = 5.404689,
            active = TRUE, stringsAsFactors = FALSE
          ))),
          stringsAsFactors = FALSE
        )
      )
    },
    .package = "rAgenceBio"
  )
  
  res <- ab_search("CECOBIO")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  # Colonnes de premier niveau + raw conservées
  expect_true(all(c("id", "nomAnnuaire", "siret", "numeroBio",
                    "codePostal", "ville", "raw") %in% names(res)))
  # Colonnes dépliées en "<champ>_<sousChamp>"
  expect_equal(res$adressesOperateurs_lieu, "60 AVENUE SAINT JUST")
  expect_equal(res$adressesOperateurs_codePostal, "13004")
  expect_equal(res$adressesOperateurs_lat, 43.316748)
  expect_equal(res$categories_nom, "Magasins spécialisés")
  expect_match(res$adressesOperateurs, "codePostal=13004", fixed = TRUE)
  # nomAnnuaire calculé par repli
  expect_equal(res$nomAnnuaire, "CECOBIO")
  
  # expand = FALSE : comportement historique (colonnes de premier niveau)
  res_raw <- ab_search("CECOBIO", expand = FALSE)
  expect_false("adressesOperateurs_lieu" %in% names(res_raw))
  expect_true(all(c("id", "nomAnnuaire", "siret", "numeroBio",
                    "codePostal", "ville", "raw") %in% names(res_raw)))
})

test_that("ab_search can duplicate rows per address with long = TRUE", {
  local_mocked_bindings(
    .ab_request = function(url, query, error_on_404 = TRUE) {
      list(
        nbTotal = 1L,
        items = data.frame(
          id = 28792L, raisonSociale = "CECOBIO", siret = "51438127600013",
          numeroBio = 126851L,
          adressesOperateurs = I(list(data.frame(
            id = c(1L, 2L),
            codePostal = c("13004", "13005"),
            ville = c("MARSEILLE", "AUBAGNE"),
            lat = c(43.3, 43.29), long = c(5.4, 5.57),
            stringsAsFactors = FALSE
          ))),
          stringsAsFactors = FALSE
        )
      )
    },
    .package = "rAgenceBio"
  )
  
  res <- ab_search("CECOBIO", long = TRUE)
  expect_equal(nrow(res), 2)
  expect_equal(res$adressesOperateurs_codePostal, c("13004", "13005"))
  expect_equal(res$codePostal, c("13004", "13005"))
  expect_equal(res$ville, c("MARSEILLE", "AUBAGNE"))
  # Colonnes d'opérateur et raw répétées sur chaque ligne
  expect_equal(res$nomAnnuaire, c("CECOBIO", "CECOBIO"))
  expect_equal(vapply(res$raw, nrow, integer(1)), c(1L, 1L))
})

test_that("ab_get returns a flattened tibble by default (mocked)", {
  local_mocked_bindings(
    .ab_request = function(url, query, error_on_404 = TRUE) {
      list(data.frame(
        id = 28792L, raisonSociale = "CECOBIO", siret = "51438127600013",
        numeroBio = 126851L,
        adressesOperateurs = I(list(data.frame(
          codePostal = "13004", ville = "MARSEILLE",
          stringsAsFactors = FALSE
        ))),
        stringsAsFactors = FALSE
      ))
    },
    .package = "rAgenceBio"
  )
  
  flat <- ab_get(126851L)
  expect_s3_class(flat, "tbl_df")
  expect_equal(nrow(flat), 1)
  expect_equal(flat$adressesOperateurs_codePostal, "13004")
  expect_equal(flat$codePostal, "13004")
  expect_equal(flat$ville, "MARSEILLE")
  expect_false(any(vapply(flat, is.list, logical(1))))
  
  # flatten = FALSE : l'enregistrement brut en liste
  op <- ab_get(126851L, flatten = FALSE)
  expect_type(op, "list")
  expect_equal(op$raisonSociale, "CECOBIO")
})

# Faux .ab_request pour ab_get() : une réponse par numeroBio connu, NULL pour
# les autres (comme error_on_404 = FALSE sur un 404). Les paramètres reçus sont
# consignés dans `env$queries` pour vérifier les appels effectués.
.fake_ab_get_request <- function(env) {
  records <- list(
    "126851" = data.frame(
      id = 28792L, raisonSociale = "CECOBIO", siret = "51438127600013",
      numeroBio = 126851L,
      adressesOperateurs = I(list(data.frame(
        codePostal = "13004", ville = "MARSEILLE", stringsAsFactors = FALSE
      ))),
      stringsAsFactors = FALSE
    ),
    "125787" = data.frame(
      id = 11111L, raisonSociale = "FERME DU XX", siret = "48380302900048",
      numeroBio = 125787L,
      adressesOperateurs = I(list(data.frame(
        codePostal = "76240", ville = "BELBEUF", stringsAsFactors = FALSE
      ))),
      stringsAsFactors = FALSE
    )
  )
  env$queries <- list()
  env$error_on_404 <- NULL
  function(url, query = NULL, error_on_404 = TRUE) {
    env$queries[[length(env$queries) + 1]] <- query
    env$error_on_404 <- c(env$error_on_404, error_on_404)
    rec <- records[[as.character(query$numeroBio)]]
    if (is.null(rec)) NULL else list(rec)
  }
}

test_that("ab_get accepts several numeroBio and binds them (mocked)", {
  env <- new.env()
  local_mocked_bindings(
    .ab_request = .fake_ab_get_request(env),
    .package = "rAgenceBio"
  )
  
  res <- ab_get(c(126851L, 125787L))
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_equal(res$numeroBio, c(126851L, 125787L))
  expect_equal(res$codePostal, c("13004", "76240"))
  expect_equal(res$ville, c("MARSEILLE", "BELBEUF"))
  expect_false(any(vapply(res, is.list, logical(1))))
  
  # Une requête par numéro, chacune avec un paramètre de longueur 1
  # (régression : « values must be length 1 » dans httr::modify_url)
  expect_length(env$queries, 2)
  expect_true(all(vapply(env$queries, function(q) length(q$numeroBio) == 1L,
                         logical(1))))
  # Un numéro inconnu ne doit pas faire échouer la boucle
  expect_false(any(env$error_on_404))
})

test_that("ab_get accepts character and numeric vectors alike (mocked)", {
  env <- new.env()
  local_mocked_bindings(
    .ab_request = .fake_ab_get_request(env),
    .package = "rAgenceBio"
  )
  
  res_chr <- ab_get(c("126851", "125787"))
  res_num <- ab_get(c(126851, 125787))
  expect_equal(res_chr$numeroBio, res_num$numeroBio)
  expect_equal(nrow(res_chr), 2)
})

test_that("ab_get ignores unknown numeroBio among several (mocked)", {
  env <- new.env()
  local_mocked_bindings(
    .ab_request = .fake_ab_get_request(env),
    .package = "rAgenceBio"
  )
  
  res <- ab_get(c(126851L, 999999999L, 125787L))
  expect_equal(nrow(res), 2)
  expect_equal(res$numeroBio, c(126851L, 125787L))
  # Les trois numéros ont bien été interrogés
  expect_length(env$queries, 3)
})

test_that("ab_get returns NULL when no numeroBio is found (mocked)", {
  env <- new.env()
  local_mocked_bindings(
    .ab_request = .fake_ab_get_request(env),
    .package = "rAgenceBio"
  )
  
  expect_null(ab_get(999999999L))
  expect_null(ab_get(c(999999999L, 888888888L)))
  expect_null(ab_get(numeric(0)))
  expect_null(ab_get(NA))
})

test_that("ab_get drops duplicated numeroBio before querying (mocked)", {
  env <- new.env()
  local_mocked_bindings(
    .ab_request = .fake_ab_get_request(env),
    .package = "rAgenceBio"
  )
  
  res <- ab_get(c(126851L, 126851L, 125787L))
  expect_equal(nrow(res), 2)
  expect_length(env$queries, 2)
})

test_that("ab_get(flatten = FALSE) returns a named list for several numeroBio (mocked)", {
  env <- new.env()
  local_mocked_bindings(
    .ab_request = .fake_ab_get_request(env),
    .package = "rAgenceBio"
  )
  
  raw <- ab_get(c(126851L, 125787L), flatten = FALSE)
  expect_type(raw, "list")
  expect_named(raw, c("126851", "125787"))
  expect_equal(raw[["126851"]]$raisonSociale, "CECOBIO")
  expect_equal(raw[["125787"]]$raisonSociale, "FERME DU XX")
  
  # Un seul numéro : l'enregistrement directement (comportement inchangé)
  one <- ab_get(126851L, flatten = FALSE)
  expect_equal(one$raisonSociale, "CECOBIO")
})

test_that("progress helpers stay silent in non-interactive sessions", {
  skip_if(interactive())
  expect_null(.ab_progress_new(10))
  expect_null(.ab_progress_new(1))
  expect_no_error(.ab_progress_tick(NULL, 2))
  expect_no_error(.ab_progress_done(NULL))
})
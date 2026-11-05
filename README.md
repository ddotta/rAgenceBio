# rAgenceBio

Client R pour l'**API Agence Bio** — annuaire des professionnels certifiés
biologiques en France.

## Installation

```r
# Depuis le dépôt Git
> remotes::install_github("ddotta/rAgenceBio")


# Ou depuis un dossier local
devtools::install_local()
```

## Fonctions principales

| Fonction | Description |
|---|---|
| `ab_search(query, code_postal, limit, expand, long)` | Recherche d'opérateurs (texte libre, code postal) — résultat entièrement aplati et déplié |
| `ab_get(numeroBio, flatten = TRUE)` | Fiche complète d'un opérateur par numéro bio — tibble entièrement aplati |
| `ab_by_siret(siret, expand, long)` | Recherche par SIRET (1 ou plusieurs) — résultat aplati, une ligne par adresse |
| `ab_suggestions(query)` | Suggestions de noms (dérivées de la recherche) |
| `ab_flatten(x, level)` | Aplatissement des données imbriquées en tibble tidy |
| `ab_expand(x, columns, long)` | Déplie les colonnes "nom=valeur" aplaties en colonnes séparées |

## Exemples

### 1. Recherche par mot-clé

```r
library(rAgenceBio)

# Rechercher des brasseries bio : résultat entièrement aplati et déplié
# (colonnes id, nomAnnuaire, codePostal, ville + tous les champs de l'API
# en colonnes : adressesOperateurs_lieu, categories_nom, ... ; raw conserve
# l'enregistrement brut complet)
brasseries <- ab_search("brasserie", limit = 10)
print(brasseries)
#> # A tibble: 10 × 49
#>       id nomAnnuaire    siret     numeroBio codePostal ville
#>    <int> <chr>         <chr>       <int> <chr>      <chr>
#> 1     12 Brasserie XX  123456789  119135  69850      Lyon
#> # ℹ 42 more variables: adressesOperateurs_lieu <chr>, adressesOperateurs_codePostal <chr>, …
#> #   categories_id <int>, categories_nom <chr>, activites_nom <chr>, raw <list>, …

# Une ligne par adresse pour les opérateurs multi-sites : long = TRUE
multi <- ab_search("brasserie", limit = 10, long = TRUE)

# Seulement les colonnes historiques (id, nomAnnuaire, siret, numeroBio,
# codePostal, ville, raw) : expand = FALSE
light <- ab_search("brasserie", limit = 10, expand = FALSE)

```

### 2. Recherche par SIRET

```r
# Un seul SIRET : résultat entièrement aplati (aucun data.frame imbriqué),
# avec les champs multi-valués dépliés en colonnes séparées
ops <- ab_by_siret("51438127600013")
print(ops)
#> # A tibble: 1 × 49
#>       id raisonSociale denominationcourante siret          numeroBio codePostal ville   lat  long
#>    <int> <chr>         <chr>                <chr>              <int> <chr>      <chr> <dbl> <dbl>
#> 1  28792 CECOBIO       DOME N BIO           51438127600013    126851 13004      MARSE…  43.3  5.40
#> # ℹ 40 more variables: adressesOperateurs_id <int>, adressesOperateurs_lieu <chr>, …
#> #   adressesOperateurs_codePostal <chr>, adressesOperateurs_ville <chr>, …
#> #   categories_id <int>, categories_nom <chr>, activites_id <int>, activites_nom <chr>, …

ops$adressesOperateurs_lieu
#> [1] "60 AVENUE SAINT JUST"
ops$adressesOperateurs_codePostal
#> [1] "13004"
ops$categories_nom
#> [1] "Magasins spécialisés"

# productions reste une chaîne lisible (sous-enregistrements imbriqués) ;
# pour une ligne par production : ab_flatten(ops, level = "productions")
ops$productions
#> [1] "id=1318; code=47.00.11; nom=Commerce de détail de fruits et légumes frais; …"

# Opérateur avec plusieurs adresses : une ligne par adresse (par défaut)
ops_multi <- ab_by_siret("48380302900048")
ops_multi[, c("adressesOperateurs_lieu", "adressesOperateurs_codePostal",
              "codePostal", "ville")]
#> # A tibble: 2 × 4
#>   adressesOperateurs_lieu       adressesOperateurs_codePostal codePostal ville
#>   <chr>                         <chr>                         <chr>      <chr>
#> 1 RUE DU PACIFIQUE              76240                         76240      BELBEUF
#> 2 218 Rue des canadiens         76520                         76520      Les Authieux-sur-le-Port-Saint-Ouen

# Une ligne par opérateur (valeurs jointes) : long = FALSE
ops_wide <- ab_by_siret("48380302900048", long = FALSE)

# Colonnes d'origine conservées si besoin : expand = FALSE
ops_raw <- ab_by_siret("51438127600013", expand = FALSE)

# Plusieurs SIRETs (les SIRETs inconnus ou invalides sont ignorés),
# avec barre de progression en session interactive
ops_multi <- ab_by_siret(c("51438127600013", "98765432100022"))
print(ops_multi)
#> # A tibble: 1 × 30  (seul le SIRET connu est conservé)
```

### 3. Fiche d'un opérateur par numéro bio

```r
# Résultat entièrement aplati et déplié (une ligne par adresse), comme
# ab_by_siret() : champs scalaires en colonnes, champs multi-valués dépliés
# en colonnes adressesOperateurs_*, categories_*, ... (productions reste une
# chaîne lisible)
op <- ab_get(126851)
print(op)
#> # A tibble: 1 × 49
#>       id raisonSociale denominationcourante siret          numeroBio codePostal ville   lat  long
#>    <int> <chr>         <chr>                <chr>              <int> <chr>      <chr> <dbl> <dbl>
#> 1  28792 CECOBIO       DOME N BIO           51438127600013    126851 13004      MARSE…  43.3  5.40
#> # ℹ 40 more variables: adressesOperateurs_id <int>, adressesOperateurs_lieu <chr>, …

op$adressesOperateurs_lieu
#> [1] "60 AVENUE SAINT JUST"

# Enregistrement brut sous forme de liste si besoin
raw <- ab_get(126851, flatten = FALSE)
print(raw$certificats)
```

### 4. Données aplaties (tidy)

```r
# Un tibble avec une ligne par opérateur
ops <- ab_search("maraîchage", limit = 20)
flat <- ab_flatten(ops, level = "summary")
print(flat)
#> # A tibble: 20 × 20
#>       id nom            numeroBio codePostal ville    activites ...

# Une ligne par production
prods <- ab_flatten(ops, level = "productions")
print(prods)
#> # A tibble: 47 × 8
#>       id nom            production_code production_nom etat anneeReference
#>    <int> <chr>          <chr>           <chr>         <chr>        <int>

# Une ligne par adresse
addrs <- ab_flatten(ops, level = "addresses")

# Une ligne par certificat
certs <- ab_flatten(ops, level = "certificates")
```

### 5. Suggestions de recherche

```r
sugg <- ab_suggestions("la ferme")
print(sugg)
#> # A tibble: 50 × 3
#>   nomAnnuaire     nom              codePostal
#>   <chr>           <chr>            <chr>
#> 1 La Ferme du XX  LA FERME DU XX   33000
#> ...
```

### 6. Analyse combinée avec dplyr

```r
library(dplyr)

# Opérateurs bio par département, avec leurs productions
ops <- ab_search("producteur", limit = 50)
flat_prods <- ab_flatten(ops, level = "productions")
flat_addr <- ab_flatten(ops, level = "addresses")

# Croiser avec le tibble d'adresses
combined <- flat_prods %>%
  left_join(
    flat_addr %>% group_by(numeroBio) %>% summarise(ville = first(ville), .groups = "drop"),
    by = "numeroBio"
  )

# Compter les opérateurs par type de production
combined %>%
  group_by(production_nom) %>%
  summarise(nb = n(), .groups = "drop") %>%
  arrange(desc(nb)) %>%
  head(10)
```

## Barre de progression

Les fonctions qui enchaînent plusieurs appels API (`ab_by_siret()` avec
plusieurs SIRET, `ab_search()` qui pagine) affichent une barre de
progression en **session interactive** (console R, RStudio). Elle est
automatiquement désactivée dans les scripts non interactifs
(`Rscript`, rapports) pour ne pas polluer la sortie.

Pour la désactiver :

```r
options(rAgenceBio.progress = FALSE)
```

## API utilisée

- **Open Data** : [`opendata.agencebio.org`](https://opendata.agencebio.org)
  (« API Professionnels BIO », documentée sur
  [data.gouv.fr](https://www.data.gouv.fr/dataservices/api-professionnels-bio))

## Limites de l'API

- **50 requêtes / seconde / IP** (limite côté serveur)
- **500 résultats maximum** par page (`nb`/`debut`) ;
  `ab_search()` récupère automatiquement les pages nécessaires
- Les données sont celles des opérateurs **certifiés** par les organismes
  certificateurs agréés par l'INAO

## Proxies et certificats SSL

Derrière un proxy d'entreprise qui **intercepte le trafic HTTPS** (inspection
TLS / SSL bumping), le proxy présente son propre certificat, signé par une
autorité de certification interne. Ce certificat n'est pas dans le magasin
de certificats racine utilisé par R (bundle CA livré avec R, contrairement au
navigateur qui, lui, fait confiance au magasin Windows de l'entreprise).
`httr`/libcurl échoue alors avec une erreur du type
« SSL peer certificate … was not OK ».

**Solutions, de la plus propre à la moins recommandée :**

1. **Liste blanche** : autoriser `opendata.agencebio.org` dans le proxy pour
   qu'il laisse passer le trafic sans interception. La vérification SSL reste
   active, rien à changer côté R. C'est l'option recommandée.
2. **Certificat de l'entreprise** : donner à R le certificat CA interne pour
   que la vérification reste active :
   ```r
   httr::set_config(httr::config(cainfo = "chemin/vers/ca-entreprise.pem"))
   ```
3. **Désactiver la vérification** (dernier recours, à réserver à un
   environnement maîtrisé — le trafic n'est plus authentifié) :
   ```r
   options(rAgenceBio.ssl_verifypeer = FALSE)
   brasseries <- ab_search("brasserie", limit = 10)
   ```

Le package n'utilise qu'un seul hôte, qui doit être autorisé (liste blanche)
par le proxy :

- `opendata.agencebio.org`

## Licence

GPL-3

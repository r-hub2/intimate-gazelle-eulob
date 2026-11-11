# eulob 0.2.0

## New features

- Expanded coverage to 20 European territorial scopes,
  including 19 scopes with indexed entities.
- Added cross-register organization lookup through
  `eulob_cross_registers()`.
- Added approximate name matching with cosine similarity,
  Jaccard, Jaro-Winkler and Damerau-Levenshtein.
- Extended country-specific and register-specific searches.

## Data

- Expanded the catalog to 42,265 indexed entries.
- Updated the normalized-name and entity-reference catalogs.
- Preserved Netherlands historical data separately because
  reliable organization identification was unavailable.

## Tests

- Added regression tests for cosine similarity.
- Extended cross-register search tests.
- Updated catalog integrity and search-method tests.

# eulob 0.1.0

## Initial release

- Introduced the initial European lobbying-register catalog.
- Established the package API and documentation.

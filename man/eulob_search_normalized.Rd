\name{eulob_search_normalized}
\alias{eulob_search_normalized}
\title{Search normalized lobbying names and candidate equivalences}
\usage{
eulob_search_normalized(
  query,
  method = c("strict", "exploratory", "both"),
  limit = 100L,
  min_similarity = 0.88,
  candidate_filter = c("focused", "all")
)
}
\arguments{
\item{query}{A single non-empty organization name.}
\item{method}{One of \code{"strict"}, \code{"exploratory"} or \code{"both"}.}
\item{limit}{Maximum number of name records to return.}
\item{min_similarity}{Minimum similarity for exploratory fuzzy candidates, from 0 to 1.}
\item{candidate_filter}{Use \code{"focused"} to prioritize lexical matches, or \code{"all"} to preserve all exploratory candidates.}
}
\description{
Searches original lobbying registrations using a conservative
nominal matching method or an expanded exploratory method.
}
\details{
Strict mode includes exact name matches, documented registration
aliases and highly conservative orthographic candidates.
Exploratory mode also includes fuzzy and shared-prefix candidates.
The both option returns the expanded results with their matching
levels identified. No method verifies common legal identity.
}
\value{
A data frame retaining the original country, register,
lobby identifier and name, with matching evidence,
similarity and review indicators.
}

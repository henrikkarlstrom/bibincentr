#' Author affiliation information for publications
#'
#' @param url A string representing the contributor
#' page for a publication.
#'
#' @return The function returns a list of institution codes
#' that can be checked for the presence of non-Norwegian
#' institutions to determine whether the publication is
#' an international co-publication.
#' 
#' @export
#'
#' @examples
#' get_contributor_info("https://api.cristin.no/v2/results/1674937/contributors")

get_contributor_info <- function(url) {
  data <- httr::GET(url)
  
  innhold <- jsonlite::fromJSON(
    httr::content(data, "text"))
  
  affils <- lapply(
    innhold[["affiliations"]],
    function(x) {
      x[["institution"]][["cristin_institution_id"]]
    })
}

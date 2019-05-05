#' Get publication data from the CRIStin API
#'
#' Fetches authorship data for peer-reviewed academic
#' publications with at least one Norwegian author.
#' 
#' @param category A string specifying that the API should
#' only return peer-reviewed publications - CRIStin contains
#' all sorts of research results, including dissemination
#' efforts or non-peer reviewed publications. These are
#' filtered out here.
#' 
#' @param published_since,published_before Numeric, sets
#' the first and last year of the query.
#' 
#' @param page,per_page Numeric, pagination parameters for
#' the API.
#' 
#' @param fields A string specifying the set of fields the
#' API should return, in order for it to return the time
#' created field.
#'
#' @return The function returns a data frame of unique 
#' publications with the following columns:
#' - 'ID':            numeric, a unique identifier for 
#'                    each publication
#' - 'Date':          POSIXtc, the date the publication 
#'                    was registered in CRIStin
#' - 'Contributors':  string, a URL for the author 
#'                    affiliation information in the API
#' 
#' 
#' @export
#' 
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' 
#' @examples
#' get_data(published_since = 2019, category = "MONOGRAPHACA")

get_data <- function(category = "(ARTICLE or MONOGRAPHACA or ANTHOLOGYACA)",
                     published_since = 2004,
                     published_before = 2019,
                     page = 1,
                     per_page = 999,
                     fields = "all") {
  
  counter <- 0
  total <- 1
  frame <- data.frame()
  
  while(counter < total) {
    
    data <- httr::GET(url = "https://api.cristin.no/v2/results?", 
                query = list(category = category,
                             published_since = published_since,
                             published_before = published_before, 
                             per_page = per_page,
                             page = page,
                             fields = fields))
    
    if(httr::status_code(data) != 200 || 
       data$headers$`x-total-count` == 0) {
      
      counter <- total + 1
      return(NA)
      
    } else {
      
      total <- as.numeric(data$headers$`x-total-count`)
      counter <- counter + 1000
      page <- page + 1
      
      #
      content <- jsonlite::fromJSON(
        httr::content(data, "text"))
      
      content <- content %>%
        dplyr::mutate(Date = .data[["created"]][["date"]],
                      Contributors = .data[["contributors"]][["url"]]) %>%
        dplyr::select(.data[["cristin_result_id"]], 
                      .data[["Date"]], 
                      .data[["Contributors"]])
      
      frame <- dplyr::bind_rows(frame, content)
      
    }
  }
  return(frame)
}
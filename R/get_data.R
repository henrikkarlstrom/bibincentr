#' Get publication data from the CRIStin API
#'
#' Fetches authorship data for peer-reviewed academic
#' publications with at least one Norwegian author
#'
#'
#' @param unit A string representing an institution or
#' sub-unit of an institution in the CRIStin. It takes
#' the form of four values separated by periods, where
#' each value represents one unit at that level in the
#' institutional hierarchy of a Norwegian research
#' institution.
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
#' @return The function returns a data frame of unique 
#' publications with the following columns:
#' - 'ID': numeric, a unique identifier for each publication
#' - 'Date': POSIXtc, the date the publication was registered
#' in CRIStin
#' 
#' @export
#'
#' @examples
#' get_data("test")

get_data <- function(unit,
                     category = "(ARTICLE or MONOGRAPHACA or ANTHOLOGYACA)",
                     published_since = 2004,
                     published_before = 2019,
                     page = 1,
                     per_page = 999) {
  
  counter <- 0
  total <- 1
  frame <- data.frame()
  
  while(counter < total) {
    
    data <- GET(url = "https://api.cristin.no/v2/results?", 
                query = list(unit = unit, 
                             category = category,
                             published_since = published_since,
                             published_before = published_before, 
                             per_page = per_page,
                             page = page))
    
    if(status_code(data) != 200 || data$headers$`x-total-count` == 0) {
      
      counter <- total + 1
      return(NA)
      
    } else {
      
      total <- as.numeric(data$headers$`x-total-count`)
      counter <- counter + 1000
      page <- page + 1
      
      #
      content <- fromJSON(content(data, "text"))
      
      content <- content %>%
        select(cristin_result_id, 
               date_published, 
               contributors) %>%
        mutate(contributors = contributors$url)
      
      frame <- bind_rows(frame, content)
      
    }
  }
  return(ramme)
}
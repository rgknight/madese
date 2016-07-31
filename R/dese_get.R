require(httr)
require(rvest)


#' Get data from Massachusetts DESE 
#' 
#' Get public school information from Massachusetts \href{http://profiles.doe.mass.edu/state_report/}{State Reports}.
#' 
#' A wrapper function that execute a GET to http://profiles.doe.mass.edu/state_report/ 
#' and parses the results.
#' 
#' @param report the name of the report you are getting
#' @param query any additional query needed to complete the url
#' @return data.frame
#' @seealso \code{\link{GET}} and \code{\link{rvest}} which this function wraps
#' @examples dese_get('enrollmentbyracegender', query=list('mode'='school', 'year'='2016))
dese_get <- function(report, query=NULL){
  
  baseurl <- 'http://profiles.doe.mass.edu'
  ua <- user_agent("http://github.com/hadley/httr")
  
  basic_query <- list(
    'Continue'='View+Report',
    'export_excel'='yes'
  )
  
  full_query <- append(basic_query, query)
  
  print(full_query)
  
  url <- modify_url(baseurl, 
                    path=paste0('state_report/', report, '.aspx'),
                    query=full_query)
  
  print(url)
  
  resp <- GET(url, ua)
  
  parsed <- content(resp, as="text")
  
  if (grepl("404 - Page Not Found", parsed)) {
    stop(
      sprintf(
        "Request failed; report not found [%s]\n%s", 
        status_code(resp),
        resp$url
      ),
      call. = FALSE
    )
  } 
  
  data <- read_html(parsed) %>%
    html_node('table') %>%
    html_table(header = T, fill=T)

  data
}

#' Enrollment by Race and Gender
#' 
#' Get the \href{http://profiles.doe.mass.edu/state_report/enrollmentbyracegender.aspx}{Enrollemnt by Race/Gender} Report from Massachusetts DESE 
#' 
#' @param year a character string with the numeric year (e.g., school year 2015-2016 is "2016")
#' @param mode district or school 
#' @return data.frame
#' @seealso \code{\link{dese_get}} which this function wraps
#' @export
#' @examples
#' dese_enrollmentbyracegender("2016")
dese_enrollmentbyracegender <- function(year, mode='school'){
  query <- list(
    'mode'=mode,
    'year'=as.character(year)
  )
    
  dese_get('enrollmentbyracegender', query)
  
}

#' Enrollment by Selected Populations
#' 
#' Get the \href{http://profiles.doe.mass.edu/state_report/selectedpopulations.aspx}{Enrollemnt by Selected Populations} Report from Massachusetts DESE 
#' (first language not english, english language learners, students with disabilities, etc)
#' 
#' @param year a character string with the numeric year (e.g., school year 2015-2016 is "2016")
#' @param mode district or school 
#' @return data.frame
#' @seealso \code{\link{dese_get}} which this function wraps
#' @export
#' @examples
#' dese_selectedpopulations("2016")
dese_selectedpopulations <- function(year, mode='school'){
  query <- list(
    'mode'=mode,
    'year'=as.character(year)
  )
  
  data <- dese_get('selectedpopulations', query)
  
  # data has two header rows; remove extra row
  extra_names <- data[1, ]
  data <- data[-1, ]
  cols <- ncol(data)
  names(data)[3:cols] <- paste0(names(data)[3:cols], "_", extra_names[3:cols])
  
  data
}

#' Not working yet...
#' #' MCAS Achievement Results
#' #' 
#' #' Get \href{http://profiles.doe.mass.edu/state_report/mcas.aspx}{MCAS} Reports from Massachusetts DESE 
#' #' 
#' #' 
#' #' @param year a character string with the numeric year (e.g., school year 2015-2016 is "2016")
#' #' @param mode district or school 
#' #' @param SchoolType All, Elementary, Middle, etc
#' #' @param studentGroup 'AL' for all, 
#' #' @param grade 'AL' for all, 10, 08, etc
#' #' @return data.frame
#' #' @seealso \code{\link{dese_get}} which this function wraps
#' #' @export
#' #' @examples
#' #' dese_mcas("2014")
#' dese_mcas <- function(year, SchoolType='All', studentGroup='AL', grade='AL',mode='school'){
#' 
#'     query <- list(
#'     'reportType'=mode,
#'     'grade'=grade,
#'     'year'=as.character(year),
#'     'apply2006'='Y',
#'     'studentGroup'='AL',
#'     'SchoolType'='All'
#'   )
#'   
#'   dese_get('mcas', query)
#' }
#' 

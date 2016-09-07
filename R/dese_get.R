require(dplyr)

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
#' @import httr
#' @import rvest
#' @seealso \code{\link{GET}} and \code{\link{rvest}} which this function wraps
#' @examples
#' dese_get('enrollmentbyracegender', query=list('mode'='school', 'year'='2016'))
dese_get <- function(report, query=NULL){

  baseurl <- 'http://profiles.doe.mass.edu'
  ua <- user_agent("https://github.com/rgknight/madese")

  url <- modify_url(baseurl,
                    path=paste0('state_report/', report, '.aspx'),
                    query=query)

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

  parsed
}

#' Enrollment by Race and Gender
#'
#' Get the \href{http://profiles.doe.mass.edu/state_report/enrollmentbyracegender.aspx}{Enrollemnt by Race/Gender} Report from Massachusetts DESE
#'
#' @param year a character string with the numeric year (e.g., school year 2015-2016 is "2016")
#' @param mode district or school
#' @return data.frame
#' @seealso \code{\link{dese_get}} which this function wraps
#' @import rvest
#' @export
#' @examples
#' dese_enrollmentbyracegender("2016")
dese_enrollmentbyracegender <- function(year, mode='school'){
  query <- list(
    'mode'=mode,
    'year'=as.character(year),
    'Continue'='View+Report',
    'export_excel'='yes'
  )

  parsed <- dese_get('enrollmentbyracegender', query)

  read_html(parsed) %>%
    html_node('table') %>%
    html_table(header = T, fill=T)


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
#' @import rvest
#' @export
#' @examples
#' dese_selectedpopulations("2016")
dese_selectedpopulations <- function(year, mode='school'){
  query <- list(
    'mode'=mode,
    'year'=as.character(year),
    'Continue'='View+Report',
    'export_excel'='yes'
  )

  data <- dese_get('selectedpopulations', query)

  data <- read_html(parsed) %>%
    html_node('table') %>%
    html_table(header = T, fill=T)

  # data has two header rows; remove extra row
  extra_names <- data[1, ]
  data <- data[-1, ]
  cols <- ncol(data)
  names(data)[3:cols] <- paste0(names(data)[3:cols], "_", extra_names[3:cols])

  data
}

#' Attrition
#'
#' Get the \href{http://profiles.doe.mass.edu/state_report/attrition.aspx}{Attrition}
#' Report from Massachusetts DESE website.
#' Attrition is students who left the school over the summer.
#'
#' @param year the year of the report
#' @param mode district or school
#' @param group get the report for a specific student group. Defaults to all students.
#' @seealso \code{\link{dese_get}} which this function wraps
#' @import rvest
#' @importFrom magrittr extract2
#' @importFrom xml2 read_html
#' @export
#' @examples
#' dese_attrition("2016")
dese_attrition <- function(year, mode='SCHOOL', group='ALL'){
  query <- list(
    'ctl00$ContentPlaceHolder1$reportType'=toupper(mode),
    'ctl00$ContentPlaceHolder1$fycode'=as.character(year),
    'ctl00$ContentPlaceHolder1$studentGroup'=toupper(group),
    'export_excel'='yes'
  )

  parsed <- dese_get('attrition', query)

  result <- read_html(parsed) %>%
    html_nodes('table') %>%
    extract2(2) %>%
    html_table(header = T, fill=T)

  result <- result %>%
    mutate_at(vars(3:15),funs(gsub(" ", "", .)))

  result
}

# Not working yet...
# #' MCAS Achievement Results
# #'
# #' Get \href{http://profiles.doe.mass.edu/state_report/mcas.aspx}{MCAS} Reports from Massachusetts DESE
# #'
# #'
# #' @param year a character string with the numeric year (e.g., school year 2015-2016 is "2016")
# #' @param mode district or school
# #' @param SchoolType All, Elementary, Middle, etc
# #' @param studentGroup 'AL' for all,
# #' @param grade 'AL' for all, 10, 08, etc
# #' @return data.frame
# #' @seealso \code{\link{dese_get}} which this function wraps
# #' @examples
# #' dese_mcas("2014")
# dese_mcas <- function(year, SchoolType='All', studentGroup='AL', grade='AL',mode='school'){
#
#     query <- list(
#     'reportType'=mode,
#     'grade'=grade,
#     'year'=as.character(year),
#     'apply2006'='Y',
#     'studentGroup'='AL',
#     'SchoolType'='All'
#   )
#
#   dese_get('mcas', query)
# }
#

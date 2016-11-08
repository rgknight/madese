#' dese_multiget
#'
#' Note: Not working.
#' Get multiple years, groups and/or modes in a single call.
#' The result is a named list of dataframes where the names have the format
#' "[report]_[mode]_[group]_[year]"
#'
#' @param report Quoted name of the report where the name is an madese function with form dese_[report]
#' @param year One or more years to get (as a list if multiple)
#' @param mode One or more modes to get (usually SCHOOL)
#' @param group One or more student sub-groups to get
#' @import httr
#' @import rvest
#' @examples
#' mode <- "SCHOOL"
#' years <- 2015:2016
#' groups <- "ALL"
#' dese_multiget('enrollmentbyracegender', modes=mode, groups=groups, years=years))

dese_multiget <- function(report, ...){
  params <- list(...)
  years <- params$years
  groups <- params$groups
  modes <- params$modes

  alldata_len <- length(modes)*length(groups)*length(years)
  alldata <- vector("list", alldata_len)
  allnames <- vector("list", alldata_len)
  i <- 1

  params <- list(...)

  params_len <- length(params)

  for (param in params){
    for (group in groups){
      for (year in years){
        params <- list(...)
        print(sprintf('Getting %s for %s', collapse(params)))
        idata <- do.call(report, params)
        idata$year  <- year
        idata$mode  <- mode
        idata$group <- group

        iname <- paste(report, mode, group, year, sep="_")

        allnames[[i]] <- iname
        alldata[[i]] <- idata

        i <- i + 1
      }
    }
  }

  names(alldata) <- allnames
}


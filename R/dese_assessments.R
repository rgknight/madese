#' dese_parcc
#'
#' Get the PARCC Report from Massachusetts DESE
#'
#' @param year a character string with the numeric year (e.g., school year 2015-2016 is "2016")
#' @param mode district or school
#' @param group Student group
#' @param schooltype middle school, elementary school, etc
#' @param testtype The grade / subject of the test
#' @import rvest
#' @importFrom magrittr extract2
#' @importFrom xml2 read_html
#' @export
#' @examples
#' dese_enrollmentbyracegender("2016")

dese_parcc <- function(year="All", mode='All', group='All', schooltype='All', testtype='All'){

  # List all possible options so that folks don't have to do it themselves
  allyears <- 2015:2016
  allmodes <- list('SCHOOL', 'DISTRICT')

  allgroups <- list(
    "AL:AL",  # All
    "ED:N",   # Not economically disadvantaged
    "ED:Y",   # Economincally disadvantaged
    "GE:01",  # Male
    "GE:02",  # Female
    "HN:Y",   # High Needs
    "MH:Y",   # Moderate or High Severity
    "RA1:01", # American Indian or Alaskan Native
    "RA1:02", # African American Black
    "RA1:03", # Asian
    "RA1:04", # Hispanic / Latino
    "RA1:05", # White
    "RA1:06", # Native Hawaiian or Pacific Islander
    "RA1:07", # Multi-race, non-Hispanic or Latino
    "SS:LEP", # ELL
    "SS:LEPFLEP", # ELL and FELL
    "SS:SPED", # SPED
    "T1:0",   # Non-Title 1
    "T1:1")   # Title 1

  allschooltypes <- list(
    "All",
    "Elementary%20School",
    "Elementary-Middle%20School",
    "Middle%20School",
    "Middle-High%20School%20or%20K-12",
    "High%20School"
  )

  alltesttypes <-list(
    "MAT3-8",
    "ELA3-8",
    "ELA08",
    "ELA08",
    "ELA07",
    "ELA06",
    "ELA05",
    "ELA04",
    "ELA03",
    "MAT08",
    "MAT07",
    "MAT06",
    "MAT05",
    "MAT04",
    "MAT03",
    "ALG01-G8"
  )

  # Replace the default values with the lists if not overwritten
  if (year == "All"){year = allyears}
  if (mode == "All"){mode = allmodes}
  if (group == "All"){group = allgroups}
  if (schooltype == "All"){schooltype = allschooltypes}
  if (testtype == "All"){testtype = alltesttypes}

  # Expand options into a df with all option combinations to make the loops easier
  params <- expand.grid(year, mode, group, schooltype, testtype, stringsAsFactors = F)
  names(params) <- c("year", "mode", "group", "schooltype", "testtype")

  alldat <- vector("list", nrow(params))

  for (i in 1:nrow(params)){
    # Create and send queries for each version
    yr <- as.character(params[i, "year"])
    md <- toupper(params[i, "mode"])
    gp <- toupper(params[i, "group"])
    st <- toupper(params[i, "schooltype"])
    tt <- as.character(params[i, "testtype"])

    rnames <- make.names(do.call(paste, c(params[i, ], sep="_")))

    query <- list(
      'ctl00$ContentPlaceHolder1$year'=yr,
      'ctl00$ContentPlaceHolder1$reportType'=md,
      'ctl00$ContentPlaceHolder1$studentGroup'=gp,
      'ctl00$ContentPlaceHolder1$SchoolType'=st,
      'ctl00$ContentPlaceHolder1$testtype'=tt,
      'export_excel'='yes'
    )

    print(sprintf("Getting %s", rnames))

    parsed <- dese_get('parcc', query)

    if (grepl("No Data Exists for this search criteria", parsed)){
      print(sprintf("No data exists for %s", rnames))
      next
    }

    result <- read_html(parsed) %>%
      html_nodes('table') %>%
      extract2(3) %>%
      html_table(header = T, fill=T)

    result$year <- yr
    result$mode <- md
    result$group <- gp
    result$school_type <- st
    result$test_type <- tt

    alldat[[i]] <- result


    names(alldat)[i] <- rnames
  }

  alldat
}

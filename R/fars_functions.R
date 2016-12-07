#' Reads data from .csv file
#'
#' This function takes a filename, reads data from that file, and returns
#' a data frame containing that data. The function fars_read suppresses default
#' behaviour from readr::read_csv, which would otherwise display a progress bar when reading larger files.
#' You can customise this function to include the progress bar by editing
#' the function to replace 'progress = FALSE' with 'progress= TRUE'.
#'
#' @param filename A character string giving the filename (including path if required) of the data to be read
#'
#' If the file cannot be found, the function will stop with the message:
#'    "<filename> does not exist".
#'
#' @importFrom  readr read_csv
#'
#' @return  A data frame containing the data contained in the file with the given filename.
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#' @export
fars_read <- function(filename) {
  filepath = system.file("extdata", "accident_2013.csv.bz2", package="Asswk4Package")
  if(!file.exists(filepath))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filepath, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
#' Creates filename
#'
#' This function takes a year as input and returns a filename,"accident_<year>.csv.bz2".
#' If required, this function can be modified to include a path to a specific data directory by changing
#' the line sprintf("accident_%d.csv.bz2", year) to sprintf("mypath/accident_%d.csv.bz2", year)
#'
#' @param year An integer, or string which is convertable to an integer
#'
#' If the input parameter is not convertable to a string, a warning message will be produced.
#' The warning message for an input of "foo" reads: 'In make_filename("foo") : NAs introduced by coercion'
#'
#' @return String containing input parameter to be used as a filename.
#' For an input parameter of "2013", the returned string is "accident_2013.csv.bz2". For a non-integer input value of,
#' for example, "foo", the returned string is "accident_NA.csv.bz2"
#'
#' @examples
#' make_filename("2013")
#' make_filename(2014)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read accident data from fars data files.
#'
#' This function reads accident data from fars data files for the years specified as an input.
#' The corresponding filename is constructed for each year, and the data is read from that file.
#'
#' @param years A vector containing the required years, or a single integer year
#'
#' If, for any particular year, the file or data is not found,
#' a warning message is returned for the invalid year.
#'
#'
#' @return String containing input parameter to be used as a filename.
#' For an input parameter of "2013", the returned string is "accident_2013.csv.bz2".
#' For an input that cannot be converted to an integer,
#' for example, "foo", the returned string is ""accident_NA.csv.bz2"
#'
#' @examples
#' fars_read_years(2013)
#' fars_read_years(c(2013:2015))
#' fars_read_years(2013:2015)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate_(dat, year = ~ year) %>%
        dplyr::select_(.dots = c("MONTH", "year"))
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize fars accident data for a given set of years
#'
#' This function takes a set of years, and returns accident data from the corresponding fars files.
#'
#' @param years A vector containing the years for which data is required
#'
#' If the input parameter is not convertable to a string, a warning message will be produced.
#' The warning message for an input of "foo" reads: 'In make_filename("foo") : NAs introduced by coercion'
#'
#'
#' The function fars_summarize_years imports from packages dplyr and tidyr as follows:
#' @importFrom  dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @return dataframe
#' For an input parameter of "1994", the returned string is "accident_1994.csv.bz2". For a non-integer input value of,
#' for example, "foo", the returned string is ""accident_NA.csv.bz2"
#'
#' @examples
#' fars_summarize_years(c(2013,2014))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_( ~ year, ~ MONTH) %>%
    dplyr::summarize_(n = ~ n()) %>%
    tidyr::spread_(key_col = 'year', value_col = 'n')
}
#' Plot fars accident data
#'
#' This function takes an integer corresponding to a state state and a year, and plots the data from the corresponding fars file.
#'
#' @param state.num A length one vector containing integer, or string which is convertable to an integer
#' @param year A length one vector containg integer, or string which is convertable to an integer
#' If the input parameter is not convertable to a string, a warning message will be produced.
#' The warning message for an input of "foo" reads: 'In make_filename("foo") : NAs introduced by coercion'
#'
#' The function imports the following packages:
#'                                              dplyr::filter
#'                                              maps::map
#'                                              graphics::points
#'
#' @return NULL
#' For an input parameter of "1994", the returned string is "accident_1994.csv.bz2". For a non-integer input value of,
#' for example, "foo", the returned string is ""accident_NA.csv.bz2"
#'
#' @examples
#' fars_map_state(1,c(2013))
#' fars_map_state("1",2015)
#' fars_map_state(1,"2014")
#'
#' @importFrom  maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, ~ STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

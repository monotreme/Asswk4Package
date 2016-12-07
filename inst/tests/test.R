#' tests functions in the Asswk4Package
#'
#' This function tests the fars_read function
#'
#' @param year An integer or character string of an integer of a year.
#'
#' @import testthat
#'
#' @return  A string containing the filename of the accident data for the input year
#'
#' @examples
#' expect_that(make_filename("2013"),matches("accident_2013.csv.bz2"))
#'
#' @export
# test make_filename
expect_that(make_filename("2013"),matches("accident_2013.csv.bz2"))

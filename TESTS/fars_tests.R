library(testthat)
# test fars_Read
expect_that(fars_read("nonexistentfile.txt"),throws_error())
# test make_filenam
expect_that(make_filename("2013"),matches("accident_2013.csv.bz2"))

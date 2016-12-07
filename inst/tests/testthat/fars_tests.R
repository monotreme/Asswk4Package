library(testthat)
# test fars_Read
expect_that(fars_read("nonexistentfile.txt"),throws_error())
# test make_filenam
expect_that(make_filename("2013"),matches("accident_2013.csv.bz2"))
# test read_years

expect_that(read_years(2013),is_a("data.frame"))

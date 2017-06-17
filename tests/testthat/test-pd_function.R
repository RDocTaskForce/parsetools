#! This file was automatically produced by lint on  2017-06-06 12:03:28
#! changes will be overwritten.
context('tests extracted from file `/mnt/data/projects/rdtf/parsetools/R/pd_function.R`')
test_that("'is_pd_function'", {#! @testthat is_pd_function
    pd <- get_parse_data(parse(text="function(){}"))
    expect_true(is_pd_function(pd))

    pd <- get_parse_data(parse(text="fun <- function(){}"))
    expect_false(is_pd_function(pd))
})

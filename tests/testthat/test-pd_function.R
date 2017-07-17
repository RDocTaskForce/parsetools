#! This file was automatically produced by documentation::extract_tests on  2017-07-08 09:16:20
#! changes will be overwritten.
context('tests extracted from file `C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/pd_function.R`')
#line 35 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/pd_function.R"
test_that("is_pd_function", {#! @testthat is_pd_function
    pd <- get_parse_data(parse(text="function(){}"))
    expect_true(is_pd_function(pd))

    pd <- get_parse_data(parse(text="fun <- function(){}"))
    expect_false(is_pd_function(pd))
})

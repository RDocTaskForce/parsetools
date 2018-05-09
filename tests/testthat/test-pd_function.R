#! This file was automatically produced by documentation::extract_tests on  2018-04-30 10:01:17
#! changes will be overwritten.
context('tests extracted from file `C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/pd_function.R`')
#line 41 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/pd_function.R"
test_that('is_pd_function', {#! @testthat is_pd_function
    pd <- get_parse_data(parse(text="function(){}", keep.source=TRUE))
    expect_true(is_pd_function(pd))

    pd <- get_parse_data(parse(text="fun <- function(){}", keep.source=TRUE))
    expect_false(is_pd_function(pd))
})

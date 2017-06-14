#! This file was automatically produced by lint on  2017-06-06 12:03:28
#! changes will be overwritten.
context('tests extracted from file `./R/check_id.R`')
test_that("'._check_id'", {#!@testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    
    expect_identical(._check_id(pd), pd$id, info="Passing parse-data")
    expect_error(._check_id(iris)         , info="data.frame but not parse-data")
    expect_identical(._check_id(1)  , 1L  , info="convert numeric to integer")
    expect_identical(._check_id(1.1), 1L  , info="convert numeric to integer")
    expect_error(._check_id(TRUE)         , info="passing logical that cannot be converted.")
})

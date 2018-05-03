#! This file was automatically produced by documentation::extract_tests on  2018-05-03 10:17:17
#! changes will be overwritten.
context('tests extracted from file `/home/aredd/projects/rdtf/parsetools/R/checks.R`')
#line 37 "/home/aredd/projects/rdtf/parsetools/R/checks.R"
test_that('._check_id', {#!@testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))
    
    expect_identical(._check_id(pd), pd$id, info="Passing parse-data")
    expect_error(._check_id(iris)         , info="data.frame but not parse-data")
    expect_identical(._check_id(1)  , 1L  , info="convert numeric to integer")
    expect_identical(._check_id(1.1), 1L  , info="convert numeric to integer")
    expect_error(._check_id(TRUE)         , info="passing logical that cannot be converted.")
})
#line 54 "/home/aredd/projects/rdtf/parsetools/R/checks.R"
test_that('._check_parse_data', {#!@testing
    df <- getParseData(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))
    pd <- ._check_parse_data(df)
    expect_is(pd, "parse-data")
    expect_error(._check_parse_data(NULL), "Cannot convert to parse-data.")
})

#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `checks.R`')
#line 43 "/rdtf/parsetools/R/checks.R"
test_that('._check_id', {#!@testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))

    expect_error(._check_id(pd)  , "id must be an object that can be coerced to an integer", info="Passing parse-data")
    expect_error(._check_id(iris), "id must be an object that can be coerced to an integer", info="data.frame but not parse-data")
    expect_identical(._check_id(1)  , 1L  , info="convert numeric to integer")
    expect_identical(._check_id(1.1), 1L  , info="convert numeric to integer")
    expect_error(._check_id(TRUE)         , info="passing logical that cannot be converted.")
    expect_error( ._check_id(1000, pd)
                , 'id\\(1000\\) is not present in given parse-data.'
                , info="passing logical that cannot be converted."
                )
})
#line 64 "/rdtf/parsetools/R/checks.R"
test_that('._check_parse_data', {#!@testing
    df <- getParseData(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))
    pd <- ._check_parse_data(df)
    expect_is(pd, "parse-data")
    expect_error(._check_parse_data(NULL), "Cannot convert to parse-data.", info="passing NULL")
    expect_error(._check_parse_data(iris), "names of data do not conform", info="passing non-conforming data.frame")
})

#! This file was automatically produced by lint on  2017-06-06 12:03:28
#! changes will be overwritten.
context('tests extracted from file `./R/get_parse_data.R`')
test_that("'valid_parse_data'", {#!@test
    df <- getParseData(parse(text="rnorm(10,0,1)"))
    expect_true (valid_parse_data(df), 'parse-data')
    expect_equal(valid_parse_data(datasets::iris      ), "names of data do not conform.")
    expect_equal(valid_parse_data(stats::rnorm(10,0,1)), "Not a data.frame object")
})
test_that("'as_parse_data'", {#!@testing
    df <- getParseData(parse(text="rnorm(10,0,1)"))
    expect_is   (as_parse_data(df), 'parse-data')
    expect_error(as_parse_data(datasets::iris), "Cannot convert to parse-data: names of data do not conform.")
    expect_error(as_parse_data(stats::rnorm(10,0,1)), "Cannot convert to parse-data: Not a data.frame object")
})
test_that("'fix_eq_assign'", {#! @testthat
pd <- utils::getParseData(parse(text="a=1"))
fixed.pd <- fix_eq_assign(pd)
expect_true(nrow(pd)+1 == nrow(fixed.pd))
expect_that(sum(fixed.pd$parent==0), equals(1))

pd <- utils::getParseData(parse(text="a=1\nb<-2\nc=3\nd<<-4"))
fixed.pd <- fix_eq_assign(pd)
expect_true(nrow(pd)+2 == nrow(fixed.pd))
expect_that(sum(fixed.pd$parent==0), equals(4))

pd <- utils::getParseData(parse(text="a=b=1"))
fixed.pd <- fix_eq_assign(pd)
expect_true(nrow(pd)+2 == nrow(fixed.pd))
expect_that(sum(fixed.pd$parent==0), equals(1))
})
test_that("'`[.parse-data`'", {#!@testing
    pd       <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    expect_is(pd, 'parse-data')
    expect_is(pd[pd$parent==0, ], 'parse-data')
    expect_false(is(pd[pd$parent==0, 'id'], 'parse-data'))
})
test_that("'`-.parse-data`'", {#! @test `-.parse-data`
pd <- get_parse_data(parse(text={
"{# Section Block
#' Roxygen Line Beore
nested <-
function(x){
    #' line inside
    cat(\"hello world\")
}
}
"
}))
comments <- get_comments(pd)
expect_is(comments, 'parse-data')
clean.pd <- pd - comments

expect_is(clean.pd, 'parse-data')
expect_true(!any(comments$id %in% clean.pd$id))

})

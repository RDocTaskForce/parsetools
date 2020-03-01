#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `pd_call.R`')
#line 65 "R/pd_call.R"
test_that('is_call', {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}, keep.source=TRUE))
    ids <- roots(pd)
    id <- ids[[3]]
    expect_true (pd_is_call(ids[[3]], pd))
    expect_false(pd_is_call(ids[[1]], pd))
    expect_equal(pd_is_call(ids, pd), c(F, F, T))
})
#line 77 "R/pd_call.R"
test_that('non-symbol calls', {#@test non-symbol calls
    text <- 'function_array[[1]](1)'
    text <- 'getAnywhere(rnorm)[1](1)'
    pd <- get_parse_data(parse(text=text, keep.source = TRUE))
    id <- roots(pd)
    expect_true(pd_is_call(id, pd))
})
#line 102 "R/pd_call.R"
test_that('is_symbol_call', {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}, keep.source=TRUE))
    ids <- roots(pd)
    id <- ids[[3]]
    expect_true (pd_is_symbol_call(id, pd))
    expect_false(pd_is_symbol_call(ids[[1]], pd))
    expect_equal(pd_is_symbol_call(ids, pd), c(F, F, T))

    expect_false(pd_is_symbol_call(ids[[1]], pd))
})
#line 116 "R/pd_call.R"
test_that('non-symbol call', {#@test non-symbol call
    pd <- get_parse_data(parse(text={"
        (function()cat('hello world!'))()
    "}, keep.source=TRUE))
    id <- roots(pd)
    expect_true(pd_is_call(id, pd))
    expect_false(pd_is_symbol_call(id, pd))
})
#line 138 "R/pd_call.R"
test_that('call_symbol', {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}, keep.source=TRUE))
    ids <- roots(pd)
    id <- ids[[3]]
    expect_equal( pd_get_call_symbol_id(id, pd)
                , .find_text('plot'))
})
#line 192 "R/pd_call.R"
test_that('call_args', {#! @testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))
    test.object <- pd_get_call_arg_ids(roots(pd), pd=pd)

    expect_is(test.object, 'integer')
    expect_equal(names(test.object), c('', 'mean', 'sd'))
    expect_identical( test.object
                    , c( 5L
                       , mean=parent(.find_text('0'))
                       , sd  =parent(.find_text('1')))
                    )
    pd <- get_parse_data(parse(text='alist(x, y=z, ...=)', keep.source=TRUE))
    expect_identical( call_args(all_call_ids(pd), pd=pd)
                    , c( parent(.find_text('x'))
                       , y = parent(.find_text('z'))
                       , '...'=NA_integer_))
})

#! This file was automatically produced by lint on  2017-06-06 12:03:28
#! changes will be overwritten.
context('tests extracted from file `./R/pd_call.R`')
test_that("'is_pd_call'", {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_true (is_pd_call(pd, ids[[3]]))
    expect_false(is_pd_call(pd, ids[[1]]))
    expect_equal(is_pd_call(pd, ids), c(F, F, T))
    
})
test_that("'is_pd_symbol_call'", {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_true (is_pd_symbol_call(pd, id))
    expect_false(is_pd_symbol_call(pd, ids[[1]]))
    expect_equal(is_pd_symbol_call(pd, ids), c(F, F, T))
})
test_that("'get_pd_call_symbol_id'", {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_equal(get_pd_call_symbol_id(pd, id), 45L)
})
test_that("'get_pd_call_symbol'", {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_equal(get_pd_call_symbol(pd, id), pd['45',])
})
test_that("'get_pd_call_args'", {#! @testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    args <- get_pd_call_args(pd)

    expect_is(args, 'list')
    expect_equal(names(args), c('', 'mean', 'sd'))
    expect_equivalent( args
                     , list( pd['4', ], mean=pd['11', ], sd=pd['18', ])
                     )
})
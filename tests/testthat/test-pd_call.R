#! This file was automatically produced by documentation::extract_tests on  2018-05-03 10:17:17
#! changes will be overwritten.
context('tests extracted from file `/home/aredd/projects/rdtf/parsetools/R/pd_call.R`')
#line 40 "/home/aredd/projects/rdtf/parsetools/R/pd_call.R"
test_that('pd_is_call', {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_true (pd_is_call(ids[[3]]), pd)
    expect_false(pd_is_call(ids[[1]]), pd)
    expect_equal(pd_is_call(ids, pd), c(F, F, T))
    
})
#line 75 "/home/aredd/projects/rdtf/parsetools/R/pd_call.R"
test_that('pd_is_symbol_call', {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_true (pd_is_symbol_call(id, pd))
    expect_false(pd_is_symbol_call(ids[[1]], pd))
    expect_equal(pd_is_symbol_call(ids, pd), c(F, F, T))
})
#line 104 "/home/aredd/projects/rdtf/parsetools/R/pd_call.R"
test_that('pd_get_call_symbol_id', {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_equal(pd_get_call_symbol_id(id, pd), 45L)
})
#line 129 "/home/aredd/projects/rdtf/parsetools/R/pd_call.R"
test_that('pd_get_call_symbol', {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_equal(pd_get_call_symbol(id, pd), pd['45',])
})
#line 166 "/home/aredd/projects/rdtf/parsetools/R/pd_call.R"
test_that('pd_get_call_args', {#! @testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    args <- pd_get_call_args(pd=pd)

    expect_is(args, 'list')
    expect_equal(names(args), c('', 'mean', 'sd'))
    expect_equivalent( args
                     , list( pd['4', ], mean=pd['11', ], sd=pd['18', ])
                     )
})

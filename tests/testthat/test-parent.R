#! This file was automatically produced by documentation::extract_tests on  2018-04-30 10:01:16
#! changes will be overwritten.
context('tests extracted from file `C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/parent.R`')
#line 35 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/parent.R"
test_that('get_parent_id', {#! @testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))
    expect_identical(get_parent_id(pd, 1), 3L)
    expect_is(get_parent_id(pd, 1), "integer")
    
    expect_is(get_parent_id(pd, 10000), "integer", info="missing parent")
    expect_identical(get_parent_id(pd, 10000), NA_integer_, info="missing parent")
    
    expect_identical(get_parent_id(pd, pd), pd$parent)
    expect_identical(get_parent_id(pd, 0L), NA_integer_)
})
#line 104 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/parent.R"
test_that('get_ancestor_ids', {#! @testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))
    expect_identical(get_ancestor_ids(pd, 1, nancestors=Inf, aggregate=TRUE , include.self=TRUE , only.present = FALSE), c(1L, 3L, 23L,0L), info = "defaults, but fully specified.")
    expect_identical(get_ancestor_ids(pd, 1, nancestors=Inf, aggregate=TRUE , include.self=FALSE, only.present = FALSE), c(    3L, 23L,0L), info = "include.self=FALSE")
    expect_identical(get_ancestor_ids(pd, 1, nancestors= 2 , aggregate=TRUE , include.self=FALSE, only.present = FALSE), c(    3L, 23L   ), info = "nancestors=2, include.self=FALSE")
    expect_identical(get_ancestor_ids(pd, 1, nancestors= 2 , aggregate=TRUE , include.self=TRUE , only.present = FALSE), c(1L, 3L, 23L   ), info = "nancestors=2, include.self=TRUE")
    expect_identical(get_ancestor_ids(pd, 1, nancestors= 2 , aggregate=FALSE, include.self=FALSE, only.present = FALSE),           23L    , info = "nancestors= 2, aggregate=FALSE")
    expect_identical(get_ancestor_ids(pd, 1, nancestors= 0 , aggregate=FALSE, include.self=TRUE , only.present = FALSE),    1L             , info = "nancestors=0, include.self=TRUE")
    
    expect_identical(get_ancestor_ids(pd, 1, nancestors=Inf, aggregate=FALSE, include.self=FALSE, only.present = FALSE),               0L , info = "nancestors= 2, aggregate=FALSE")
    expect_identical(get_ancestor_ids(pd, 1, nancestors=Inf, aggregate=FALSE, include.self=FALSE, only.present = TRUE ),           23L    , info = "nancestors= 2, aggregate=FALSE")
    expect_identical(get_ancestor_ids(pd,23, nancestors=Inf, aggregate=FALSE, include.self=FALSE, only.present = TRUE ),           23L    , info = "nancestors= 2, aggregate=FALSE")
    expect_identical(get_ancestor_ids(pd,23, nancestors=Inf, aggregate=TRUE , include.self=FALSE, only.present = TRUE ), integer(0)       , info = "nancestors= 2, aggregate=FALSE")
    
    expect_error(get_ancestor_ids(pd, 1, nancestors=  0, include.self=FALSE))
    expect_error(get_ancestor_ids(pd, 1, nancestors= -1))
    
    expect_is(get_ancestor_ids(pd, c(11, 18)), 'list')
    expect_identical(get_ancestor_ids(pd, c(23, 11), Inf, T, T, F), list(c(23L, 0L), c(11L, 12L, 23L, 0L)))
    expect_identical(get_ancestor_ids(pd, c(23, 11),  2L, T, T, F), list(c(23L, 0L), c(11L, 12L, 23L    )))
    expect_identical(get_ancestor_ids(pd, c(23, 11),  2L, T, F, F), list(c(     0L), c(     12L, 23L    )))
    expect_identical(get_ancestor_ids(pd, c(23, 11),  2L, F, F, F), list(c(     0L), c(          23L    )))
    expect_identical(get_ancestor_ids(pd, c(23, 11), Inf, T, T, T), list(c(23L    ), c(11L, 12L, 23L    )))
    expect_identical(get_ancestor_ids(pd, c(23, 11), Inf, F, T, T), list(c(23L    ), c(          23L    )))
})

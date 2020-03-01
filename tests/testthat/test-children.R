#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `children.R`')
#line 100 "R/children.R"
test_that('children', {#! @test
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))
    id <- pd[pd$parent==0, 'id']
    kids <- pd[pd$parent==id, 'id']
    expect_equal( pd_get_children_ids(id, pd, 1, include.self = FALSE)
                , kids
                , info="for default values"
                )

    expect_equal( pd_get_children_ids(id, pd, 1, include.self=TRUE)
                , c(id,kids)
                , info='include.self=TRUE'
                )

    grandkids <- pd[pd$parent %in% kids, 'id']
    expect_equal( pd_get_children_ids( id, pd, 2, include.self=FALSE
                                     , aggregate = FALSE
                                     )
                , grandkids
                , info='ngenerations=2, include.self=FALSE, aggregate=FALSE'
                )

    expect_equal( sort(pd_get_children_ids( id, pd
                                          , ngenerations=2
                                          , include.self=FALSE
                                          , aggregate = TRUE
                                          ))
                , sort(c(kids, grandkids))
                , info='ngenerations=2, include.self=FALSE, aggregate=TRUE'
                )

    expect_equal( sort(pd_get_children_ids( id, pd
                                          , ngenerations=2
                                          , include.self=TRUE
                                          , aggregate = TRUE
                                          ))
                , sort(c(id, kids, grandkids))
                , info='ngenerations=2, include.self=TRUE, aggregate=TRUE'
                )

    expect_error( pd_get_children_ids(.Machine$integer.max, pd)
                , "id\\([0-9]+\\) is not present in given parse-data."
                )
    expect_true( all(pd$id %in% pd_get_children_ids(0, pd, Inf)))
})
#line 160 "R/children.R"
test_that('get_children_pd', {#!@test
    'rnorm(10, mean=0, sd=1)' -> text
    pd       <- get_parse_data(parse(text=text, keep.source=TRUE))
    id       <- pd[match('rnorm', pd$text), 'parent']

    expect_identical( get_children_pd(id, pd), utils::head(pd, 1), info='defaults')
    expect_identical( get_children_pd(id, pd, include.self=TRUE), utils::head(pd, 2), info='include.self=TRUE')

    expect_identical( get_children_pd(id=parent(id), pd=pd, ngenerations=1, include.self=FALSE)
                    , pd[pd$parent==parent(id),]
                    , info='defaults')

    expect_identical( get_children_pd(id=parent(id), pd=pd, ngenerations=1, include.self=TRUE)
                    , pd[pd$parent==parent(id) | pd$id==parent(id),]
                    , info='defaults')

    expect_identical( get_children_pd(id=parent(id), pd=pd, ngenerations=2, include.self=TRUE)
                    , pd
                    , info='defaults')

    expect_identical( get_children_pd(id=parent(id), pd=pd, ngenerations=2, include.self=FALSE, aggregate=FALSE)
                    , pd[pd$parent != parent(id) & pd$parent != 0, ]
                    , info='defaults')

    expect_error(get_children_pd(id=pd$id, pd=pd))
})
#line 194 "R/children.R"
test_that('n_children', {#@testing
    ex.file <- system.file("examples", "example.R", package="parsetools")
    exprs <- parse(ex.file, keep.source = TRUE)
    pd <- get_parse_data(exprs)

    expect_equal(n_children(roots(pd)), c(3, 3, 8))
})

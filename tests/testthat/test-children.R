#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `children.R`')
#line 97 "R/children.R"
test_that('children', {#! @test
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))
    id <- pd[pd$parent==0, 'id']
    expect_equal( pd_get_children_ids(id, pd, 1, include.self = FALSE)
                , c(3,2,5,6,9,10,12,13,16,17,19,20)
                , info="for default values"
                )

    expect_equal( pd_get_children_ids(id, pd, 1, include.self=TRUE)
                , c(23,3,2,5,6,9,10,12,13,16,17,19,20)
                , info='include.self=TRUE'
                )

    expect_equal( pd_get_children_ids( id, pd, 2, include.self=FALSE
                                  , aggregate = FALSE
                                  )
                , c(1,4,11,18)
                , info='ngenerations=2, include.self=FALSE, aggregate=FALSE'
                )

    expect_equal( pd_get_children_ids( id, pd
                                  , ngenerations=2
                                  , include.self=FALSE
                                  , aggregate = TRUE
                                  )
                , c(c(3,2,5,6,9,10,12,13,16,17,19,20), c(1,4,11,18))
                , info='ngenerations=2, include.self=FALSE, aggregate=TRUE'
                )

    expect_equal( pd_get_children_ids( id, pd
                                  , ngenerations=2
                                  , include.self=TRUE
                                  , aggregate = TRUE
                                  )
                , c(23, c(3,2,5,6,9,10,12,13,16,17,19,20), c(1,4,11,18))
                , info='ngenerations=2, include.self=TRUE, aggregate=TRUE'
                )

    expect_error( pd_get_children_ids(.Machine$integer.max, pd)
                , "id\\([0-9]+\\) is not present in given parse-data."
                )
    expect_true( all(pd$id %in% pd_get_children_ids(0, pd, Inf)))
})
#line 157 "R/children.R"
test_that('get_children_pd', {#!@test
    'rnorm(10, mean=0, sd=1)' -> text
    pd       <- get_parse_data(parse(text=text, keep.source=TRUE))
    id       <- 3

    expect_identical( get_children_pd(3, pd), utils::head(pd, 1), info='defaults')
    expect_identical( get_children_pd(3, pd, include.self=TRUE), utils::head(pd, 2), info='include.self=TRUE')

    expect_identical( get_children_pd(id=23, pd=pd, ngenerations=1, include.self=FALSE)
                    , pd[pd$parent==23,]
                    , info='defaults')

    expect_identical( get_children_pd(id=23, pd=pd, ngenerations=1, include.self=TRUE)
                    , pd[pd$parent==23 | pd$id==23,]
                    , info='defaults')

    expect_identical( get_children_pd(id=23, pd=pd, ngenerations=2, include.self=TRUE)
                    , pd
                    , info='defaults')

    expect_identical( get_children_pd(id=23, pd=pd, ngenerations=2, include.self=FALSE, aggregate=FALSE)
                    , pd[pd$parent != 23 & pd$parent != 0, ]
                    , info='defaults')

    expect_error(get_children_pd(id=pd$id, pd=pd))
})

#! This file was automatically produced by documentation::extract_tests on  2018-05-04 11:22:55
#! changes will be overwritten.
context('tests extracted from file `children.R`')
#line 59 "/home/aredd/projects/rdtf/parsetools/R/children.R"
test_that('get_child_ids', {#! @test
    pd       <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    id       <- pd[pd$parent==0, 'id']
    kids.ids <- get_child_ids(id, pd, 1, include.self = FALSE)
    expect_equal( kids.ids, c(3,2,5,6,9,10,12,13,16,17,19,20)
                , info="for default values"
                )

    kids.ids <- get_child_ids(id, pd, 1, include.self=TRUE)
    expect_equal( kids.ids, c(23,3,2,5,6,9,10,12,13,16,17,19,20)
                , info='include.self=TRUE'
                )

    kids.ids <- get_child_ids( id, pd, 2, include.self=FALSE
                             , all.generations = FALSE
                             )
    expect_equal( kids.ids, c(1,4,11,18)
                , info='ngenerations=2, include.self=FALSE, all.generations=FALSE'
                )

    kids.ids <- get_child_ids( id, pd
                             , ngenerations=2
                             , include.self=FALSE
                             , all.generations = TRUE
                             )
    expect_equal( kids.ids, c(c(3,2,5,6,9,10,12,13,16,17,19,20), c(1,4,11,18))
                , info='ngenerations=2, include.self=FALSE, all.generations=TRUE'
                )

    kids.ids <- get_child_ids( id, pd
                             , ngenerations=2
                             , include.self=TRUE
                             , all.generations = TRUE
                             )
    expect_equal( kids.ids, c(23, c(3,2,5,6,9,10,12,13,16,17,19,20), c(1,4,11,18))
                , info='ngenerations=2, include.self=TRUE, all.generations=TRUE'
                )
                
    expect_identical( get_child_ids(.Machine$integer.max, pd), integer(0))
    expect_true( all(pd$id %in% get_child_ids(0, pd, Inf)))
})
#line 110 "/home/aredd/projects/rdtf/parsetools/R/children.R"
test_that('get_child', {#!@test
    'rnorm(10, mean=0, sd=1)' -> text
    pd       <- get_parse_data(parse(text=text))
    id       <- 3
    expect_identical( get_child(3, pd), utils::head(pd, 1), info='defaults')
    expect_identical( get_child(3, pd, include.self=TRUE), utils::head(pd, 2), info='include.self=TRUE')

    expect_identical( get_child(id=23, pd=pd, ngenerations=1, include.self=FALSE)
                    , pd[pd$parent==23,]
                    , info='defaults')

    expect_identical( get_child(id=23, pd=pd, ngenerations=1, include.self=TRUE)
                    , pd[pd$parent==23 | pd$id==23,]
                    , info='defaults')

    expect_identical( get_child(id=23, pd=pd, ngenerations=2, include.self=TRUE)
                    , pd
                    , info='defaults')

    expect_identical( get_child(id=23, pd=pd, ngenerations=2, include.self=FALSE, all.generations=FALSE)
                    , pd[pd$parent != 23 & pd$parent != 0, ]
                    , info='defaults')
})
#line 152 "/home/aredd/projects/rdtf/parsetools/R/children.R"
test_that('get_children', {#! @test
    'rnorm(10, mean=0, sd=1)' -> text
    pd  <- get_parse_data(parse(text=text))
    res <- get_children(id=23, pd)
    expect_is(res, 'list')
    expect_equal(length(res), 1)
})

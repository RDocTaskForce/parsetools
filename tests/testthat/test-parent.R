#! This file was automatically produced by documentation::extract_tests on  2018-05-03 10:17:17
#! changes will be overwritten.
context('tests extracted from file `/home/aredd/projects/rdtf/parsetools/R/parent.R`')
#line 35 "/home/aredd/projects/rdtf/parsetools/R/parent.R"
test_that('get_parent_id', {#! @testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    expect_identical(get_parent_id(1, pd), 3L)
    expect_is(get_parent_id(1, pd), "integer")
    
    expect_is(get_parent_id(10000, pd), "integer", info="missing parent")
    expect_identical(get_parent_id(10000, pd), NA_integer_, info="missing parent")
    
    expect_identical(get_parent_id(pd, pd), pd$parent)
    expect_identical(get_parent_id(0L, pd), NA_integer_)
})
#line 106 "/home/aredd/projects/rdtf/parsetools/R/parent.R"
test_that('get_ancestor_ids', {#! @testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    expect_identical(get_ancestor_ids( 1, pd,nancestors=Inf, aggregate=TRUE , include.self=TRUE , only.present = FALSE), c(1L, 3L, 23L,0L), info = "defaults, but fully specified.")
    expect_identical(get_ancestor_ids( 1, pd,nancestors=Inf, aggregate=TRUE , include.self=FALSE, only.present = FALSE), c(    3L, 23L,0L), info = "include.self=FALSE")
    expect_identical(get_ancestor_ids( 1, pd,nancestors= 2 , aggregate=TRUE , include.self=FALSE, only.present = FALSE), c(    3L, 23L   ), info = "nancestors=2, include.self=FALSE")
    expect_identical(get_ancestor_ids( 1, pd,nancestors= 2 , aggregate=TRUE , include.self=TRUE , only.present = FALSE), c(1L, 3L, 23L   ), info = "nancestors=2, include.self=TRUE")
    expect_identical(get_ancestor_ids( 1, pd,nancestors= 2 , aggregate=FALSE, include.self=FALSE, only.present = FALSE),           23L    , info = "nancestors= 2, aggregate=FALSE")
    expect_identical(get_ancestor_ids( 1, pd,nancestors= 0 , aggregate=FALSE, include.self=TRUE , only.present = FALSE),    1L             , info = "nancestors=0, include.self=TRUE")
                                          
    expect_identical(get_ancestor_ids( 1, pd,nancestors=Inf, aggregate=FALSE, include.self=FALSE, only.present = FALSE),               0L , info = "nancestors= 2, aggregate=FALSE")
    expect_identical(get_ancestor_ids( 1, pd,nancestors=Inf, aggregate=FALSE, include.self=FALSE, only.present = TRUE ),           23L    , info = "nancestors= 2, aggregate=FALSE")
    expect_identical(get_ancestor_ids(23, pd,nancestors=Inf, aggregate=FALSE, include.self=FALSE, only.present = TRUE ),           23L    , info = "nancestors= 2, aggregate=FALSE")
    expect_identical(get_ancestor_ids(23, pd,nancestors=Inf, aggregate=TRUE , include.self=FALSE, only.present = TRUE ), integer(0)       , info = "nancestors= 2, aggregate=FALSE")
    
    expect_error(get_ancestor_ids(1, pd, nancestors=  0, include.self=FALSE))
    expect_error(get_ancestor_ids(1, pd, nancestors= -1))
    
    expect_is(get_ancestor_ids(c(11, 18), pd), 'list')
    expect_identical(get_ancestor_ids(c(23, 11), pd, Inf, T, T, F), list(c(23L, 0L), c(11L, 12L, 23L, 0L)))
    expect_identical(get_ancestor_ids(c(23, 11), pd,  2L, T, T, F), list(c(23L, 0L), c(11L, 12L, 23L    )))
    expect_identical(get_ancestor_ids(c(23, 11), pd,  2L, T, F, F), list(c(     0L), c(     12L, 23L    )))
    expect_identical(get_ancestor_ids(c(23, 11), pd,  2L, F, F, F), list(c(     0L), c(          23L    )))
    expect_identical(get_ancestor_ids(c(23, 11), pd, Inf, T, T, T), list(c(23L    ), c(11L, 12L, 23L    )))
    expect_identical(get_ancestor_ids(c(23, 11), pd, Inf, F, T, T), list(c(23L    ), c(          23L    )))
})
#line 131 "/home/aredd/projects/rdtf/parsetools/R/parent.R"
test_that('last parameter', {#! @testing last parameter
'
function(){
setClass( "testClass"
     , slots = c( x="numeric" #< the x field
                , y="matrix"  #< the y field
                )
     )
 }' %>% 
    parse(text = .) %>%
    get_parse_data() -> pd

    root.id <- all_root_ids(pd)
    body.id <- get_function_body_id(root.id, pd)
    id <- pd[pd$text=="#< the x field", 'id']

    expect_true(root.id %in% get_ancestor_ids(id, pd))
    expect_false(root.id %in% get_ancestor_ids(id, pd, last=body.id))

    id2 <- pd[pd$text=="#< the y field", 'id']

    value <- get_ancestor_ids(c(id, id2), pd, last = body.id, include.self =FALSE)
    expect_identical(value[[1]], value[[2]])
    expect_false(root.id %in% value[[1]])
    expect_false(root.id %in% value[[2]])
})

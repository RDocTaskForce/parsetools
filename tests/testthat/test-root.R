#! This file was automatically produced by documentation::extract_tests on  2017-07-20 10:45:47
#! changes will be overwritten.
context('tests extracted from file `C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/root.R`')
#line 53 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/root.R"
test_that("is_root", {#! @testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    expect_true (is_root(pd, 23))
    expect_false(is_root(pd,  1))
    expect_equal(sum(is_root(pd)), 1)

    
    pd <- get_parse_data(parse(text={'{
        x <- rnorm(10, mean=0, sd=1)
        y <- runif(10)
        plot(x,y)
    }'}))
    expect_true(is_root(pd, 68), info="Grouping root")
    expect_true(is_root(pd, 30), info="Root within grouping.")
    expect_equal(sum(is_root(pd)), 4)
    expect_equal(sum(is_root(pd, c(68, 30, 46, 62))), 4)
    expect_false(is_root(pd, 66))
    
    
    expect_equal(sum(is_root(pd, pd$id, ignore.groups=FALSE)), 1)
    expect_error(is_root(pd, 0L))
    
    pd[pd$parent %in% c(0,68) & pd$token == 'expr', ]
    expect_false(is_root(pd, 30, ignore.groups = FALSE))
    expect_equal(is_root(pd, c(68, 30), ignore.groups = FALSE), c(TRUE, FALSE))
    
    pd <- get_parse_data(parse(text={"
        # a comment outside the grouping
        {# A grouping
        #' An Roxygen Comment
        hw <- function(){
            {# Another Grouping
             # but not a root since it is burried within a function
             1+2 #< an expression that is not a root.
            }
            3+4 #< also not a root
        }
        4+5 #< this is a root expression
        }
        6+7 #< a regular root expression
    "}))
    id <- max(pd[pd$token =="'{'", 'parent'])
    expect_true(is_root(pd, id, ignore.groups = TRUE))
    id <- min(pd[pd$token =="'{'", 'parent'])
    expect_equal(get_family(pd, id)[3,'text'], "# Another Grouping")
    
    ids <- pd[pd$token =="'{'", 'parent']
    expect_equal(is_root(pd, ids, ignore.groups = TRUE ), c(TRUE, FALSE, FALSE))
    expect_equal(is_root(pd, ids, ignore.groups = FALSE), c(TRUE, FALSE, FALSE))
    
    pd <- get_parse_data(parse(text="
        # a comment
        an_expression()
    "))
    expect_false(is_root(pd, pd[1,'id']))
})
#line 140 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/root.R"
test_that("all_root_ids", {#!@testthat all_root_ids
    pd <- get_parse_data(parse(text={"a <- 1
        {# section 1
        b <- 2
        {# section 2
        c <- 3
        }# end of section 1
        d <- 4
        }# end of section 2
        e <- 5
    "}))
    expect_equal(all_root_ids(pd, TRUE), c(7, 52, 63))
    
    roots <- all_root_ids(pd, FALSE)
    expect_equal(roots, c(7, 19, 31, 47, 63))
    expect_equal(getParseText(pd, roots), c('a <- 1','b <- 2', 'c <- 3', 'd <- 4', 'e <- 5'))
    
    pd <- get_parse_data(parse(text="
        # a comment
        an_expression()
    "))
    expect_equal( all_root_ids(pd), -pd[1,'parent'])

    pd <- utils::getParseData(parse(text={"
    {# grouped code
        # normal comment
        #' Documenation before
        hw <- function(){
            #! documentation comment inside.
            print('hello world')
        }
    }
    {#Second Group
        1+2
    }
    # Comment 3
    4+5
    "}))
    id <- all_root_ids(pd)
    expect_equal(id, c(43, 61, 74))
})
#line 199 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/root.R"
test_that("all_root_nodes", {#!@testing
    pd <- get_parse_data(parse(text={"a <- 1
        {# section 1
        b <- 2
        {# section 2
        c <- 3
        }# end of section 1
        d <- 4
        }# end of section 2
        e <- 5
    "}))
    expect_equal(all_root_nodes(pd, TRUE)$id   , c(7, 52, 63))
    expect_equal(all_root_nodes(pd, TRUE)$line1, c(1,  2,  9))

    expect_equal(all_root_nodes(pd, FALSE)$id   , c(7, 19, 31, 47, 63))
    expect_equal(all_root_nodes(pd, FALSE)$line1, c(1,  3,  5,  7,  9))
})
#line 237 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/root.R"
test_that("ascend_to_root", {#@testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    expect_equal(ascend_to_root(pd, id=23), 23)
    expect_equal(ascend_to_root(pd, id=1), 23)
    expect_identical(ascend_to_root(pd, id=0), 0L)
    
    pd <- get_parse_data(parse(text={"
        #' hello world
        hw <- function(){
            #! title
            print('hello world!')
        }
        #' comment after
    "}))
    expect_equal(ascend_to_root(pd, 3), 34)
    
    expect_equal(ascend_to_root(pd), c(rep(34, 20), 0))
})

#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `root.R`')
#line 71 "R/root.R"
test_that('is_root', {#@testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))
    root <- pd$id[pd$parent==0]
    leaf <- pd$id[pd$parent!=0][1]

    expect_true (pd_is_root(root, pd))
    expect_false(pd_is_root(leaf, pd))
    expect_equal(sum(pd_is_root(pd$id, pd=pd)), 1)


    pd <- get_parse_data(parse(text={'{
        x <- rnorm(10, mean=0, sd=1)
        y <- runif(10)
        plot(x,y)
    }'}, keep.source=TRUE))
    group.root <- pd$id[pd$parent==0]
    roots <- children(group.root)[-1]
    leaf <- .find_text('0')

    expect_true(pd_is_root(group.root, pd), info="Grouping root")
    expect_true(pd_is_root(roots[[1]], pd), info="Root within grouping.")
    expect_equal(sum(pd_is_root(pd$id, pd=pd)), 4)
    expect_equal(sum(pd_is_root(c(group.root, roots), pd)), 4)
    expect_false(pd_is_root(leaf, pd))

    expect_equal(sum(pd_is_root(pd$id, pd, ignore.groups=FALSE)), 1)
    expect_error(pd_is_root(0L, pd))

    pd[pd$parent %in% c(0,group.root) & pd$token == 'expr', ]
    expect_false(pd_is_root(roots[[1]], pd, ignore.groups = FALSE))
    expect_equal(pd_is_root(c(group.root, roots[[1]]), pd, ignore.groups = FALSE), c(TRUE, FALSE))

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
    "}, keep.source=TRUE))
    id <- max(pd[pd$token =="'{'", 'parent'])
    expect_true(pd_is_root(id, pd, ignore.groups = TRUE))
    id <- min(pd[pd$token =="'{'", 'parent'])
    expect_equal(get_family_pd(id, pd)[3,'text'], "# Another Grouping")

    ids <- pd[pd$token =="'{'", 'parent']
    expect_equal(pd_is_root(ids, pd, ignore.groups = TRUE ), c(TRUE, FALSE, FALSE))
    expect_equal(pd_is_root(ids, pd, ignore.groups = FALSE), c(TRUE, FALSE, FALSE))

    pd <- get_parse_data(parse(text="
        # a comment
        an_expression()
    ", keep.source=TRUE))
    expect_false(pd_is_root(pd[1,'id'], pd))
})
#line 158 "R/root.R"
test_that('roots', {#@testing
    pd <- get_parse_data(parse(text={"a <- 1
        {# section 1
        b <- 2
        {# section 2
        c <- 3
        }# end of section 1
        d <- 4
        }# end of section 2
        e <- 5
    "}, keep.source=TRUE))

    bases <- pd[pd$parent==0, 'id']

    groups <- parent(.find_text('{'))
    expect_equal(pd_all_root_ids(pd, TRUE), bases)

    roots <- pd_all_root_ids(pd, FALSE)
    expected <- parent(.find_text('<-'))
    expect_equal(roots, expected)
    expect_equal(getParseText(pd, roots), c('a <- 1','b <- 2', 'c <- 3', 'd <- 4', 'e <- 5'))

    pd <- get_parse_data(parse(text="
        # a comment
        an_expression()
    ", keep.source=TRUE))
    expect_equal( pd_all_root_ids(pd), -pd[1,'parent'])

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
    "}, keep.source=TRUE))
    expect_equal(pd_all_root_ids(pd), pd[pd$parent==0, 'id'])
})
#line 220 "R/root.R"
test_that('all_root_nodes', {#!@testing
    pd <- get_parse_data(parse(text={"a <- 1
        {# section 1
        b <- 2
        {# section 2
        c <- 3
        }# end of section 1
        d <- 4
        }# end of section 2
        e <- 5
    "}, keep.source=TRUE))
    expect_equal(all_root_nodes(pd, TRUE)$id   , c(7, 52, 63))
    expect_equal(all_root_nodes(pd, TRUE)$line1, c(1,  2,  9))

    expect_equal(all_root_nodes(pd, FALSE)$id   , c(7, 19, 31, 47, 63))
    expect_equal(all_root_nodes(pd, FALSE)$line1, c(1,  3,  5,  7,  9))
})
#line 258 "R/root.R"
test_that('ascend_to_root', {#@testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))
    root <- roots(pd)

    expect_equal(ascend_to_root(id=root, pd), root)
    expect_equal(ascend_to_root(id=1 , pd), root)
    expect_identical(ascend_to_root(id=0, pd), 0L)

    pd <- get_parse_data(parse(text={"
        #' hello world
        hw <- function(){
            #! title
            print('hello world!')
        }
        #' comment after
    "}, keep.source=TRUE))
    root <- roots(pd)

    expect_equal(ascend_to_root(.find_text("#' hello world"), pd), root)
    expect_equal(ascend_to_root(pd$id, pd=pd), c(rep(root, nrow(pd)-1), 0L))

    pd <- get_parse_data(parse(text={"
    {   #' hello world
        hw <- function(){
            #! title
            print('hello world!')
        }
        #' comment after
    }"}, keep.source=TRUE))

    expect_false( ascend_to_root(.find_text('hw'), pd) %in% roots(pd))
    expect_true( ascend_to_root(.find_text('hw'), pd) %in% roots(pd, FALSE))

    expect_true(is_root(next_sibling(.find_text("#' hello world"))))
})

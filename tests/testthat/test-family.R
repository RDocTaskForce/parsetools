#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `family.R`')
#line 59 "R/family.R"
test_that('get_family_pd', {#@testing
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
    id <- ascend_to_root(pd[pd$text == 'c','id'], pd)
    expect_identical(get_family_pd(id, pd), pd[19:24,])

    pd <- get_parse_data(parse(text={"
        # normal comment
        #' Documenation before
        hw <- function(){
            #! documentation comment inside.
            print('hello world')
        }
    "}, keep.source=TRUE))
    fam <- get_family_pd(37, pd, include.doc.comments=TRUE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")
    fam <- get_family_pd(37, pd, include.doc.comments=TRUE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], "#' Documenation before")
    fam <- get_family_pd(37, pd, include.doc.comments=FALSE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")
    fam <- get_family_pd(37, pd, include.doc.comments=FALSE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], "hw")

    pd <- get_parse_data(parse(text={"
    #demonstration of grouped code.
    {
        # normal comment
        #' Documenation before
        hw <- function(){
            #! documentation comment inside.
            print('hello world')
        }
    }"}, keep.source=TRUE))
    group.id <- roots(pd)
    expect_true(pd_is_grouping(group.id, pd))
    id <- expr.id <- roots(pd, FALSE)

    fam <- get_family_pd(expr.id, pd, include.doc.comments=FALSE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], 'hw')
    fam <- get_family_pd(expr.id, pd, include.doc.comments=TRUE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], "#' Documenation before")
    fam <- get_family_pd(expr.id, pd, include.doc.comments=TRUE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")


})

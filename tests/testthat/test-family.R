#! This file was automatically produced by documentation::extract_tests on  2018-04-30 10:01:15
#! changes will be overwritten.
context('tests extracted from file `C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/family.R`')
#line 60 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/family.R"
test_that('get_family', {#!@testing
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
    id <- ascend_to_root(pd, pd[pd$text == 'c','id'])
    expect_identical(get_family(pd, id), pd[19:24,])
    
    pd <- get_parse_data(parse(text={"
        # normal comment
        #' Documenation before
        hw <- function(){
            #! documentation comment inside.
            print('hello world')
        }
    "}, keep.source=TRUE))
    fam <- get_family(pd, 37, include.doc.comments=TRUE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")
    fam <- get_family(pd, 37, include.doc.comments=TRUE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], "#' Documenation before")
    fam <- get_family(pd, 37, include.doc.comments=FALSE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")
    fam <- get_family(pd, 37, include.doc.comments=FALSE, include.regular.comments=FALSE)
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
    group.id <- all_root_ids(pd)
    expect_true(is_grouping(pd, group.id))
    id <- expr.id <- all_root_ids(pd, FALSE)
    
    fam <- get_family(pd, expr.id, include.doc.comments=FALSE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], 'hw')
    fam <- get_family(pd, expr.id, include.doc.comments=TRUE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], "#' Documenation before")
    fam <- get_family(pd, expr.id, include.doc.comments=TRUE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")
    
    
})
#line 175 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/family.R"
test_that('get_firstborn', {#!@testing
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
    expect_equal(get_firstborn(pd, 52)$token, "'{'")
    expect_equal(get_firstborn(pd, 7)$text, "<-")
    
    expect_warning(get_firstborn_id(pd, c(7, 52, .Machine$integer.max)))
    expect_identical( suppressWarnings(get_firstborn_id(pd, c(7, 52, .Machine$integer.max)))
                    , c(2L, 10L, NA_integer_))
    
    expect_true(is_firstborn(2, pd=pd))
    expect_identical(suppressWarnings(is_firstborn(-1)), NA)
    expect_identical(is_firstborn(c(1,3,7)), c(TRUE, FALSE, TRUE))
})

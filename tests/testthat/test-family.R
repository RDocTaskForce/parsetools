#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `family.R`')
#line 60 "/rdtf/parsetools/R/family.R"
test_that('pd_get_family', {#@testing
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
    expect_identical(pd_get_family(id, pd), pd[19:24,])

    pd <- get_parse_data(parse(text={"
        # normal comment
        #' Documenation before
        hw <- function(){
            #! documentation comment inside.
            print('hello world')
        }
    "}, keep.source=TRUE))
    fam <- pd_get_family(37, pd, include.doc.comments=TRUE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")
    fam <- pd_get_family(37, pd, include.doc.comments=TRUE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], "#' Documenation before")
    fam <- pd_get_family(37, pd, include.doc.comments=FALSE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")
    fam <- pd_get_family(37, pd, include.doc.comments=FALSE, include.regular.comments=FALSE)
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

    fam <- pd_get_family(expr.id, pd, include.doc.comments=FALSE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], 'hw')
    fam <- pd_get_family(expr.id, pd, include.doc.comments=TRUE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], "#' Documenation before")
    fam <- pd_get_family(expr.id, pd, include.doc.comments=TRUE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")


})
#line 149 "/rdtf/parsetools/R/family.R"
test_that('next_sibling', {#@testing
    pd <- get_parse_data(parse(text='a+b', keep.source = TRUE))
    id <- parent(.find_text('a'))
    expect_equal( pd_get_next_sibling_id(id,pd)
                , parent(.find_text('b'))
                )
    expect_identical( pd_get_next_sibling_id(.find_text('a', pd), pd), NA_integer_)
    expect_identical( pd_get_next_sibling_id(.find_text('+', pd), pd)
                    , parent(.find_text('a', pd))
                    )
    expect_length(pd_get_next_sibling_id(pd$id, pd), nrow(pd))
    expect_error(pd_get_next_sibling_id(1e9L, pd))
    expect_error(pd_get_next_sibling_id(id, id))
})
#line 181 "/rdtf/parsetools/R/family.R"
test_that('prev_sibling', {#@testing
    pd <- get_parse_data(parse(text='a+b', keep.source = TRUE))
    id <- parent(.find_text('b'))
    expect_equal( pd_get_prev_sibling_id(id,pd)
                , parent(.find_text('a'))
                )
    expect_identical( pd_get_prev_sibling_id(.find_text('b', pd), pd), NA_integer_)
    expect_identical( pd_get_prev_sibling_id(parent(.find_text('a', pd)), pd)
                    , .find_text('+', pd))
    expect_length(pd_get_prev_sibling_id(pd$id, pd), nrow(pd))
    expect_error(pd_get_prev_sibling_id(1e9L, pd))
    expect_error(pd_get_prev_sibling_id(id, id))
})
#line 227 "/rdtf/parsetools/R/family.R"
test_that('firstborn', {#@testing
    pd <- get_parse_data(parse(text='a+b', keep.source = TRUE))
    fb <- pd_get_firstborn_id(roots(pd), pd)
    expect_identical(token(fb), "'+'")
    expect_true(pd_is_firstborn(fb, pd))
    expect_true(pd_is_firstborn(roots(pd), pd))
    expect_false(pd_is_firstborn(next_sibling(fb), pd))

    expect_true(fb %in% siblings(fb,pd))
    expect_length(siblings(fb,pd), 3L)
    expect_equal(sum(pd_is_firstborn(siblings(fb,pd), pd)), 1L)
})

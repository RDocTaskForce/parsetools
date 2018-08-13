#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `grouping.R`')
#line 50 "/rdtf/parsetools/R/grouping.R"
test_that('is_grouping', {#@testing
    pd <- get_parse_data(parse(text='{
        this(is+a-grouping)
    }', keep.source=TRUE))
    expect_true (pd_is_grouping(25L, pd))
    expect_false(pd_is_grouping( 1L, pd))

    expect_is(pd_is_grouping(pd$id, pd=pd), 'logical')
    expect_equal(sum(pd_is_grouping(pd$id, pd=pd)), 1)

    expect_equal(sum(pd_is_grouping(pd$id, pd=pd)), 1L)

    pd <- get_parse_data(parse(text='
        {# first Group
        {# nested group
            "expression in double nested group"
        }
        }
        ', keep.source=TRUE))
    expect_equal(sum(pd_is_grouping(pd$id, pd=pd)), 2)
})
#line 77 "/rdtf/parsetools/R/grouping.R"
test_that('all_grouping_ids', {#@testing
    pd <- get_parse_data(parse(text='{
        this(is+a-grouping)
    }', keep.source=TRUE))

    expect_is(all_grouping_ids(pd), 'integer')
    expect_equal(length(all_grouping_ids(pd)), 1)
    expect_equal(all_grouping_ids(pd), 25)
})
#line 114 "/rdtf/parsetools/R/grouping.R"
test_that('fix_grouping_comment_association', {#@testing
    pd <- get_parse_data(parse(text={"
    {# grouped code
        # normal comment
        #' Documenation before
        hw <- function(){
            #! documentation comment inside.
            print('hello world')
        }
    }
    {# Second Group
        1+2
    }
    # Comment 3
    4+5
    "}, keep.source=TRUE))

    ids <- all_grouping_ids(pd)
    fixed <- fix_grouping_comment_association(ids, pd)

    expect_identical( -parent(.find_text("#' Documenation before", fixed), fixed)
                    , parent(.find_text('<-'))
                    )

    expect_identical(fixed[-6], pd[-6])
    expect_equal( parent(all_comment_ids(fixed), fixed)
                , c(-38, -38, -38, 34, -56, -74)
                )
})
#line 143 "/rdtf/parsetools/R/grouping.R"
test_that('fix_grouping_comment_association Special case', {#@test fix_grouping_comment_association Special case
    pd <- get_parse_data(parse(text={"
    {#' Documenation before
        hw <- function(){
            #! documentation comment inside.
            print('hello world')
        }
    }"}, keep.source=TRUE))

    fixed <- fix_grouping_comment_association(roots(pd), pd)


    expect_equal(nrow(pd), nrow(fixed))
    expect_identical(pd$id, fixed$id)

    cid <- .find_text("#' Documenation before")
    expect_true( parent(cid, fixed) !=  parent(cid, pd))

    expect_true(is_assignment(abs(parent(cid, fixed)), fixed))
    expect_true(!any(is_comment(children(roots(fixed), fixed), fixed)))
})

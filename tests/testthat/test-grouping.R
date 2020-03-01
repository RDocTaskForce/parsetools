#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `grouping.R`')
#line 50 "R/grouping.R"
test_that('is_grouping', {#@testing
    pd <- get_parse_data(parse(text='{
        this(is+a-grouping)
    }', keep.source=TRUE))
    id <- pd[match("'{'", pd$token), 'id']
    gid <- parent(id)
    expect_true (pd_is_grouping(gid, pd))
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
#line 79 "R/grouping.R"
test_that('all_grouping_ids', {#@testing
    pd <- get_parse_data(parse(text='{
        this(is+a-grouping)
    }', keep.source=TRUE))

    expect_is(all_grouping_ids(pd), 'integer')
    expect_equal(length(all_grouping_ids(pd)), 1)
    expect_equal(all_grouping_ids(pd), pd[match("'{'", pd$token), 'parent'])
})
#line 116 "R/grouping.R"
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

    text(all_comment_ids(fixed), fixed)

    x <- pd_get_ancestor_ids(.find_text('print', fixed), fixed)

    inside.parent <- max(x[is_in_function(x, fixed) & ! is_function(x)])



    expect_equal( abs(parent(all_comment_ids(fixed), fixed))
                , c( rep(ascend_to_root(.find_text('hw', fixed), fixed), 3)
                   , inside.parent
                   , parent(.find_text('+'))
                   )
                )
})
#line 157 "R/grouping.R"
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

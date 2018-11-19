#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `siblings.R`')
#line 61 "R/siblings.R"
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
#line 89 "R/siblings.R"
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

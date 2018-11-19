#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `firstborn.R`')
#line 53 "R/firstborn.R"
test_that('firstborn', {#@testing
    pd <- get_parse_data(parse(text='a+b', keep.source = TRUE))
    fb <- pd_get_firstborn(roots(pd), pd)
    expect_identical(token(fb), "'+'")
    expect_true(pd_is_firstborn(fb, pd))
    expect_true(pd_is_firstborn(roots(pd), pd))
    expect_false(pd_is_firstborn(next_sibling(fb), pd))

    expect_true(fb %in% siblings(fb,pd))
    expect_length(siblings(fb,pd), 3L)
    expect_equal(sum(pd_is_firstborn(siblings(fb,pd), pd)), 1L)
})

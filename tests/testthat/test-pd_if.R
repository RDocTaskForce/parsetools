#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `pd_if.R`')
#line 121 "R/pd_if.R"
test_that('if structures', {#!@testing if structures
    pd <- get_parse_data(parse(text={"
        if(predicate){
            body
        } else {
            alternate
        }
    "}, keep.source=TRUE))
    id <- roots(pd) # 33

    expect_true(pd_is_if(id,pd))
    expect_equal(pd_get_if_predicate_id(id, pd),  7L)
    expect_equal(pd_get_if_branch_id   (id, pd), 18L)
    expect_equal(pd_get_if_alternate_id(id, pd), 30L)
})

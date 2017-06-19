#! This file was automatically produced by lint on  2017-06-06 12:03:28
#! changes will be overwritten.
context('tests extracted from file `/mnt/data/projects/rdtf/parsetools/R/pd_if.R`')
test_that("'if structures'", {#!@testing if structures
    pd <- get_parse_data(parse(text={"
        if(predicate){
            body
        } else {
            alternate
        }
    "}))
    id <- all_root_ids(pd) # 33
    
    expect_true(is_if_expr(pd, id))
    expect_equal(get_if_predicate_id(pd, id),  7L)
    expect_equal(get_if_branch_id   (pd, id), 18L)
    expect_equal(get_if_alternate_id(pd, id), 30L)
})

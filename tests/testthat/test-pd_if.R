#! This file was automatically produced by documentation::extract_tests on  2018-05-03 10:17:17
#! changes will be overwritten.
context('tests extracted from file `/home/aredd/projects/rdtf/parsetools/R/pd_if.R`')
#line 80 "/home/aredd/projects/rdtf/parsetools/R/pd_if.R"
test_that('if structures', {#!@testing if structures
    pd <- get_parse_data(parse(text={"
        if(predicate){
            body
        } else {
            alternate
        }
    "}))
    id <- all_root_ids(pd) # 33
    
    expect_true(pd_is_if(id))
    expect_equal(pd_get_if_predicate_id(id, pd),  7L)
    expect_equal(pd_get_if_branch_id   (id, pd), 18L)
    expect_equal(pd_get_if_alternate_id(id, pd), 30L)
})

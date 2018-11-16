#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `pd_assign.R`')
#line 23 "R/pd_assign.R"
test_that('is_assignment', {#@testing
    pd1 <- get_parse_data(parse(text="x <-  1", keep.source=TRUE))
    expect_true(pd_is_assignment(roots(pd1), pd=pd1))
    pd2 <- get_parse_data(parse(text="x <<- 1", keep.source=TRUE))
    expect_true(pd_is_assignment(roots(pd2), pd=pd2))
    pd3 <- get_parse_data(parse(roots(pd), text="1 ->  x", keep.source=TRUE))
    expect_true(pd_is_assignment(roots(pd3), pd=pd3))
    pd4 <- get_parse_data(parse(text="1 ->> x", keep.source=TRUE))
    expect_true(pd_is_assignment(roots(pd4), pd=pd4))
    pd5 <- get_parse_data(parse(text="x = 1", keep.source=TRUE))
    expect_true(pd_is_assignment(roots(pd5), pd=pd5))
})
#line 61 "R/pd_assign.R"
test_that('assign_value', {#!@testing
pd <- get_parse_data(parse(text="x<-1", keep.source=TRUE))
expect_equal(pd_get_assign_value_id(all_assignment_ids(pd), pd=pd), 5L)

pd <- get_parse_data(parse(text="x=1", keep.source=TRUE))
expect_equal(pd_get_assign_value_id(all_assignment_ids(pd), pd=pd), 5L)

pd <- get_parse_data(parse(text="x<<-1", keep.source=TRUE))
expect_equal(pd_get_assign_value_id(all_assignment_ids(pd), pd=pd), 5L)

pd <- get_parse_data(parse(text="1->x", keep.source=TRUE))
expect_equal(pd_get_assign_value_id(all_assignment_ids(pd), pd=pd), 2L)

pd <- get_parse_data(parse(text="1->>x", keep.source=TRUE))
expect_equal(pd_get_assign_value_id(all_assignment_ids(pd), pd=pd), 2L)
})
#line 103 "R/pd_assign.R"
test_that('assign_variable', {#!@testthat
pd <- get_parse_data(parse(text="hello_world <- function(){
    print('hello world')
}
", keep.source=TRUE))

    expect_true(pd_is_assignment(roots(pd), pd=pd))
    expect_equal( pd_get_assign_variable_id(roots(pd), pd=pd)
                , parent(.find_text("hello_world")))
})

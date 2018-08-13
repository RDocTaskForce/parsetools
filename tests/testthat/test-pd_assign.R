#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `pd_assign.R`')
#line 24 "/rdtf/parsetools/R/pd_assign.R"
test_that('is_assignment', {#@testing
    pd <- get_parse_data(parse(text="x <-  1", keep.source=TRUE))
    expect_true(pd_is_assignment(roots(pd), pd=pd))
    pd <- get_parse_data(parse(text="x <<- 1", keep.source=TRUE))
    expect_true(pd_is_assignment(roots(pd), pd=pd))
    pd <- get_parse_data(parse(roots(pd), text="1 ->  x", keep.source=TRUE))
    expect_true(pd_is_assignment(roots(pd), pd=pd))
    pd <- get_parse_data(parse(text="1 ->> x", keep.source=TRUE))
    expect_true(pd_is_assignment(roots(pd), pd=pd))
    pd <- get_parse_data(parse(text="x = 1", keep.source=TRUE))
    expect_true(pd_is_assignment(roots(pd), pd=pd))
})
#line 62 "/rdtf/parsetools/R/pd_assign.R"
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
#line 104 "/rdtf/parsetools/R/pd_assign.R"
test_that('assign_variable', {#!@testthat
pd <- get_parse_data(parse(text="hello_world <- function(){
    print('hello world')
}
", keep.source=TRUE))

    expect_true(pd_is_assignment(roots(pd), pd=pd))
    expect_equal( pd_get_assign_variable_id(roots(pd), pd=pd)
                , parent(.find_text("hello_world")))
})

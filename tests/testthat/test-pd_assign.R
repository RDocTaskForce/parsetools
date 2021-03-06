#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `pd_assign.R`')
#line 41 "C:/rdtf/parsetools/R/pd_assign.R"
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
    expect_equal(sum(pd_is_assignment(pd5$id, pd5)), 1)
    if(R.version$major < 4) {
        expect_true(pd_is_assignment(roots(pd5), pd=pd5))
    } else {
        expect_false(pd_is_assignment(roots(pd5), pd=pd5))
        expect_true(pd_is_assignment(firstborn(roots(pd5), pd5), pd5), pd5)
    }

})
#line 81 "C:/rdtf/parsetools/R/pd_assign.R"
test_that('assign_value', {#!@testing
pd <- get_parse_data(parse(text="x<-1", keep.source=TRUE))
expect_equal(pd_get_assign_value_id(all_assignment_ids(pd), pd=pd), 5L)

pd <- get_parse_data(parse(text="x=1", keep.source=TRUE))
expect_equal( pd_get_assign_value_id(all_assignment_ids(pd), pd=pd)
            , parent(.find_text('1'))
            )

pd <- get_parse_data(parse(text="x<<-1", keep.source=TRUE))
expect_equal( pd_get_assign_value_id(all_assignment_ids(pd), pd=pd)
            , parent(.find_text('1'))
            )

pd <- get_parse_data(parse(text="1->x", keep.source=TRUE))
expect_equal(pd_get_assign_value_id(all_assignment_ids(pd), pd=pd)
            , parent(.find_text('1'))
            )

pd <- get_parse_data(parse(text="1->>x", keep.source=TRUE))
expect_equal( pd_get_assign_value_id(all_assignment_ids(pd), pd=pd)
            , parent(.find_text('1')))
})
#line 123 "C:/rdtf/parsetools/R/pd_assign.R"
test_that('assign_variable', {#!@testthat
pd <- get_parse_data(parse(text="hello_world <- function(){
    print('hello world')
}
", keep.source=TRUE))

    expect_true(pd_is_assignment(roots(pd), pd=pd))
    expect_equal( pd_get_assign_variable_id(roots(pd), pd=pd)
                , parent(.find_text("hello_world")))
})
#line 133 "C:/rdtf/parsetools/R/pd_assign.R"
test_that('right_assign', {#@test right_assign
    pd <- get_parse_data(parse(text="'hello_world' -> hw", keep.source=TRUE))
    expect_true(pd_is_assignment(roots(pd), pd))
    expect_equal( pd_get_assign_variable_id(roots(pd), pd=pd)
                , parent(.find_text("hw")))
})

#! This file was automatically produced by documentation::extract_tests on  2018-04-30 10:01:16
#! changes will be overwritten.
context('tests extracted from file `C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/pd_assign.R`')
#line 22 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/pd_assign.R"
test_that('is_pd_assignment', {#! @testthat is_pd_assignment
    pd <- get_parse_data(parse(text="x <-  1", keep.source=TRUE))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="x <<- 1", keep.source=TRUE))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="1 ->  x", keep.source=TRUE))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="1 ->> x", keep.source=TRUE))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="x = 1", keep.source=TRUE))
    expect_true(is_pd_assignment(pd))
})
#line 55 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/pd_assign.R"
test_that('get_pd_assign_value_id', {#!@testing
pd <- get_parse_data(parse(text="x<-1", keep.source=TRUE))
val.id <- get_pd_assign_value_id(pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="x=1", keep.source=TRUE))
val.id <- get_pd_assign_value_id(pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="x<<-1", keep.source=TRUE))
val.id <- get_pd_assign_value_id(pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="1->x", keep.source=TRUE))
val.id <- get_pd_assign_value_id(pd)
expect_equal(val.id, 2L)

pd <- get_parse_data(parse(text="1->>x", keep.source=TRUE))
val.id <- get_pd_assign_value_id(pd)
expect_equal(val.id, 2L)
})
#line 90 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/pd_assign.R"
test_that('get_pd_assign_value', {#! @testthat get_pd_assign_value
pd <- get_parse_data(parse(text="x<-1", keep.source=TRUE))

val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="x=1", keep.source=TRUE))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="x<<-1", keep.source=TRUE))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="1->x", keep.source=TRUE))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="1->>x", keep.source=TRUE))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)
})
#line 132 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/pd_assign.R"
test_that('get_pd_assign_variable', {#!@testthat
    pd <- get_parse_data(parse(text ={"hello_world <- function(){
        print('hello world')
    }
    "}, keep.source=TRUE))

    expect_true(is_pd_assignment(pd))
    var.pd <- get_pd_assign_variable(pd)
    expect_equal(getParseText(var.pd, all_root_ids(var.pd)), "hello_world")

})
#line 165 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/pd_assign.R"
test_that('get_pd_assign_variable_id', {#!@testthat
"hello_world <- function(){
    print('hello world')
}
" %>%
parse(text = ., keep.source=TRUE) %>%
get_parse_data() %>%
sort-> pd

    expect_true(is_pd_assignment(pd))

    var.pd <- get_pd_assign_variable(pd)
    var.id <- get_pd_assign_variable_id(pd)
    expect_equal(var.id, all_root_ids(var.pd))
})

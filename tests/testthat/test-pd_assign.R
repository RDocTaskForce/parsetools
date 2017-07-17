#! This file was automatically produced by documentation::extract_tests on  2017-07-08 09:16:20
#! changes will be overwritten.
context('tests extracted from file `C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/pd_assign.R`')
#line 19 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/pd_assign.R"
test_that("is_pd_assignment", {#! @testthat is_pd_assignment
    pd <- get_parse_data(parse(text="x <-  1"))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="x <<- 1"))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="1 ->  x"))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="1 ->> x"))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="x = 1"))
    expect_true(is_pd_assignment(pd))
})
#line 47 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/pd_assign.R"
test_that("get_pd_assign_value_id", {#!@testing
pd <- get_parse_data(parse(text="x<-1"))
val.id <- get_pd_assign_value_id(pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="x=1"))
val.id <- get_pd_assign_value_id(pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="x<<-1"))
val.id <- get_pd_assign_value_id(pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="1->x"))
val.id <- get_pd_assign_value_id(pd)
expect_equal(val.id, 2L)

pd <- get_parse_data(parse(text="1->>x"))
val.id <- get_pd_assign_value_id(pd)
expect_equal(val.id, 2L)
})
#line 79 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/pd_assign.R"
test_that("get_pd_assign_value", {#! @testthat get_pd_assign_value
pd <- get_parse_data(parse(text="x<-1"))

val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="x=1"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="x<<-1"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="1->x"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="1->>x"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)
})
#line 117 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/pd_assign.R"
test_that("get_pd_assign_variable", {#!@testthat
    pd <- get_parse_data(parse(text ={"hello_world <- function(){
        print('hello world')
    }
    "}))

    expect_true(is_pd_assignment(pd))
    var.pd <- get_pd_assign_variable(pd)
    expect_equal(getParseText(var.pd, all_root_ids(var.pd)), "hello_world")

})
#line 144 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/pd_assign.R"
test_that("get_pd_assign_variable_id", {#!@testthat
"hello_world <- function(){
    print('hello world')
}
" %>%
parse(text = .) %>%
get_parse_data() %>%
sort-> pd

    expect_true(is_pd_assignment(pd))

    var.pd <- get_pd_assign_variable(pd)
    var.id <- get_pd_assign_variable_id(pd)
    expect_equal(var.id, all_root_ids(var.pd))
})
#line 169 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/pd_assign.R"
test_that("is_pd_function", {#! @testthat is_pd_function
    pd <- get_parse_data(parse(text="function(){}"))
    expect_true(is_pd_function(pd))

    pd <- get_parse_data(parse(text="fun <- function(){}"))
    expect_false(is_pd_function(pd))
})

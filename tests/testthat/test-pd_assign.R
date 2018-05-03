#! This file was automatically produced by documentation::extract_tests on  2018-05-03 10:17:17
#! changes will be overwritten.
context('tests extracted from file `/home/aredd/projects/rdtf/parsetools/R/pd_assign.R`')
#line 22 "/home/aredd/projects/rdtf/parsetools/R/pd_assign.R"
test_that('pd_is_assignment', {#! @testthat pd_is_assignment
    pd <- get_parse_data(parse(text="x <-  1"))
    expect_true(pd_is_assignment(pd=pd))
    pd <- get_parse_data(parse(text="x <<- 1"))
    expect_true(pd_is_assignment(pd=pd))
    pd <- get_parse_data(parse(text="1 ->  x"))
    expect_true(pd_is_assignment(pd=pd))
    pd <- get_parse_data(parse(text="1 ->> x"))
    expect_true(pd_is_assignment(pd=pd))
    pd <- get_parse_data(parse(text="x = 1"))
    expect_true(pd_is_assignment(pd=pd))
})
#line 55 "/home/aredd/projects/rdtf/parsetools/R/pd_assign.R"
test_that('pd_get_assign_value_id', {#!@testing
pd <- get_parse_data(parse(text="x<-1"))
val.id <- pd_get_assign_value_id(pd=pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="x=1"))
val.id <- pd_get_assign_value_id(pd=pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="x<<-1"))
val.id <- pd_get_assign_value_id(pd=pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="1->x"))
val.id <- pd_get_assign_value_id(pd=pd)
expect_equal(val.id, 2L)

pd <- get_parse_data(parse(text="1->>x"))
val.id <- pd_get_assign_value_id(pd=pd)
expect_equal(val.id, 2L)
})
#line 90 "/home/aredd/projects/rdtf/parsetools/R/pd_assign.R"
test_that('pd_get_assign_value', {#! @testthat pd_get_assign_value
pd <- get_parse_data(parse(text="x<-1"))

val.pd <- pd_get_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="x=1"))
val.pd <- pd_get_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="x<<-1"))
val.pd <- pd_get_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="1->x"))
val.pd <- pd_get_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="1->>x"))
val.pd <- pd_get_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)
})
#line 132 "/home/aredd/projects/rdtf/parsetools/R/pd_assign.R"
test_that('pd_get_assign_variable', {#!@testthat
    pd <- get_parse_data(parse(text ={"hello_world <- function(){
        print('hello world')
    }
    "}))

    expect_true(pd_is_assignment(pd=pd))
    var.pd <- pd_get_assign_variable(pd=pd)
    expect_equal(getParseText(var.pd, all_root_ids(var.pd)), "hello_world")
})
#line 164 "/home/aredd/projects/rdtf/parsetools/R/pd_assign.R"
test_that('pd_get_assign_variable_id', {#!@testthat
"hello_world <- function(){
    print('hello world')
}
" %>%
parse(text = .) %>%
get_parse_data() %>%
sort-> pd

    expect_true(pd_is_assignment(pd=pd))

    var.pd <- pd_get_assign_variable(pd=pd)
    var.id <- pd_get_assign_variable_id(pd=pd)
    expect_equal(var.id, all_root_ids(var.pd))
})

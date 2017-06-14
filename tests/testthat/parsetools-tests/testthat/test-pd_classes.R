#! This file was automatically produced by lint on  2017-06-06 12:03:28
#! changes will be overwritten.
context('tests extracted from file `./R/pd_classes.R`')
test_that("'is_pd_assignment'", {#! @testthat is_pd_assignment
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
test_that("'get_pd_call_args'", {#! @testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    args <- get_pd_call_args(pd)

    expect_is(args, 'list')
    expect_equal(names(args), c('', 'mean', 'sd'))
    expect_equivalent( args
                     , list( pd[5,], mean=pd[10,], sd=pd[15,])
                     )


})
test_that("'get_pd_assign_value'", {#! @testthat get_pd_assign_value
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
test_that("'get_pd_assign_variable'", {#!@testthat
    pd <- get_parse_data(parse(text ={"hello_world <- function(){
        print('hello world')
    }
    "}))

    expect_true(is_pd_assignment(pd))
    var.pd <- get_pd_assign_variable(pd)
    expect_equal(getParseText(var.pd, all_root_ids(var.pd)), "hello_world")

})
test_that("'get_pd_assign_variable_id'", {#!@testthat
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
test_that("'is_pd_function'", {#! @testthat is_pd_function
    pd <- get_parse_data(parse(text="function(){}"))
    expect_true(is_pd_function(pd))

    pd <- get_parse_data(parse(text="fun <- function(){}"))
    expect_false(is_pd_function(pd))
})

#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `pd_function.R`')
#line 41 "/rdtf/parsetools/R/pd_function.R"
test_that('pd_is_function', {#! @testthat pd_is_function
    pd <- get_parse_data(parse(text="function(){}", keep.source=TRUE))
    expect_true(pd_is_function(roots(pd), pd))

    pd <- get_parse_data(parse(text="fun <- function(){}", keep.source=TRUE))
    expect_false(pd_is_function(roots(pd), pd))

    expect_length(all_function_ids(pd), 1L)


})
#line 71 "/rdtf/parsetools/R/pd_function.R"
test_that('function_body', {#@testing
pd <- get_parse_data(parse(text="hello_world <- function(){
    print('hello world')
}
", keep.source=TRUE))

    id <- all_function_ids(pd)
    expect_equal(pd_get_function_body_id(id, pd), parent(.find_text('{')))

    pd <- get_parse_data(parse(text='function(l,r)paste(l,r)', keep.source=TRUE))
    expect_identical( pd_get_function_body_id(all_function_ids(pd), pd=pd)
                    , parent(parent(.find_text('paste')), pd)
                    )
})
#line 99 "/rdtf/parsetools/R/pd_function.R"
test_that('function_args', {#@testing
pd <- get_parse_data(parse(text='pd_get_function_arg_ids <-
function( pd                    #< parse data
        , id = pd_all_root_ids(pd) #< id number
        ){}', keep.source=TRUE))

    id <- all_function_ids(pd)
    expect_identical( text(pd_get_function_arg_ids(id, pd), pd=pd)
                    , c('(', 'pd', '#< parse data', ','
                       , 'id', '=', '', '#< id number', ')'
                       )
                    )
})
#line 128 "/rdtf/parsetools/R/pd_function.R"
test_that('function_arg_variables', {#@testing
pd <- get_parse_data(parse(text='pd_get_function_arg_ids <-
function( pd                    #< parse data
        , id = pd_all_root_ids(pd) #< id number
        ){}', keep.source=TRUE))
    id <- assign_value(all_assignment_ids(pd))
    expected <- pd[pd$parent==id & pd$text %in% c('pd', 'id'), 'id']
    expect_identical(pd_get_function_arg_variable_ids(id, pd), expected)
    expect_error(pd_get_function_arg_variable_ids(roots(pd), pd))
})
#line 163 "/rdtf/parsetools/R/pd_function.R"
test_that('is_function_arg', {#@testing
    pd <- get_parse_data(parse(text='
    function( a, b = 1){
        cat("hello world")
    }', keep.source=TRUE))

    id <- .find_text('a')
    expect_true(pd_is_function_arg(id, pd))
    expect_false(pd_is_function_arg(.find_text('"hello world"'), pd))

    expect_length(is_function_arg(pd$id, pd), nrow(pd))
    expect_equal(sum(is_function_arg(pd$id, pd)), 4)
})
#line 194 "/rdtf/parsetools/R/pd_function.R"
test_that('function_arg_associated_comments', {#@testing
pd <- get_parse_data(parse(text='pd_get_function_arg_ids <-
function( pd                    #< parse data
                                #< continuation comment
        , id = pd_all_root_ids(pd)
        ){}', keep.source=TRUE))

    function.id <- assign_value(all_assignment_ids(pd), pd)
    arg.ids <- function_arg_variables(function.id, pd)
    id <- arg.ids[[1]]

    expect_identical(text(pd_get_function_arg_associated_comment_ids(id, pd), pd=pd)
                    , c('#< parse data', '#< continuation comment'))

    expect_length(pd_get_function_arg_associated_comment_ids(arg.ids[[2]], pd), 0)
})

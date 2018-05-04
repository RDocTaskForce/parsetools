#! This file was automatically produced by documentation::extract_tests on  2018-05-04 11:22:55
#! changes will be overwritten.
context('tests extracted from file `pd_function.R`')
#line 41 "/home/aredd/projects/rdtf/parsetools/R/pd_function.R"
test_that('pd_is_function', {#! @testthat pd_is_function
    pd <- get_parse_data(parse(text="function(){}", keep.source=TRUE))
    expect_true(pd_is_function(pd=pd))

    pd <- get_parse_data(parse(text="fun <- function(){}", keep.source=TRUE))
    expect_false(pd_is_function(pd=pd))
})
#line 66 "/home/aredd/projects/rdtf/parsetools/R/pd_function.R"
test_that('get_function_body_id', {#@testing
"hello_world <- function(){
    print('hello world')
}
" %>%
    parse(text = .) %>%
    get_parse_data() %>%
    sort-> pd

    id <- pd_get_assign_value_id(pd=pd)
    body.id <- get_function_body_id(id, pd)
    
    expected.body.id <- subset(pd, token == "'{'")$parent
    expect_equal(body.id, expected.body.id)

pd <- get_parse_data(parse(text='function(l,r)paste(l,r)', keep.source=TRUE))
    base.id <- subset(pd, text=='paste')$parent
    expected <- get_parent_id(base.id, pd)
    body.id <- get_function_body_id(pd=pd)
    expect_identical(body.id, expected)
})
#line 96 "/home/aredd/projects/rdtf/parsetools/R/pd_function.R"
test_that('get_function_arg_ids', {#@testing
'get_function_arg_ids <- 
function( pd                    #< parse data
        , id = all_root_ids(pd) #< id number
        ){}' %>%
    parse(text = .) %>%
    get_parse_data() -> pd
    
    id <- pd_get_assign_value_id(pd=pd)
    arg.ids <- get_function_arg_ids(id, pd)
  
    expect_identical( text(arg.ids, pd=pd)
                    , c('(', 'pd', '#< parse data', ','
                       , 'id', '=', '', '#< id number', ')'
                       )
                    )
})
#line 121 "/home/aredd/projects/rdtf/parsetools/R/pd_function.R"
test_that('get_function_arg_variable_ids', {#@testing
'get_function_arg_ids <- 
function( pd                    #< parse data
        , id = all_root_ids(pd) #< id number
        ){}' %>%
    parse(text = .) %>%
    get_parse_data() -> pd
    
    id <- pd_get_assign_value_id(pd=pd)
    expected <- pd[pd$parent==id & pd$text %in% c('pd', 'id'), 'id']
    
    expect_identical(get_function_arg_variable_ids(id, pd), expected)
})
#line 137 "/home/aredd/projects/rdtf/parsetools/R/pd_function.R"
test_that('pd_is_function_arg', {#@testing
'get_function_arg_ids <- 
function( pd                    #< parse data
                                #^ continuation comment
        , id = all_root_ids(pd) #< id number
        ){}' %>%
    parse(text = .) %>%
    get_parse_data() -> pd
    
    
})
#line 159 "/home/aredd/projects/rdtf/parsetools/R/pd_function.R"
test_that('get_function_arg_associated_comment_ids', {#@testing
'get_function_arg_ids <- 
function( pd                    #< parse data
                                #< continuation comment
        , id = all_root_ids(pd)
        ){}' %>%
    parse(text = .) %>%
    get_parse_data() -> pd
    
    function.id <- pd_get_assign_value_id(pd=pd)
    arg.ids <- get_function_arg_variable_ids(function.id, pd)
    id <- arg.ids[[1]]
    
    value <- get_function_arg_associated_comment_ids(id, pd)
    expect_identical(text(value, pd=pd), c('#< parse data', '#< continuation comment'))
    
    expect_length(get_function_arg_associated_comment_ids(arg.ids[[2]], pd), 0)
})

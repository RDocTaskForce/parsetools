{#######################################################################
# pd_function.R
# This file is part of the R package `parsetools`.
#
# Author: Andrew Redd
# Copyright: 2017 University of Utah
#
# LICENSE
# ========
# The R package `parsetools` is free software: 
# you can redistribute it and/or modify it under the terms of the 
# GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) 
# any later version.
#
# This software is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License 
# along with this program. If not, see http://www.gnu.org/licenses/.
#
}#######################################################################

#' @export
pd_is_function <-
function( id = all_root_ids(pd)
        , pd = get('pd', parent.frame()) 
        ){
    #' @title test if a function
    #' @inheritParams pd_is_assignment
    #' @description
    #'   Test if the \code{id} points to a function.
    #'   
    if (length(id) > 1) sapply(id, pd_is_function, pd=pd)
    kids.pd <- get_child(id=id, pd, ngenerations=1, FALSE)
    kids.pd[1, 'token'] == 'FUNCTION'
    #' @return a logical vector, same length as \code{id}.
}
if(F){#! @testthat pd_is_function
    pd <- get_parse_data(parse(text="function(){}", keep.source=TRUE))
    expect_true(pd_is_function(pd=pd))

    pd <- get_parse_data(parse(text="fun <- function(){}", keep.source=TRUE))
    expect_false(pd_is_function(pd=pd))
}

#' @describeIn pd_is_function Obtain the body of a function
#' @export
get_function_body_id <- 
function( id = all_root_ids(pd)
        , pd=get('pd', parent.frame()) 
        ){
    if (length(id)>1L) return(sapply(id, get_function_body_id, pd=pd))
    max(get_child_ids(id, pd))
}
if(F){#@testing
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
}

#' @describeIn pd_is_function Obtain the ids for the arguments of a function
#' @export
get_function_arg_ids <- 
function( id = pd$id
        , pd = get('pd', parent.frame())
        ){
    utils::tail(utils::head(get_child_ids(id=id, pd=pd), -1), -1)
}
if(F){#@testing
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
}

get_function_arg_variable_ids <- 
function( id = pd$id
        , pd = get('pd', parent.frame())
        ){
    arg.ids <- get_function_arg_ids(id, pd)
    arg.ids[token(arg.ids, pd=pd) == 'SYMBOL_FORMALS']
}
if(F){#@testing
'get_function_arg_ids <- 
function( pd                    #< parse data
        , id = all_root_ids(pd) #< id number
        ){}' %>%
    parse(text = .) %>%
    get_parse_data() -> pd
    
    id <- pd_get_assign_value_id(pd=pd)
    expected <- pd[pd$parent==id & pd$text %in% c('pd', 'id'), 'id']
    
    expect_identical(get_function_arg_variable_ids(id, pd), expected)
}

pd_is_function_arg <- 
function(id, pd){}
if(F){#@testing
'get_function_arg_ids <- 
function( pd                    #< parse data
                                #^ continuation comment
        , id = all_root_ids(pd) #< id number
        ){}' %>%
    parse(text = .) %>%
    get_parse_data() -> pd
    
    
}

get_function_arg_associated_comment_ids <- 
function( id = pd$id
        , pd = get('pd', parent.frame())
        ){
    stopifnot(length(id)==1)
    sibling.args <- get_function_arg_variable_ids(get_parent_id(id, pd), pd)
    all.siblings  <- get_sibling_ids(id, pd)
    comments <- intersect(get_relative_comment_ids(pd), all.siblings)
    comments[associate_relative_comments(comments) == id]
}
if(F){#@testing
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
}




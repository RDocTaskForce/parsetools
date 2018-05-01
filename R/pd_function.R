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
is_pd_function <-
function( pd #< a [parse-data] object
        , id = all_root_ids(pd)
        ){
    #' @title test if a function
    #' @inheritParams is_pd_assignment
    #' @description
    #'   Test if the \code{id} points to a function.
    #'   
    if (length(id) > 1) sapply(id, is_pd_function, pd=pd)
    kids.pd <- get_child(id=id, pd, ngenerations=1, FALSE)
    kids.pd[1, 'token'] == 'FUNCTION'
    #' @return a logical vector, same length as \code{id}.
}
if(F){#! @testthat is_pd_function
    pd <- get_parse_data(parse(text="function(){}", keep.source=TRUE))
    expect_true(is_pd_function(pd))

    pd <- get_parse_data(parse(text="fun <- function(){}", keep.source=TRUE))
    expect_false(is_pd_function(pd))
}

#' @describeIn is_pd_function Obtain the body of a function
#' @export
get_function_body_id <- 
function( pd, id = all_root_ids(pd)){
    if (length(id)>1L) return(sapply(id, get_function_body_id, pd=pd))
    max(get_child_ids(pd, id))
}
if(F){#@testing
"hello_world <- function(){
    print('hello world')
}
" %>%
    parse(text = .) %>%
    get_parse_data() %>%
    sort-> pd

    id <- get_pd_assign_value_id(pd)
    body.id <- get_function_body_id(pd, id)
    
    expected.body.id <- subset(pd, token == "'{'")$parent
    expect_equal(body.id, expected.body.id)
    
pd <- get_parse_data(parse(text='function(l,r)paste(l,r)', keep.source=TRUE))
    base.id <- subset(pd, text=='paste')$parent
    expected <- get_parent_id(pd, base.id)
    body.id <- get_function_body_id(pd)
    expect_identical(body.id, expected)
}

#' @describeIn is_pd_function Obtain the ids for the arguments of a function
#' @export
get_function_arg_ids <- 
function(pd, id = all_root_ids(pd)){
    tail(head(get_child_ids(pd, id), -1), -1)
}
if(F){#@testing
'get_function_arg_ids <- 
function( pd                    #< parse data
        , id = all_root_ids(pd) #< id number
        ){}' %>%
    parse(text = .) %>%
    get_parse_data() -> pd
    
    id <- get_pd_assign_value_id(pd)
    arg.ids <- get_function_arg_ids(pd, id)
  
    expect_identical( text(arg.ids, pd=pd)
                    , c('(', 'pd', '#< parse data', ','
                       , 'id', '=', '', '#< id number', ')'
                       )
                    )
}

get_function_arg_variable_ids <- 
function(pd, id = all_root_ids(pd)){
    arg.ids <- get_function_arg_ids(pd, id)
    arg.ids[token(arg.ids, pd=pd) == 'SYMBOL_FORMALS']
}
if(F){#@testing
'get_function_arg_ids <- 
function( pd                    #< parse data
        , id = all_root_ids(pd) #< id number
        ){}' %>%
    parse(text = .) %>%
    get_parse_data() -> pd
    
    id <- get_pd_assign_value_id(pd)
    expected <- pd[pd$parent==id & pd$text %in% c('pd', 'id'), 'id']
    
    expect_identical(get_function_arg_variable_ids(pd, id), expected)
}

is_pd_function_arg <- 
function(pd, id){}
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
function(pd, id){
    sibling.args <- get_function_arg_variable_ids(pd, get_parent_id(pd, id))
    all.siblings  <- get_sibling_ids(pd, id)
#~     sibling.comments <- 
    
}
if(F){#@testing
'get_function_arg_ids <- 
function( pd                    #< parse data
                                #^ continuation comment
        , id = all_root_ids(pd) #< id number
        ){}' %>%
    parse(text = .) %>%
    get_parse_data() -> pd
    
    function.id <- get_pd_assign_value_id(pd)
    arg.ids <- get_function_arg_variable_ids(pd, function.id)
    id <- arg.ids[[1]]
    
    expected <- pd[pd$parent==id & pd$text %in% c('pd', 'id'), 'id']
    
    

    
}




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
    if (length(id) > 1) return(sapply(id, pd_is_function, pd=pd))
    kids <- children(id, pd, ngenerations=1, include.self=FALSE)
    token(firstborn(id)) =='FUNCTION'
    #' @return a logical vector, same length as \code{id}.
}
if(F){#! @testthat pd_is_function
    pd <- get_parse_data(parse(text="function(){}", keep.source=TRUE))
    expect_true(pd_is_function(pd=pd))

    pd <- get_parse_data(parse(text="fun <- function(){}", keep.source=TRUE))
    expect_false(pd_is_function(pd=pd))

    pd_is_function(pd$id, pd)
}

pd_is_in_function <-
function( id = all_root_ids(pd)
        , pd = get('pd', parent.frame())
        ){
    stop("Not implimented")
}


#' @describeIn pd_is_function Obtain the body of a function
#' @export
get_function_body_id <-
function( id = all_root_ids(pd)
        , pd = get('pd', parent.frame())
        ){
    if (length(id)>1L) return(sapply(id, get_function_body_id, pd=pd))
    max(children(id, pd))
}
if(F){#@testing
pd <- get_parse_data(parse(text="hello_world <- function(){
    print('hello world')
}
", keep.source=TRUE))

    id <- assign_value(pd=pd)
    expect_equal(get_function_body_id(id, pd), parent(pd_find_text('{')))

    pd <- get_parse_data(parse(text='function(l,r)paste(l,r)', keep.source=TRUE))
    expect_identical( get_function_body_id(pd=pd)
                    , parent(parent(pd_find_text('paste')), pd)
                    )
}

#' @describeIn pd_is_function Obtain the ids for the arguments of a function
get_function_arg_ids <-
function( id = pd$id
        , pd = get('pd', parent.frame())
        ){
    utils::tail(utils::head(children(id=id, pd=pd), -1), -1)
}
function_args <- internal(get_function_arg_ids)
if(F){#@testing
pd <- get_parse_data(parse(text='get_function_arg_ids <-
function( pd                    #< parse data
        , id = all_root_ids(pd) #< id number
        ){}', keep.source=TRUE))

    id <- assign_value(pd=pd)
    expect_identical( text(get_function_arg_ids(id, pd), pd=pd)
                    , c('(', 'pd', '#< parse data', ','
                       , 'id', '=', '', '#< id number', ')'
                       )
                    )
}

get_function_arg_variable_ids <-
function( id, pd, .check = TRUE){
    if (.check)
        stopifnot( pd_is_function(id, pd)
                 , length(id) == 1L
                 )
    arg.ids <- function_args(id, pd)
    arg.ids[token(arg.ids, pd=pd) == 'SYMBOL_FORMALS']
}
function_arg_variables <- internal(get_function_arg_variable_ids)
if(F){#@testing
pd <- get_parse_data(parse(text='get_function_arg_ids <-
function( pd                    #< parse data
        , id = all_root_ids(pd) #< id number
        ){}', keep.source=TRUE))

    id <- assign_value(pd=pd)
    expected <- pd[pd$parent==id & pd$text %in% c('pd', 'id'), 'id']

    expect_identical(get_function_arg_variable_ids(id, pd), expected)
}

#' @export
#' @rdname
pd_is_function_arg <-
function(id, pd){
    #TODO
    stop("not implimented")
    pd_is_function(parent(id), pd=pd)

    children(parent(id))

}
if(F){#@testing
pd <- get_parse_data(parse(text='get_function_arg_ids <-
function( pd                    #< parse data
                                #< continuation comment
        , id = all_root_ids(pd) #< id number
        ){

        #< malplaced relative comment
        }', keep.source=TRUE))

id <- associate_relative_comments(pd=pd)
#TODO finish testing pd_is_function_arg
}

get_function_arg_associated_comment_ids <-
function( id, pd, .check = TRUE){
    if (.check)
        stopifnot( inherits(pd, 'parse-data')
                 , length(id) == 1L
                 # , pd_is_function_arg(id, pd)
                 )
    stopifnot(length(id)==1)
    sibling.args <- function_arg_variables(parent(id, pd), pd)
    all.siblings  <- siblings(id, pd)
    comments <- intersect(relative_comments(pd), all.siblings)
    comments[associate_relative_comments(comments) == id]
}
function_arg_associated_comments <- internal(get_function_arg_associated_comment_ids)
if(F){#@testing
pd <- get_parse_data(parse(text='get_function_arg_ids <-
function( pd                    #< parse data
                                #< continuation comment
        , id = all_root_ids(pd)
        ){}', keep.source=TRUE))

    function.id <- assign_value(pd=pd)
    arg.ids <- function_arg_variables(function.id, pd)
    id <- arg.ids[[1]]

    expect_identical(text(get_function_arg_associated_comment_ids(id, pd), pd=pd)
                    , c('#< parse data', '#< continuation comment'))

    expect_length(get_function_arg_associated_comment_ids(arg.ids[[2]], pd), 0)
}




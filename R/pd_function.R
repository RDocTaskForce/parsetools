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
function( id, pd, .check=TRUE){
    #' @title test if a function
    #' @inheritParams pd_is_assignment
    #' @description
    #'   Test if the \code{id} points to a function.
    if(.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    !is.na(firstborn(id)) & token(firstborn(id)) == 'FUNCTION'
    #' @return a logical vector, same length as \code{id}.
}
all_function_ids <- make_get_all(pd_is_function)
is_function <- internal(pd_is_function)
if(F){#! @testthat pd_is_function
    pd <- get_parse_data(parse(text="function(){}", keep.source=TRUE))
    expect_true(pd_is_function(roots(pd), pd))

    pd <- get_parse_data(parse(text="fun <- function(){}", keep.source=TRUE))
    expect_false(pd_is_function(roots(pd), pd))

    expect_length(all_function_ids(pd), 1L)


}

pd_is_in_function <-
function( id, pd, .check=TRUE){
    stop("Not implimented")
}


#' @describeIn pd_is_function Obtain the body of a function
#' @export
pd_get_function_body_id <-
function( id, pd, .check=TRUE){
    if(.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot(all(is_function(id, pd)))
    }
    if (length(id)>1L) return(sapply(id, pd_get_function_body_id, pd=pd))
    max(children(id, pd))
}
function_body <- internal(pd_get_function_body_id, all_function_ids(pd))
if(F){#@testing
pd <- get_parse_data(parse(text="hello_world <- function(){
    print('hello world')
}
", keep.source=TRUE))

    id <- all_function_ids(pd)
    expect_equal(pd_get_function_body_id(id, pd), parent(pd_find_text('{')))

    pd <- get_parse_data(parse(text='function(l,r)paste(l,r)', keep.source=TRUE))
    expect_identical( pd_get_function_body_id(all_function_ids(pd), pd=pd)
                    , parent(parent(pd_find_text('paste')), pd)
                    )
}

#' @describeIn pd_is_function Obtain the ids for the arguments of a function
pd_get_function_arg_ids <-
function( id, pd, .check=TRUE){
    if(.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot( length(id) == 1L
                 , pd_is_function(id, pd)
                 )
    }
    utils::tail(utils::head(children(id=id, pd=pd), -1), -1)
}
function_args <- internal(pd_get_function_arg_ids, all_function_ids(pd))
if(F){#@testing
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
}

#' Retrieve the variable for a function argument
#' @export
pd_get_function_arg_variable_ids <-
function( id, pd, .check = TRUE){
    if(.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot( length(id) == 1L
                 , pd_is_function(id, pd)
                 )
    }
    arg.ids <- function_args(id, pd)
    arg.ids[token(arg.ids, pd=pd) == 'SYMBOL_FORMALS']
}
function_arg_variables <- internal(pd_get_function_arg_variable_ids, all_function_ids(pd))
if(F){#@testing
pd <- get_parse_data(parse(text='pd_get_function_arg_ids <-
function( pd                    #< parse data
        , id = pd_all_root_ids(pd) #< id number
        ){}', keep.source=TRUE))
    id <- assign_value(all_assignment_ids(pd))
    expected <- pd[pd$parent==id & pd$text %in% c('pd', 'id'), 'id']
    expect_identical(pd_get_function_arg_variable_ids(id, pd), expected)
    expect_error(pd_get_function_arg_variable_ids(roots(pd), pd))
}

#' @export
#' @rdname pd_get_function_arg_variable_ids
pd_get_function_arg_variable_text <-
    function(id, pd, .check=TRUE)
        pd_get_function_arg_variable_ids(id=id, pd=pd, .check=.check)


pd_is_function_arg <-
function(id, pd, .check=TRUE){
    if(.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    is_function(parent(id)) &
    (
        token(id) %in% c('SYMBOL_FORMALS', 'EQ_FORMALS') |
        (
            token(id) == 'expr' &
            !is.na(next_sibling(id)) & token(next_sibling(id)) != "'}'" &
            !is.na(prev_sibling(id)) & token(prev_sibling(id)) != "'{'"
        )
    )
}
is_function_arg <- internal(pd_is_function_arg)
if(F){#@testing
    pd <- get_parse_data(parse(text='
    function( a, b = 1){
        cat("hello world")
    }', keep.source=TRUE))

    id <- pd_find_text('a')
    expect_true(pd_is_function_arg(id, pd))
    expect_false(pd_is_function_arg(pd_find_text('"hello world"'), pd))

    expect_length(is_function_arg(pd$id, pd), nrow(pd))
    expect_equal(sum(is_function_arg(pd$id, pd)), 4)
}

#' @export
#' @rdname
pd_get_function_arg_associated_comment_ids <-
function( id, pd, .check = TRUE){
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot( length(id) == 1L
                 , pd_is_function_arg(id, pd)
                 )
    }
    stopifnot(length(id)==1)
    sibling.args <- function_arg_variables(parent(id, pd), pd)
    all.siblings  <- siblings(id, pd)
    comments <- intersect(all_relative_comment_ids(pd), all.siblings)
    comments[relative_comment_associateds(comments) == id]
}
function_arg_associated_comments <- internal(pd_get_function_arg_associated_comment_ids)
if(F){#@testing
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
}




# pd_call.R ###########################################################
#                                                                     #
# This file is part of the R package `parsetools`.                    #
#                                                                     #
# Author: Andrew Redd                                                 #
# Copyright: 2018 The R Consortium                                    #
#                                                                     #
# LICENSE                                                             #
# ========                                                            #
# The R package `parsetools` is free software:                        #
# you can redistribute it and/or modify it under the terms of the     #
# GNU General Public License as published by the Free Software        #
# Foundation, either version 3 of the License, or (at your option)    #
# any later version.                                                  #
#                                                                     #
# This software is distributed in the hope that it will be useful,    #
# but WITHOUT ANY WARRANTY; without even the implied warranty of      #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the        #
# GNU General Public License for more details.                        #
#                                                                     #
# You should have received a copy of the GNU General Public License   #
# along with this program. If not, see http://www.gnu.org/licenses/.  #
#_____________________________________________________________________#
#' @include internal.R


#' @name calls
#' @title Call nodes
#' @description
#' Call nodes represent function calls.
#'
#' @details
#' The traditional call of
#' `function_name(arguments)` is a symbol call as `function_name`
#' is the symbol directly referencing the function to call.
#' Other calls may exists such as `function_array[[1]]()` which
#' first indexes the `function_array` then calls the returned function.
#' This qualifies as a call expression but not a symbol call expression.
#' We are often only concerned with symbol calls and not the anonymous
#' version.
#'
#' @inheritParams family-nodes
#' @example inst/examples/example-pd.R
#' @example inst/examples/example-roots.R
#' @example inst/examples/example-call.R
NULL

#' @describeIn calls Test if the node is a call expression.
#' @param calls an optional list of calls to restrict consideration to.
pd_is_call <-
function( id, pd, calls = NULL, .check=TRUE){
    if(.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    if (length(id)>1) return(sapply(id, pd_is_call, pd=pd))
    if (token(id) != 'expr') return(FALSE)
    fb <- firstborn(id)
    if (token(fb) == "'('") return(TRUE)
    if (token(fb) == "expr" && token(next_sibling(fb)) == "'('") return(TRUE)
    return(FALSE)
}
all_call_ids <- make_get_all(pd_is_call)
is_call <- internal(pd_is_call)
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}, keep.source=TRUE))
    ids <- roots(pd)
    id <- ids[[3]]
    expect_true (pd_is_call(ids[[3]], pd))
    expect_false(pd_is_call(ids[[1]], pd))
    expect_equal(pd_is_call(ids, pd), c(F, F, T))
}
if(FALSE){#@test non-symbol calls
    text <- 'function_array[[1]](1)'
    text <- 'getAnywhere(rnorm)[1](1)'
    pd <- get_parse_data(parse(text=text, keep.source = TRUE))
    id <- roots(pd)
    expect_true(pd_is_call(id, pd))
}

#' @describeIn calls Test if the node is specifically a symbol call expression.
pd_is_symbol_call <-
function( id, pd, .check=TRUE){
    if(.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    if (length(id) > 1) return(sapply(id, pd_is_symbol_call, pd=pd))
    if (!pd_is_call(id, pd)) return(FALSE)
    second <- next_sibling(firstborn(id, pd), pd)
    if (token(second) != "expr") return(FALSE)
    grandchild <- firstborn(second, pd)
    token(grandchild) == 'SYMBOL_FUNCTION_CALL'
    #' @return a logical of the same length as \code{id}
}
all_symbol_call_ids <- make_get_all(pd_is_symbol_call)
is_symbol_call <- internal(pd_is_symbol_call)
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}, keep.source=TRUE))
    ids <- roots(pd)
    id <- ids[[3]]
    expect_true (pd_is_symbol_call(id, pd))
    expect_false(pd_is_symbol_call(ids[[1]], pd))
    expect_equal(pd_is_symbol_call(ids, pd), c(F, F, T))

    expect_false(pd_is_symbol_call(ids[[1]], pd))
}
if(FALSE){#@test non-symbol call
    pd <- get_parse_data(parse(text={"
        (function()cat('hello world!'))()
    "}, keep.source=TRUE))
    id <- roots(pd)
    expect_true(pd_is_call(id, pd))
    expect_false(pd_is_symbol_call(id, pd))
}

#' @describeIn calls Get the symbol, i.e. the name of the function, being called.
pd_get_call_symbol_id <-
function( id, pd, .check=TRUE){
    if(.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot(all(is_symbol_call(id,pd)))
    }
    if (length(id)>1) return(sapply(id, pd_get_call_symbol_id, pd=pd))
    stopifnot(pd_is_symbol_call(id, pd))
    children(next_sibling(firstborn(id)))
}
call_symbol <- internal(pd_get_call_symbol_id)
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}, keep.source=TRUE))
    ids <- roots(pd)
    id <- ids[[3]]
    expect_equal( pd_get_call_symbol_id(id, pd)
                , .find_text('plot'))
}

#' @describeIn calls test Get the set of arguments to the function call.
pd_get_call_arg_ids <-
function( id, pd, .check=TRUE){
    #' @title get ids of the arguments of a call.
    #' @inheritParams pd_is_symbol_call
    #' @description
    #'   Retrieves the ids of the arguments of a call as an integer vector.
    if(.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot( length(id)==1L
                 , is_call(id,pd)
                 )
    }
    kids <- children(id, pd)
    ix <- text(kids) %in% c('(', ',', ')')
    if (all(ix[-1])) return(integer(0))
    groups <- cumsum(ix)
    args <- sapply( split(kids, groups)[c(-1L, -max(groups)-1L)]
                  , `[`, -1)
    val <- sapply( args, function(arg){
                      if (length(arg) == 1) return(arg)
                      if (length(arg) == 2) {
                          #! @note Are there cases other than [base::alist()] that could result in a two
                          #! id argument?
                          stopifnot( text(call_symbol(id)) == 'alist')
                          return(NA_integer_)
                      }
                      if (length(arg) == 3) {
                          stopifnot( all(token(arg) == c('SYMBOL_SUB', 'EQ_SUB', 'expr')) )
                          return(arg[[3]])
                      }
                      stop("I don't know how to handle this :(") # nocov
                  })
    names(val) <- sapply(args, function(arg){
                      if (length(arg) == 1) return('')
                      if (length(arg) >  1) return(text(arg[1]))
                  })
    return(val)
    #' @return a named list where each element is the id for the `expr` element of the argument.
}
call_args <- internal(pd_get_call_arg_ids, roots(pd))
if(FALSE){#! @testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))
    test.object <- pd_get_call_arg_ids(roots(pd), pd=pd)

    expect_is(test.object, 'integer')
    expect_equal(names(test.object), c('', 'mean', 'sd'))
    expect_identical( test.object
                    , c( 5L
                       , mean=parent(.find_text('0'))
                       , sd  =parent(.find_text('1')))
                    )
    pd <- get_parse_data(parse(text='alist(x, y=z, ...=)', keep.source=TRUE))
    expect_identical( call_args(all_call_ids(pd), pd=pd)
                    , c( parent(.find_text('x'))
                       , y = parent(.find_text('z'))
                       , '...'=NA_integer_))
}


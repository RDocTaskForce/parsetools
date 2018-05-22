{#######################################################################
# pd_call.R
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
pd_is_call <-
function( id, pd, calls = NULL, .check=TRUE){
    #' @title Is a call?
    #' @inheritParams pd_get_children_ids
    #' @param calls Acceptable calls, if \code{NULL} (default) all calls allowed.
    #' @description
    #'   Checks if the \code{id} identifies in \code{pd} a call expression.
    if(.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    if (length(id)>1) return(sapply(id, pd_is_call, pd=pd))
    if (token(id) != 'expr') return(FALSE)
    token(firstborn(id)) == "'('"
}
all_call_ids <- make_get_all(pd_is_call)
is_call <- internal(pd_is_call)
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}, keep.source=TRUE))
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_true (pd_is_call(ids[[3]]), pd)
    expect_false(pd_is_call(ids[[1]]), pd)
    expect_equal(pd_is_call(ids, pd), c(F, F, T))
}


#' @export
pd_is_symbol_call <-
function( id, pd, .check=TRUE){
    #' @title Check if the call is specifically a symbol call
    #' @inheritParams pd_is_call
    #' @description
    #'   Checks if the \code{id} identifies in \code{pd} specifically a
    #'   symbol call expression, That is a call from a symbol.
    if(.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    if (length(id) > 1) return(sapply(id, pd_is_symbol_call, pd=pd))
    if (!pd_is_call(id, pd)) return(FALSE)
    eldest <- firstborn(id, pd)
    if (token(eldest) != "'('") return(FALSE)
    second <- next_sibling(eldest, pd)
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
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_true (pd_is_symbol_call(id, pd))
    expect_false(pd_is_symbol_call(ids[[1]], pd))
    expect_equal(pd_is_symbol_call(ids, pd), c(F, F, T))
}

#' @export
pd_get_call_symbol_id <-
function( id, pd, .check=TRUE){
    #' @title Get the symbol of the function being called.
    #' @inheritParams pd_is_symbol_call
    #' @description
    #'    Gets the id of the symbol of the call.
    #'    That is the name of the function being called.
    if(.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot(all(is_symbol_call(id,pd)))
    }
    if (length(id)>1) return(sapply(id, pd_get_call_symbol_id, pd=pd))
    if (!pd_is_symbol_call(id, pd)) return(NA_integer_)
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
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_equal(pd_get_call_symbol_id(id, pd), 45L)
}

#' @export
pd_get_call_arg_ids <-
function( id, pd, .check=TRUE){
    #' @title get the arguments of a call.
    #' @inheritParams pd_is_symbol_call
    #' @description
    #'   Retrieves the arguments of a call as a list.
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
                          #' @note Are there cases other than alist that could result in a two
                          #' id argument?
                          stopifnot( text(call_symbol(id)) == 'alist')
                          return(NA_integer_)
                      }
                      if (length(arg) == 3) {
                          stopifnot( all(token(arg) == c('SYMBOL_SUB', 'EQ_SUB', 'expr')) )
                          return(arg[[3]])
                      }
                      stop("I don't know how to handle this :(")
                  })
    names(val) <- sapply(args, function(arg){
                      if (length(arg) == 1) return('')
                      if (length(arg) >  1) return(text(arg[1]))
                      stop()
                  })
    return(val)
    #' @return a named list where each element is the id for the 'expr' element of the argument.
}
call_args <- internal(pd_get_call_arg_ids, all_root_ids(pd))
if(FALSE){#! @testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))
    test.object <- pd_get_call_arg_ids(all_root_ids(pd), pd=pd)

    expect_is(test.object, 'integer')
    expect_equal(names(test.object), c('', 'mean', 'sd'))
    expect_identical(test.object, c(5L, mean=12L, sd=19L))
    pd <- get_parse_data(parse(text='alist(x, y=z, ...=)', keep.source=TRUE))
    expect_identical( call_args(pd=pd)
                    , c( parent(pd_find_text('x'))
                       , y = parent(pd_find_text('z'))
                       , '...'=NA_integer_))
}

cumand <- function(a)Reduce('&&', a, right=TRUE, accumulate = TRUE)
pd_call_arg_text <- function(args, arg.name, which=1L, pd=get('pd', parent.frame())) {
    # args[[ifelse('signature' %in% names(args), 'signature', 2L)]]

    # arg.is.named <- if(is.name)


    arg <- args[[match(arg.name, names(args), which)]]
    while (token(fname.arg) == 'expr') fname.arg <- firstborn(fname.arg)
    if (token(fname.arg) == 'STR_CONST') unquote(text(fname.arg)) else
        line_error(prev.id, "Cannot infer method name for setMethod.")
}


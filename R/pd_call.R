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
function( id = all_root_ids(pd)[1]       #< id of interest
        , pd = get('pd', parent.frame()) #< parse data of assignemnt
        , calls = NULL
        ){
    #' @title Is a call?
    #' @inheritParams get_child_ids
    #' @description
    #'   Checks if the \code{id} identifies in \code{pd} a call expression.
    if (length(id)>1) return(sapply(id, pd_is_call, pd=pd))
    if (token(id) != 'expr') return(FALSE)
    token(get_firstborn_id(id, pd)) == "'('"
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_true (pd_is_call(ids[[3]]), pd)
    expect_false(pd_is_call(ids[[1]]), pd)
    expect_equal(pd_is_call(ids, pd), c(F, F, T))
    
}


#' @export
pd_is_symbol_call <-
function( id = all_root_ids(pd)[1]       #< id of interest
        , pd = get('pd', parent.frame()) #< parse data of assignemnt
        ){
    #' @title Check if the call is specifically a symbol call
    #' @inheritParams pd_is_call
    #' @description
    #'   Checks if the \code{id} identifies in \code{pd} specifically a 
    #'   symbol call expression, That is a call from a symbol.
    if (length(id) > 1) return(sapply(id, pd_is_symbol_call, pd=pd))
    if (!pd_is_call(id, pd)) return(FALSE)
    eldest <- get_firstborn_id(id, pd)
    if (token(eldest) != "'('") return(FALSE)
    second <- get_next_sibling_id(eldest, pd)
    if (token(second) != "expr") return(FALSE)
    grandchild <- get_firstborn_id(second, pd)
    token(grandchild) == 'SYMBOL_FUNCTION_CALL'
    #' @return a logical of the same length as \code{id}
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_true (pd_is_symbol_call(id, pd))
    expect_false(pd_is_symbol_call(ids[[1]], pd))
    expect_equal(pd_is_symbol_call(ids, pd), c(F, F, T))
}


#' @export
pd_get_call_symbol_id <- 
function( id = all_root_ids(pd)[1]       #< id of interest
        , pd = get('pd', parent.frame()) #< parse data of assignemnt
        ){
    #' @title Get the symbol of the function being called.
    #' @inheritParams pd_is_symbol_call
    #' @description
    #'    Gets the id of the symbol of the call.  
    #'    That is the name of the function being called.
    if (length(id)>1) return(sapply(id, pd_get_call_symbol_id, pd=pd))
    if (!pd_is_symbol_call(id, pd)) return(NA_integer_)
    get_child_ids( 
        get_next_sibling_id( 
            get_firstborn_id(id, pd), pd), pd)
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_equal(pd_get_call_symbol_id(id, pd), 45L)
}

#' @export
pd_get_call_symbol <-
function( id = all_root_ids(pd)[1]       #< id of interest
        , pd = get('pd', parent.frame()) #< parse data of assignemnt
        ){
    #' @title Get the symbol of the function being called.
    #' @inheritParams pd_is_symbol_call
    #' @description a wrapper to \code{\link{pd_is_symbol_call}} to subset
    #'     the \code{\link{parse-data}}.
    stopifnot(pd_is_symbol_call(id, pd))
    get_child( 
        get_next_sibling_id( 
            get_firstborn_id(id, pd), pd), pd)
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_equal(pd_get_call_symbol(id, pd), pd['45',])
}

#' @export
pd_get_call_args <-
function( id = all_root_ids(pd)[1]       #< id of interest
        , pd = get('pd', parent.frame()) #< parse data of assignemnt
        ){
    #' @title get the arguments of a call.
    #' @inheritParams pd_is_symbol_call
    #' @description
    #'   Retrieves the arguments of a call as a list.
    kids <- get_child(id=id, pd=pd, ngenerations=1L, include.self=FALSE)
    groups <- cumsum(kids$token %in% c("'('", "','", "')'"))
    args <- split( kids[groups > 0 & groups < max(groups),][-1,]
                 , groups[groups > 0 & groups < max(groups)][-1]
                 )
    arg.ids <- sapply(args, function(.).[!(.$token %in% c("','", "SYMBOL_SUB", "EQ_SUB")), 'id'])
    arg.pd  <- get_children(arg.ids, pd=pd)
    names(arg.pd) <-
        sapply(args, function(.){
                    if('SYMBOL_SUB' %in% .$token)
                        .['SYMBOL_SUB' == .$token, 'text']
                    else
                        ''
                })
    arg.pd
    #' @return a named list where each element is the parse-data for the argument.
}
if(FALSE){#! @testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    args <- pd_get_call_args(pd=pd)

    expect_is(args, 'list')
    expect_equal(names(args), c('', 'mean', 'sd'))
    expect_equivalent( args
                     , list( pd['4', ], mean=pd['11', ], sd=pd['18', ])
                     )
}


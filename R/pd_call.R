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
is_pd_call <-
function( pd            #< parse data of assignemnt
        , id = all_root_ids(pd)[1] #< id of interest.
        ){
    #! Check if provided parse-data is a function call.
    if (length(id)>1) return(sapply(id, is_pd_call, pd=pd))
    if (token(id) != 'expr') return(FALSE)
    token(get_firstborn_id(pd, id)) == "'('"
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_true (is_pd_call(pd, ids[[3]]))
    expect_false(is_pd_call(pd, ids[[1]]))
    expect_equal(is_pd_call(pd, ids), c(F, F, T))
    
}


#' @export
is_pd_symbol_call <-
function( pd            #< parse data of assignemnt
        , id = all_root_ids(pd)[1] #< id of interest.
        ){
    #! Check if the call is specifically a symbol call
    if (length(id) > 1) return(sapply(id, is_pd_call, pd=pd))
    if (!is_pd_call(pd, id)) return(FALSE)
    eldest <- get_firstborn_id(pd, id)
    if (token(eldest) != "'('") return(FALSE)
    second <- get_next_sibling_id(pd, eldest)
    if (token(second) != "expr") return(FALSE)
    grandchild <- get_firstborn_id(pd, second)
    token(grandchild) == 'SYMBOL_FUNCTION_CALL'
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_true (is_pd_symbol_call(pd, id))
    expect_false(is_pd_symbol_call(pd, ids[[1]]))
    expect_equal(is_pd_symbol_call(pd, ids), c(F, F, T))
}


#' @export
get_pd_call_symbol_id <- 
function( pd            #< parse data of assignemnt
        , id = all_root_ids(pd)[1] #< id of interest.
        ){
    #! Get the symbol of the function being called.
    stopifnot(is_pd_symbol_call(pd,id))
    get_child_ids(pd, 
        get_next_sibling_id(pd, 
            get_firstborn_id(pd, id)))
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_equal(get_pd_call_symbol_id(pd, id), 45L)
}

#' @export
get_pd_call_symbol <-
function( pd            #< parse data of assignemnt
        , id = all_root_ids(pd)[1] #< id of interest.
        ){
    #! Get the symbol of the function being called.
    stopifnot(is_pd_symbol_call(pd,id))
    get_child(pd, 
        get_next_sibling_id(pd, 
            get_firstborn_id(pd, id)))
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    ids <- all_root_ids(pd)
    id <- ids[[3]]
    expect_equal(get_pd_call_symbol(pd, id), pd['45',])
}

#' @export
get_pd_call_args <-
function( pd            #< parse data of assignemnt
        , id = all_root_ids(pd)[1] #< id of interest.
        ){
    kids <- get_child(pd=pd, id=id, ngenerations=1L, include.self=FALSE)
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
}
if(FALSE){#! @testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    args <- get_pd_call_args(pd)

    expect_is(args, 'list')
    expect_equal(names(args), c('', 'mean', 'sd'))
    expect_equivalent( args
                     , list( pd['4', ], mean=pd['11', ], sd=pd['18', ])
                     )
}


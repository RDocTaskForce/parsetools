{#######################################################################
# pd_classes.R
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


assignment.opperators <- c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN")

#' @export
is_pd_assignment <-
function( pd            #< parse data of assignemnt
        , id = all_root_ids(pd)[1] #< id of interest.
        ){
    #! check if the provided parse-data is an assignment expression.
    id <- ._check_id(id)
    if(pd[pd$id == id, 'token'] != 'expr')
        FALSE
    kids.pd <- get_children(pd, id, ngenerations=1)[[1]]
    kids.pd <- kids.pd[kids.pd[['token']] != 'expr', ]

    any(kids.pd[['token']] %in% assignment.opperators)
}
if(F){#! @testthat is_pd_assignment
    pd <- get_parse_data(parse(text="x <-  1"))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="x <<- 1"))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="1 ->  x"))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="1 ->> x"))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="x = 1"))
    expect_true(is_pd_assignment(pd))
}

#' @export
is_pd_call <-
function( pd            #< parse data of assignemnt
        , id = all_root_ids(pd)[1] #< id of interest.
        ){
    #! Check if provided parse-data is a function call.
    if(pd[pd$id == id, 'token'] != 'expr') return(FALSE)
    get_firstborn(pd, id)$token == "'('"
}

#' @export
is_pd_symbol_call <-
function( pd            #< parse data of assignemnt
        , id = all_root_ids(pd)[1] #< id of interest.
        ){
    #! Check if the call is specifically a symbol call
    is_pd_call(pd, id)
    kids.pd <- get_child(id=id, pd=pd, ngenerations=1, include.self=FALSE)
    expr.ids <- kids.pd[kids.pd$token == 'expr', 'id']
    fb <- get_firstborn(pd, min(expr.ids))
    fb$token == 'SYMBOL_FUNCTION_CALL'
}

#' @export
get_pd_call_symbol <-
function( pd            #< parse data of assignemnt
        , id = all_root_ids(pd)[1] #< id of interest.
        ){
    #! Get the symbol of the function being called.
    stopifnot(is_pd_call(pd,id))
    kids.pd <- get_child(id=id, pd=pd, ngenerations=1, include.self=FALSE)
    kids.expr <- kids.pd[kids.pd$token == 'expr', 'id']
    expr.ids <- kids.pd[kids.pd$token == 'expr', 'id']
    fb <- get_firstborn(pd, min(expr.ids))
    stopifnot(fb$token == 'SYMBOL_FUNCTION_CALL')
    fb
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
                     , list( pd[5,], mean=pd[10,], sd=pd[15,])
                     )


}



#' @export
get_pd_assign_value <-
function( pd #< The [parse-data] object, representing an assignment
        ){
    #! get the value of an assignment operator expression.
    #!
    #! This function assumes correct structure and does not check for compliance.
    kids.pd <- sort(get_child(id=all_root_ids(pd), pd, 1, FALSE))
    switch( kids.pd[2, 'token']
          , RIGHT_ASSIGN = get_family(pd, id = utils::head(kids.pd$id,1))
          , LEFT_ASSIGN  = get_family(pd, id = utils::tail(kids.pd$id,1))
          , EQ_ASSIGN    = get_family(pd, id = utils::tail(kids.pd$id,1))
          )
}

#' @export
get_pd_assign_value_id <-
function( pd #< The [parse-data] object, representing an assignment
        , id = all_root_ids(pd)
        ){
    #! Get the id for the value portion of an assignment operator expression.
    if(length(id) > 1)
        sapply(id, get_pd_assign_value_id, pd=pd)
    child.ids <- get_child_ids(pd, id, 1, FALSE)
    type <- pd[pd$id %in% child.ids & pd$token %in% assignment.opperators, 'token']
    switch( type
          , RIGHT_ASSIGN = min(child.ids)
          , max(child.ids)
          )
}
if(FALSE){#! @testthat get_pd_assign_value
pd <- get_parse_data(parse(text="x<-1"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="x=1"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="x<<-1"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="1->x"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="1->>x"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)
}

#' @export
get_pd_assign_variable <-
function( pd #< The [parse-data] object, representing an assignment
        ){
    #! get the value of an assignment operator expression.
    #!
    #! This function assumes correct structure and does not check for compliance.
    kids.pd <- sort(get_child(id=all_root_ids(pd), pd, 1, FALSE))
    switch( kids.pd[2, 'token']
          , RIGHT_ASSIGN = get_family(pd, id = utils::tail(kids.pd$id,1))
          , LEFT_ASSIGN  = get_family(pd, id = utils::head(kids.pd$id,1))
          , EQ_ASSIGN    = get_family(pd, id = utils::head(kids.pd$id,1))
          )
    #! @return will return the parse data, which is typically rooted by an 'expr' token.
}
if(F){#!@testthat
    pd <- get_parse_data(parse(text ={"hello_world <- function(){
        print('hello world')
    }
    "}))

    expect_true(is_pd_assignment(pd))
    var.pd <- get_pd_assign_variable(pd)
    expect_equal(getParseText(var.pd, all_root_ids(var.pd)), "hello_world")

}

#' @export
get_pd_assign_variable_id <-
function( pd #< The [parse-data] object, representing an assignment
        , id = all_root_ids(pd)
        ){
    #! Get the id for the variable portion of an assignment operator expression.
    if(length(id) > 1)
        sapply(id, get_pd_assign_variable_id, pd=pd)
    child.ids   <- get_child_ids(pd, id, 1, FALSE)
    assign.pd   <- pd[pd$id %in% child.ids & pd$token %in% assignment.opperators, ]
    switch( assign.pd$token
          , RIGHT_ASSIGN = max(child.ids)
          , min(setdiff(child.ids, assign.pd$id))
          )
}
if(F){#!@testthat
"hello_world <- function(){
    print('hello world')
}
" %>%
parse(text = .) %>%
get_parse_data() %>%
sort-> pd

    expect_true(is_pd_assignment(pd))

    var.pd <- get_pd_assign_variable(pd)
    var.id <- get_pd_assign_variable_id(pd)
    expect_equal(var.id, all_root_ids(var.pd))
}

#' @export
is_pd_function <-
function( pd #< a [parse-data] object
        , id = all_root_ids(pd)
        ){
    #! test if parse data is a function
    kids.pd <- get_child(id=id, pd, ngenerations=1, FALSE)
    kids.pd[1, 'token'] == 'FUNCTION'
}
if(F){#! @testthat is_pd_function
    pd <- get_parse_data(parse(text="function(){}"))
    expect_true(is_pd_function(pd))

    pd <- get_parse_data(parse(text="fun <- function(){}"))
    expect_false(is_pd_function(pd))
}

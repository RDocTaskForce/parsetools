{#######################################################################
# pd_if.R
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
is_if_expr <- 
function( pd, id){
    #' @title Is if expression?
    #' @inheritParams get_child_ids
    #' @description
    #'   Tests if the id(s) represent if expressions.
    if (length(id)>1) sapply(id, is_if_expr, pd=pd)
    (token(id) == 'expr') &&
    (token(get_firstborn_id(pd, id)) == 'IF')
    #' @return a logical vector of same length as id.
}
#' @export
get_if_predicate_id <- 
function( pd, id ){
    #' @title Get if predicate id
    #' @inheritParams is_if_expr
    #' @description
    #'   Returns the id of the predicate of the if statemment, 
    #'   i.e. the conditional statement.
    stopifnot(is_if_expr(pd, id))
    kids <- get_child_ids(pd, id)
    if (length(kids)<5) stop("inproper if statement")
    kids[[3L]]
}
#' @export
get_if_branch_id <- 
function( pd, id){
    #' @title Get branch of if statment.
    #' @inheritParams is_if_expr
    #' @description
    #'   Returns the id of the body of the branch executed if the predicate 
    #'   evaluates to true.
    stopifnot(is_if_expr(pd, id))
    kids <- get_child_ids(pd, id)
    if (length(kids)<5) stop("inproper if statement")
    branch.id <- kids[[5L]]
    #TODO fix when a comment is in the way.
    #' @return an id integer.
}
#' @export
get_if_alternate_id <- 
function( pd, id){
    #' @title Get the alternate branch of if statement
    #' @inheritParams is_if_expr
    #' @description
    #'   Gets the id of the alternate branch, i.e. the else branch.
    stopifnot(is_if_expr(pd, id))
    kids <- get_child_ids(pd, id)
    if (length(kids)<7 || token(kids[[6]]) != 'ELSE') 
        stop("inproper if-else statement")
    kids[[7L]]
    #' @return an id integer.
}
if(FALSE){#!@testing if structures
    pd <- get_parse_data(parse(text={"
        if(predicate){
            body
        } else {
            alternate
        }
    "}, keep.source=TRUE))
    id <- all_root_ids(pd) # 33
    
    expect_true(is_if_expr(pd, id))
    expect_equal(get_if_predicate_id(pd, id),  7L)
    expect_equal(get_if_branch_id   (pd, id), 18L)
    expect_equal(get_if_alternate_id(pd, id), 30L)
}


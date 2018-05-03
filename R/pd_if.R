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

pd_is_if <- 
function(id = pd$id, pd = get('pd', parent.frame())){
    #' @title Is if expression?
    #' @inheritParams get_child_ids
    #' @description
    #'   Tests if the id(s) represent if expressions.
    if (length(id)>1) sapply(id, pd_is_if, pd=pd)
    (token(id) == 'expr') &&
    (token(get_firstborn_id(pd, id)) == 'IF')
    #' @return a logical vector of same length as id.
}

#' @export
pd_get_if_predicate_id <- 
function(id = pd$id, pd = get('pd', parent.frame())){
    #' @title Get if predicate id
    #' @inheritParams pd_is_if
    #' @description
    #'   Returns the id of the predicate of the if statemment, 
    #'   i.e. the conditional statement.
    stopifnot(pd_is_if(id))
    kids <- get_child_ids(pd, id)
    if (length(kids)<5) stop("inproper if statement")
    kids[[3L]]
}
#' @export
pd_get_if_branch_id <- 
function(id = pd$id, pd = get('pd', parent.frame())){
    #' @title Get branch of if statment.
    #' @inheritParams pd_is_if
    #' @description
    #'   Returns the id of the body of the branch executed if the predicate 
    #'   evaluates to true.
    stopifnot(pd_is_if(id))
    kids <- get_child_ids(pd, id)
    if (length(kids)<5) stop("inproper if statement")
    branch.id <- kids[[5L]]
    #TODO fix when a comment is in the way.
    #' @return an id integer.
}
#' @export
pd_get_if_alternate_id <- 
function(id = pd$id, pd = get('pd', parent.frame())){
    #' @title Get the alternate branch of if statement
    #' @inheritParams pd_is_if
    #' @description
    #'   Gets the id of the alternate branch, i.e. the else branch.
    stopifnot(pd_is_if(id))
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
    "}))
    id <- all_root_ids(pd) # 33
    
    expect_true(pd_is_if(id))
    expect_equal(pd_get_if_predicate_id(id, pd),  7L)
    expect_equal(pd_get_if_branch_id   (id, pd), 18L)
    expect_equal(pd_get_if_alternate_id(id, pd), 30L)
}


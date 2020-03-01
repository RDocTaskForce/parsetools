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

#' @name if-statements
#' @title If Statement Nodes
#' @description
#' These function navigate logic statements.
#'
#' @details
#' If statements have the form of the following.
#' ```
#'     if (predicate) branch else alternate
#' ```
#' The `predicate` refers to the logical test being performed.
#' The `branch` is the statement or block that is executed if `predicate` evaluates true.
#' The `alternate` is the statement of block that is executed if `predicate` returns false.
#'
#' @inheritParams pd_get_children_ids
#' @example inst/examples/example-pd.R
#' @example inst/examples/example-roots.R
#' @example inst/examples/example-if.R
NULL


#' @describeIn if-statements Is node an if expression.
pd_is_if <-
function(id, pd, .check=TRUE){
    if (.check) {
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    if (length(id)>1) return(sapply(id, pd_is_if, pd=pd)) #nocov
    (token(id) == 'expr') &&
    (token(firstborn(id)) == 'IF')
}
is_if <- internal(pd_is_if)
all_if_ids <- make_get_all(pd_is_if)

#' @describeIn if-statements Get the predicate node.
pd_get_if_predicate_id <-
function(id, pd, .check=TRUE){
    #' @title Get if predicate id
    #' @inheritParams pd_is_if
    #' @description
    #'   Returns the id of the predicate of the if statement,
    #'   i.e. the conditional statement.
    if (.check) {
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot(all(is_if(id,pd)))
    }
    kids <- children(id, pd)
    if (length(kids)<5) stop("inproper if statement") #nocov
    kids[[3L]]
}
if_predicate <- internal(pd_get_if_predicate_id, all_if_ids(pd))

#' @describeIn if-statements Get the `branch` statement or block node.
pd_get_if_branch_id <-
function(id, pd, .check=TRUE){
    #' @title Get branch of if statement.
    #' @inheritParams pd_is_if
    #' @description
    #'   Returns the id of the body of the branch executed if the predicate
    #'   evaluates to true.
    if (.check) {
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot(all(is_if(id,pd)))
    }
    kids <- children(id, pd)
    if (length(kids)<5) stop("inproper if statement") #nocov
    branch.id <- kids[[5L]]
    #TODO fix when a comment is in the way.
    #' @return an id integer.
}
if_branch <- internal(pd_get_if_branch_id, all_if_ids(pd))

#' @describeIn if-statements Get the `alternate` statement or block node.
pd_get_if_alternate_id <-
function(id, pd, .check=TRUE){
    #' @title Get the alternate branch of if statement
    #' @inheritParams pd_is_if
    #' @description
    #'   Gets the id of the alternate branch, i.e. the else branch.
    if (.check) {
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot(all(is_if(id,pd)))
    }
    kids <- children(id, pd)
    if (length(kids)<7 || token(kids[[6]]) != 'ELSE')
        stop("inproper if-else statement") #nocov
    kids[[7L]]
    #' @return an id integer.
}
if_alternate <- internal(pd_get_if_alternate_id, all_if_ids(pd))

if(FALSE){#!@testing if structures
    pd <- get_parse_data(parse(text={"
        if(predicate){
            body
        } else {
            alternate
        }
    "}, keep.source=TRUE))
    id <- roots(pd) # 33

    expect_true(pd_is_if(id,pd))
    expect_equal(pd_get_if_predicate_id(id, pd), parent(.find_text('predicate')))
    expect_equal(pd_get_if_branch_id   (id, pd), parent(parent(.find_text('body'))))
    expect_equal(pd_get_if_alternate_id(id, pd), parent(parent(.find_text('alternate'))))
}


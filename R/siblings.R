# siblings.R ##########################################################
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


## Siblings ####
#' @title Navigate siblings
#' @description
#' These functions help to navigate siblings, nodes with the same parent.
#'
#' @inheritParams pd_get_children_ids
#'
NULL

#' @describeIn family-nodes Identify siblings of `id`.
pd_get_sibling_ids <- function(id, pd, .check=TRUE){
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot(length(id) == 1)
    }
    children(parent(id, pd), pd)
}
siblings <- internal(pd_get_sibling_ids)


#' @describeIn family-nodes Get the next younger sibling.
pd_get_next_sibling_id <-
function(id, pd, .check=TRUE){
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    if (length(id) > 1L) return(sapply(id, pd_get_next_sibling_id, pd=pd, .check=FALSE))
    sids <- siblings(id, pd)
    . <- which(sids>id)
    if (length(.)) min(sids[.]) else NA_integer_
}
next_sibling <- internal(pd_get_next_sibling_id)
if(FALSE){#@testing
    pd <- get_parse_data(parse(text='a+b', keep.source = TRUE))
    id <- parent(.find_text('a'))
    expect_equal( pd_get_next_sibling_id(id,pd)
                , parent(.find_text('b'))
                )
    expect_identical( pd_get_next_sibling_id(.find_text('a', pd), pd), NA_integer_)
    expect_identical( pd_get_next_sibling_id(.find_text('+', pd), pd)
                    , parent(.find_text('a', pd))
                    )
    expect_length(pd_get_next_sibling_id(pd$id, pd), nrow(pd))
    expect_error(pd_get_next_sibling_id(1e9L, pd))
    expect_error(pd_get_next_sibling_id(id, id))
}


#' @describeIn family-nodes Get the next older sibling.
pd_get_prev_sibling_id <- function(id, pd, .check=TRUE){
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    if (length(id) > 1L) return(sapply(id, pd_get_prev_sibling_id, pd=pd, .check=FALSE))
    sibs <- siblings(id, pd)
    . <- which(sibs<id)
    if (length(.)) max(sibs[.]) else NA_integer_
}
prev_sibling <- internal(pd_get_prev_sibling_id)
if(FALSE){#@testing
    pd <- get_parse_data(parse(text='a+b', keep.source = TRUE))
    id <- parent(.find_text('b'))
    expect_equal( pd_get_prev_sibling_id(id,pd)
                , parent(.find_text('a'))
                )
    expect_identical( pd_get_prev_sibling_id(.find_text('b', pd), pd), NA_integer_)
    expect_identical( pd_get_prev_sibling_id(parent(.find_text('a', pd)), pd)
                    , .find_text('+', pd))
    expect_length(pd_get_prev_sibling_id(pd$id, pd), nrow(pd))
    expect_error(pd_get_prev_sibling_id(1e9L, pd))
    expect_error(pd_get_prev_sibling_id(id, id))
}

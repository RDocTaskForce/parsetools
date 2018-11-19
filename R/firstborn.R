# firstborn.R #########################################################
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

#' @describeIn nodes Test if `id` is firstborn.
pd_is_firstborn <- function(id, pd, .check=TRUE){
    #' @inheritParams pd_get_children_ids
    #' @description
    #'   Test if an expression is the firstborn, i.e. oldest or lowest id.
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    id == firstborn(parent(id, pd), pd)
}


#' @describein nodes Get the firstborn child of `id`.
pd_get_firstborn <-
function(id, pd, .check=TRUE){
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    if (length(id) > 1L) return(sapply(id, pd_get_firstborn, pd=pd))
    kids <- children(id=id, pd=pd)
    if (length(kids)==0 ) return(NA_integer_)
    else min(kids)
}
#@internal
firstborn <- internal(pd_get_firstborn)
if(FALSE){#@testing
    pd <- get_parse_data(parse(text='a+b', keep.source = TRUE))
    fb <- pd_get_firstborn(roots(pd), pd)
    expect_identical(token(fb), "'+'")
    expect_true(pd_is_firstborn(fb, pd))
    expect_true(pd_is_firstborn(roots(pd), pd))
    expect_false(pd_is_firstborn(next_sibling(fb), pd))

    expect_true(fb %in% siblings(fb,pd))
    expect_length(siblings(fb,pd), 3L)
    expect_equal(sum(pd_is_firstborn(siblings(fb,pd), pd)), 1L)
}

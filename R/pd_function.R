{#######################################################################
# pd_function.R
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
is_pd_function <-
function( pd #< a [parse-data] object
        , id = all_root_ids(pd)
        ){
    #' @title test if a function
    #' @inheritParams is_pd_assignment
    #' @description
    #'   Test if the \code{id} points to a function.
    #'   
    if (length(id) > 1) sapply(id, is_pd_function, pd=pd)
    kids.pd <- get_child(id=id, pd, ngenerations=1, FALSE)
    kids.pd[1, 'token'] == 'FUNCTION'
    #' @return a logical vector, same length as \code{id}.
}
if(F){#! @testthat is_pd_function
    pd <- get_parse_data(parse(text="function(){}", keep.source=TRUE))
    expect_true(is_pd_function(pd))

    pd <- get_parse_data(parse(text="fun <- function(){}", keep.source=TRUE))
    expect_false(is_pd_function(pd))
}

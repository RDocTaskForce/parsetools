{#######################################################################
# checks.R
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

._check_id <- function(id){
    #! Verify and/or extract id is valid.
    if (is.data.frame(id)){
        stopifnot(inherits(id, 'parse-data'))
        id <- id[['id']]
    }
    if (is.numeric(id) && !is.integer(id)) 
        id <- as.integer(id)
    stopifnot(is.integer(id))
    return(invisible(id))
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    
    expect_identical(._check_id(pd), pd$id, info="Passing parse-data")
    expect_error(._check_id(iris)         , info="data.frame but not parse-data")
    expect_identical(._check_id(1)  , 1L  , info="convert numeric to integer")
    expect_identical(._check_id(1.1), 1L  , info="convert numeric to integer")
    expect_error(._check_id(TRUE)         , info="passing logical that cannot be converted.")
}

._check_parse_data <- function(pd){
    #! check_validity of parse_data
    if(inherits(pd, 'parse-data')) return(pd)
    else if(inherits(pd, 'data.frame')){
        as_parse_data(pd)
    } else stop("Cannot convert to parse-data.")
}


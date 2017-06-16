{#######################################################################
# accessors.R
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

#@internal 
token <- function(id=pd$id, pd=get('pd', parent.frame())){
    pd[match(id, pd$id), 'token']
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    expect_equal(token(), pd$token)
    expect_equal(token(c(45,3, 58), pd), c("SYMBOL_FUNCTION_CALL", "SYMBOL", "expr"))
}

#@internal
text <- function(id=pd$id, pd=get('pd', parent.frame())){
    pd[match(id, pd$id), 'text']
}
if(FALSE){#!@testing 
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    expect_equal(text(pd$id, pd), pd$text)
    expect_equal(text(), pd$text)
    expect_equal(text(c(45,3, 58), pd), c("plot", "x", ""))
}

#@internal
nodes <- function(id, pd=get('pd', parent.frame())){
    pd[match(id, pd$id), ]
}
if(FALSE){#!@testing
        pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    expect_equal(nodes(pd$id, pd), pd)
    expect_equal(nodes(pd$id), pd)
    expect_equal(nodes(c(45,3, 58), pd), pd[c('45', '3', '58'), ])
    
}

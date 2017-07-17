{#######################################################################
# accessors.R
# This file is part of the R package `parsetools`.
#
# Author: Andrew Redd
# Copyright: 2017 The R Consortium
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

#@internal
start_line <- function(id, pd=get('pd', parent.frame())){
    pd[match(id, pd$id), 'line1']
}

#@internal
start_col <- function(id, pd=get('pd', parent.frame())){
    pd[match(id, pd$id), 'col1']
}

#@internal
end_line <- function(id, pd=get('pd', parent.frame())){
    pd[match(id, pd$id), 'line2']
}

#@internal
end_col <- function(id, pd=get('pd', parent.frame())){
    pd[match(id, pd$id), 'col2']
}

#@internal
filename <- function(pd){
    src            <- attr(pd, 'srcfile')
    if (!is.null(src)) src$filename else "<UNKNOWN>"
}

#@internal
lines <- function(id, pd=get('pd', parent.frame())){
    text <- utils::getParseText(pd, id)
    unlist(strsplit(text, '\n', fixed=TRUE))
}

#@internal 
is_terminal <- function(id, pd=get('pd', parent.frame())){
    pd[match(id, pd$id), 'terminal']
}

#@internal 
is_first_on_line <- function(id, pd=get('pd', parent.frame())){
    c(T, utils::head(pd$line2, -1) != utils::tail(pd$line1, -1)) [match(id, pd$id)]
}
if(FALSE){#@tesrting
"'

' -> a.multiline.string" %>% parse(text=.) %>% get_parse_data() -> pd

expect_true (is_first_on_line(1))
expect_false(is_first_on_line(2))

pd <-{
"function(x, y){
x+
y+
1
}
" %>% parse(text=.) %>% 
get_parse_data}

}

#@internal 
is_last_on_line <- function(id, pd=get('pd', parent.frame())){
    c(diff(pd$line1)!=0, T)[match(id, pd$id)]
}
if(FALSE){#@testing
#TODO
}

#@internal 
spans_multiple_lines <- function(id, pd=get('pd', parent.frame())){
    start_line(id) != end_line(id)
}
if(FALSE){#@testing
"'

' -> a.multiline.string" %>% parse(text=.) %>% get_parse_data() -> pd
expect_true(spans_multiple_lines(1, pd))
expect_false(spans_multiple_lines(4, pd))
expect_true(spans_multiple_lines(all_root_ids(pd), pd))
}

terminal_ids_on_line <- function(line, pd=get('pd', parent.frame())){
    pd$id[pd$line1 <= line & pd$line2 >= line & pd$terminal]
}
if(F){#@testing
"      {
         {1 + 3}
{2 + sin(pi)}
      }
" %>% parse(text=.) %>%
get_parse_data -> pd
expect_equal(terminal_ids_on_line(1), 1)
expect_equal(text(terminal_ids_on_line(2)), c('{', '1', '+', '3', '}'))

"'

' -> a.multiline.string" %>% parse(text=.) %>% get_parse_data() -> pd
expect_equal(text(terminal_ids_on_line(1, pd)), "'\n\n'")
expect_equal(terminal_ids_on_line(2, pd), 1)
expect_equal(terminal_ids_on_line(4, pd), integer(0))
}

ids_starting_on_line <- function(line, pd=get('pd', parent.frame())){
    pd$id[pd$line1 == line]
}
ids_ending_on_line <- function(line, pd=get('pd', parent.frame())){
    pd$id[pd$line2 == line]
}
if(FALSE){#@testing
pd <- {"((1+
2)+
3)+
4" %>% parse(text=.) %>%
get_parse_data}

expect_identical(ids_starting_on_line(1), head(pd$id, 10))
expect_identical(ids_starting_on_line(4), tail(pd$id, 2))
expect_identical(ids_ending_on_line(1), 1:5)
expect_identical(ids_ending_on_line(4), c(26L, 23L, 24L))

}

get_prev_terminal_id <- function(pd, id=pd$id){
    if (length(id)>1) return (sapply(id, get_prev_terminal_id, pd=pd))
    ix <- which( pd$line1 <= start_line(id)
               & pd$col1  <  start_col(id) 
               & pd$terminal
               )
    if (!any(ix)) return (NA_integer_)
    pd$id[max(ix)]
}
if(FALSE){#@testing
    pd <- "   rnorm( 10,  0,   3)" %>% parse(text=.) %>% get_parse_data()
    id <- 4
    expect_equal(get_prev_terminal_id(pd, id), 2L)

    expect_equal( get_prev_terminal_id(pd, pd$id)
                , c(NA, NA, NA, 1, rep(2, 2), 4, 6, 6, 9, 11, 11, 14)
                )
}



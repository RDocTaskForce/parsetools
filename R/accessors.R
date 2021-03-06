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
# Foundation, either version 2 of the License, or (at your option)
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
#' @include internal.R

#' @name internal
#' @title Internal Functions
#' @param pd the parse data.
#' @param id the ID of the expression
#' @param line a line number
#' @description These functions are for internal use but are documented
#' here for reference.
NULL

#@internal
token <- function(id=pd$id, pd=get('pd', parent.frame())){
#' @describeIn internal Extract the token
    pd[match(id, pd$id), 'token']
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}, keep.source=TRUE))
    expect_equal(token(), pd$token)
    ids <- pd$id[match(c('rnorm', 'x', '<-'), pd$text)]
    expect_equal( token(ids, pd)
                , c("SYMBOL_FUNCTION_CALL", "SYMBOL", "LEFT_ASSIGN"))
}

#@internal
text <- function(id=pd$id, pd=get('pd', parent.frame())){
#' @describeIn internal Extract the text
    pd[match(id, pd$id), 'text']
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}, keep.source=TRUE))
    text <- c('rnorm', 'x', '<-')
    ids <- pd$id[match(c('rnorm', 'x', '<-'), pd$text)]
    expect_equal(text(pd$id, pd), pd$text)
    expect_equal(text(ids), text)
    expect_equal(text(ids, pd), text)
}

#@internal
nodes <- function(id, pd=get('pd', parent.frame())){
#' @describeIn internal Extract only the specified node(s).
    pd[match(id, pd$id), ]
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}, keep.source=TRUE))
    expect_equal(nodes(pd$id, pd), pd)
    expect_equal(nodes(pd$id), pd)
    expect_equal(nodes(c(45,3, 58), pd), pd[c('45', '3', '58'), ])
}

#@internal
start_line <- function(id, pd=get('pd', parent.frame())){
#' @describeIn internal Get the line the expression starts on.
    pd[match(id, pd$id), 'line1']
}

#@internal
start_col <- function(id, pd=get('pd', parent.frame())){
#' @describeIn internal Get the column the expression starts on.
    pd[match(id, pd$id), 'col1']
}

#@internal
end_line <- function(id, pd=get('pd', parent.frame())){
#' @describeIn internal Get the line the expression ends on.
    pd[match(id, pd$id), 'line2']
}

#@internal
end_col <- function(id, pd=get('pd', parent.frame())){
#' @describeIn internal Get the column the expression ends on.
    pd[match(id, pd$id), 'col2']
}

#@internal
filename <- function(pd=get('pd', parent.frame())){
#' @describeIn internal Extract the filename if available, otherwise return "<UNKNOWN>".
    src            <- attr(pd, 'srcfile')
    if (!is.null(src)) src$filename else "<UNKNOWN>"
}
if(FALSE){#@test
    pd <- get_parse_data(parse(text="1+1"))
    expect_identical(filename(pd), "<text>")

    attr(pd, 'srcfile') <- NULL
    expect_identical(filename(pd), "<UNKNOWN>")
}


#@internal
lines <- function(id, pd=get('pd', parent.frame())){
#' @describeIn internal Extract the lines of text.
    text <- utils::getParseText(pd, id)
    unlist(strsplit(text, '\n', fixed=TRUE))
}

#@internal
is_terminal <- function(id, pd=get('pd', parent.frame())){
#' @describeIn internal does id represent a terminal node.
    pd[match(id, pd$id), 'terminal']
}

#@internal
is_first_on_line <- function(id, pd=get('pd', parent.frame())){
#' @describeIn internal is an expression the first one on a line?
    c(T, utils::head(pd$line2, -1) != utils::tail(pd$line1, -1)) [match(id, pd$id)]
}
if(FALSE){#@testing
pd <- get_parse_data(parse(text="'

' -> a.multiline.string", keep.source=TRUE))

expect_true (is_first_on_line(1))
expect_false(is_first_on_line(2))

pd <- get_parse_data(parse(text={
"function(x, y){
x+
y+
1
}
"}, keep.source=TRUE))


}

#@internal
is_last_on_line <- function(id, pd=get('pd', parent.frame())){
#' @describeIn internal Is expression the last terminal node on the line?
    if (!is_terminal(id, pd)) return(FALSE)
    max(pd[pd$line2 == end_line(id, pd), 'col2']) == end_col(id, pd)
}
if(FALSE){#@testing
pd <- get_parse_data(parse(text="'

' -> a.multiline.string", keep.source=TRUE))

expect_false(is_last_on_line(1, pd))
expect_true(is_last_on_line(4, pd))
expect_false(is_last_on_line(6, pd))
}

#@internal
spans_multiple_lines <- function(id, pd=get('pd', parent.frame())){
#' @describeIn internal does the expression span multiple lines?
    start_line(id) != end_line(id)
}
if(FALSE){#@testing
pd <- get_parse_data(parse(text="'

' -> a.multiline.string", keep.source=TRUE))
expect_true(spans_multiple_lines(1, pd))
expect_false(spans_multiple_lines(4, pd))
expect_true(spans_multiple_lines(pd_all_root_ids(pd), pd))
}

#@internal
terminal_ids_on_line <- function(line, pd=get('pd', parent.frame())){
#' @describeIn internal Get the ids on a given line that are terminal nodes.
    pd$id[pd$line1 <= line & pd$line2 >= line & pd$terminal]
}
if(F){#@testing
pd <- get_parse_data(parse(text="      {
         {1 + 3}
{2 + sin(pi)}
      }
", keep.source=TRUE))

expect_equal(terminal_ids_on_line(1), 1)
expect_equal(text(terminal_ids_on_line(2)), c('{', '1', '+', '3', '}'))

pd <- get_parse_data(parse(text="'

' -> a.multiline.string", keep.source=TRUE))
expect_equal(text(terminal_ids_on_line(1, pd)), "'\n\n'")
expect_equal(terminal_ids_on_line(2, pd), 1)
expect_equal(terminal_ids_on_line(4, pd), integer(0))
}

#@internal
ids_starting_on_line <- function(line, pd=get('pd', parent.frame())){
#' @describeIn internal Get ids for nodes that start on the given line
    pd$id[pd$line1 == line]
}

#@internal
ids_ending_on_line <- function(line, pd=get('pd', parent.frame())){
#' @describeIn internal Get ids for nodes that end on the given line
    pd$id[pd$line2 == line]
}
if(FALSE){#@testing
pd <- get_parse_data(parse(text={"((1+
2)+
3)+
4"}, keep.source=TRUE))

expect_identical(ids_starting_on_line(1), head(pd$id, 10))
expect_identical(ids_starting_on_line(4), tail(pd$id, 2))
expect_identical(ids_ending_on_line(1), 1:5)
expect_identical(ids_ending_on_line(4), c(26L, 23L, 24L))

}

#@internal
prev_terminal <- function(id=pd$id, pd=get('pd', parent.frame())){
#' @describeIn internal Get the id for the terminal expression that is immediately prior to the one given.
    if (length(id)>1) return (sapply(id, prev_terminal, pd=pd))
    ix <- which( pd$line1 <= start_line(id)
               & pd$col1  <  start_col(id)
               & pd$terminal
               )
    if (!any(ix)) return (NA_integer_)
    pd$id[max(ix)]
}
if(FALSE){#@testing
    pd <- get_parse_data(parse(text="   rnorm( 10,  0,   3)", keep.source=TRUE))
    ids <- pd$id[match(c('10', '(', 'rnorm'), pd$text)]
    id <- ids[[1]]
    expect_equal( prev_terminal(ids[[1]], pd), ids[[2]])
    expect_equal( prev_terminal(ids[[2]], pd), ids[[3]])
    expect_equal( prev_terminal(ids[[3]], pd), NA_integer_)
    expect_equal( prev_terminal(ids, pd=pd)
                , c(utils::tail(ids, -1), NA_integer_)
                )
}

#@internal
expr_text <- function(id, pd=get('pd', parent.frame())){
#' @describeIn internal
#' If id represents an `expr` token reiterate on the firstborn.
#' Throws an error if anything but an expression or text if found.
    if (length(id)>1L) return(sapply(id, expr_text, pd=pd))
    while (token(id) == 'expr' && n_children(id) == 1L)
        id <- firstborn(id)
    if (token(id) != 'STR_CONST')
        col_error(id, "a string constant is expected.")
    unquote(text(id))
}
if(FALSE){#@testing
    pd <- get_parse_data(parse(text="
        signature(x='hello', y='world')
    ", keep.source=TRUE))
    ids <- c( parent(.find_text("'hello'"))
            , parent(.find_text("'world'"))
            )
    expect_identical(expr_text(ids, pd), c("hello", "world"))
    expect_error( expr_text(pd_all_root_ids(pd))
                , "<text>:2:9:  a string constant is expected."
                )
}


#' @rdname accessors
#' @title Accessor functions
#'
#' @param pd the parse data.
#' @param id the ID of the expression
#' @description
#'   This collection of function can be used to easily access elements of
#'   the parse data information.
#'
#' @aliases pd_text pd_token pd_start_line pd_end_line pd_filename pd_start_col pd_end_col
pd_text <- external(text)
pd_token <- external(token)
pd_start_line <- external(start_line)
pd_end_line <- external(end_line)
pd_filename <- external(filename)
pd_start_col <- external(start_col)
pd_end_col <- external(end_col)

{#######################################################################
# root.R
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

#' @name root
#' @title Root IDs
#'
#' @description
#' Root IDs constitute the id of a stand alone expression.
#' That is one that is not contained inside of another call or expression.
#' The one exception to this is code blocks denoted by curly braces
#' that are not themselves part of another call or expression;
#' these we call code groups.
#' In definition, A root node is defined to be a node that either
#' has no parent or whose parent is a grouping node.
#'
#' @details
#' If `ignore.groups=TRUE` then groupings are ignored and root nodes within the
#' group are interpreted as roots, otherwise nodes within a group are not
#' interpreted as root.  Groupings are always interpreted as root if the
#' parent is 0 or if the parent is a group and also a root.
#'
#' @inheritParams pd_get_children_ids
#'
#' @aliases root root-nodes root-ids
#' @seealso see \code{\link{pd_is_grouping}} for details on what a grouping is.
list()

#' @export
pd_is_root <-
function( id, pd
        , ignore.groups = TRUE  #< Ignore groups? see details.
        , .check=TRUE
        ){
    #' @describeIn root Test if a node is a root node
    #' @param ignore.groups Should \link[=pd_is_grouping]{groupings} be ignored?
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    if (length(id) > 1) return(sapply(id, pd_is_root, pd=pd, ignore.groups=ignore.groups))
    if (!(id %in% pd$id)) stop("id not present in pd")
    if (pd[pd$id == id,'token'] != 'expr') return(FALSE)
    parent <- pd[pd$id == id,'parent']
    if (parent == 0 ) return(TRUE)
    if (ignore.groups && pd_is_grouping(parent, pd)) return(TRUE)
    return(FALSE)
}
if(FALSE){#! @testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))
    expect_true (pd_is_root(23, pd))
    expect_false(pd_is_root( 1, pd))
    expect_equal(sum(pd_is_root(pd=pd)), 1)


    pd <- get_parse_data(parse(text={'{
        x <- rnorm(10, mean=0, sd=1)
        y <- runif(10)
        plot(x,y)
    }'}, keep.source=TRUE))
    expect_true(pd_is_root(68, pd), info="Grouping root")
    expect_true(pd_is_root(30, pd), info="Root within grouping.")
    expect_equal(sum(pd_is_root(pd=pd)), 4)
    expect_equal(sum(pd_is_root(c(68, 30, 46, 62), pd)), 4)
    expect_false(pd_is_root(66, pd))

    expect_equal(sum(pd_is_root(pd$id, pd, ignore.groups=FALSE)), 1)
    expect_error(pd_is_root(0L, pd))

    pd[pd$parent %in% c(0,68) & pd$token == 'expr', ]
    expect_false(pd_is_root(30, pd, ignore.groups = FALSE))
    expect_equal(pd_is_root(c(68, 30), pd, ignore.groups = FALSE), c(TRUE, FALSE))

    pd <- get_parse_data(parse(text={"
        # a comment outside the grouping
        {# A grouping
        #' An Roxygen Comment
        hw <- function(){
            {# Another Grouping
             # but not a root since it is burried within a function
             1+2 #< an expression that is not a root.
            }
            3+4 #< also not a root
        }
        4+5 #< this is a root expression
        }
        6+7 #< a regular root expression
    "}, keep.source=TRUE))
    id <- max(pd[pd$token =="'{'", 'parent'])
    expect_true(pd_is_root(id, pd, ignore.groups = TRUE))
    id <- min(pd[pd$token =="'{'", 'parent'])
    expect_equal(get_family_pd(id, pd)[3,'text'], "# Another Grouping")

    ids <- pd[pd$token =="'{'", 'parent']
    expect_equal(pd_is_root(ids, pd, ignore.groups = TRUE ), c(TRUE, FALSE, FALSE))
    expect_equal(pd_is_root(ids, pd, ignore.groups = FALSE), c(TRUE, FALSE, FALSE))

    pd <- get_parse_data(parse(text="
        # a comment
        an_expression()
    ", keep.source=TRUE))
    expect_false(pd_is_root(pd[1,'id'], pd))
}


.excluded.root.tokens <- c("'{'", "'}'", comment.classes$class, "NORMAL_COMMENT")
#' @export
all_root_ids <-
function( pd                    #< parse data from `<get_parse_data>`
        , include.groups = TRUE #< Include groups as root nodes (T)
                                #^ or descend into [groups](is.grouped) for roots?
        ){
    #' @describeIn root give all root ids in `pd`
    #' @param include.groups Include groups as root nodes (T)
    #'                       or descend into [groups](is.grouped) for roots?
    roots <- pd[ !(abs(pd$parent) %in% pd$id                )
               & !(    pd$token   %in% .excluded.root.tokens)
               , 'id']
    while (!include.groups && any(. <- pd_is_grouping(roots, pd))) {
        groups <- roots[.]
        sub.ids <-
            pd[ pd$parent %in% groups
              & !(pd$token %in% .excluded.root.tokens)
              , 'id']
        roots <- sort(c(roots[!.], sub.ids))
    }
    return(roots)
}
if(F){#!@testthat all_root_ids
    pd <- get_parse_data(parse(text={"a <- 1
        {# section 1
        b <- 2
        {# section 2
        c <- 3
        }# end of section 1
        d <- 4
        }# end of section 2
        e <- 5
    "}, keep.source=TRUE))
    expect_equal(all_root_ids(pd, TRUE), c(7, 52, 63))

    roots <- all_root_ids(pd, FALSE)
    expect_equal(roots, c(7, 19, 31, 47, 63))
    expect_equal(getParseText(pd, roots), c('a <- 1','b <- 2', 'c <- 3', 'd <- 4', 'e <- 5'))

    pd <- get_parse_data(parse(text="
        # a comment
        an_expression()
    ", keep.source=TRUE))
    expect_equal( all_root_ids(pd), -pd[1,'parent'])

    pd <- utils::getParseData(parse(text={"
    {# grouped code
        # normal comment
        #' Documenation before
        hw <- function(){
            #! documentation comment inside.
            print('hello world')
        }
    }
    {#Second Group
        1+2
    }
    # Comment 3
    4+5
    "}, keep.source=TRUE))
    id <- all_root_ids(pd)
    expect_equal(id, c(43, 61, 74))
}

all_root_nodes <-
function( pd                    #< parse data from `<get_parse_data>`
        , include.groups = TRUE #< descend into grouped code \code{\{\}}?
        ){
    #' @title Find all root node from parse data
    #' @inheritParams pd_get_children_ids
    #' @param include.groups descend into grouped code \code{\{\}}?
    #'
    #' @description
    #'   A root node in a file is a standalone expression, such as in
    #'   source file a function definition.
    #'   when discussing a subset it is any expression that does not have
    #'   a parent in the subset.
    pd[pd$id %in% all_root_ids(pd, include.groups=include.groups), ]
    #' @return \code{\link{parse-data}} with for the root nodes.
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"a <- 1
        {# section 1
        b <- 2
        {# section 2
        c <- 3
        }# end of section 1
        d <- 4
        }# end of section 2
        e <- 5
    "}, keep.source=TRUE))
    expect_equal(all_root_nodes(pd, TRUE)$id   , c(7, 52, 63))
    expect_equal(all_root_nodes(pd, TRUE)$line1, c(1,  2,  9))

    expect_equal(all_root_nodes(pd, FALSE)$id   , c(7, 19, 31, 47, 63))
    expect_equal(all_root_nodes(pd, FALSE)$line1, c(1,  3,  5,  7,  9))
}

#@internal
ascend_to_root <-
function( id = pd$id
        , pd = get('pd', parent.frame())
        , ignore.groups=TRUE    #< Ignore groups? see <pd_is_root>.
        , .check=TRUE
        ) {
    #' @describeIn root ascend from id to root
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    if (length(id) > 1L) return(sapply(id, ascend_to_root, pd=pd, ignore.groups=ignore.groups))
    while (TRUE) {
        if (is.na(id) || id == 0) return(0L)
        if (id < 0) id <- -id
        if (pd_is_root(id, pd, ignore.groups=ignore.groups)) return(id)
        id <- parent(id)
    }
}
if(FALSE){#@testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))
    expect_equal(ascend_to_root(id=23, pd), 23)
    expect_equal(ascend_to_root(id=1 , pd), 23)
    expect_identical(ascend_to_root(id=0, pd), 0L)

    pd <- get_parse_data(parse(text={"
        #' hello world
        hw <- function(){
            #! title
            print('hello world!')
        }
        #' comment after
    "}, keep.source=TRUE))
    expect_equal(ascend_to_root(3, pd), 34)

    expect_equal(ascend_to_root(pd=pd), c(rep(34, 20), 0))
}

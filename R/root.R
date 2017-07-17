{#######################################################################
# is_root.R
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

#' @export
is_root <- 
function( pd, id = pd$id
        , ignore.groups = TRUE  #< Ignore groups? see details.
        ){
    #' @title Test if a node is a root node
    #' @inheritParams get_child_ids
    #' @param ignore.groups Should \link[=is_grouping]{groupings} be ignored?
    #' @description
    #' A root node is defined to be a node that either has no parent
    #' or whose parent is a grouping node.
    id <- ._check_id(id)
    if (length(id) > 1) return(sapply(id, FUN=is_root, pd=pd, ignore.groups=ignore.groups))
    if (!(id %in% pd$id)) stop("id not present in pd")
    if (pd[pd$id == id,'token'] != 'expr') return(FALSE)
    parent <- pd[pd$id == id,'parent']
    if (parent == 0 ) return(TRUE)
    if (ignore.groups && is_grouping(pd, parent)) return(TRUE)
    #' @details 
    #' If `ignore.groups=TRUE` then groupings are ignored and root nodes within the 
    #' group are interpreted as roots, otherwise nodes within a group are not interpreted as root.
    #' Groupings are always interpreted as root if the parent is 0 or if the parent is a group and also 
    #' a root.
    return(FALSE)
    #' @return a logical vector of same length as \code{id}
}
if(FALSE){#! @testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    expect_true (is_root(pd, 23))
    expect_false(is_root(pd,  1))
    expect_equal(sum(is_root(pd)), 1)

    
    pd <- get_parse_data(parse(text={'{
        x <- rnorm(10, mean=0, sd=1)
        y <- runif(10)
        plot(x,y)
    }'}))
    expect_true(is_root(pd, 68), info="Grouping root")
    expect_true(is_root(pd, 30), info="Root within grouping.")
    expect_equal(sum(is_root(pd)), 4)
    expect_equal(sum(is_root(pd, c(68, 30, 46, 62))), 4)
    expect_false(is_root(pd, 66))
    
    
    expect_equal(sum(is_root(pd, pd$id, ignore.groups=FALSE)), 1)
    expect_error(is_root(pd, 0L))
    
    pd[pd$parent %in% c(0,68) & pd$token == 'expr', ]
    expect_false(is_root(pd, 30, ignore.groups = FALSE))
    expect_equal(is_root(pd, c(68, 30), ignore.groups = FALSE), c(TRUE, FALSE))
    
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
    "}))
    id <- max(pd[pd$token =="'{'", 'parent'])
    expect_true(is_root(pd, id, ignore.groups = TRUE))
    id <- min(pd[pd$token =="'{'", 'parent'])
    expect_equal(get_family(pd, id)[3,'text'], "# Another Grouping")
    
    ids <- pd[pd$token =="'{'", 'parent']
    expect_equal(is_root(pd, ids, ignore.groups = TRUE ), c(TRUE, FALSE, FALSE))
    expect_equal(is_root(pd, ids, ignore.groups = FALSE), c(TRUE, FALSE, FALSE))
    
    pd <- get_parse_data(parse(text="
        # a comment
        an_expression()
    "))
    expect_false(is_root(pd, pd[1,'id']))
}


.excluded.root.tokens <- c("'{'", "'}'", comment.classes$class, "NORMAL_COMMENT")
#' @export
all_root_ids <-
function( pd                    #< parse data from `<get_parse_data>`
        , include.groups = TRUE #< Include groups as root nodes (T)
                                #^ or descend into [groups](is.grouped) for roots?
        ){
    #! give the root ids in `pd`
    roots <- pd[ !(abs(pd$parent) %in% pd$id                )
               & !(    pd$token   %in% .excluded.root.tokens)
               , 'id']
    while (!include.groups && any(. <- is_grouping(pd, roots))) {
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
    "}))
    expect_equal(all_root_ids(pd, TRUE), c(7, 52, 63))
    
    roots <- all_root_ids(pd, FALSE)
    expect_equal(roots, c(7, 19, 31, 47, 63))
    expect_equal(getParseText(pd, roots), c('a <- 1','b <- 2', 'c <- 3', 'd <- 4', 'e <- 5'))
    
    pd <- get_parse_data(parse(text="
        # a comment
        an_expression()
    "))
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
    "}))
    id <- all_root_ids(pd)
    expect_equal(id, c(43, 61, 74))
}

#' @export
all_root_nodes <-
function( pd                    #< parse data from `<get_parse_data>`
        , include.groups = TRUE #< descend into grouped code \code{\{\}}?
        ){
    #! Find all root node from parse data
    #!
    #! A root node in a file is a standalone expression, such as in
    #! source file a function definition.
    #! when discussing a subset it is any expression that does not have
    #! a parent in the subset.
    pd[pd$id %in% all_root_ids(pd, include.groups=include.groups), ]
    #! @return parse data with for the root nodes.
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
    "}))
    expect_equal(all_root_nodes(pd, TRUE)$id   , c(7, 52, 63))
    expect_equal(all_root_nodes(pd, TRUE)$line1, c(1,  2,  9))

    expect_equal(all_root_nodes(pd, FALSE)$id   , c(7, 19, 31, 47, 63))
    expect_equal(all_root_nodes(pd, FALSE)$line1, c(1,  3,  5,  7,  9))
}

#' @export
ascend_to_root <-
function( pd, id = pd$id
        , ignore.groups=TRUE    #< Ignore groups? see <is_root>.
        ) {
    id <- ._check_id(id)
    if (length(id) > 1) return(sapply(id, ascend_to_root, pd=pd, ignore.groups=ignore.groups))
    parent <- id
    while (TRUE) {
        if (is.na(parent) || parent == 0) return(0)
        if (parent < 0) parent <- -parent
        if (is_root(pd, parent, ignore.groups=ignore.groups)) return(parent)
        parent <- get_parent_id(pd, parent)
    }
    #! @return the root id for the row.
}
if(FALSE){#@testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    expect_equal(ascend_to_root(pd, id=23), 23)
    expect_equal(ascend_to_root(pd, id=1), 23)
    expect_identical(ascend_to_root(pd, id=0), 0L)
    
    pd <- get_parse_data(parse(text={"
        #' hello world
        hw <- function(){
            #! title
            print('hello world!')
        }
        #' comment after
    "}))
    expect_equal(ascend_to_root(pd, 3), 34)
    
    expect_equal(ascend_to_root(pd), c(rep(34, 20), 0))
}

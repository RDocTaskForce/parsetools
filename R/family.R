{#######################################################################
# family.R
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

#' @include internal.R

#' @export
get_family <-
function( id, pd
        , include.self = TRUE
        , ngenerations = Inf
        , ...
        , include.doc.comments     = TRUE
        , include.regular.comments = FALSE
        ){
    #' @name get_family
    #' @title Get family of nodes.
    #' @inheritParams get_children_ids
    #' @param ...                       currently ignored.
    #' @param include.doc.comments      include associated documentation comments.
    #' @param include.regular.comments  include associated regular comments.
    #' @description
    #'   Subset the \code{pd} to the family of \code{id}.
    id <- ._check_id(id)
    kids <- children(id, pd, include.self=include.self, ngenerations=ngenerations, ...)
    cids <-
        if (include.doc.comments || include.regular.comments){
            if (pd_is_grouping(parent <- get_parent_id(id, pd), pd)) {
                pd <- fix_grouping_comment_association(parent, pd)
            }
            pd[ pd$token %in% c( if (include.doc.comments    ) comment.classes$class
                               , if (include.regular.comments) 'NORMAL_COMMENT'
                               )
              & pd$parent == -id
              , 'id']
        }
    pd[pd$id %in% c(kids, cids), ]
    #' @return a subset of the \code{\link{parse-data}} \code{pd}.
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
    id <- ascend_to_root(pd[pd$text == 'c','id'], pd)
    expect_identical(get_family(id, pd), pd[19:24,])

    pd <- get_parse_data(parse(text={"
        # normal comment
        #' Documenation before
        hw <- function(){
            #! documentation comment inside.
            print('hello world')
        }
    "}, keep.source=TRUE))
    fam <- get_family(37, pd, include.doc.comments=TRUE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")
    fam <- get_family(37, pd, include.doc.comments=TRUE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], "#' Documenation before")
    fam <- get_family(37, pd, include.doc.comments=FALSE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")
    fam <- get_family(37, pd, include.doc.comments=FALSE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], "hw")

    pd <- get_parse_data(parse(text={"
    #demonstration of grouped code.
    {
        # normal comment
        #' Documenation before
        hw <- function(){
            #! documentation comment inside.
            print('hello world')
        }
    }"}, keep.source=TRUE))
    group.id <- all_root_ids(pd)
    expect_true(pd_is_grouping(group.id, pd))
    id <- expr.id <- all_root_ids(pd, FALSE)

    fam <- get_family(expr.id, pd, include.doc.comments=FALSE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], 'hw')
    fam <- get_family(expr.id, pd, include.doc.comments=TRUE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], "#' Documenation before")
    fam <- get_family(expr.id, pd, include.doc.comments=TRUE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")


}

#' @export
get_sibling_ids <- function(id, pd=get('pd', parent.frame())){
    #' @title Identify siblings.
    #' @inheritParams get_children_ids
    #' @description \subsection{get_sibling_ids}{
    #'   A convenience function for identifying siblings of the given id.
    #'   Siblings are nodes with the same parent.
    #' }
    if (length(id) > 1) return(lapply(id, get_sibling_ids, pd=pd))
    children(parent(id, pd), pd)
}
siblings <- internal(get_sibling_ids)

#' @export
get_next_sibling_id <- function(id, pd=get('pd', parent.frame())){
    #' @rdname get_sibling_ids
    #' @description \subsection{get_next_sibling_id}{
    #'   gives the id of the next youngest sibling of the current id.
    #' }
    sids <- get_sibling_ids(id, pd)
    . <- which(sids>id)
    if (length(.)) sids[min(.)] else NA_integer_
}
next_sibling <- internal(get_next_sibling_id)

#' @export
get_prev_sibling_id <- function(id, pd=get('pd', parent.frame())){
    #' @rdname get_sibling_ids
    #' @description \subsection{get_prev_sibling_id}{
    #'   gives the id of the next older sibling of the current id.
    #' }
    sids <- get_sibling_ids(id, pd)
    . <- which(sids<id)
    if (length(.)) sids[max(.)] else NA_integer_
}
prev_sibling <- internal(get_prev_sibling_id)

#' @export
#' @title Test if id is the firstborn.
pd_is_firstborn <- function(id, pd=get('pd', parent.frame())){
    #' @inheritParams get_children_ids
    #' @description
    #'   Test if an expression is the firstborn, ie. oldest or lowest id.
    id == get_firstborn_id(get_parent_id(id, pd), pd)
}

#' @export
#' @title get the firstborn child.
get_firstborn_id <-
function(id=all_root_ids(pd), pd=get('pd', parent.frame())){
    #' @inheritParams get_children_ids
    #' @description
    #'   Get the id of the firstborn child of id.
    #'   Without the "_id" is a wrapper for giving the nodes.
    id <- ._check_id(id)
    if (length(id) > 1L) return(sapply(id, get_firstborn_id, pd=pd))
    kids <- children(id=id, pd=pd)
    if (length(kids)==0 ) return(NA_integer_)
    else min(kids)
}
#' @internal
firstborn <- internal(get_firstborn_id)
if(F){#@testing


}

#' @export
#' @rdname get_firstborn_id
get_firstborn <-
function(id=all_root_ids(pd), pd=get('pd', parent.frame()))
    nodes(get_firstborn_id(id, pd))

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
    expect_equal(get_firstborn(52, pd)$token, "'{'")
    expect_equal(get_firstborn(7 , pd)$text, "<-")

    expect_identical( suppressWarnings(get_firstborn_id(c(7, 52, .Machine$integer.max), pd))
                    , c(2L, 10L, NA_integer_))

    expect_true(pd_is_firstborn(2, pd=pd))
    expect_identical(suppressWarnings(pd_is_firstborn(-1,pd)), NA)
    expect_identical(pd_is_firstborn(c(1,3,7),pd), c(TRUE, FALSE, TRUE))
}


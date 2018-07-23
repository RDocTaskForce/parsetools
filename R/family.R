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

get_family_pd <-
function( id, pd
        , include.self = TRUE
        , ngenerations = Inf
        , ...
        , include.doc.comments     = TRUE
        , include.regular.comments = FALSE
        ){
    #' @name get_family_pd
    #' @title Get family of nodes.
    #' @inheritParams pd_get_children_ids
    #' @param ...                       currently ignored.
    #' @param include.doc.comments      include associated documentation comments.
    #' @param include.regular.comments  include associated regular comments.
    #' @description
    #'   Subset the \code{pd} to the family of \code{id}.
    id <- ._check_id(id)
    kids <- children(id, pd, include.self=include.self, ngenerations=ngenerations, ...)
    cids <-
        if (include.doc.comments || include.regular.comments){
            if (!is_root(id, ignore.groups=FALSE) && is_grouping(parent(id))) {
                pd <- fix_grouping_comment_association(parent(id), pd, .check=FALSE)
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
if(FALSE){#@testing
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
    expect_identical(get_family_pd(id, pd), pd[19:24,])

    pd <- get_parse_data(parse(text={"
        # normal comment
        #' Documenation before
        hw <- function(){
            #! documentation comment inside.
            print('hello world')
        }
    "}, keep.source=TRUE))
    fam <- get_family_pd(37, pd, include.doc.comments=TRUE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")
    fam <- get_family_pd(37, pd, include.doc.comments=TRUE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], "#' Documenation before")
    fam <- get_family_pd(37, pd, include.doc.comments=FALSE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")
    fam <- get_family_pd(37, pd, include.doc.comments=FALSE, include.regular.comments=FALSE)
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
    group.id <- roots(pd)
    expect_true(pd_is_grouping(group.id, pd))
    id <- expr.id <- roots(pd, FALSE)

    fam <- get_family_pd(expr.id, pd, include.doc.comments=FALSE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], 'hw')
    fam <- get_family_pd(expr.id, pd, include.doc.comments=TRUE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], "#' Documenation before")
    fam <- get_family_pd(expr.id, pd, include.doc.comments=TRUE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")


}


pd_get_sibling_ids <- function(id, pd, .check=TRUE){
    #' @title Identify siblings.
    #' @inheritParams pd_get_children_ids
    #' @description \subsection{pd_get_sibling_ids}{
    #'   A convenience function for identifying siblings of the given id.
    #'   Siblings are nodes with the same parent.
    #' }
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot(length(id) == 1)
    }
    children(parent(id, pd), pd)
}
siblings <- internal(pd_get_sibling_ids)


pd_get_next_sibling_id <-
function(id, pd, .check=TRUE){
    #' @rdname pd_get_sibling_ids
    #' @description \subsection{pd_get_next_sibling_id}{
    #'   gives the id of the next youngest sibling of the current id.
    #' }
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



pd_get_prev_sibling_id <- function(id, pd, .check=TRUE){
    #' @rdname pd_get_sibling_ids
    #' @description \subsection{pd_get_prev_sibling_id}{
    #'   gives the id of the next older sibling of the current id.
    #' }
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


#' @title Test if id is the firstborn.
pd_is_firstborn <- function(id, pd, .check=TRUE){
    #' @inheritParams pd_get_children_ids
    #' @description
    #'   Test if an expression is the firstborn, ie. oldest or lowest id.
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    id == firstborn(parent(id, pd), pd)
}


#' @title get the firstborn child.
pd_get_firstborn_id <-
function(id, pd, .check=TRUE){
    #' @inheritParams pd_get_children_ids
    #' @description
    #'   Get the id of the firstborn child of id.
    #'   Without the "_id" is a wrapper for giving the nodes.
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    if (length(id) > 1L) return(sapply(id, pd_get_firstborn_id, pd=pd))
    kids <- children(id=id, pd=pd)
    if (length(kids)==0 ) return(NA_integer_)
    else min(kids)
}
#@internal
firstborn <- internal(pd_get_firstborn_id)


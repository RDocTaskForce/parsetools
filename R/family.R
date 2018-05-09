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


#' @export
get_family <-
function( pd, id
        , include.self = TRUE
        , ngenerations = Inf
        , ...
        , include.doc.comments     = TRUE
        , include.regular.comments = FALSE
        ){
    #' @name get_family
    #' @title Get family of nodes.
    #' @inheritParams get_child_ids
    #' @param ...                       currently ignored. 
    #' @param include.doc.comments      include associated documentation comments.
    #' @param include.regular.comments  include associated regular comments.
    #' @description
    #'   Subset the \code{pd} to the family of \code{id}.
    id <- ._check_id(id)
    kids <- get_child_ids(pd, id, include.self=include.self, ngenerations=ngenerations, ...)
    cids <- 
        if (include.doc.comments || include.regular.comments){
            if (is_grouping(pd, parent <- get_parent_id(pd, id))) {
                pd <- fix_grouping_comment_association(pd, parent)
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
    id <- ascend_to_root(pd, pd[pd$text == 'c','id'])
    expect_identical(get_family(pd, id), pd[19:24,])
    
    pd <- get_parse_data(parse(text={"
        # normal comment
        #' Documenation before
        hw <- function(){
            #! documentation comment inside.
            print('hello world')
        }
    "}, keep.source=TRUE))
    fam <- get_family(pd, 37, include.doc.comments=TRUE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")
    fam <- get_family(pd, 37, include.doc.comments=TRUE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], "#' Documenation before")
    fam <- get_family(pd, 37, include.doc.comments=FALSE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")
    fam <- get_family(pd, 37, include.doc.comments=FALSE, include.regular.comments=FALSE)
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
    expect_true(is_grouping(pd, group.id))
    id <- expr.id <- all_root_ids(pd, FALSE)
    
    fam <- get_family(pd, expr.id, include.doc.comments=FALSE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], 'hw')
    fam <- get_family(pd, expr.id, include.doc.comments=TRUE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], "#' Documenation before")
    fam <- get_family(pd, expr.id, include.doc.comments=TRUE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")
    
    
}

#' @export
get_sibling_ids <- function(pd, id){
    #' @title Identify siblings.
    #' @inheritParams get_child_ids
    #' @description \subsection{get_sibling_ids}{
    #'   A convenience function for identifying siblings of the given id.
    #'   Siblings are nodes with the same parent.
    #' }
    get_child_ids(pd, get_parent_id(pd, id))
}

#' @export
get_next_sibling_id <- function(pd, id){
    #' @rdname get_sibling_ids
    #' @description \subsection{get_next_sibling_id}{
    #'   gives the id of the next youngest sibling of the current id.
    #' }
    sids <- get_sibling_ids(pd, id)
    . <- which(sids>id)
    if (length(.)) sids[min(.)] else NA_integer_
}
#' @export
get_prev_sibling_id <- function(pd, id){
    #' @rdname get_sibling_ids
    #' @description \subsection{get_prev_sibling_id}{
    #'   gives the id of the next older sibling of the current id.
    #' }
    sids <- get_sibling_ids(pd, id)
    . <- which(sids<id)
    if (length(.)) sids[max(.)] else NA_integer_
}

#' @export
#' @title Test if id is the firstborn.
is_firstborn <- function(id, pd=get('pd', parent.frame())){ 
    #' @inheritParams get_child_ids
    #' @description
    #'   Test if an expression is the firstborn, ie. oldest or lowest id.
    id == get_firstborn_id(pd, get_parent_id(pd,id))
}

#' @export
#' @title get the firstborn child.
get_firstborn_id <-
function(pd, id=all_root_ids(pd)){
    #' @inheritParams get_child_ids
    #' @description
    #'   Get the id of the firstborn child of id.
    #'   Without the "_id" is a wrapper for giving the nodes.
    id <- ._check_id(id)
    kids <- lapply(id, get_child_ids, pd=pd)
    as.integer(sapply(kids, min))
}

#' @export
get_firstborn <-
function(pd, id=all_root_ids(pd)){
    #' @rdname get_firstborn_id
    nodes(get_firstborn_id(pd, id))
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
    expect_equal(get_firstborn(pd, 52)$token, "'{'")
    expect_equal(get_firstborn(pd, 7)$text, "<-")
    
    expect_warning(get_firstborn_id(pd, c(7, 52, .Machine$integer.max)))
    expect_identical( suppressWarnings(get_firstborn_id(pd, c(7, 52, .Machine$integer.max)))
                    , c(2L, 10L, NA_integer_))
    
    expect_true(is_firstborn(2, pd=pd))
    expect_identical(suppressWarnings(is_firstborn(-1)), NA)
    expect_identical(is_firstborn(c(1,3,7)), c(TRUE, FALSE, TRUE))
}


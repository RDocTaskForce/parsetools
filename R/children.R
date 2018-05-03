{#######################################################################
# chilren.R
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
#' @title Get all nodes that are children of `id`.
get_child_ids <-
function( id, pd = get('pd', parent.frame())
        , ngenerations    = 1       
        , include.self    = FALSE   
        , all.generations = TRUE    
        ) {
    #' @param pd              The \code{\link{parse-data}} information
    #' @param id              id of the expression of interest
    #' @param ngenerations    Number of levels to descend.
    #' @param include.self    Should the root node (\code{id}) be included?
    #' @param all.generations Should all generations(TRUE) or only the
    #'                        the final (FALSE) generation be returned?
    #' 
    #' @description
    #'   Get all ids in `pd` that are children of \code{id}.
    #'   i.e. lower in the heirarchy or with id as a parent.
    #'   If \code{ngenerations} is greater than 1 and \code{all.generations} 
    #'   is \code{TRUE}, all descendents are aggregated and returned.
    id <- ._check_id(id)
    parents <- id
    ids <- if(include.self) parents else integer(0)
    while(ngenerations != 0) {
        ngenerations <- ngenerations - 1
        old.ids <- ids
        new.ids <- pd[pd$parent %in% parents, 'id']
        parents <-
        ids <- unique(c(if(all.generations)ids , new.ids))
        if (identical(ids, old.ids)) break
    }
    ids
}
if(FALSE){#! @test
    pd       <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    id       <- pd[pd$parent==0, 'id']
    kids.ids <- get_child_ids(id, pd, 1, include.self = FALSE)
    expect_equal( kids.ids, c(3,2,5,6,9,10,12,13,16,17,19,20)
                , info="for default values"
                )

    kids.ids <- get_child_ids(id, pd, 1, include.self=TRUE)
    expect_equal( kids.ids, c(23,3,2,5,6,9,10,12,13,16,17,19,20)
                , info='include.self=TRUE'
                )

    kids.ids <- get_child_ids( id, pd, 2, include.self=FALSE
                             , all.generations = FALSE
                             )
    expect_equal( kids.ids, c(1,4,11,18)
                , info='ngenerations=2, include.self=FALSE, all.generations=FALSE'
                )

    kids.ids <- get_child_ids( id, pd
                             , ngenerations=2
                             , include.self=FALSE
                             , all.generations = TRUE
                             )
    expect_equal( kids.ids, c(c(3,2,5,6,9,10,12,13,16,17,19,20), c(1,4,11,18))
                , info='ngenerations=2, include.self=FALSE, all.generations=TRUE'
                )

    kids.ids <- get_child_ids( id, pd
                             , ngenerations=2
                             , include.self=TRUE
                             , all.generations = TRUE
                             )
    expect_equal( kids.ids, c(23, c(3,2,5,6,9,10,12,13,16,17,19,20), c(1,4,11,18))
                , info='ngenerations=2, include.self=TRUE, all.generations=TRUE'
                )
                
    expect_identical( get_child_ids(.Machine$integer.max, pd), integer(0))
    expect_true( all(pd$id %in% get_child_ids(0, pd, Inf)))
}

#' @export
get_child <- 
function( id, pd = get('pd', parent.frame())
        , ...       #< passed to <get_child_ids>.
        ) {
    #' @inheritParams get_child_ids
    #' @rdname get_children
    pd[pd$id %in% get_child_ids( id, pd,...), ]
}
if(FALSE){#!@test
    'rnorm(10, mean=0, sd=1)' -> text
    pd       <- get_parse_data(parse(text=text))
    id       <- 3
    expect_identical( get_child(3, pd), utils::head(pd, 1), info='defaults')
    expect_identical( get_child(3, pd, include.self=TRUE), utils::head(pd, 2), info='include.self=TRUE')

    expect_identical( get_child(id=23, pd=pd, ngenerations=1, include.self=FALSE)
                    , pd[pd$parent==23,]
                    , info='defaults')

    expect_identical( get_child(id=23, pd=pd, ngenerations=1, include.self=TRUE)
                    , pd[pd$parent==23 | pd$id==23,]
                    , info='defaults')

    expect_identical( get_child(id=23, pd=pd, ngenerations=2, include.self=TRUE)
                    , pd
                    , info='defaults')

    expect_identical( get_child(id=23, pd=pd, ngenerations=2, include.self=FALSE, all.generations=FALSE)
                    , pd[pd$parent != 23 & pd$parent != 0, ]
                    , info='defaults')
}

#' @export
get_children <-
function( id, pd = get('pd', parent.frame())
        , ...       #< passed to <get_child>.
        ){
    #' @inheritParams get_child_ids
    #' @param ... passed on.
    #' @title Find the children of an expression
    #'
    #' @description 
    #'   This takes the \code{pd} and find all the children of the expression
    #'   with the given \code{id}.
    #' 
    #' @family  parse-functions
    #' @return a list of parse-data objects corresponding to \code{id}
    id <- ._check_id(id)
    return(lapply(id, get_child, pd=pd, ...))
}
if(FALSE){#! @test
    'rnorm(10, mean=0, sd=1)' -> text
    pd  <- get_parse_data(parse(text=text))
    res <- get_children(id=23, pd)
    expect_is(res, 'list')
    expect_equal(length(res), 1)
}

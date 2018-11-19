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

#' @include internal.R

#' @name nodes
#' @title Family-wise Node Identification and Navigation.
#'
#' @description
#' Parse data is organized into a hierachry of nodes. These functions provide
#' simple ways to identify the nodes of interest, often from a specified node
#' of interest.
#'
#' @details
#' The language parsetools uses is that of family.
#' Similar to a family each node could have: a \dfn{parent}, the node that contains the
#' node in question; \dfn{children}, the nodes contained by the given node;
#' \dfn{ancestors}, the collection of nodes that contain the given node, it's parent,
#' it's parent's parent, and so on; and \dfn{descendents}, the collection of nodes that are
#' contained by the given node or contained by those nodes, and so on.
#' Terminology is analogous, a \dfn{generation} is all the the nodes at the same depth in
#' the heirachry. A node may have \dfn{siblings}, the set of nodes with the same parent.
#' If a node does not have a parent it is called a \dfn{root} node.
#'
#' Similarly, age is also used as an analogy for ease of navigation.  Generally, nodes
#' are numbered by the order that they are encountered, when parsing the source.
#' Therefore the node with the smallest `id` among a set of siblings is referred to the
#' \dfn{firstborn}.  This is give the special designation as it is the most often of children
#' used, as it likely determines the type of call or expression that is represented by the node.
#' The firstborn has no 'older' siblings, the 'next' sibling would be the next oldest, i.e. the
#' node among siblings with the smallest id, but is not smaller that the reference node id.
#'
#' In all cases when describing function the `id`, is assumed to be in the context of the
#' parse data object `pd` and for convencience refers to the node associated with said `id`.
#'
#' @param pd              The \code{\link{parse-data}} information
#' @param id              id of the expression of interest
#' @param ngenerations    Number of generations to go forwards or backwards.
#' @param include.self    Should the root node (\code{id}) be included?
#' @param aggregate       Should aggregate(TRUE) or only the
#'                        the final (FALSE) generation be returned?
#' @param .check          Perform checks for input validation?
#' @param ...             arguments passed on.
NULL

#' @describeIn nodes Get all nodes that are children of `id`.
#'   Get all ids in `pd` that are children of \code{id}.
#'   i.e. lower in the hierarchy or with id as a parent.
#'   If \code{ngenerations} is greater than 1 and \code{aggregate}
#'   is \code{TRUE}, all descendents are aggregated and returned.
pd_get_children_ids <-
function( id, pd
        , ngenerations    = 1
        , include.self    = FALSE
        , aggregate = TRUE
        , .check=TRUE
        ) {
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    parents <- id
    ids <- if(include.self) parents else integer(0)
    while(ngenerations != 0) {
        ngenerations <- ngenerations - 1
        old.ids <- ids
        new.ids <- pd[pd$parent %in% parents, 'id']
        parents <-
        ids <- unique(c(if(aggregate)ids , new.ids))
        if (identical(ids, old.ids)) break
    }
    ids
}
children <- internal(pd_get_children_ids)
if(FALSE){#! @test
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))
    id <- pd[pd$parent==0, 'id']
    expect_equal( pd_get_children_ids(id, pd, 1, include.self = FALSE)
                , c(3,2,5,6,9,10,12,13,16,17,19,20)
                , info="for default values"
                )

    expect_equal( pd_get_children_ids(id, pd, 1, include.self=TRUE)
                , c(23,3,2,5,6,9,10,12,13,16,17,19,20)
                , info='include.self=TRUE'
                )

    expect_equal( pd_get_children_ids( id, pd, 2, include.self=FALSE
                                  , aggregate = FALSE
                                  )
                , c(1,4,11,18)
                , info='ngenerations=2, include.self=FALSE, aggregate=FALSE'
                )

    expect_equal( pd_get_children_ids( id, pd
                                  , ngenerations=2
                                  , include.self=FALSE
                                  , aggregate = TRUE
                                  )
                , c(c(3,2,5,6,9,10,12,13,16,17,19,20), c(1,4,11,18))
                , info='ngenerations=2, include.self=FALSE, aggregate=TRUE'
                )

    expect_equal( pd_get_children_ids( id, pd
                                  , ngenerations=2
                                  , include.self=TRUE
                                  , aggregate = TRUE
                                  )
                , c(23, c(3,2,5,6,9,10,12,13,16,17,19,20), c(1,4,11,18))
                , info='ngenerations=2, include.self=TRUE, aggregate=TRUE'
                )

    expect_error( pd_get_children_ids(.Machine$integer.max, pd)
                , "id\\([0-9]+\\) is not present in given parse-data."
                )
    expect_true( all(pd$id %in% pd_get_children_ids(0, pd, Inf)))
}

get_children_pd <-
function( id, pd
        , ...       #< passed to <pd_get_children_ids>.
        , .check = TRUE
        ) {
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot( length(id) == 1L
                 , inherits(pd, 'parse-data')
                 )
    }
    pd[pd$id %in% children( id, pd,...), ]
}
if(FALSE){#!@test
    'rnorm(10, mean=0, sd=1)' -> text
    pd       <- get_parse_data(parse(text=text, keep.source=TRUE))
    id       <- 3

    expect_identical( get_children_pd(3, pd), utils::head(pd, 1), info='defaults')
    expect_identical( get_children_pd(3, pd, include.self=TRUE), utils::head(pd, 2), info='include.self=TRUE')

    expect_identical( get_children_pd(id=23, pd=pd, ngenerations=1, include.self=FALSE)
                    , pd[pd$parent==23,]
                    , info='defaults')

    expect_identical( get_children_pd(id=23, pd=pd, ngenerations=1, include.self=TRUE)
                    , pd[pd$parent==23 | pd$id==23,]
                    , info='defaults')

    expect_identical( get_children_pd(id=23, pd=pd, ngenerations=2, include.self=TRUE)
                    , pd
                    , info='defaults')

    expect_identical( get_children_pd(id=23, pd=pd, ngenerations=2, include.self=FALSE, aggregate=FALSE)
                    , pd[pd$parent != 23 & pd$parent != 0, ]
                    , info='defaults')

    expect_error(get_children_pd(id=pd$id, pd=pd))
}


#' Count the number of children
n_children <- function(id=pd$id, pd=get('pd', parent.frame())){
    #' @inheritParams pd_get_children_ids
    if (length(id)>1L) sapply(id, n_children, pd=pd)
    length(children(id))
}

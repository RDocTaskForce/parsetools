{#######################################################################
# chilren.R
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
get_child_ids <-
function( pd                        #< The <parse-data> information
        , id                        #< id of the expression of interest
        , ngenerations    = 1       #< Number of levels to descend.
        , include.self    = FALSE   #< Should the root node (`id`) be included?
        , all.generations = TRUE    #< Should all generations(TRUE) or only the
                                    #^ the final (FALSE) generation be returned?
        ) {
    #!  Get all nodes that are children of `id`.
    #!  @export
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
    id       <- all_root_ids(pd)
    kids.ids <- get_child_ids(pd, id, 1, include.self = FALSE)
    expect_equal( kids.ids, c(3,2,5,6,9,10,12,13,16,17,19,20)
                , info="for default values"
                )

    kids.ids <- get_child_ids(pd, id, 1, include.self=TRUE)
    expect_equal( kids.ids, c(23,3,2,5,6,9,10,12,13,16,17,19,20)
                , info='include.self=TRUE'
                )

    kids.ids <- get_child_ids( pd, id, 2, include.self=FALSE
                             , all.generations = FALSE
                             )
    expect_equal( kids.ids, c(1,4,11,18)
                , info='ngenerations=2, include.self=FALSE, all.generations=FALSE'
                )

    kids.ids <- get_child_ids( pd, id
                             , ngenerations=2
                             , include.self=FALSE
                             , all.generations = TRUE
                             )
    expect_equal( kids.ids, c(c(3,2,5,6,9,10,12,13,16,17,19,20), c(1,4,11,18))
                , info='ngenerations=2, include.self=FALSE, all.generations=TRUE'
                )

    kids.ids <- get_child_ids( pd, id
                             , ngenerations=2
                             , include.self=TRUE
                             , all.generations = TRUE
                             )
    expect_equal( kids.ids, c(23, c(3,2,5,6,9,10,12,13,16,17,19,20), c(1,4,11,18))
                , info='ngenerations=2, include.self=TRUE, all.generations=TRUE'
                )
}

#' @export
get_child <- 
function( pd, id 
        , ...       #< passed to <get_child_ids>.
        ) {
    #! @InheritParams get_child_ids
    #! @rdname get_children
    #! @export
    pd[pd$id %in% get_child_ids( pd, id,...), ]
}
if(FALSE){#!@test
    'rnorm(10, mean=0, sd=1)' -> text
    pd       <- get_parse_data(parse(text=text))
    id       <- 3
    expect_identical( get_child(pd, 3), utils::head(pd, 1), info='defaults')
    expect_identical( get_child(pd, 3, include.self=TRUE), utils::head(pd, 2), info='include.self=TRUE')

    expect_identical( get_child(pd=pd, id=23, ngenerations=1, include.self=FALSE)
                    , pd[pd$parent==23,]
                    , info='defaults')

    expect_identical( get_child(pd=pd, id=23, ngenerations=1, include.self=TRUE)
                    , pd[pd$parent==23 | pd$id==23,]
                    , info='defaults')

    expect_identical( get_child(pd=pd, id=23, ngenerations=2, include.self=TRUE)
                    , pd
                    , info='defaults')

    expect_identical( get_child(pd=pd, id=23, ngenerations=2, include.self=FALSE, all.generations=FALSE)
                    , pd[pd$parent != 23 & pd$parent != 0, ]
                    , info='defaults')
}

#' @export
get_children <-
function( pd, id
        , ...       #< passed to <get_child>.
        ){
    #! @InheritParams get_child_ids
    #! Find the children of an expression
    #!
    #! This takes the \code{pd} and find all the children of the expression
    #! with the given \code{id}.
    #! 
    #! @param id the id of the given expression in \code{pd}
    #! @param pd the data from a parsed file or expression.
    #!   The results of \code{\link{getParseData}}.
    #! @param ngenerations the number of levels to search.  If a negative number is
    #!   given all children will be found.
    #!
    #! @family  parse-functions
    #! @export
    id <- ._check_id(id)
    return(lapply(id, get_child, pd=pd, ...))
}
if(FALSE){#! @test
    'rnorm(10, mean=0, sd=1)' -> text
    pd  <- get_parse_data(parse(text=text))
    res <- get_children(pd, id=23)
    expect_is(res, 'list')
    expect_equal(length(res), 1)
}

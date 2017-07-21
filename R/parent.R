{#######################################################################
# parent.R
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
get_parent_id <- function(pd, id=pd$id) {
    #' @title Get the parent of the expression identified by `id` in `pd`.
    #' @inheritParams get_child_ids
    #' 
    #' @description Get the parent of the expression identified by `id` in `pd`.
    id <- ._check_id(id)
    pd[match(id, pd$id), 'parent']
}
if(FALSE){#! @testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    expect_identical(get_parent_id(pd, 1), 3L)
    expect_is(get_parent_id(pd, 1), "integer")
    
    expect_is(get_parent_id(pd, 10000), "integer", info="missing parent")
    expect_identical(get_parent_id(pd, 10000), NA_integer_, info="missing parent")
    
    expect_identical(get_parent_id(pd, pd), pd$parent)
    expect_identical(get_parent_id(pd, 0L), NA_integer_)
}

#' @export
get_ancestor_ids <- 
function( pd, id
        , nancestors   = Inf  #< Number of generations to go back
        , aggregate    = TRUE #< All (T) or only final (F).
        , include.self = TRUE #< should `id` be included in list of ancestors
                              #^ if `aggregate` is true.
        , only.present = FALSE#< should the list be restricted to only those 
                              #^ node that are present?  Most revelvent for 
                              #^ when parent is zero.
        ){
    #' @title Get the ancestors of id in pd.
    #' 
    #' @inheritParams get_parent_id
    #' @param nancestors     Number of generations to go back
    #' @param aggregate      All (T) or only final (F).
    #' @param include.self   should `id` be included in list of ancestors
    #'                       if `aggregate` is true.
    #' @param only.present   should the list be restricted to only those 
    #'                       node that are present?  Most revelvent for 
    #'                       when parent is zero.
    #' @description
    #'    Get the ancestors for \code{id}.  See argument descriptions for details.
    id <- ._check_id(id)
    stopifnot( nancestors >= 0
             , include.self || (nancestors >  0)
             )
    if (length(id) > 1) {
            return(lapply( id, get_ancestor_ids, pd=pd
                         , nancestors   = nancestors
                         , aggregate    = aggregate
                         , include.self = include.self
                         , only.present = only.present
                         ))
    }
    
    if ( include.self && only.present && !(id %in% pd$id))
            stop("only.present=TRUE and include.self=TRUE but id is not present in pd.")
    if (nancestors == 0){
        if ( include.self              ) return (id)
        if (!include.self &&  aggregate) return (integer(0))
    } 
    if (aggregate) ancestors <- if (include.self) id else integer(0)
    while(nancestors > 0L){
        nancestors <- nancestors - 1
        parent <- get_parent_id(pd, id)
        if (is.na(parent)) break
        if (only.present && !parent %in% pd$id){
            parent <- id
            break
        }
        if (aggregate) ancestors <- c(ancestors, parent)
        if (parent==0) break
        id <- parent
    }
    if (aggregate) ancestors else parent
}
if(FALSE){#! @testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    expect_identical(get_ancestor_ids(pd, 1, nancestors=Inf, aggregate=TRUE , include.self=TRUE , only.present = FALSE), c(1L, 3L, 23L,0L), info = "defaults, but fully specified.")
    expect_identical(get_ancestor_ids(pd, 1, nancestors=Inf, aggregate=TRUE , include.self=FALSE, only.present = FALSE), c(    3L, 23L,0L), info = "include.self=FALSE")
    expect_identical(get_ancestor_ids(pd, 1, nancestors= 2 , aggregate=TRUE , include.self=FALSE, only.present = FALSE), c(    3L, 23L   ), info = "nancestors=2, include.self=FALSE")
    expect_identical(get_ancestor_ids(pd, 1, nancestors= 2 , aggregate=TRUE , include.self=TRUE , only.present = FALSE), c(1L, 3L, 23L   ), info = "nancestors=2, include.self=TRUE")
    expect_identical(get_ancestor_ids(pd, 1, nancestors= 2 , aggregate=FALSE, include.self=FALSE, only.present = FALSE),           23L    , info = "nancestors= 2, aggregate=FALSE")
    expect_identical(get_ancestor_ids(pd, 1, nancestors= 0 , aggregate=FALSE, include.self=TRUE , only.present = FALSE),    1L             , info = "nancestors=0, include.self=TRUE")
    
    expect_identical(get_ancestor_ids(pd, 1, nancestors=Inf, aggregate=FALSE, include.self=FALSE, only.present = FALSE),               0L , info = "nancestors= 2, aggregate=FALSE")
    expect_identical(get_ancestor_ids(pd, 1, nancestors=Inf, aggregate=FALSE, include.self=FALSE, only.present = TRUE ),           23L    , info = "nancestors= 2, aggregate=FALSE")
    expect_identical(get_ancestor_ids(pd,23, nancestors=Inf, aggregate=FALSE, include.self=FALSE, only.present = TRUE ),           23L    , info = "nancestors= 2, aggregate=FALSE")
    expect_identical(get_ancestor_ids(pd,23, nancestors=Inf, aggregate=TRUE , include.self=FALSE, only.present = TRUE ), integer(0)       , info = "nancestors= 2, aggregate=FALSE")
    
    expect_error(get_ancestor_ids(pd, 1, nancestors=  0, include.self=FALSE))
    expect_error(get_ancestor_ids(pd, 1, nancestors= -1))
    
    expect_is(get_ancestor_ids(pd, c(11, 18)), 'list')
    expect_identical(get_ancestor_ids(pd, c(23, 11), Inf, T, T, F), list(c(23L, 0L), c(11L, 12L, 23L, 0L)))
    expect_identical(get_ancestor_ids(pd, c(23, 11),  2L, T, T, F), list(c(23L, 0L), c(11L, 12L, 23L    )))
    expect_identical(get_ancestor_ids(pd, c(23, 11),  2L, T, F, F), list(c(     0L), c(     12L, 23L    )))
    expect_identical(get_ancestor_ids(pd, c(23, 11),  2L, F, F, F), list(c(     0L), c(          23L    )))
    expect_identical(get_ancestor_ids(pd, c(23, 11), Inf, T, T, T), list(c(23L    ), c(11L, 12L, 23L    )))
    expect_identical(get_ancestor_ids(pd, c(23, 11), Inf, F, T, T), list(c(23L    ), c(          23L    )))
}

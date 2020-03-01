# family.R ############################################################
#                                                                     #
# This file is part of the R package `parsetools`.                    #
#                                                                     #
# Author: Andrew Redd                                                 #
# Copyright: 2017 The R Consortium                                    #
#                                                                     #
# LICENSE                                                             #
# ========                                                            #
# The R package `parsetools` is free software:                        #
# you can redistribute it and/or modify it under the terms of the     #
# GNU General Public License as published by the Free Software        #
# Foundation, either version 3 of the License, or (at your option)    #
# any later version.                                                  #
#                                                                     #
# This software is distributed in the hope that it will be useful,    #
# but WITHOUT ANY WARRANTY; without even the implied warranty of      #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the        #
# GNU General Public License for more details.                        #
#                                                                     #
# You should have received a copy of the GNU General Public License   #
# along with this program. If not, see http://www.gnu.org/licenses/.  #
#_____________________________________________________________________#

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

    lid <- pd[match('LEFT_ASSIGN', pd$token), 'parent']
    fam <- get_family_pd(lid, pd, include.doc.comments=TRUE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")
    fam <- get_family_pd(lid, pd, include.doc.comments=TRUE, include.regular.comments=FALSE)
    expect_equal(fam[1,'text'], "#' Documenation before")
    fam <- get_family_pd(lid, pd, include.doc.comments=FALSE, include.regular.comments=TRUE)
    expect_equal(fam[1,'text'], "# normal comment")
    fam <- get_family_pd(lid, pd, include.doc.comments=FALSE, include.regular.comments=FALSE)
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


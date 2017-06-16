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
        , include.doc.comments     = TRUE   #< include associated documentation comments.
        , include.regular.comments = FALSE  #< include associated regular comments.
        ){
    #! Get family of nodes.
    #! @InheritParams get_child_ids
    #!
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
    id <- ascend_to_root(pd, pd[pd$text == 'c','id'])
    expect_identical(get_family(pd, id), pd[19:24,])
    
    pd <- get_parse_data(parse(text={"
        # normal comment
        #' Documenation before
        hw <- function(){
            #! documentation comment inside.
            print('hello world')
        }
    "}))
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
    }"}))
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
    get_child_ids(pd, get_parent_id(pd, id))
}

#' @export
get_next_sibling_id <- function(pd, id){
    sids <- get_sibling_ids(pd, id)
    . <- which(sids>id)
    if(length(.)) sids[min(.)] else NA_integer_
}



#' @export
get_firstborn_id <-
function(pd, id=all_root_ids(pd)){
    id <- ._check_id(id)
    kids <- lapply(id, get_child_ids, pd=pd)
    Filter(is.finite, sapply(kids, min))
}

#' @export
get_firstborn <-
function(pd, id=all_root_ids(pd)){
    pd[pd$id %in% get_firstborn_id(pd, id), ]
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
    expect_equal(get_firstborn(pd, 52)$token, "'{'")
    expect_equal(get_firstborn(pd, 7)$text, "<-")
}


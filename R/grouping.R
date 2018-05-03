{#######################################################################
# grouping.R
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
is_grouping <- 
function( id = pd$id
        , pd = get('pd', parent.frame())
        ){
  #' @title test if an id is a grouping element
  #' @param id id number in \code{pd}
  #' @param pd parse data to use to check \code{id}
  #' 
  id <- ._check_id(id)
  if(length(id) > 1) return(sapply(id, is_grouping, pd=pd))

  child  <- get_child(id, pd, 1)
  parent <- get_parent_id(id, pd)
  #' @description
  #' a grouping is defined as a non empty set 
  return(  nrow(child)
        #! started with a '{' token and 
        && child$token[1] == "'{'"
        #! and there is no parent or the parent is also a grouping.
        && (parent == 0 || is_grouping(parent, pd)))
  #! @return a logical indicating if the root node(s) is a grouping node or not
}
if(FALSE){#! @testing
    pd <- get_parse_data(parse(text='{
        this(is+a-grouping)
    }'))
    expect_true (is_grouping(25, pd))
    expect_false(is_grouping( 1, pd))
    
    expect_is(is_grouping(pd=pd), 'logical')
    expect_equal(sum(is_grouping(pd=pd)), 1)
}

#' @export
#' @title get the grouping ids
#' @inheritParams get_child_ids
#' @description get the ids that represent the grouping nodes.
#' @return an integer vector of ids.
get_groupings <- function(pd) {pd[is_grouping(pd=pd), 'id']}
if(FALSE){#! @testing
    pd <- get_parse_data(parse(text='{
        this(is+a-grouping)
    }'))
    
    expect_is(get_groupings(pd), 'integer')
    expect_equal(length(get_groupings(pd)), 1)
    expect_equal(get_groupings(pd), 25)
}

fix_grouping_comment_association <- 
function( id = get_groupings(pd)
        , pd = get('pd', parent.frame())
        ){
    id <- ._check_id(id)
    stopifnot(is_grouping(id, pd))
    for (i in id) {
        cids <- get_child_ids(i, pd)
        for (cid in cids) 
            if (is_comment(pd, cid)) {
                n <- get_next_sibling_id(cid, pd)
                while (!is.na(n) && is_comment(pd, n))
                    n <- get_next_sibling_id(n, pd)
                if (!is.na(n)) 
                    pd[ pd$id == cid, 'parent'] <- -ascend_to_root(n, pd)
            }
    }
    return(pd)
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
    {# grouped code
        # normal comment
        #' Documenation before
        hw <- function(){
            #! documentation comment inside.
            print('hello world')
        }
    }
    {# Second Group
        1+2
    }
    # Comment 3
    4+5
    "}))
    fixed <- fix_grouping_comment_association(pd=pd)
    
    expect_identical(fixed[-6], pd[-6])
    expect_equal(get_comments(fixed)$parent, c(-38, -38, -38, 34, -56, -74))
}


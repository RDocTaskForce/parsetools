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
pd_is_grouping <-
function( id, pd, .check=TRUE){
  #' @title test if an id is a grouping element
  #' @param id id number in \code{pd}
  #' @param pd parse data to use to check \code{id}
  #'
  if (.check){
     pd <- ._check_parse_data(pd)
     id <- ._check_id(id, pd)
  }
  if(length(id) > 1) return(sapply(id, pd_is_grouping, pd=pd))

  #' @description
  #' a grouping is defined as a non empty set
  return(  length(children(id))
        #' started with a '{' token and
        && token(firstborn(id)) == "'{'"
        #' and there is no parent or the parent is also a grouping.
        && ( parent(id) == 0
          || pd_is_grouping(parent(id), pd)
           )
        )
  #! @return a logical indicating if the root node(s) is a grouping node or not
}
if(FALSE){#! @testing
    pd <- get_parse_data(parse(text='{
        this(is+a-grouping)
    }', keep.source=TRUE))
    expect_true (pd_is_grouping(25L, pd))
    expect_false(pd_is_grouping( 1L, pd))

    expect_is(pd_is_grouping(pd=pd), 'logical')
    expect_equal(sum(pd_is_grouping(pd=pd)), 1)

    expect_equal(sum(pd_is_grouping(pd=pd)), 1L)

    pd <- get_parse_data(parse(text='
        {# first Group
        {# nested group
            "expression in double nested group"
        }
        }
        '))
    expect_equal(sum(pd_is_grouping(pd=pd)), 2)
}

#' @export
#' @title get the grouping ids
#' @inheritParams pd_get_children_ids
#' @description get the ids that represent the grouping nodes.
#' @return an integer vector of ids.
all_grouping_ids <- make_get_all(pd_is_grouping)
    # function(pd) {pd[pd_is_grouping(pd=pd), 'id']}
if(FALSE){#! @testing
    pd <- get_parse_data(parse(text='{
        this(is+a-grouping)
    }', keep.source=TRUE))

    expect_is(all_grouping_ids(pd), 'integer')
    expect_equal(length(all_grouping_ids(pd)), 1)
    expect_equal(all_grouping_ids(pd), 25)
}

fix_grouping_comment_association <-
function( id = all_grouping_ids(pd)
        , pd = get('pd', parent.frame())
        , .check=TRUE
        ){
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot(pd_is_grouping(id, pd))
    }
    for (i in id) {
        cids <- children(i, pd)
        for (cid in cids)
            if (is_comment(cid, pd)) {
                n <- next_sibling(cid)
                while (!is.na(n) && is_comment(n,pd))
                    n <- next_sibling(n)
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
    "}, keep.source=TRUE))
    id <- all_grouping_ids(pd)

    fixed <- fix_grouping_comment_association(pd=pd)

    expect_identical(fixed[-6], pd[-6])
    expect_equal(parent(all_comment_ids(fixed), fixed), c(-38, -38, -38, 34, -56, -74))
}


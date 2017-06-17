{#######################################################################
# iff_blocks.R
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
is_iff_block <-
function( pd
        , id=all_root_ids(pd)
        , allow.short=TRUE      #< Should `F` be interpreted as FALSE.
        ){
    #! test if an expresion ID points to a `if(FALSE)` statement.
    #! @keyword internal
    pd <- ._check_parse_data(pd)
    id <- ._check_id(id)
    if (length(id) > 1) return(sapply(id, is_iff_block, pd=pd, allow.short=allow.short))
    
    if (pd[pd$id==id,'token'] != 'expr') return(FALSE)
    kids <- get_child_ids(pd, id)
    if (length(kids) < 2) return(FALSE)
    if (!identical(pd[match(utils::head(kids, 2), pd$id), 'token'], c("IF", "'('"))) return(FALSE)
    grandkids <- parsetools::get_child_ids( pd, kids[[3]])
    if (length(grandkids) != 1) return(FALSE)
    row <- pd[match(grandkids, pd$id),]
    return( ( row[['token']] == "NUM_CONST" && row[['text']] == "FALSE")
          || allow.short && ( row[['token']] == "SYMBOL" && row[['text']] == "F")
          )
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        if(FALSE){# an if(FALSE) block
        
        }
        if(F){# also an if(FALSE) block
        }
        {# not an if(F)block
        }
    "}))
    id <- all_root_ids(pd)
    
    expect_true(is_iff_block(pd, id[[1]]))
    expect_true(is_iff_block(pd, id[[2]]))
    expect_false(is_iff_block(pd, id[[2]], FALSE))
    expect_false(is_iff_block(pd, id[[3]]))
    expect_equal(is_iff_block(pd, id), c(TRUE, TRUE, FALSE))
    expect_equal(is_iff_block(pd), c(TRUE, TRUE, FALSE))
}

#' @export
all_iff_ids <- 
function( pd
        , root.only=TRUE        #< only root blocks(`TRUE`) or all block (`FALSE`)
        , ignore.groups=FALSE   #< Ignore code grouping
        , ...                   #< passed to <is_iff_block>
        ){
    #! @inheritParams  is_iff_block
    pd <- ._check_parse_data(pd)
    id <- if (root.only) all_root_ids(pd, !ignore.groups) else pd$id
    is.iff <- is_iff_block(pd, id, ...)
    id[is.iff]
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        if(FALSE){# an if(FALSE) block
        
        }
        if(F){# also an if(FALSE) block
        }
        {# grouping block
            if(F){# iff nested in group
            
            }
        }
        hw <- function(){
            if(F){# nested in a function
                
            }
            print('hello world')
        }
    "}))
    iff.ids <- all_iff_ids(pd, root.only=TRUE, ignore.groups = FALSE)
    expect_equal(length(iff.ids), 2)
    
    iff.ids <- all_iff_ids(pd, root.only=TRUE, ignore.groups = TRUE)
    expect_equal(length(iff.ids), 3)
    
    iff.ids <- all_iff_ids(pd, root.only=FALSE, ignore.groups = FALSE)
    expect_equal(length(iff.ids), 4)
}


is_if_expr <- 
function( pd, id){
    if (length(id)>1) sapply(id, is_if_expr, pd=pd)
    (token(id) == 'expr') &&
    (token(get_firstborn_id(pd, id)) == 'IF')
}
get_if_predicate_id <- 
function( pd, id ){
    stopifnot(is_if_expr(pd, id))
    kids <- get_child_ids(pd, id)
    if (length(kids)<5) stop("inproper if statement")
    kids[[3L]]
}
get_if_branch_id <- 
function( pd, id){
    stopifnot(is_if_expr(pd, id))
    kids <- get_child_ids(pd, id)
    if (length(kids)<5) stop("inproper if statement")
    kids[[5L]]
}
get_if_alternate_id <- 
function( pd, id){
    stopifnot(is_if_expr(pd, id))
    kids <- get_child_ids(pd, id)
    if (length(kids)<7 || token(kids[[6]]) != 'ELSE') 
        stop("inproper if-else statement")
    kids[[7L]]
}
if(FALSE){#!@testing if structures
    pd <- get_parse_data(parse(text={"
        if(predicate){
            body
        } else {
            alternate
        }
    "}))
    token(id, pd)
    id <- all_root_ids(pd) # 33
    
    expect_true(is_if_expr(pd, id))
    expect_equal(get_if_predicate_id(pd, id),  7L)
    expect_equal(get_if_branch_id   (pd, id), 18L)
    expect_equal(get_if_alternate_id(pd, id), 30L)
}


#' @export
iff_is_tagged <- 
function( pd, tag, id
        , doc.only = TRUE
        , ...
        ){
    if (length(id) > 1) return(sapply(id, iff_is_tagged, pd=pd, tag=tag, doc.only=doc.only))
    if (!is_iff_block(pd, id)) return(FALSE)
    if (token(. <- get_if_branch_id(pd, id)) != 'expr') return(FALSE)
    if (token(. <- get_firstborn_id(pd, . )) != "'{'" ) return(FALSE)
    if (!is_comment(pd, . <- get_next_sibling_id(pd, .))) return(FALSE)
    if (doc.only && !is_doc_comment(pd, .)) return(FALSE)
    return(has_tag(pd, tag, .))
}
if(FALSE){#!@ testing
    pd  <- get_parse_data(parse(text={"
        if(FALSE){#!@tag
        }
        if(F){#@tag
        }
        if(F){# @tag
        }
        {#!@tag 
        # not an if(F) block
        }
        {#@tag
        }
        {# @tag
        }
        "}))
    tag <- 'tag'
    id  <- all_root_ids(pd)
    expect_equal(length(id), 6)
    expect_true (iff_is_tagged(pd, tag, id[[1]]))
    expect_true (iff_is_tagged(pd, tag, id[[3]], FALSE))
    expect_false(iff_is_tagged(pd, tag, id[[3]], TRUE ))
    expect_false(iff_is_tagged(pd, tag, id[[6]]))
    expect_equal(iff_is_tagged(pd, tag, id)
                , c(T,T,F,F,F,F))
    expect_equal(iff_is_tagged(pd, tag, id, FALSE)
                , c(T,T,T,F,F,F))
}

all_tagged_iff_ids <- 
function(pd, tag, doc.only=TRUE){
    id <- all_iff_ids(pd)
    is.tagged <- iff_is_tagged(pd, tag, id, doc.only=doc.only)
    id[is.tagged]
}
if(FALSE){#!@ testing
    pd  <- get_parse_data(parse(text={"
        if(FALSE){#!@tag
            # yes
        }
        if(F){#@tag
            # yes
        }
        if(F){# @tag
            # determines doc.only parameter
        }
        {#!@tag 
            # not an if(F) block
        }
        {#@tag
            # no
        }
        {# @tag
            # no
        }
        "}))
    tag <- 'tag'
    id  <- all_root_ids(pd)
    tagged.iff.ids <- all_tagged_iff_ids(pd, tag)
}





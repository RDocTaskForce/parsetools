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

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

#@internal
unquote <- function(x){
    #! remove quotes from x
    #! @param x a [character] string.
    gsub("^('|\")(.*)\\1$", "\\2",x)
}

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
    
    if (token(id) != 'expr') return(FALSE)
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

#' @export
iff_is_tagged <- 
function( pd, tag, id
        , doc.only = TRUE
        , ...
        ){
    if (length(id) > 1) 
        return(sapply(id, iff_is_tagged, pd=pd, tag=tag, doc.only=doc.only))
    if (!is_iff_block(pd, id)) return(FALSE)
    if (token(. <- get_if_branch_id(pd, id)) != 'expr')   return(FALSE)
    if (token(. <- get_firstborn_id(pd, . )) != "'{'" )   return(FALSE)
    if (!is_comment(pd, . <- get_next_sibling_id(pd, .))) return(FALSE)
    if (doc.only && !is_doc_comment(pd, .)) return(FALSE)
    return(has_tag(pd, tag, .))
}
if(FALSE){#!@testing
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
                
    pd <- get_parse_data(parse(text='rnorm(1)'))
    expect_false(iff_is_tagged(pd, tag, all_root_ids(pd)))            
    
    pd <- get_parse_data(parse(text='if(F)#!@tag not in block\nF'))
    expect_false(iff_is_tagged(pd, tag, all_root_ids(pd)))            
    
    pd <- get_parse_data(parse(text='if(F){FALSE}'))
    expect_false(iff_is_tagged(pd, tag, all_root_ids(pd)))            
    
    pd <- get_parse_data(parse(text='if(F){# @tag\nF\n}'))
    expect_false(iff_is_tagged(pd, tag, all_root_ids(pd)))            
    
    pd <- get_parse_data(parse(text='if(F){#@tag\nF\n}'))
    expect_true(iff_is_tagged(pd, tag, all_root_ids(pd)))    
}

#' @export
all_tagged_iff_ids <- 
function(pd, tag, doc.only=TRUE){
    id <- all_iff_ids(pd)
    is.tagged <- iff_is_tagged(pd, tag, id, doc.only=doc.only)
    id[is.tagged]
}
if(FALSE){#!@testing
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


get_iff_associated_name <-
function(pd, id){ 
    prev.id  <- get_prev_sibling_id(pd, id)
    while (TRUE){
        if (is.na(prev.id)) return(NULL)
        if (!is_iff_block(pd, prev.id)) break
        prev.id <- get_prev_sibling_id(pd, prev.id)
    }
    if (is_pd_assignment(pd, prev.id)) {
        #! If the user does not provide the information string
        #! it will be infered as the name of the assignment which
        #! immediately preceeded the block(s).
        
        value.id <- get_pd_assign_value_id(pd, prev.id)
        structure( utils::getParseText(pd, get_pd_assign_variable_id(pd, prev.id))
                 , type = if (is_pd_function(pd, value.id)) "function_assignment"
                          else "assignment"
                 )
    } else if(is_pd_symbol_call(pd, prev.id)) {
        switch( text(get_pd_call_symbol_id(pd, prev.id))
              , setClass = {
                    args <- get_pd_call_args(pd, prev.id)
                    #! The names for `setClass` calls will also be inferred.
                    name <- unquote(args[[if('Class' %in% names(args)) 'Class' else 1L]][1,'text'])
                    structure(name, type = "setClass")
                }
              , setMethod = {
                    args <- get_pd_call_args(pd, prev.id)
                    fname <- unquote(args[[ifelse('f' %in% names(args), 'f', 1L)]][1,'text'])
                    signature <- args[[ifelse('signature' %in% names(args), 'signature', 2L)]]
                    signature <- paste(unquote(signature$text), collapse=',')
                    name <- paste(fname, signature, sep='.')
                    structure(name, type="setMethod")
                    
                }
              , setGeneric = {
                    args <- get_pd_call_args(pd, prev.id)
                    fname <- unquote(args[[ifelse('f' %in% names(args), 'f', 1L)]][1,'text'])
                    structure(fname, type='setGeneric')
                }
              , NULL)
    }
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={'
    if(F){#!@testing
        # a malplaced testing block
        FALSE
    }
    hello_world <- function(){
        print("hello world")
    }
    if(FALSE){#!@testthat
        expect_output(hello_world(), "hello world")
    }
    
    ldf <- data.frame(id = 1:26, letters)
    if(FALSE){#!@testing
        # not a function assignment
    }

    f2 <- function(){stop("this does nothing")}
    if(F){#! @example
        hw()
    }
    if(F){#! @test
        expect_error(f2())
    }
    
    setClass("A")
    if(F){#!@testing 
        #testing a setClass
    }
    
    setMethod("print", "A")
    if(F){#!@testing 
        #testing a setMethod
    }
    
    setGeneric("my_generic", function(x){x})
    if(F){#!@testing 
        #testing a setClass
    }
    
    rnorm(10)
    if(F){#!@testing
        # no previous name
    }
    '}))
    iff.ids <- all_tagged_iff_ids(pd, c('testing', 'testthat', 'test'))
    
    expect_null( get_iff_associated_name(pd, iff.ids[[1L]]), info="iff at beginning")
    expect_equal( get_iff_associated_name(pd, iff.ids[[2L]])
                , structure("hello_world", type = "function_assignment")
                , info="iff after function assignment")
    expect_equal( get_iff_associated_name(pd, iff.ids[[3L]])
                , structure("ldf", type = "assignment")
                , info="iff after other assignment")
    expect_equal( get_iff_associated_name(pd, iff.ids[[4L]])
                , structure("f2", type = "function_assignment")
                , info="iff after other iff")
    expect_equal( get_iff_associated_name(pd, iff.ids[[5L]])
                , structure("A", type = "setClass")
                , info="iff after other iff")
    expect_equal( get_iff_associated_name(pd, iff.ids[[6L]])
                , structure("print.A", type = "setMethod")
                , info="iff after other iff")
    expect_equal( get_iff_associated_name(pd, iff.ids[[7L]])
                , structure("my_generic", type = "setGeneric")
                , info="iff after other iff")
    expect_null ( get_iff_associated_name(pd, iff.ids[[8L]])
                , info="following call")
}



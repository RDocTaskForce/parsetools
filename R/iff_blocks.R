{#######################################################################
# iff_blocks.R
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

#@internal
unquote <- function(x){
    #! remove quotes from x
    #! @param x a [character] string.
    gsub("^('|\")(.*)\\1$", "\\2",x)
}

pd_is_iff <-
function( id, pd
        , allow.short=TRUE      #< Should `F` be interpreted as FALSE.
        , .check=TRUE
        ){
    #' @title test if an expresion ID points to a `if(FALSE)` statement.
    #' @aliases iff-blocks
    #' @inheritParams pd_get_children_ids
    #' @param allow.short Should \code{F} be interpreted as FALSE.
    #'
    #' @description
    #'   This function tests if an expression id is the root of an
    #'   \code{if(FALSE)} block, which many users use to deactive code but
    #'   can also be used to support including examples and testing code
    #'   in the same file as the source code.
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id)
    }
    if (length(id) > 1)
        return(sapply(id, pd_is_iff, pd=pd, allow.short=allow.short))

    if (token(id) != 'expr') return(FALSE)
    kids <- children(id, pd)
    if (length(kids) < 2) return(FALSE)
    if (!identical(pd[match(utils::head(kids, 2), pd$id), 'token'], c("IF", "'('"))) return(FALSE)
    grandkids <- children( kids[[3]], pd)
    row <- pd[match(grandkids, pd$id),]
    return( ( row[['token']] == "NUM_CONST" && row[['text']] == "FALSE")
          || allow.short && ( row[['token']] == "SYMBOL" && row[['text']] == "F")
          )
    #' @return A logical vector of same length as id indicating if the id
    #'      represents a \code{if(FALSE)} block.
}
is_iff <- internal(pd_is_iff, roots(pd))
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        if(FALSE){# an if(FALSE) block

        }
        if(F){# also an if(FALSE) block
        }
        {# not an if(F)block
        }
        if(FALSE) expect_true(TRUE) #< IFF but not a block
    "}, keep.source=TRUE))

    expect_true (pd_is_iff(roots(pd)[[1]], pd))
    expect_true (pd_is_iff(roots(pd)[[2]], pd))
    expect_false(pd_is_iff(roots(pd)[[2]], pd, FALSE))
    expect_false(pd_is_iff(roots(pd)[[3]], pd))
    expect_true (pd_is_iff(roots(pd)[[4]], pd))

    expect_equal(pd_is_iff(roots(pd), pd), c(TRUE, TRUE, FALSE, TRUE))
    expect_equal(   is_iff(pd=pd), c(TRUE, TRUE, FALSE, TRUE))
}

pd_is_iff_block <-
function( id, pd
        , allow.short=TRUE      #< Should `F` be interpreted as FALSE.
        , .check=TRUE
        ){
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id)
    }
    if (length(id) > 1)
        return(sapply( id, pd_is_iff_block, pd=pd
                     , allow.short=allow.short
                     , .check=FALSE)) # nocov
    if (!is_iff(id=id, pd=pd, allow.short=allow.short)) return(FALSE)
    kids <- children(id, pd)
    (token(baby <- max(kids)) == 'expr') &&
    (token(firstborn(baby)) == "'{'")
}
is_iff_block <- internal(pd_is_iff_block, roots(pd))
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        if(FALSE){# an if(FALSE) block

        }
        if(F){# also an if(FALSE) block
        }
        {# not an if(F)block
        }
        if(FALSE) expect_true(TRUE) #< IFF but not a block
    "}, keep.source=TRUE))

    expect_true (pd_is_iff_block(roots(pd)[[1]], pd))
    expect_true (pd_is_iff_block(roots(pd)[[2]], pd))
    expect_false(pd_is_iff_block(roots(pd)[[2]], pd, FALSE))
    expect_false(pd_is_iff_block(roots(pd)[[3]], pd))
    expect_false(pd_is_iff_block(roots(pd)[[4]], pd))
    expect_equal(pd_is_iff_block(roots(pd), pd), c(TRUE, TRUE, FALSE, FALSE))
    expect_equal(pd_is_iff_block(roots(pd), pd, FALSE), c(TRUE, FALSE, FALSE, FALSE))
    expect_equal(   is_iff_block(pd=pd), c(TRUE, TRUE, FALSE, FALSE))
}

all_iff_ids <-
function( pd
        , root.only=TRUE        #< only root blocks(`TRUE`) or all block (`FALSE`)
        , ignore.groups=FALSE   #< Ignore code grouping
        , ...                   #< passed to <pd_is_iff_block>
        ){
    #' @title Identify all \code{if(FALSE)} blocks ids.
    #' @inheritParams  pd_is_iff_block
    #' @param root.only        only root blocks(`TRUE`) or all block (`FALSE`)
    #' @param ignore.groups    Ignore code grouping
    #' @param ...              passed to \code{\link{pd_is_iff_block}}
    #'
    #' @description
    #'   Retreives all the ids from pd that identify
    #'   \code{\link[=iff-blocks]{if(FALSE)}} blocks.
    #'   See \code{\link{pd_is_root}} for details on \code{root.only}.
    #'   See \code{\link{pd_is_grouping}} for details on groups affected by
    #'   \code{ignore.groups}.
    #'
    #' @return an integer vector of all ids identifying
    #'   \code{\link[=iff-blocks]{if(FALSE)}}\link[=iff-blocks]{ blocks}.
    pd <- ._check_parse_data(pd)
    id <- if (root.only) roots(pd, !ignore.groups) else pd$id
    if (!length(id)) return(integer(0))
    is.iff <- pd_is_iff_block(id, pd, ...)
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
    "}, keep.source=TRUE))
    iff.ids <- all_iff_ids(pd, root.only=TRUE, ignore.groups = FALSE)
    expect_equal(length(iff.ids), 2)

    iff.ids <- all_iff_ids(pd, root.only=TRUE, ignore.groups = TRUE)
    expect_equal(length(iff.ids), 3)

    iff.ids <- all_iff_ids(pd, root.only=FALSE, ignore.groups = FALSE)
    expect_equal(length(iff.ids), 4)
}

pd_is_tagged_iff <-
function( id, pd, tag
        , doc.only = TRUE
        , ...
        , .check=TRUE
        ){
    #' @title Test tagged \code{if(FALSE)} blocks.
    #' @inheritParams pd_has_tag
    #' @param doc.only  Should comments be restricted to documentation style
    #'                  comments only?
    #'
    #' @seealso \code{\link{pd_is_iff_block}}, \code{\link{pd_has_tag}}
    #' @description
    #'   This functions tests if an id is: \enumerate{
    #'
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }

    if (length(id) > 1)
        return(sapply(id, pd_is_tagged_iff, pd=pd, tag=tag, doc.only=doc.only))
    #'   \item an \code{if(FALSE)} block.
    if (!pd_is_iff_block(id, pd)) return(FALSE)
    #'   \item is a curly braced group of code.
    if (token(. <- if_branch(id)) != 'expr')   return(FALSE)      # nocov
    if (token(. <- firstborn( . , pd)) != "'{'" )   return(FALSE) # nocov
    #'   \item has a comment as the first parsed element.
    if (!is_comment(. <- next_sibling(.))) return(FALSE)
    #'   \item and that it's a documentation comment if doc.only is true.
    if (doc.only && !is_doc_comment(.)) return(FALSE)
    #'   \item and finally that the comment contains the identified \code{tag(s)}.
    return(pd_has_tag(., pd, tag))
    #'   }
    #' @return a logical vector indicating if the \code{id} in \code{pd}
    #'      identifies an \code{\link[=iff-blocks]{if(FALSE)}}
    #'      \link[=iff-blocks]{ block} that also has the tag identified
    #'      in \code{tag}.
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
        if(FALSE)#@tag not valid
            FALSE
        "}, keep.source=TRUE))
    tag <- 'tag'
    id  <- roots(pd)
    expect_equal(length(id), 7)
    expect_true (pd_is_tagged_iff(id[[1]], pd, tag))
    expect_true (pd_is_tagged_iff(id[[3]], pd, tag, FALSE))
    expect_false(pd_is_tagged_iff(id[[3]], pd, tag, TRUE ))
    expect_false(pd_is_tagged_iff(id[[6]], pd, tag))
    expect_false(pd_is_tagged_iff(id[[7]], pd, tag))
    expect_equal(pd_is_tagged_iff(id, pd, tag)
                , c(T,T,F,F,F,F,F))
    expect_equal(pd_is_tagged_iff(id, pd, tag, FALSE)
                , c(T,T,T,F,F,F,F))

    pd <- get_parse_data(parse(text='rnorm(1)', keep.source=TRUE))
    expect_false(pd_is_tagged_iff(roots(pd), pd, tag))

    pd <- get_parse_data(parse(text='if(F)#!@tag not in block\nF', keep.source=TRUE))
    expect_false(pd_is_tagged_iff(roots(pd), pd, tag))

    pd <- get_parse_data(parse(text='if(F){FALSE}', keep.source=TRUE))
    expect_false(pd_is_tagged_iff(roots(pd), pd, tag))

    pd <- get_parse_data(parse(text='if(F){# @tag\nF\n}', keep.source=TRUE))
    expect_false(pd_is_tagged_iff(roots(pd), pd, tag))

    pd <- get_parse_data(parse(text='if(F){#@tag\nF\n}', keep.source=TRUE))
    expect_true(pd_is_tagged_iff(roots(pd), pd, tag))
}

all_tagged_iff_ids <-
function(pd, tag, doc.only=TRUE){
    #' @title Find all tagged \code{if(FALSE)} blocks.
    #' @inheritParams pd_is_tagged_iff
    #' @description
    #'   Retrieves all ids identifying \code{\link[=iff-blocks]{if(FALSE)}}
    #'   blocks that are also tagged with \code{tag}.
    #'   See \code{\link{pd_is_tagged_iff}} for details.
    #'
    #' @seealso \code{\link{pd_is_iff_block}}, \code{\link{pd_is_tagged_iff}},
    #'          \code{\link{pd_has_tag}}
    #' @return an integer vector giving the ids in \code{pd} that identify
    #'      \code{\link[=iff-blocks]{if(FALSE)}}\link[=iff-blocks]{ blocks}
    #'      that are also tagged with \code{tag}.
    id <- all_iff_ids(pd)
    if (!length(id)) return(id)
    is.tagged <- pd_is_tagged_iff(id=id, tag=tag, pd=pd, doc.only=doc.only)
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
        "}, keep.source=TRUE))
    tag <- 'tag'
    id  <- roots(pd)
    tagged.iff.ids <- all_tagged_iff_ids(pd, tag)

    pd  <- get_parse_data(parse(text={"
        # this has no iff blocks
        "}, keep.source=TRUE))
    tag <- 'tag'
    tagged.iff.ids <- all_tagged_iff_ids(pd, tag)
    expect_identical(tagged.iff.ids, integer(0))
}

#@ internal
pd_get_iff_associated_name_id <-
function(id, pd, .check=TRUE){
    #' @title find the name that should be assocciated with an \code{if(FALSE)} block.
    #' @inheritParams pd_is_iff_block
    #'
    #' @description
    #'   For \code{\link[=iff-blocks]{if(FALSE)}} documentation blocks, such as
    #'   \code{@testing} and \code{@example} blocks, a user may supply an
    #'   information string which gives the name information for tests and
    #'   examples.  for example, in \code{"if(FALSE)\{#@test my special test"}
    #'   the information string is "my special test".
    #'
    #'   The more common case is when there is no information string.
    #'   In these cases the name is inferred by the previous assignemnt or
    #'   declaration.
    #'
    #' The \code{id} argument should identify one and only one
    #' \code{\link[=iff-blocks]{if(FALSE)}} block, but as this is an internal
    #' function, argument checks are not performed.
    #'
    #' @details
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot(all(pd_is_iff_block(id, pd)))
    }
    if (length(id)>1L) return(sapply(id, pd_get_iff_associated_name_id, pd=pd, .check=FALSE))
    prev.id  <- prev_sibling(id, pd)
    while (TRUE){
        #' \code{\link[=iff-blocks]{if(FALSE)}} blocks can be placed
        #' sequentially and \code{pd_get_iff_associated_name_id} will
        #' navigate back until it finds a non-IFF block to use for the name.
        #' This way users can place mutliple tests and examples after a
        #' declaration.
        #'
        if (is.na(prev.id)) return(NULL)
        if (!pd_is_iff_block(prev.id, pd)) break
        prev.id <- prev_sibling(prev.id, pd)
    }
    if (pd_is_assignment(prev.id, pd)) {
        #' If the previous expression is an assignment, the asignee variable of
        #' the assignment is chosen as the name.
        value.id <- assign_value(prev.id)
        structure( utils::getParseText(pd, assign_variable(prev.id))
                 , type = if (is_function(value.id)) "function_assignment"
                          else "assignment"
                 )
        #' An attribute 'type' is also set on the return value.
        #' For function assignments \code{type="function_assignment"},
        #' for all other assignments \code{type="assignment"}.
        #'
    } else if(is_symbol_call(prev.id,pd)) {
        switch( text(call_symbol(prev.id, pd))
              , setClass = {
                    #' The names for \code{link{setClass}} calls will also be inferred.
                    #' The name of the class is taken as the name, but the
                    #' return value also has the attribute of
                    #' \code{type="setClass"}.
                    #' Note that it is common to assign the result of
                    #' \code{\link{setClass}} to a variable, which may or
                    #' may not match the class name.  In those cases the
                    #' assignment operation takes priority and would have
                    #' \code{type="assignment"}.
                    #'
                    args <- call_args(prev.id)
                    line_error_if (length(args) == 0, prev.id,
                        ": setClass must be called with a Class argument.")
                    name <- {
                            class.arg <- if ('Class' %in% names(args)) args[['Class']] else args[[1L]]
                            while (token(class.arg) == 'expr') class.arg <- firstborn(class.arg)
                            if (token(class.arg) == 'STR_CONST') unquote(text(class.arg)) else
                                line_error(prev.id, 'cannot infer Class argument of setClass at')
                        }
                    structure(name, type = "setClass")
                }
              , setMethod = {
                    #' The names for \code{\link{setMethod}} will assume
                    #' the S3 convention of \code{<method>.<class>}.
                    args <- call_args(prev.id)
                    line_error_if(length(args)==0, prev.id,
                        "setMethod must be called with arguments.")
                    fname <- {
                        fname.arg <- args[[ifelse('f' %in% names(args), 'f', 1L)]]
                        while (token(fname.arg) == 'expr') fname.arg <- firstborn(fname.arg)
                        if (token(fname.arg) == 'STR_CONST') unquote(text(fname.arg)) else
                            line_error(prev.id, "Cannot infer method name for setMethod.")
                    }
                    #' In the case the the signature is more than just the class,
                    #' the signature will be collapsed, separated by commas.
                    signature <- {
                        # args[[ifelse('signature' %in% names(args), 'signature', 2L)]]
                        sig.arg <- args[[ifelse('signature' %in% names(args), 'signature', 2L)]]
                        if (is_symbol_call(sig.arg,pd)) {
                            if (!(text(call_symbol(sig.arg)) %in% c('signature', 'c')))
                                line_error(sig.arg, 'Cannot infer signature for setMethod.')
                            args <- call_args(sig.arg)
                            sig.args.text <- expr_text(args)
                        } else expr_text(sig.arg)
                    }
                    name <- paste0(fname, paste0(',', signature, collapse=''), '-method')
                    structure(name, type="setMethod")
                    #' the type attribute will be set to \code{"setMethod"}.
                    #'
                }
              , setGeneric = {
                    #' \code{\link{setGeneric}} can also be used with the name
                    #' of the generic function the inferred name and
                    #' \code{type="setGeneric"}.
                    args <- call_args(prev.id)
                    line_error_if(length(args)==0, prev.id,
                        "setGeneric must be called with arguments.")
                    fname <- {
                        fname.arg <- args[[ifelse('f' %in% names(args), 'f', 1L)]]
                        while (token(fname.arg) == 'expr') fname.arg <- firstborn(fname.arg)
                        if (token(fname.arg) == 'STR_CONST') unquote(text(fname.arg)) else
                            line_error(prev.id, "Cannot infer method name for setGeneric")
                    }
                    structure(fname, type='setGeneric')
                }
              , setAs = {#coerce,call,usage
                    #' \code{\link{setAs}} infers coerce methods.
                    #' \code{type="setAs"}.
                    args <- call_args(prev.id)
                    line_error_if(length(args) < 2, prev.id,
                        "setAs must be called with arguments.")
                    fname <- 'coerce'
                    from <- {
                        fname.arg <- args[[ifelse('from' %in% names(args), 'from', 1L)]]
                        while (token(fname.arg) == 'expr') fname.arg <- firstborn(fname.arg)
                        if (token(fname.arg) == 'STR_CONST') unquote(text(fname.arg)) else
                            line_error(prev.id, "Cannot infer from class for setAs")
                    }
                    to <- {
                        fname.arg <- args[[ifelse('to' %in% names(args), 'to', 2L)]]
                        while (token(fname.arg) == 'expr') fname.arg <- firstborn(fname.arg)
                        if (token(fname.arg) == 'STR_CONST') unquote(text(fname.arg)) else
                            line_error(prev.id, "Cannot infer to argument for setAs")
                    }
                    structure( paste0(paste(fname, from, to, sep=','), '-method')
                             , from=from, to=to, type='setAs')
                }
              , NULL#' if not specified above the function returns \code{\link{NULL}}.
              )
    }
}
iff_associated_name <- internal(pd_get_iff_associated_name_id, all_iff_ids(pd))
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

    setMethod("fun", c("A","B"), function(x,y){
        x+y
    })
    if(F){#!@testing
        #testing a setMethod with multiple signature elements.
    }

    setAs("class1", "class2", function(from){new(from[[1]], "class2")})
    if(F){#!@testing
        #testing setAs
    }
    '}, keep.source=TRUE))
    iff.ids <- all_tagged_iff_ids(pd, c('testing', 'testthat', 'test'))

    expect_null( pd_get_iff_associated_name_id(iff.ids[[1L]], pd), info="iff at beginning")
    expect_equal( pd_get_iff_associated_name_id(iff.ids[[2L]], pd)
                , structure("hello_world", type = "function_assignment")
                , info="iff after function assignment")
    expect_equal( pd_get_iff_associated_name_id(iff.ids[[3L]], pd)
                , structure("ldf", type = "assignment")
                , info="iff after other assignment")
    expect_equal( pd_get_iff_associated_name_id(iff.ids[[4L]], pd)
                , structure("f2", type = "function_assignment")
                , info="iff after other iff")
    expect_equal( pd_get_iff_associated_name_id(iff.ids[[5L]], pd)
                , structure("A", type = "setClass")
                , info="iff after other iff")
    expect_equal( pd_get_iff_associated_name_id(iff.ids[[6L]], pd)
                , structure("print,A-method", type = "setMethod")
                , info="iff after other iff")
    expect_equal( pd_get_iff_associated_name_id(iff.ids[[7L]], pd)
                , structure("my_generic", type = "setGeneric")
                , info="iff after other iff")
    expect_null ( pd_get_iff_associated_name_id(iff.ids[[8L]], pd)
                , info="following call")
    expect_equal( pd_get_iff_associated_name_id(iff.ids[[9L]], pd)
                , structure("fun,A,B-method", type = "setMethod")
                , info="iff after other iff")
    expect_equal( pd_get_iff_associated_name_id(iff.ids[[10L]], pd)
                , structure("coerce,class1,class2-method", type = "setAs"
                           , from='class1', to='class2' )
                , info="setAs")
}



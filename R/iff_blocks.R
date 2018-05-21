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

#@internal
is_iff_block <-
function( id = all_root_ids(pd)
        , pd = get('pd', parent.frame())
        , allow.short=TRUE      #< Should `F` be interpreted as FALSE.
        ){
    #' @title test if an expresion ID points to a `if(FALSE)` statement.
    #' @aliases iff-blocks
    #' @inheritParams get_children_ids
    #' @param allow.short Should \code{F} be interpreted as FALSE.
    #'
    #' @description
    #'   This function tests if an expression id is the root of an
    #'   \code{if(FALSE)} block, which many users use to deactive code but
    #'   can also be used to support including examples and testing code
    #'   in the same file as the source code.
    #'
    #' @keywords internal
    pd <- ._check_parse_data(pd)
    id <- ._check_id(id)
    if (length(id) > 1) return(sapply(id, is_iff_block, pd=pd, allow.short=allow.short))

    if (token(id) != 'expr') return(FALSE)
    kids <- get_children_ids(id, pd)
    if (length(kids) < 2) return(FALSE)
    if (!identical(pd[match(utils::head(kids, 2), pd$id), 'token'], c("IF", "'('"))) return(FALSE)
    grandkids <- get_children_ids( kids[[3]], pd)
    if (length(grandkids) != 1) return(FALSE)
    row <- pd[match(grandkids, pd$id),]
    return( ( row[['token']] == "NUM_CONST" && row[['text']] == "FALSE")
          || allow.short && ( row[['token']] == "SYMBOL" && row[['text']] == "F")
          )
    #' @return A logical vector of same length as id indicating if the id
    #'      represents a \code{if(FALSE)} block.
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        if(FALSE){# an if(FALSE) block

        }
        if(F){# also an if(FALSE) block
        }
        {# not an if(F)block
        }
    "}, keep.source=TRUE))
    id <- all_root_ids(pd)

    expect_true (is_iff_block(id[[1]], pd))
    expect_true (is_iff_block(id[[2]], pd))
    expect_false(is_iff_block(id[[2]], pd, FALSE))
    expect_false(is_iff_block(id[[3]], pd))
    expect_equal(is_iff_block(id, pd), c(TRUE, TRUE, FALSE))
    expect_equal(is_iff_block(pd=pd), c(TRUE, TRUE, FALSE))
}

#' @export
all_iff_ids <-
function( pd
        , root.only=TRUE        #< only root blocks(`TRUE`) or all block (`FALSE`)
        , ignore.groups=FALSE   #< Ignore code grouping
        , ...                   #< passed to <is_iff_block>
        ){
    #' @title Identify all \code{if(FALSE)} blocks ids.
    #' @inheritParams  is_iff_block
    #' @param root.only        only root blocks(`TRUE`) or all block (`FALSE`)
    #' @param ignore.groups    Ignore code grouping
    #' @param ...              passed to \code{\link{is_iff_block}}
    #'
    #' @description
    #'   Retreives all the ids from pd that identify
    #'   \code{\link[=iff-blocks]{if(FALSE)}} blocks.
    #'   See \code{\link[=root]{is_root}} for details on \code{root.only}.
    #'   See \code{\link{pd_is_grouping}} for details on groups affected by
    #'   \code{ignore.groups}.
    #'
    #' @return an integer vector of all ids identifying
    #'   \code{\link[=iff-blocks]{if(FALSE)}}\link[=iff-blocks]{ blocks}.
    pd <- ._check_parse_data(pd)
    id <- if (root.only) all_root_ids(pd, !ignore.groups) else pd$id
    if (!length(id)) return(integer(0))
    is.iff <- is_iff_block(id, pd, ...)
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

#' @export
iff_is_tagged <-
function( id, tag, pd = get('pd', parent.frame())
        , doc.only = TRUE
        , ...
        ){
    #' @title Test tagged \code{if(FALSE)} blocks.
    #' @inheritParams has_tag
    #' @param doc.only  Should comments be restricted to documentation style
    #'                  comments only?
    #'
    #' @seealso \code{\link{is_iff_block}}, \code{\link{has_tag}}
    #' @description
    #'   This functions tests if an id is: \enumerate{
    #'
    #'
    if (length(id) > 1)
        return(sapply(id, iff_is_tagged, pd=pd, tag=tag, doc.only=doc.only))
    #'   \item an \code{if(FALSE)} block.
    if (!is_iff_block(id, pd)) return(FALSE)
    #'   \item is a curly braced group of code.
    if (token(. <- pd_get_if_branch_id(id)) != 'expr')   return(FALSE)
    if (token(. <- get_firstborn_id( . , pd)) != "'{'" )   return(FALSE)
    #'   \item has a comment as the first parsed element.
    if (!is_comment(pd, . <- get_next_sibling_id(., pd))) return(FALSE)
    #'   \item and that it's a documentation comment if doc.only is true.
    if (doc.only && !is_doc_comment(pd, .)) return(FALSE)
    #'   \item and finally that the comment contains the identified \code{tag(s)}.
    return(has_tag(pd, tag, .))
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
        "}, keep.source=TRUE))
    tag <- 'tag'
    id  <- all_root_ids(pd)
    expect_equal(length(id), 6)
    expect_true (iff_is_tagged(id[[1]], tag, pd))
    expect_true (iff_is_tagged(id[[3]], tag, pd, FALSE))
    expect_false(iff_is_tagged(id[[3]], tag, pd, TRUE ))
    expect_false(iff_is_tagged(id[[6]], tag, pd))
    expect_equal(iff_is_tagged(id, tag, pd)
                , c(T,T,F,F,F,F))
    expect_equal(iff_is_tagged(id, tag, pd, FALSE)
                , c(T,T,T,F,F,F))

    pd <- get_parse_data(parse(text='rnorm(1)', keep.source=TRUE))
    expect_false(iff_is_tagged(all_root_ids(pd), tag, pd))

    pd <- get_parse_data(parse(text='if(F)#!@tag not in block\nF', keep.source=TRUE))
    expect_false(iff_is_tagged(all_root_ids(pd), tag, pd))

    pd <- get_parse_data(parse(text='if(F){FALSE}', keep.source=TRUE))
    expect_false(iff_is_tagged(all_root_ids(pd), tag, pd))

    pd <- get_parse_data(parse(text='if(F){# @tag\nF\n}', keep.source=TRUE))
    expect_false(iff_is_tagged(all_root_ids(pd), tag, pd))

    pd <- get_parse_data(parse(text='if(F){#@tag\nF\n}', keep.source=TRUE))
    expect_true(iff_is_tagged(all_root_ids(pd), tag, pd))
}

#' @export
all_tagged_iff_ids <-
function(pd, tag, doc.only=TRUE){
    #' @title Find all tagged \code{if(FALSE)} blocks.
    #' @inheritParams iff_is_tagged
    #' @description
    #'   Retrieves all ids identifying \code{\link[=iff-blocks]{if(FALSE)}}
    #'   blocks that are also tagged with \code{tag}.
    #'   See \code{\link{iff_is_tagged}} for details.
    #'
    #' @seealso \code{\link{is_iff_block}}, \code{\link{iff_is_tagged}},
    #'          \code{\link{has_tag}}
    #' @return an integer vector giving the ids in \code{pd} that identify
    #'      \code{\link[=iff-blocks]{if(FALSE)}}\link[=iff-blocks]{ blocks}
    #'      that are also tagged with \code{tag}.
    id <- all_iff_ids(pd)
    if (!length(id)) return(id)
    is.tagged <- iff_is_tagged(id=id, tag=tag, pd=pd, doc.only=doc.only)
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
    id  <- all_root_ids(pd)
    tagged.iff.ids <- all_tagged_iff_ids(pd, tag)

    pd  <- get_parse_data(parse(text={"
        # this has no iff blocks
        "}, keep.source=TRUE))
    tag <- 'tag'
    tagged.iff.ids <- all_tagged_iff_ids(pd, tag)
    expect_identical(tagged.iff.ids, integer(0))
}

#@ internal
get_iff_associated_name <-
function(id, pd = get('pd', parent.frame())){
    #' @title find the name that should be assocciated with an \code{if(FALSE)} block.
    #' @inheritParams is_iff_block
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
    prev.id  <- get_prev_sibling_id(id, pd)
    while (TRUE){
        #' \code{\link[=iff-blocks]{if(FALSE)}} blocks can be placed
        #' sequentially and \code{get_iff_associated_name} will
        #' navigate back until it finds a non-IFF block to use for the name.
        #' This way users can place mutliple tests and examples after a
        #' declaration.
        #'
        if (is.na(prev.id)) return(NULL)
        if (!is_iff_block(prev.id, pd)) break
        prev.id <- get_prev_sibling_id(prev.id, pd)
    }
    if (pd_is_assignment(prev.id, pd)) {
        #' If the previous expression is an assignment, the asignee variable of
        #' the assignment is chosen as the name.
        value.id <- pd_get_assign_value_id(prev.id)
        structure( utils::getParseText(pd, pd_get_assign_variable_id(pd, prev.id))
                 , type = if (pd_is_function(value.id)) "function_assignment"
                          else "assignment"
                 )
        #' An attribute 'type' is also set on the return value.
        #' For function assignments \code{type="function_assignment"},
        #' for all other assignments \code{type="assignment"}.
        #'
    } else if(pd_is_symbol_call(prev.id)) {
        switch( text(pd_get_call_symbol_id(prev.id, pd))
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
                    args <- pd_get_call_arg_ids(prev.id)
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
                    args <- pd_get_call_arg_ids(prev.id)
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
                        sig.arg <- args[[ifelse('signature' %in% names(args), 'f', 1L)]]
                        while (token(fname.arg) == 'expr') fname.arg <- firstborn(fname.arg)
                        if (token(fname.arg) == 'STR_CONST') unquote(text(fname.arg)) else
                            line_error(prev.id, "Cannot infer method name for setMethod.")
                    }
                    signature <- paste(unquote(signature$text), collapse=',')
                    name <- paste(fname, signature, sep='.')
                    structure(name, type="setMethod")
                    #' the type attribute will be set to \code{"setMethod"}.
                    #'
                }
              , setGeneric = {
                    #' \code{\link{setGeneric}} can also be used with the name
                    #' of the generic function the inferred name and
                    #' \code{type="setGeneric"}.
                    args <- pd_get_call_arg_ids(prev.id)
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
              , NULL#' if not specified above the function returns \code{\link{NULL}}.
              )
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
    '}, keep.source=TRUE))
    iff.ids <- all_tagged_iff_ids(pd, c('testing', 'testthat', 'test'))

    expect_null( get_iff_associated_name(iff.ids[[1L]], pd), info="iff at beginning")
    expect_equal( get_iff_associated_name(iff.ids[[2L]], pd)
                , structure("hello_world", type = "function_assignment")
                , info="iff after function assignment")
    expect_equal( get_iff_associated_name(iff.ids[[3L]], pd)
                , structure("ldf", type = "assignment")
                , info="iff after other assignment")
    expect_equal( get_iff_associated_name(iff.ids[[4L]], pd)
                , structure("f2", type = "function_assignment")
                , info="iff after other iff")
    expect_equal( get_iff_associated_name(iff.ids[[5L]], pd)
                , structure("A", type = "setClass")
                , info="iff after other iff")
    expect_equal( get_iff_associated_name(iff.ids[[6L]], pd)
                , structure("print.A", type = "setMethod")
                , info="iff after other iff")
    expect_equal( get_iff_associated_name(iff.ids[[7L]], pd)
                , structure("my_generic", type = "setGeneric")
                , info="iff after other iff")
    expect_null ( get_iff_associated_name(iff.ids[[8L]], pd)
                , info="following call")
}



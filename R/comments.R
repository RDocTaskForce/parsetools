{#######################################################################
# comments.R
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
# Foundation, either version 2 of the License, or (at your option)
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
comment.classes <- data.frame(
      prefix = c( "#'"             , "#!"         , "#<"              , "#^"                  , "#@"         )
    , class  = c( "ROXYGEN_COMMENT", "DOC_COMMENT", "RELATIVE_COMMENT", "CONTINUATION_COMMENT", "TAG_COMMENT")
    , stringsAsFactors = FALSE
    )

classify_comment.character <- function(x){
    stopifnot(is.character(x))
    lead <- substring(x, 1, 2)
    ifelse( nchar(x) > 0
          , ifelse( substring(x, 1, 1) == "#"
                  , ifelse( lead == comment.classes[1, 1], comment.classes[1, 2]
                  , ifelse( lead == comment.classes[2, 1], comment.classes[2, 2]
                  , ifelse( lead == comment.classes[3, 1], comment.classes[3, 2]
                  , ifelse( lead == comment.classes[4, 1], comment.classes[4, 2]
                  , ifelse( lead == comment.classes[5, 1], comment.classes[5, 2]
                          , "NORMAL_COMMENT"
                          )))))
                  , "")
          , "")
}
if(FALSE){#@testing
    expect_equal(classify_comment.character("## normal comment       "), "NORMAL_COMMENT")
    expect_equal(classify_comment.character("#' Roxygen comment      "), "ROXYGEN_COMMENT")
    expect_equal(classify_comment.character("#! Documentation comment"), "DOC_COMMENT")
    expect_equal(classify_comment.character("#< Relative comment     "), "RELATIVE_COMMENT")
    expect_equal(classify_comment.character("#^ Continuation comment "), "CONTINUATION_COMMENT")
    expect_equal(classify_comment.character("#@ Tag comment          "), "TAG_COMMENT")

    expect_equal(classify_comment.character("1"), "")
}
classify_comment.data.frame <- function(x){
    stopifnot(valid_parse_data(x))
    idx <- x$token == "COMMENT"
    x[idx, "token"] <- classify_comment.character(x[idx, "text"])
    structure(x, class=c("parse-data", "data.frame"))
}
if(FALSE){#@testing
    x <-
    df <- utils::getParseData(parse(text="{
        ## normal comment
        #' Roxygen comment
        #! Documentation comment
        #< Relative comment
        #^ Continuation comment
        #@ Tag comment
    }", keep.source=TRUE))
    pd <- classify_comment.data.frame(df)
    expect_is(pd, 'data.frame')
    expect_is(pd, 'parse-data')
    expect_equal( pd$token
                , c( "expr", "'{'"
                   , "NORMAL_COMMENT", "ROXYGEN_COMMENT", "DOC_COMMENT"
                   , "RELATIVE_COMMENT", "CONTINUATION_COMMENT", "TAG_COMMENT"
                   , "'}'")
                )
}
classify_comment <- function(x)UseMethod("classify_comment")
if(FALSE){#@testing
    df <- utils::getParseData(parse(text="{
        ## normal comment
        #' Roxygen comment
        #! Documentation comment
        #< Relative comment
        #^ Continuation comment
        #@ Tag comment
    }", keep.source=TRUE))
    pd <- classify_comment(df)
    comments <- nodes(all_comment_ids(pd))
    expect_is(comments, 'data.frame')
    expect_is(comments, 'parse-data')
    expect_equal( comments$token
                , c( "NORMAL_COMMENT", "ROXYGEN_COMMENT", "DOC_COMMENT"
                   , "RELATIVE_COMMENT", "CONTINUATION_COMMENT", "TAG_COMMENT"
                   )
                )
}


#' @title Is this a comment?
#' @description
#'   \subsection{pd_is_comment}{
#'      Test if an id represents a comment of any kind.
#'   }
#' @return Should return a logical vector, for parse-data and data.frame should
#'         be length of \code{nrow(x)}.  For character same length as x.
pd_is_comment <- function(id, pd, .check=TRUE){
    #' @inheritParams pd_get_children_ids
    if (.check){# nocov start
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }# nocov end
    token(id, pd) %in% c(comment.classes$class, "NORMAL_COMMENT")
}
all_comment_ids <- make_get_all(pd_is_comment)
is_comment <- internal(pd_is_comment)
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={"
        ## normal comment
        #' Roxygen comment
        #! Documentation comment
        #< Relative comment
        #^ Continuation comment
        #@ Tag comment
        Hello
    "}, keep.source=TRUE))
    rtn <- is_comment(pd$id, pd=pd)
    expect_is(rtn, 'logical')
    expect_equal(rtn, c(T,T,T,T,T,T,F,F))

    expect_equal( all_comment_ids(pd), (1:6)*3L)
}


#' @rdname pd_is_comment
#' @description
#'   \subsection{pd_is_relative_comment}{
#'       Tests if the comment is a relative (location dependent) type comment.
#'   }
pd_is_relative_comment <- function(id, pd, .check=TRUE){
    if(.check){# nocov start
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }# nocov end
    token(id, pd) == "RELATIVE_COMMENT"
}
all_relative_comment_ids <- make_get_all(pd_is_relative_comment)

#' @rdname pd_is_comment
#' @description
#'   \subsection{pd_all_relative_comment_ids}{
#'       Retrieve all ids associated with relative comments.
#'   }
pd_all_relative_comment_ids <- external(all_relative_comment_ids)
is_relative_comment <- internal(pd_is_relative_comment)
if(F){#@testing
    pd <- get_parse_data(parse(text={"
        ## normal comment
        #' Roxygen comment
        #! Documentation comment
        #< Relative comment
        #^ Continuation comment
        #@ Tag comment
        Hello
    "}, keep.source=TRUE))

    expect_is(rtn <- pd_is_relative_comment(pd$id, pd=pd), 'logical')
    expect_equal(rtn, c(F,F,F,T,F,F,F,F))
}



#' @rdname pd_is_comment
#' @description
#'   \subsection{pd_is_doc_comment}{
#'       Additionally tests if the comment is a documentation type comment.
#'   }
pd_is_doc_comment <- function(id, pd, .check=TRUE){
    if (.check){# nocov start
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }# nocov end
    token(id) %in% comment.classes$class
}
all_doc_comment_ids <- make_get_all(pd_is_doc_comment)
is_doc_comment <- internal(pd_is_doc_comment)
if(FALSE){#@testing
    pd <- get_parse_data(parse(text="{
        ## normal comment
        #' Roxygen comment
        #! Documentation comment
        #< Relative comment
        #^ Continuation comment
        #@ Tag comment
    }", keep.source=TRUE))
    rtn <- is_doc_comment(pd$id, pd=pd)
    expect_is(rtn, 'logical')
    expect_equal(rtn, c(F,F,F,T,T,T,T,T,F))

    pd <- get_parse_data(parse(text="{
        ## normal comment
        #' Roxygen comment
        #! Documentation comment
        #< Relative comment
        #^ Continuation comment
        #@ Tag comment
    }", keep.source=TRUE))
    rtn <- is_doc_comment(pd$id, pd=pd)
    expect_is(rtn, 'logical')
    expect_equal(rtn, c(F,F,F,T,T,T,T,T,F))
}

#' @export
strip_doc_comment_leads.character <- function(comment, rm.space=TRUE){
    comment <- gsub("^\\s+", "", comment)
    comment <- gsub("^#[!^<'{}@]", "", comment)
    if(rm.space) comment <- trimws(comment)
    comment
}
if(FALSE){#@testing
    expect_equal(strip_doc_comment_leads.character("#  normal comment       "), "#  normal comment")
    expect_equal(strip_doc_comment_leads.character("#' Roxygen comment      "), "Roxygen comment")
    expect_equal(strip_doc_comment_leads.character("#! Documentation comment"), "Documentation comment")
    expect_equal(strip_doc_comment_leads.character("#< Relative comment     "), "Relative comment")
    expect_equal(strip_doc_comment_leads.character("#^ Continuation comment "), "Continuation comment")
    expect_equal(strip_doc_comment_leads.character("#@ Tag comment          "), "Tag comment")
}

#' @export
strip_doc_comment_leads.data.frame <- function(comment, rm.space=TRUE){
    #@rdname strip_doc_comment_leads
    pd <- ._check_parse_data(comment)
    pd$text <- strip_doc_comment_leads.character(pd$text, rm.space=rm.space)
    pd
}
if(FALSE){#@testing
    pd <- get_parse_data(parse(text="{
        ## normal comment
        #' Roxygen comment
        #! Documentation comment
        #< Relative comment
        #^ Continuation comment
        #@ Tag comment
    }", keep.source=TRUE))
    comments <- nodes(all_comment_ids(pd), pd)
    pd2 <- strip_doc_comment_leads.data.frame(comments)
    expect_is(pd2, 'data.frame')
    expect_is(pd2, 'parse-data')
    expect_equal( pd2$text
                , c( "## normal comment", "Roxygen comment"
                   , "Documentation comment", "Relative comment"
                   , "Continuation comment", "Tag comment"
                   )
                )
}

#' @export
#' @title Remove the characters identifying a documentation comment.
#' @param comment The text of the comments or parse data.
#' @param rm.space      should the space at the beginning of the line be removed.
#' @description
#' Remove the characters identifying a documentation comment as a
#' document comment leaving only the relevant text.
strip_doc_comment_leads <-
function( comment, rm.space = TRUE)
    UseMethod("strip_doc_comment_leads")

if(FALSE){#@testing
    expect_equal(strip_doc_comment_leads("#  normal comment       "), "#  normal comment")
    expect_equal(strip_doc_comment_leads("#' Roxygen comment      "), "Roxygen comment")
    expect_equal(strip_doc_comment_leads("#! Documentation comment"), "Documentation comment")
    expect_equal(strip_doc_comment_leads("#< Relative comment     "), "Relative comment")
    expect_equal(strip_doc_comment_leads("#^ Continuation comment "), "Continuation comment")
    expect_equal(strip_doc_comment_leads("#@ Tag comment          "), "Tag comment")

    pd <- get_parse_data(parse(text="{
        ## normal comment
        #' Roxygen comment
        #! Documentation comment
        #< Relative comment
        #^ Continuation comment
        #@ Tag comment
    }", keep.source=TRUE))
    comments <- nodes(all_comment_ids(pd))
    pd2 <- strip_doc_comment_leads(comments)
    expect_is(pd2, 'data.frame')
    expect_is(pd2, 'parse-data')
    expect_equal( pd2$text
                , c( "## normal comment", "Roxygen comment"
                   , "Documentation comment", "Relative comment"
                   , "Continuation comment", "Tag comment"
                   )
                )
}

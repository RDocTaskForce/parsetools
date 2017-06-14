{#######################################################################
# comments.R
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
if(FALSE){#! @testing
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
if(FALSE){#! @testing
    x <- 
    df <- utils::getParseData(parse(text="{
        ## normal comment           
        #' Roxygen comment          
        #! Documentation comment    
        #< Relative comment         
        #^ Continuation comment     
        #@ Tag comment              
    }"))
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
if(FALSE){#! @testing
    df <- utils::getParseData(parse(text="{
        ## normal comment           
        #' Roxygen comment          
        #! Documentation comment    
        #< Relative comment         
        #^ Continuation comment     
        #@ Tag comment              
    }"))
    pd <- classify_comment(df)
    comments <- get_comments(pd)
    expect_is(comments, 'data.frame')
    expect_is(comments, 'parse-data')
    expect_equal( comments$token
                , c( "NORMAL_COMMENT", "ROXYGEN_COMMENT", "DOC_COMMENT"
                   , "RELATIVE_COMMENT", "CONTINUATION_COMMENT", "TAG_COMMENT"
                   )
                )
}

#' @export
is_comment <- function(x, ...)UseMethod("is_comment")

#' @export
is_comment.character <- function(x, ...){
    classify_comment(x) %in% c( comment.classes$class
                              , "NORMAL_COMMENT"
                              )
}

#' @export
`is_comment.parse-data` <- function(x, id=x$id, ...){
    x[ x$id %in% id, 'token'] %in% c( comment.classes$class
                                    , "NORMAL_COMMENT"
                                    )
}
#' @export
is_comment.data.frame <- function(x, id=x$id, ...){
    x <- classify_comment(x)
    x[ x$id %in% id, 'token'] %in% c( comment.classes$class
                                    , "NORMAL_COMMENT"
                                    )
}
if(FALSE){#!@testing 
    expect_true(is_comment("## normal comment       "))
    expect_true(is_comment("#' Roxygen comment      "))
    expect_true(is_comment("#! Documentation comment"))
    expect_true(is_comment("#< Relative comment     "))
    expect_true(is_comment("#^ Continuation comment "))
    expect_true(is_comment("#@ Tag comment          "))
    expect_false(is_comment("hello"))
    pd <- get_parse_data(parse(text={"
        ## normal comment           
        #' Roxygen comment          
        #! Documentation comment    
        #< Relative comment         
        #^ Continuation comment     
        #@ Tag comment              
        Hello
    "}))
    rtn <- is_comment(pd)
    expect_is(rtn, 'logical')
    expect_equal(rtn, c(T,T,T,T,T,T,F,F))
}


#' @export
is_doc_comment <- function(x, ...)UseMethod("is_doc_comment")
#' @export
is_doc_comment.character <- function(x, ...){classify_comment(x) %in% comment.classes$class}
#' @export
`is_doc_comment.parse-data` <- function(x, id=x$id, ...){
    x[ x$id %in% id, 'token'] %in% comment.classes$class
}
#' @export
is_doc_comment.data.frame   <- function(x, id=x$id, ...){
    x <- classify_comment(x)
    x[ x$id %in% id, 'token'] %in% comment.classes$class
}
if(FALSE){#! @testing
    expect_false(is_doc_comment("## normal comment       "))
    expect_true (is_doc_comment("#' Roxygen comment      "))
    expect_true (is_doc_comment("#! Documentation comment"))
    expect_true (is_doc_comment("#< Relative comment     "))
    expect_true (is_doc_comment("#^ Continuation comment "))
    expect_true (is_doc_comment("#@ Tag comment          "))

    pd <- get_parse_data(parse(text="{
        ## normal comment           
        #' Roxygen comment          
        #! Documentation comment    
        #< Relative comment         
        #^ Continuation comment     
        #@ Tag comment              
    }"))
    rtn <- is_doc_comment(pd)
    expect_is(rtn, 'logical')
    expect_equal(rtn, c(F,F,F,T,T,T,T,T,F))
}


#' @export
get_comments <- function(pd){ pd[is_comment(pd),]}
make_get_comment.classes <-
function( type = comment.classes$class  #< type of the comments to extract
        ){
    #! Create extraction functions for comment types.
    #! @keywords internal, utilities
    function(pd){
        x <- classify_comment(pd)
        x[x$token %in% type, ]
    }
}

#' @export
get_doc_comments          <- make_get_comment.classes()

#' @export
get_relative_comments     <- make_get_comment.classes("RELATIVE_COMMENT")

#' @export
get_continuation_comments <- make_get_comment.classes("CONTINUATION_COMMENT")

#' @export
get_argument_descriptors  <- make_get_comment.classes(c( "RELATIVE_COMMENT"
                                                       , "CONTINUATION_COMMENT"
                                                       ))

#' @export
get_associated_continuation <-
function( pd            #< parse data.
        , id=pd$id[1]   #< id of the comment of interest
        ){
    #! retrieve the continuation comments associated with the comment of interest.
    pd <- classify_comment(pd)
    start <-
    end   <- which(pd$id==id)
    if (pd[start,'token'] %in%  c("DOC_COMMENT", "RELATIVE_COMMENT")) {
        while(pd[end + 1, 'token'] == "CONTINUATION_COMMENT"){
            end <- end + 1
        }
        return(pd[start:end, ])
    } else if(pd[start, 'token'] %in% "ROXYGEN_COMMENT") {
        return(pd[start,])
    } else stop("not a valid starting comment.")
    #! @return filtered parse data with the comments, will be empty if the id 
    #! does not denote a documentation comment.
}
if(FALSE){#! @testing
    pd <- get_parse_data(parse(text="
    function( x = 0 #< just a random argument
            , y = 1 #< yet another
                    #^ argument.
            ){x**y}
    "))
    id <- get_relative_comments(pd)$id[[2]]
    
    pd[pd$token == 'CONTINUATION_COMMENT', 'id']
    x <- get_associated_continuation(pd, id)
    expect_equal( x$line1, c(3,4))
    expect_equal( x$id, c(17,19))
    expect_equal( x$text, c( "#< yet another"
                           , "#^ argument."
                           ))
}

#' @export
strip_doc_comment_leads.character <- function(comment, rm.space=TRUE){
    comment <- gsub("^\\s+", "", comment)    
    comment <- gsub("^#[!^<'{}@]", "", comment)
    if(rm.space) comment <- trimws(comment)
    comment
}
if(FALSE){#! @testing
    expect_equal(strip_doc_comment_leads.character("#  normal comment       "), "#  normal comment")
    expect_equal(strip_doc_comment_leads.character("#' Roxygen comment      "), "Roxygen comment")
    expect_equal(strip_doc_comment_leads.character("#! Documentation comment"), "Documentation comment")
    expect_equal(strip_doc_comment_leads.character("#< Relative comment     "), "Relative comment")
    expect_equal(strip_doc_comment_leads.character("#^ Continuation comment "), "Continuation comment")
    expect_equal(strip_doc_comment_leads.character("#@ Tag comment          "), "Tag comment")
}

#' @export
strip_doc_comment_leads.data.frame <- function(comment, rm.space=TRUE){
    pd <- ._check_parse_data(comment)
    pd$text <- strip_doc_comment_leads.character(pd$text, rm.space=rm.space)
    pd
}
if(FALSE){#! @testing
    pd <- utils::getParseData(parse(text="{
        ## normal comment           
        #' Roxygen comment          
        #! Documentation comment    
        #< Relative comment         
        #^ Continuation comment     
        #@ Tag comment              
    }"))
    comments <- get_comments(pd)
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
strip_doc_comment_leads <-
function( comment.text  #< The text of the comments.
        , rm.space = TRUE  #< should the space at the beginning of the line be removed.
        ){
    #! Remove the characters identifying a documentation comment.
    UseMethod("strip_doc_comment_leads")
}
if(FALSE){#! @testing
    expect_equal(strip_doc_comment_leads("#  normal comment       "), "#  normal comment")
    expect_equal(strip_doc_comment_leads("#' Roxygen comment      "), "Roxygen comment")
    expect_equal(strip_doc_comment_leads("#! Documentation comment"), "Documentation comment")
    expect_equal(strip_doc_comment_leads("#< Relative comment     "), "Relative comment")
    expect_equal(strip_doc_comment_leads("#^ Continuation comment "), "Continuation comment")
    expect_equal(strip_doc_comment_leads("#@ Tag comment          "), "Tag comment")
    
    pd <- utils::getParseData(parse(text="{
        ## normal comment           
        #' Roxygen comment          
        #! Documentation comment    
        #< Relative comment         
        #^ Continuation comment     
        #@ Tag comment              
    }"))
    comments <- get_comments(pd)
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

{#######################################################################
# tags.R
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
make_tag_regex <-
function( tag              #< tag pattern, interpreted as a regular expression
                           #^ or alternatives if more that one is passed in.
        , ...              #< discarded
        ){
    if(length(tag)>1)
        tag <- paste0("(", paste(tag, collapse="|"), ")")

    paste(comment.classes$prefix, collapse='|')
    paste0( "(?<=#|#'|#!|#<|\\s|^)"
          , "@", tag
          , "\\b"
          )
}
if(FALSE){#!@
    tag <- 'tag'
    cases <- c( '#@tag', '# @tag' , '@tag'         # TRUE
              , '@@tag'                            # maybe?
              , '#@ tag', 'tag', 'aname@tag.org'   # FALSE
              )

    expect_equal(rx <- make_tag_regex(tag), "(?<=#|#'|#!|#<|\\s|^)@tag\\b")
    expect_equal( grepl(rx, cases, perl=TRUE)
                , c(T, T, T, F, F, F, F)
                )

    other.cases <- gsub('tag', 'another', cases)

    expect_equal( rx <- make_tag_regex(c('tag', 'another'))
                , "(?<=#|#'|#!|#<|\\s|^)@(tag|another)\\b"
                )
    expect_equal( grepl(rx, c(cases, other.cases), perl=TRUE)
                , c( c(T, T, T, F, F, F, F)
                   , c(T, T, T, F, F, F, F)
                   )
                )
    edge.cases <- c('#\'@tag', '#!@tag', '#<@tag', '#@tag')
    expect_true(all(grepl(rx, edge.cases, perl=TRUE)))
}

#' @title Check if there is a documentation `@` tag.
#' @inheritParams pd_get_children_ids
#' @param tag tag(s) to test for
#' @param ... options passed on
pd_has_tag <-
function( id, pd, tag, ...){
    #' @description
    #'
    #' Check if a node of \code{parse-data} identified by \code{id}
    #' is both a comment and contains a documentation tag identified by
    #' the `@` symbol.
    tag.rx <- make_tag_regex(tag, ...)
    pd_is_comment(id, pd) & grepl(tag.rx, text(id, pd), perl=TRUE, ignore.case=TRUE)
}
if(FALSE){#!@testing
    # Note that testthat:::test_code will strip comments from code
    # this requires a parse statement.
    pd  <- get_parse_data(parse(text='fun <- function(object){
        #! function with only comment lines
        #!       @tag   TRUE
        #!      @@tag   FALSE
        #! @notag{@tag}@ FALSE
        #        @tag   TRUE, even though a regular comment
        object @tag
        NULL
    }', keep.source=TRUE))
    tag <- 'tag'
    id <- pd$id
    expect_equal(sum(pd_has_tag(id, pd, tag)), 2)
}

#@internal
has_tag <- function(id=pd$id, tag='', pd=get("pd", parent.frame())){
    pd_has_tag(id, pd, tag)
}
if(FALSE){#@testing
    pd  <- parsetools::get_parse_data(parse(text={"
        if(FALSE){#@block block content
            #' @first first content
            #' @second second content
            #' not part of second content.
            #' @last
        }
    "}, keep.source=TRUE))
    expect_equal(sum(has_tag()), 4L)
}

#@ internal
clean_tag_comments <-
function( x
        , tag
        ){
    #' @title clean tag comments
    #' @inheritParams strip_tag
    #' @description
    #'    replaces '\code{#@tag}' with '\code{#! @tag}'
    tag <- paste0("(", paste(tag, collapse="|"), ")")
    gsub(paste0("^#@", tag, "\\b"), "#! @\\1", x)
}
if(FALSE){#!@testing
    expect_equal( clean_tag_comments("#@testing", "testing")
                , "#! @testing"
                )
}

strip_tag <-
function( x     #< text to strip from
        , tag   #< tag to remove
        , ...   #< passed on options.
        ){
    #' @title  Remove a tag that identified a line.
    #' @param x     text to strip from
    #' @param tag   tag(s) to remove
    #' @param ...   passed on options]
    #' @description
    #'    Removes \code{@tag} tags from the text.
    #'    Also will remove '\code{#@tag}' replacing with '\code{#!}'.
    pattern <- paste0(make_tag_regex(tag, ...), '\\s*')
    x <- clean_tag_comments(x, tag)

    gsub( pattern=pattern, replacement='', x
        , perl=TRUE, ignore.case=TRUE)
    #< @return text with the @ tag removed.
}
if(FALSE){#! @testthat
    expect_equal( strip_tag("@tag should be removed", 'tag')
                , "should be removed")
    expect_equal( strip_tag("@nomd{@tag}@ should not be removed", 'tag')
                , "@nomd{@tag}@ should not be removed")
    expect_equal( strip_tag("@@tag should not be removed.", 'tag')
                , "@@tag should not be removed.")
}

pd_get_tagged_comment_ids <-
function( pd, tag
        , doc.only = TRUE #< Restrict to documentation comments only?
        ){
    #' @title Get tagged comment ids
    #' @inheritParams pd_has_tag
    #' @param doc.only Restrict to documentation comments only?
    #' @description
    #'   Finds all ids that are comments and contain the given '@' \code{tag}.
    #'   If doc.only is true(default) then only documentation comments are
    #'   considered, otherwise all comments are examined.
    ids <- if (doc.only)
        all_doc_comment_ids()
    else
        all_comment_ids()
    ids[pd_has_tag(ids, pd, tag)]
    #' @return an integer vector of ids.
}
if(FALSE){#!@testing
    pd  <- parsetools::get_parse_data(parse(text={"
        fun <- function(object){
            #! function with only comment lines
            #!       @tag   TRUE
            #!      @@tag   FALSE
            #! @notag{@tag}@ FALSE
            #        @tag   TRUE, even though a regular comment
            object @tag
            NULL
        }
    "}, keep.source=TRUE))
    tag <- 'tag'
    id  <- pd$id

    expect_equal(pd_get_tagged_comment_ids(pd, tag, TRUE ),   15L      )
    expect_equal(pd_get_tagged_comment_ids(pd, tag, FALSE), c(15L, 21L))
}

#' Get the content of a tag
#'
#' @inheritParams pd_has_tag
#'
#' @param all.contiguous if TRUE get all comments connected to this element.
#' @export
pd_get_comment_tag_content <- function(id, pd, tag, all.contiguous=FALSE){
    stopifnot( all(is_doc_comment(id, pd))
             , all(pd_has_tag(id, pd, tag))
             )
    if (all.contiguous){
        ids <- id
        repeat{
            ns <- next_sibling(id)
            if (token(id) != token(ns)) break
            if (has_tag(ns)) break
            if (start_line(ns) != end_line(id) + 1L) break
            ids <- c(ids, ns)
            id <- ns
        }
        trimws(strip_doc_comment_leads(strip_tag(text(ids), tag)))
    } else {
        trimws(strip_doc_comment_leads(strip_tag(text(id), tag)))
    }
}
if(FALSE){#@testing
    pd  <- parsetools::get_parse_data(parse(text={"
        if(FALSE){#@block block content
            #' @first first content
            #' @second second content
            #' not part of second content.
            #' @last
        }
    "}, keep.source=TRUE))
    expect_equal(sum(has_tag()), 4L)


    block.id <- pd_get_tagged_comment_ids(pd, 'block')
    expect_identical( pd_get_comment_tag_content(block.id, pd, 'block')
                    , "block content")
    expect_error( pd_get_comment_tag_content(block.id, pd, 'invalid'))

    first.id <- pd_get_tagged_comment_ids(pd, 'first')
    expect_identical( pd_get_comment_tag_content(first.id, pd, 'first')
                    , "first content")

    second.id <- pd_get_tagged_comment_ids(pd, 'second')
    expect_identical( pd_get_comment_tag_content(second.id, pd, 'second')
                    , "second content")
    expect_identical( pd_get_comment_tag_content(second.id, pd, 'second', all.contiguous = TRUE)
                    ,c( "second content", "not part of second content."))

    last.id <- pd_get_tagged_comment_ids(pd, 'last')
    expect_identical( pd_get_comment_tag_content(last.id, pd, 'last')
                    , "")
}

if(FALSE){#@testing edge cases
    pd  <- parsetools::get_parse_data(parse(text={"
        f <- function(){
        #' @testtag comment lines

        #' That aren't contiguous.
        #' because they are separated by a blank line.

        #' @testtag2 These are contiguous
        #'
        #' Because the line separating them is
        #' a documentation comment itself.
        print('hello world')
        #' and testtag2 ends due to an expression.
        }
    "}, keep.source=TRUE))
    expect_equal(sum(has_tag()), 2L)

    id <- pd_get_tagged_comment_ids(pd, 'testtag')
    expect_identical( pd_get_comment_tag_content(id, pd, 'testtag', TRUE)
                    , "comment lines")

    id <- pd_get_tagged_comment_ids(pd, 'testtag2')
    expect_identical( pd_get_comment_tag_content(id, pd, 'testtag2', TRUE)
                    , c( "These are contiguous"
                       , ""
                       , "Because the line separating them is"
                       , "a documentation comment itself."
                       ))
}

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
#' @inheritParams get_child_ids
#' @param tag tag to test for
#' @param ... options passed on
#' @export
has_tag <- 
function( pd, tag, id = pd$id, ...){
    #' @description
    #' 
    #' Check if a node of \code{parse-data} identified by \code{id}
    #' is both a comment and contains a documentation tag itentifed by
    #' the `@` symbol.
    tag.rx <- make_tag_regex(tag, ...)
    is_comment(pd, id) & grepl(tag.rx, text(id, pd), perl=TRUE, ignore.case=TRUE)
}
if(FALSE){#!@testing
    fun <- function(object){
        #! function with only comment lines
        #!       @tag   TRUE
        #!      @@tag   FALSE
        #! @notag{@tag}@ FALSE
        #        @tag   TRUE, even though a regular comment    
        object @tag
        NULL
    }
    pd  <- parsetools::get_parse_data(fun)
    tag <- 'tag'
    id <- pd$id
    expect_equal(sum(has_tag(pd, tag)), 2)
}

clean_tag_comments <- 
function( x
        , tag
        ){
    tag <- paste0("(", paste(tag, collapse="|"), ")")
    gsub(paste0("^#@", tag, "\\b"), "#! @\\1", x)
}
if(FALSE){#!@testing
    expect_equal( clean_tag_comments("#@testing", "testing")
                , "#! @testing"
                )
}

#' @export
strip_tag <-
function( x     #< text to strip from
        , tag   #< tag to remove
        , ...   #< options
        ){
    #! remove a tag that identified a line.
    pattern <- paste0(make_tag_regex(tag, ...), '\\s*')
    x <- clean_tag_comments(x, tag)
    
    gsub( pattern=pattern, replacement='', x
        , perl=TRUE, ignore.case=TRUE)
    #< text with the @ tag removed.
}
if(FALSE){#! @testthat
    expect_equal( strip_tag("@tag should be removed", 'tag')
                , "should be removed")
    expect_equal( strip_tag("@nomd{@tag}@ should not be removed", 'tag')
                , "@nomd{@tag}@ should not be removed")
    expect_equal( strip_tag("@@tag should not be removed.", 'tag')
                , "@@tag should not be removed.")
}

#' @export
get_tagged_comment_ids <- 
function( pd, tag
        , doc.only = TRUE #< Restrict to documentation comments only?
        ){
    ids <- if (doc.only)
        pd[is_doc_comment(pd), 'id']
    else
        pd[is_comment(pd), 'id']
    ids[has_tag(pd, tag, ids)]
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
    "}))
    tag <- 'tag'
    id  <- pd$id
    
    expect_equal(get_tagged_comment_ids(pd, tag, TRUE ),   15L      )
    expect_equal(get_tagged_comment_ids(pd, tag, FALSE), c(15L, 21L))
}







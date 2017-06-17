make_tag_regex <- 
function( tag              #< tag for pattern
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

has_tag <- 
function( pd, tag, id = pd$id, ...){
    tag.rx <- make_tag_regex(tag, ...)
    is_comment(pd, id) & grepl(tag.rx, text(id, pd), perl=TRUE)
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

    tagged <- get_tagged_lines(pd, 'tag')
    expect_equal(nrow(tagged), 1)
    expect_true("#! @tag should extract" %in% tagged$text)
    expect_false("#! @notag{@tag}@ should not extract" %in% tagged$text)
    expect_false("#! @notag{@tag}@ should not extract." %in% tagged$text)
}

#' @export
strip_tag <-
function( x     #< text to strip from
        , tag   #< tag to remove
        , ...   #< options
        ){
    #! remove a tag that identified a line.
    pattern <- paste0(make_tag_regex(tag, ...), '\\s*')
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
    id  <- pd$id
    
    expect_equal(get_tagged_comment_ids(pd, tag, TRUE ),   13L      )
    expect_equal(get_tagged_comment_ids(pd, tag, FALSE), c(13L, 19L))
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





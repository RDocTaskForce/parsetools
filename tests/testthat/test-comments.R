#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `comments.R`')
#line 48 "R/comments.R"
test_that('classify_comment.character', {#@testing
    expect_equal(classify_comment.character("## normal comment       "), "NORMAL_COMMENT")
    expect_equal(classify_comment.character("#' Roxygen comment      "), "ROXYGEN_COMMENT")
    expect_equal(classify_comment.character("#! Documentation comment"), "DOC_COMMENT")
    expect_equal(classify_comment.character("#< Relative comment     "), "RELATIVE_COMMENT")
    expect_equal(classify_comment.character("#^ Continuation comment "), "CONTINUATION_COMMENT")
    expect_equal(classify_comment.character("#@ Tag comment          "), "TAG_COMMENT")

    expect_equal(classify_comment.character("1"), "")
})
#line 64 "R/comments.R"
test_that('classify_comment.data.frame', {#@testing
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
})
#line 85 "R/comments.R"
test_that('classify_comment', {#@testing
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
})
#line 123 "R/comments.R"
test_that('is_comment', {#!@testing
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
})
#line 162 "R/comments.R"
test_that('is_relative_comment', {#@testing
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
})
#line 193 "R/comments.R"
test_that('is_doc_comment', {#@testing
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
})
#line 226 "R/comments.R"
test_that('strip_doc_comment_leads.character', {#@testing
    expect_equal(strip_doc_comment_leads.character("#  normal comment       "), "#  normal comment")
    expect_equal(strip_doc_comment_leads.character("#' Roxygen comment      "), "Roxygen comment")
    expect_equal(strip_doc_comment_leads.character("#! Documentation comment"), "Documentation comment")
    expect_equal(strip_doc_comment_leads.character("#< Relative comment     "), "Relative comment")
    expect_equal(strip_doc_comment_leads.character("#^ Continuation comment "), "Continuation comment")
    expect_equal(strip_doc_comment_leads.character("#@ Tag comment          "), "Tag comment")
})
#line 242 "R/comments.R"
test_that('strip_doc_comment_leads.data.frame', {#@testing
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
})
#line 274 "R/comments.R"
test_that('strip_doc_comment_leads', {#@testing
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
})

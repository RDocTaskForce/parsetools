#! This file was automatically produced by lint on  2017-06-06 12:03:28
#! changes will be overwritten.
context('tests extracted from file `./R/comments.R`')
test_that("'classify_comment.character'", {#! @testing
    expect_equal(classify_comment.character("## normal comment       "), "NORMAL_COMMENT")
    expect_equal(classify_comment.character("#' Roxygen comment      "), "ROXYGEN_COMMENT")
    expect_equal(classify_comment.character("#! Documentation comment"), "DOC_COMMENT")
    expect_equal(classify_comment.character("#< Relative comment     "), "RELATIVE_COMMENT")
    expect_equal(classify_comment.character("#^ Continuation comment "), "CONTINUATION_COMMENT")
    expect_equal(classify_comment.character("#@ Tag comment          "), "TAG_COMMENT")
    
    expect_equal(classify_comment.character("1"), "")
})
test_that("'classify_comment.data.frame'", {#! @testing
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
})
test_that("'classify_comment'", {#! @testing
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
})
test_that("'is_comment.data.frame'", {#!@testing 
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
})
test_that("'is_doc_comment.data.frame'", {#! @testing
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
})
test_that("'get_associated_continuation'", {#! @testing
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
})
test_that("'strip_doc_comment_leads.character'", {#! @testing
    expect_equal(strip_doc_comment_leads.character("#  normal comment       "), "#  normal comment")
    expect_equal(strip_doc_comment_leads.character("#' Roxygen comment      "), "Roxygen comment")
    expect_equal(strip_doc_comment_leads.character("#! Documentation comment"), "Documentation comment")
    expect_equal(strip_doc_comment_leads.character("#< Relative comment     "), "Relative comment")
    expect_equal(strip_doc_comment_leads.character("#^ Continuation comment "), "Continuation comment")
    expect_equal(strip_doc_comment_leads.character("#@ Tag comment          "), "Tag comment")
})
test_that("'strip_doc_comment_leads.data.frame'", {#! @testing
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
})
test_that("'strip_doc_comment_leads'", {#! @testing
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
})

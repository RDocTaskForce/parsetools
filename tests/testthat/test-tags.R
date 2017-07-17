#! This file was automatically produced by documentation::extract_tests on  2017-07-08 09:16:20
#! changes will be overwritten.
context('tests extracted from file `C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/tags.R`')
#line 56 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/tags.R"
test_that("has_tag", {#!@testing
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
})
#line 79 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/tags.R"
test_that("clean_tag_comments", {#!@testing
    expect_equal( clean_tag_comments("#@testing", "testing")
                , "#! @testing"
                )
})
#line 99 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/tags.R"
test_that("strip_tag", {#! @testthat
    expect_equal( strip_tag("@tag should be removed", 'tag')
                , "should be removed")
    expect_equal( strip_tag("@nomd{@tag}@ should not be removed", 'tag')
                , "@nomd{@tag}@ should not be removed")
    expect_equal( strip_tag("@@tag should not be removed.", 'tag')
                , "@@tag should not be removed.")
})
#line 119 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/tags.R"
test_that("get_tagged_comment_ids", {#!@testing
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
})

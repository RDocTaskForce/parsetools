#! This file was automatically produced by lint on  2017-06-06 12:03:28
#! changes will be overwritten.
context('tests extracted from file `/mnt/data/projects/rdtf/parsetools/R/tags.R`')
test_that("'has_tag'", {#!@testing
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
})
test_that("'strip_tag'", {#! @testthat
    expect_equal( strip_tag("@tag should be removed", 'tag')
                , "should be removed")
    expect_equal( strip_tag("@nomd{@tag}@ should not be removed", 'tag')
                , "@nomd{@tag}@ should not be removed")
    expect_equal( strip_tag("@@tag should not be removed.", 'tag')
                , "@@tag should not be removed.")
})
test_that("'get_tagged_comment_ids'", {#!@testing
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
})

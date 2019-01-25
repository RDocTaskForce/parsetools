#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `tags.R`')
#line 81 "R/tags.R"
test_that('pd_has_tag', {#!@testing
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
})
#line 102 "R/tags.R"
test_that('has_tag', {#@testing
    pd  <- parsetools::get_parse_data(parse(text={"
        if(FALSE){#@block block content
            #' @first first content
            #' @second second content
            #' not part of second content.
            #' @last
        }
    "}, keep.source=TRUE))
    expect_equal(sum(has_tag()), 4L)
})
#line 126 "R/tags.R"
test_that('clean_tag_comments', {#!@testing
    expect_equal( clean_tag_comments("#@testing", "testing")
                , "#! @testing"
                )
})
#line 151 "R/tags.R"
test_that('strip_tag', {#! @testthat
    expect_equal( strip_tag("@tag should be removed", 'tag')
                , "should be removed")
    expect_equal( strip_tag("@nomd{@tag}@ should not be removed", 'tag')
                , "@nomd{@tag}@ should not be removed")
    expect_equal( strip_tag("@@tag should not be removed.", 'tag')
                , "@@tag should not be removed.")
})
#line 178 "R/tags.R"
test_that('pd_get_tagged_comment_ids', {#!@testing
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
})
#line 220 "R/tags.R"
test_that('pd_get_comment_tag_content', {#@testing
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
})

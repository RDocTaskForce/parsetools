#! This file was automatically produced by the documentation package.
#! Changes will be overwritten.

context('tests extracted from file `tags.R`')
#line 81 "/rdtf/parsetools/R/tags.R"
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
#line 110 "/rdtf/parsetools/R/tags.R"
test_that('clean_tag_comments', {#!@testing
    expect_equal( clean_tag_comments("#@testing", "testing")
                , "#! @testing"
                )
})
#line 135 "/rdtf/parsetools/R/tags.R"
test_that('strip_tag', {#! @testthat
    expect_equal( strip_tag("@tag should be removed", 'tag')
                , "should be removed")
    expect_equal( strip_tag("@nomd{@tag}@ should not be removed", 'tag')
                , "@nomd{@tag}@ should not be removed")
    expect_equal( strip_tag("@@tag should not be removed.", 'tag')
                , "@@tag should not be removed.")
})
#line 162 "/rdtf/parsetools/R/tags.R"
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

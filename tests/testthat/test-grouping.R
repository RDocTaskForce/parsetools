#! This file was automatically produced by documentation::extract_tests on  2017-07-08 09:16:20
#! changes will be overwritten.
context('tests extracted from file `C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/grouping.R`')
#line 49 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/grouping.R"
test_that("is_grouping", {#! @testing
    pd <- get_parse_data(parse(text='{
        this(is+a-grouping)
    }'))
    expect_true (is_grouping(pd, 25))
    expect_false(is_grouping(pd,  1))
    
    expect_is(is_grouping(pd), 'logical')
    expect_equal(sum(is_grouping(pd)), 1)
})
#line 62 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/grouping.R"
test_that("get_groupings", {#! @testing
    pd <- get_parse_data(parse(text='{
        this(is+a-grouping)
    }'))
    
    expect_is(get_groupings(pd), 'integer')
    expect_equal(length(get_groupings(pd)), 1)
    expect_equal(get_groupings(pd), 25)
})
#line 89 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/grouping.R"
test_that("fix_grouping_comment_association", {#!@testing
    pd <- get_parse_data(parse(text={"
    {# grouped code
        # normal comment
        #' Documenation before
        hw <- function(){
            #! documentation comment inside.
            print('hello world')
        }
    }
    {# Second Group
        1+2
    }
    # Comment 3
    4+5
    "}))
    fixed <- fix_grouping_comment_association(pd)
    
    expect_identical(fixed[-6], pd[-6])
    expect_equal(get_comments(fixed)$parent, c(-38, -38, -38, 34, -56, -74))
})

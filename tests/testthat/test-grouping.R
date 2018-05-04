#! This file was automatically produced by documentation::extract_tests on  2018-05-04 11:22:55
#! changes will be overwritten.
context('tests extracted from file `grouping.R`')
#line 49 "/home/aredd/projects/rdtf/parsetools/R/grouping.R"
test_that('is_grouping', {#! @testing
    pd <- get_parse_data(parse(text='{
        this(is+a-grouping)
    }'))
    expect_true (is_grouping(25, pd))
    expect_false(is_grouping( 1, pd))
    
    expect_is(is_grouping(pd=pd), 'logical')
    expect_equal(sum(is_grouping(pd=pd)), 1)
})
#line 66 "/home/aredd/projects/rdtf/parsetools/R/grouping.R"
test_that('get_groupings', {#! @testing
    pd <- get_parse_data(parse(text='{
        this(is+a-grouping)
    }'))
    
    expect_is(get_groupings(pd), 'integer')
    expect_equal(length(get_groupings(pd)), 1)
    expect_equal(get_groupings(pd), 25)
})
#line 95 "/home/aredd/projects/rdtf/parsetools/R/grouping.R"
test_that('fix_grouping_comment_association', {#!@testing
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
    fixed <- fix_grouping_comment_association(pd=pd)
    
    expect_identical(fixed[-6], pd[-6])
    expect_equal(get_comments(fixed)$parent, c(-38, -38, -38, 34, -56, -74))
})

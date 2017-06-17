#! This file was automatically produced by lint on  2017-06-06 12:03:28
#! changes will be overwritten.
context('tests extracted from file `/mnt/data/projects/rdtf/parsetools/R/iff_blocks.R`')
test_that("'is_iff_block'", {#!@testing
    pd <- get_parse_data(parse(text={"
        if(FALSE){# an if(FALSE) block
        
        }
        if(F){# also an if(FALSE) block
        }
        {# not an if(F)block
        }
    "}))
    id <- all_root_ids(pd)
    
    expect_true(is_iff_block(pd, id[[1]]))
    expect_true(is_iff_block(pd, id[[2]]))
    expect_false(is_iff_block(pd, id[[2]], FALSE))
    expect_false(is_iff_block(pd, id[[3]]))
    expect_equal(is_iff_block(pd, id), c(TRUE, TRUE, FALSE))
    expect_equal(is_iff_block(pd), c(TRUE, TRUE, FALSE))
})
test_that("'all_iff_ids'", {#!@testing
    pd <- get_parse_data(parse(text={"
        if(FALSE){# an if(FALSE) block
        
        }
        if(F){# also an if(FALSE) block
        }
        {# grouping block
            if(F){# iff nested in group
            
            }
        }
        hw <- function(){
            if(F){# nested in a function
                
            }
            print('hello world')
        }
    "}))
    iff.ids <- all_iff_ids(pd, root.only=TRUE, ignore.groups = FALSE)
    expect_equal(length(iff.ids), 2)
    
    iff.ids <- all_iff_ids(pd, root.only=TRUE, ignore.groups = TRUE)
    expect_equal(length(iff.ids), 3)
    
    iff.ids <- all_iff_ids(pd, root.only=FALSE, ignore.groups = FALSE)
    expect_equal(length(iff.ids), 4)
})
test_that("'if structures'", {#!@testing if structures
    pd <- get_parse_data(parse(text={"
        if(predicate){
            body
        } else {
            alternate
        }
    "}))
    token(id, pd)
    id <- all_root_ids(pd) # 33
    
    expect_true(is_if_expr(pd, id))
    expect_equal(get_if_predicate_id(pd, id),  7L)
    expect_equal(get_if_branch_id   (pd, id), 18L)
    expect_equal(get_if_alternate_id(pd, id), 30L)
})

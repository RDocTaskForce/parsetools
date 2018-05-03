#! This file was automatically produced by documentation::extract_tests on  2018-05-03 10:17:17
#! changes will be overwritten.
context('tests extracted from file `/home/aredd/projects/rdtf/parsetools/R/iff_blocks.R`')
#line 68 "/home/aredd/projects/rdtf/parsetools/R/iff_blocks.R"
test_that('is_iff_block', {#!@testing
    pd <- get_parse_data(parse(text={"
        if(FALSE){# an if(FALSE) block
        
        }
        if(F){# also an if(FALSE) block
        }
        {# not an if(F)block
        }
    "}))
    id <- all_root_ids(pd)
    
    expect_true (is_iff_block(id[[1]], pd))
    expect_true (is_iff_block(id[[2]], pd))
    expect_false(is_iff_block(id[[2]], pd, FALSE))
    expect_false(is_iff_block(id[[3]], pd))
    expect_equal(is_iff_block(id, pd), c(TRUE, TRUE, FALSE))
    expect_equal(is_iff_block(pd=pd), c(TRUE, TRUE, FALSE))
})
#line 116 "/home/aredd/projects/rdtf/parsetools/R/iff_blocks.R"
test_that('all_iff_ids', {#!@testing
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
#line 180 "/home/aredd/projects/rdtf/parsetools/R/iff_blocks.R"
test_that('iff_is_tagged', {#!@testing
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
    expect_true (iff_is_tagged(id[[1]], tag, pd))
    expect_true (iff_is_tagged(id[[3]], tag, pd, FALSE))
    expect_false(iff_is_tagged(id[[3]], tag, pd, TRUE ))
    expect_false(iff_is_tagged(id[[6]], tag, pd))
    expect_equal(iff_is_tagged(id, tag, pd)
                , c(T,T,F,F,F,F))
    expect_equal(iff_is_tagged(id, tag, pd, FALSE)
                , c(T,T,T,F,F,F))
                
    pd <- get_parse_data(parse(text='rnorm(1)'))
    expect_false(iff_is_tagged(all_root_ids(pd), tag, pd))            
    
    pd <- get_parse_data(parse(text='if(F)#!@tag not in block\nF'))
    expect_false(iff_is_tagged(all_root_ids(pd), tag, pd))            
    
    pd <- get_parse_data(parse(text='if(F){FALSE}'))
    expect_false(iff_is_tagged(all_root_ids(pd), tag, pd))            
    
    pd <- get_parse_data(parse(text='if(F){# @tag\nF\n}'))
    expect_false(iff_is_tagged(all_root_ids(pd), tag, pd))            
    
    pd <- get_parse_data(parse(text='if(F){#@tag\nF\n}'))
    expect_true(iff_is_tagged(all_root_ids(pd), tag, pd))    
})
#line 244 "/home/aredd/projects/rdtf/parsetools/R/iff_blocks.R"
test_that('all_tagged_iff_ids', {#!@testing
    pd  <- get_parse_data(parse(text={"
        if(FALSE){#!@tag
            # yes
        }
        if(F){#@tag
            # yes
        }
        if(F){# @tag
            # determines doc.only parameter
        }
        {#!@tag 
            # not an if(F) block
        }
        {#@tag
            # no
        }
        {# @tag
            # no
        }
        "}))
    tag <- 'tag'
    id  <- all_root_ids(pd)
    tagged.iff.ids <- all_tagged_iff_ids(pd, tag)

    pd  <- get_parse_data(parse(text={"
        # this has no iff blocks
        "}))
    tag <- 'tag'
    tagged.iff.ids <- all_tagged_iff_ids(pd, tag)
    expect_identical(tagged.iff.ids, integer(0))
})
#line 366 "/home/aredd/projects/rdtf/parsetools/R/iff_blocks.R"
test_that('get_iff_associated_name', {#!@testing
    pd <- get_parse_data(parse(text={'
    if(F){#!@testing
        # a malplaced testing block
        FALSE
    }
    hello_world <- function(){
        print("hello world")
    }
    if(FALSE){#!@testthat
        expect_output(hello_world(), "hello world")
    }
    
    ldf <- data.frame(id = 1:26, letters)
    if(FALSE){#!@testing
        # not a function assignment
    }

    f2 <- function(){stop("this does nothing")}
    if(F){#! @example
        hw()
    }
    if(F){#! @test
        expect_error(f2())
    }
    
    setClass("A")
    if(F){#!@testing 
        #testing a setClass
    }
    
    setMethod("print", "A")
    if(F){#!@testing 
        #testing a setMethod
    }
    
    setGeneric("my_generic", function(x){x})
    if(F){#!@testing 
        #testing a setClass
    }
    
    rnorm(10)
    if(F){#!@testing
        # no previous name
    }
    '}))
    iff.ids <- all_tagged_iff_ids(pd, c('testing', 'testthat', 'test'))
    
    expect_null( get_iff_associated_name(iff.ids[[1L]], pd), info="iff at beginning")
    expect_equal( get_iff_associated_name(iff.ids[[2L]], pd)
                , structure("hello_world", type = "function_assignment")
                , info="iff after function assignment")
    expect_equal( get_iff_associated_name(iff.ids[[3L]], pd)
                , structure("ldf", type = "assignment")
                , info="iff after other assignment")
    expect_equal( get_iff_associated_name(iff.ids[[4L]], pd)
                , structure("f2", type = "function_assignment")
                , info="iff after other iff")
    expect_equal( get_iff_associated_name(iff.ids[[5L]], pd)
                , structure("A", type = "setClass")
                , info="iff after other iff")
    expect_equal( get_iff_associated_name(iff.ids[[6L]], pd)
                , structure("print.A", type = "setMethod")
                , info="iff after other iff")
    expect_equal( get_iff_associated_name(iff.ids[[7L]], pd)
                , structure("my_generic", type = "setGeneric")
                , info="iff after other iff")
    expect_null ( get_iff_associated_name(iff.ids[[8L]], pd)
                , info="following call")
})

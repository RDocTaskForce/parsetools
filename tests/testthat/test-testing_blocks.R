#! This file was automatically produced by documentation::extract_tests on  2018-05-03 10:17:17
#! changes will be overwritten.
context('tests extracted from file `/home/aredd/projects/rdtf/parsetools/R/testing_blocks.R`')
#line 98 "/home/aredd/projects/rdtf/parsetools/R/testing_blocks.R"
test_that('extract_test_block', {#!@testing
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
    
    expect_error( extract_test_block(pd, iff.ids[[1L]])
                , "illformed block at <text>:2:5"
                , info = "cannot find name for block"
                )

    expect_equal( extract_test_block(pd, iff.ids[[2L]])
                , structure(c( '#line 9 "<text>"'
                             , 'test_that(\'hello_world\', {#!@testthat'
                             , '        expect_output(hello_world(), "hello world")'
                             , '    })'
                             ), name=structure("hello_world", type = "function_assignment"))
                , info="testing after function assignment")
    expect_equal( extract_test_block(pd, iff.ids[[3L]])
                , structure(c( '#line 14 "<text>"'
                             , 'test_that(\'ldf\', {#!@testing'
                             , '        # not a function assignment'
                             , '    })'
                             ), name = structure("ldf", type = "assignment"))
                , info="testing after other assignment")
    expect_equal( extract_test_block(pd, iff.ids[[4L]])
                , structure(c( '#line 22 "<text>"'
                             , 'test_that(\'f2\', {#! @test'
                             , '        expect_error(f2())'
                             , '    })'
                             ), name=structure("f2", type = "function_assignment"))
                , info="testing after other iff")
    expect_equal( extract_test_block(pd, iff.ids[[5L]])
                , structure(c( '#line 27 "<text>"'
                             , 'test_that("setClass(\'A\', ...)", {#!@testing '
                             , '        #testing a setClass'
                             , '    })'
                             ), name="setClass(\'A\', ...)")
                , info="testing after setClass")
    expect_equal( extract_test_block(pd, iff.ids[[6L]])
                , structure(c( '#line 32 "<text>"'
                             , 'test_that(\'print.A\', {#!@testing '
                             , '        #testing a setMethod'
                             , '    })'
                             ), name=structure("print.A", type = "setMethod"))
                , info="testing after setMethod")
    expect_equal( extract_test_block(pd, iff.ids[[7L]])
                , structure(c( '#line 37 "<text>"'
                             , 'test_that("setGeneric(\'my_generic\', ...)", {#!@testing '
                             , '        #testing a setClass'
                             , '    })'
                             ), name="setGeneric('my_generic', ...)")
                , info="testing after setGeneric")
    expect_error( extract_test_block(pd, iff.ids[[8L]])
                , info="following call")
                
    expect_equal( extract_test_block(pd, iff.ids[2:3])
                , structure(c( '#line 9 "<text>"'
                             , 'test_that(\'hello_world\', {#!@testthat'
                             , '        expect_output(hello_world(), "hello world")'
                             , '    })'
                             , '#line 14 "<text>"'
                             , 'test_that(\'ldf\', {#!@testing'
                             , '        # not a function assignment'
                             , '    })'
                             )
                           , test.names = c("hello_world", "ldf")
                           , start.locations = c(1, 5)
                           )    
                , info = "multiple ids")

    pd <- get_parse_data(parse(text={"
        if(FALSE){#@testing An info string
            expect_true(T)
        }
    "}))
    expect_equal( extract_test_block(pd, all_root_ids(pd))
                , structure(c( "#line 2 \"<text>\""
                             , "test_that('An info string', {#@testing An info string"
                             , "            expect_true(T)"
                             , "        })"
                             )
                           , name = "An info string")
                , info = "using text string")
})
#line 251 "/home/aredd/projects/rdtf/parsetools/R/testing_blocks.R"
test_that('extract_test_blocks', {#! @testthat
text <- {'hello_world <- function(){
    print("hello world")
}
if(FALSE){#!@testthat
    expect_output(hello_world(), "hello world")
}

f2 <- function(){stop("this does nothing")}
if(F){#! @test
    expect_error(f2())
}
if(F){#! example
    hw()
}
'}

tmp <- tempfile(fileext = ".R")
writeLines(text, tmp)

test.blocks <- extract_test_blocks(tmp)
expect_equal( test.blocks
            , structure(c( sprintf('#line 4 "%s"', tmp)
                         , 'test_that(\'hello_world\', {#!@testthat'
                         , '    expect_output(hello_world(), "hello world")'
                         , '})'
                         , sprintf('#line 9 "%s"', tmp)
                         , 'test_that(\'f2\', {#! @test'
                         , '    expect_error(f2())'
                         , '})'
                         )
                       , test.names = c("hello_world", "f2")
                       , start.locations = c(1, 5)
                       )    
            , info = "Write to file and read back.")
})

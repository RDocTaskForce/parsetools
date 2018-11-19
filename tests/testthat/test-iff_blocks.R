#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `iff_blocks.R`')
#line 86 "R/iff_blocks.R"
test_that('is_iff', {#!@testing
    pd <- get_parse_data(parse(text={"
        if(FALSE){# an if(FALSE) block

        }
        if(F){# also an if(FALSE) block
        }
        {# not an if(F)block
        }
        if(FALSE) expect_true(TRUE) #< IFF but not a block
    "}, keep.source=TRUE))

    expect_true (pd_is_iff(roots(pd)[[1]], pd))
    expect_true (pd_is_iff(roots(pd)[[2]], pd))
    expect_false(pd_is_iff(roots(pd)[[2]], pd, FALSE))
    expect_false(pd_is_iff(roots(pd)[[3]], pd))
    expect_true (pd_is_iff(roots(pd)[[4]], pd))

    expect_equal(pd_is_iff(roots(pd), pd), c(TRUE, TRUE, FALSE, TRUE))
    expect_equal(   is_iff(pd=pd), c(TRUE, TRUE, FALSE, TRUE))
})
#line 138 "R/iff_blocks.R"
test_that('is_iff_block', {#!@testing
    pd <- get_parse_data(parse(text={"
        if(FALSE){# an if(FALSE) block

        }
        if(F){# also an if(FALSE) block
        }
        {# not an if(F)block
        }
        if(FALSE) expect_true(TRUE) #< IFF but not a block
    "}, keep.source=TRUE))

    expect_true (pd_is_iff_block(roots(pd)[[1]], pd))
    expect_true (pd_is_iff_block(roots(pd)[[2]], pd))
    expect_false(pd_is_iff_block(roots(pd)[[2]], pd, FALSE))
    expect_false(pd_is_iff_block(roots(pd)[[3]], pd))
    expect_false(pd_is_iff_block(roots(pd)[[4]], pd))
    expect_equal(pd_is_iff_block(roots(pd), pd), c(TRUE, TRUE, FALSE, FALSE))
    expect_equal(pd_is_iff_block(roots(pd), pd, FALSE), c(TRUE, FALSE, FALSE, FALSE))
    expect_equal(   is_iff_block(pd=pd), c(TRUE, TRUE, FALSE, FALSE))
})
#line 180 "R/iff_blocks.R"
test_that('all_iff_block_ids', {#!@testing
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
    "}, keep.source=TRUE))
    iff.ids <- all_iff_block_ids(pd, root.only=TRUE, ignore.groups = FALSE)
    expect_equal(length(iff.ids), 2)

    iff.ids <- all_iff_block_ids(pd, root.only=TRUE, ignore.groups = TRUE)
    expect_equal(length(iff.ids), 3)

    iff.ids <- all_iff_block_ids(pd, root.only=FALSE, ignore.groups = FALSE)
    expect_equal(length(iff.ids), 4)
})
#line 235 "R/iff_blocks.R"
test_that('pd_is_tagged_iff_block', {#!@testing
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
        if(FALSE)#@tag not valid
            FALSE
        "}, keep.source=TRUE))
    tag <- 'tag'
    id  <- roots(pd)
    expect_equal(length(id), 7)
    expect_true (pd_is_tagged_iff_block(id[[1]], pd, tag))
    expect_true (pd_is_tagged_iff_block(id[[3]], pd, tag, FALSE))
    expect_false(pd_is_tagged_iff_block(id[[3]], pd, tag, TRUE ))
    expect_false(pd_is_tagged_iff_block(id[[6]], pd, tag))
    expect_false(pd_is_tagged_iff_block(id[[7]], pd, tag))
    expect_equal(pd_is_tagged_iff_block(id, pd, tag)
                , c(T,T,F,F,F,F,F))
    expect_equal(pd_is_tagged_iff_block(id, pd, tag, FALSE)
                , c(T,T,T,F,F,F,F))

    pd <- get_parse_data(parse(text='rnorm(1)', keep.source=TRUE))
    expect_false(pd_is_tagged_iff_block(roots(pd), pd, tag))

    pd <- get_parse_data(parse(text='if(F)#!@tag not in block\nF', keep.source=TRUE))
    expect_false(pd_is_tagged_iff_block(roots(pd), pd, tag))

    pd <- get_parse_data(parse(text='if(F){FALSE}', keep.source=TRUE))
    expect_false(pd_is_tagged_iff_block(roots(pd), pd, tag))

    pd <- get_parse_data(parse(text='if(F){# @tag\nF\n}', keep.source=TRUE))
    expect_false(pd_is_tagged_iff_block(roots(pd), pd, tag))

    pd <- get_parse_data(parse(text='if(F){#@tag\nF\n}', keep.source=TRUE))
    expect_true(pd_is_tagged_iff_block(roots(pd), pd, tag))
})
#line 302 "R/iff_blocks.R"
test_that('all_tagged_iff_block_ids', {#!@testing
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
        "}, keep.source=TRUE))
    tag <- 'tag'
    id  <- roots(pd)
    tagged.iff.ids <- all_tagged_iff_block_ids(pd, tag)

    pd  <- get_parse_data(parse(text={"
        # this has no iff blocks
        "}, keep.source=TRUE))
    tag <- 'tag'
    tagged.iff.ids <- all_tagged_iff_block_ids(pd, tag)
    expect_identical(tagged.iff.ids, integer(0))
})
#line 481 "R/iff_blocks.R"
test_that('iff_associated_name', {#!@testing
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

    setMethod("fun", c("A","B"), function(x,y){
        x+y
    })
    if(F){#!@testing
        #testing a setMethod with multiple signature elements.
    }

    setAs("class1", "class2", function(from){new(from[[1]], "class2")})
    if(F){#!@testing
        #testing setAs
    }
    '}, keep.source=TRUE))
    iff.ids <- all_tagged_iff_block_ids(pd, c('testing', 'testthat', 'test'))

    expect_null( pd_get_iff_associated_name_id(iff.ids[[1L]], pd), info="iff at beginning")
    expect_equal( pd_get_iff_associated_name_id(iff.ids[[2L]], pd)
                , structure("hello_world", type = "function_assignment")
                , info="iff after function assignment")
    expect_equal( pd_get_iff_associated_name_id(iff.ids[[3L]], pd)
                , structure("ldf", type = "assignment")
                , info="iff after other assignment")
    expect_equal( pd_get_iff_associated_name_id(iff.ids[[4L]], pd)
                , structure("f2", type = "function_assignment")
                , info="iff after other iff")
    expect_equal( pd_get_iff_associated_name_id(iff.ids[[5L]], pd)
                , structure("A", type = "setClass")
                , info="iff after other iff")
    expect_equal( pd_get_iff_associated_name_id(iff.ids[[6L]], pd)
                , structure("print,A-method", type = "setMethod")
                , info="iff after other iff")
    expect_equal( pd_get_iff_associated_name_id(iff.ids[[7L]], pd)
                , structure("my_generic", type = "setGeneric")
                , info="iff after other iff")
    expect_null ( pd_get_iff_associated_name_id(iff.ids[[8L]], pd)
                , info="following call")
    expect_equal( pd_get_iff_associated_name_id(iff.ids[[9L]], pd)
                , structure("fun,A,B-method", type = "setMethod")
                , info="iff after other iff")
    expect_equal( pd_get_iff_associated_name_id(iff.ids[[10L]], pd)
                , structure("coerce,class1,class2-method", type = "setAs"
                           , from='class1', to='class2' )
                , info="setAs")
})

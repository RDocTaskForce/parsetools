#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `pd_classes.R`')
#line 200 "R/pd_classes.R"
test_that('pd_class_definitions', {#@testing pd_class_definitions
    expect_identical(pd_class_definitions$has(), logical(0))
    expect_true(pd_class_definitions$has('setClass'))
    expect_true(pd_class_definitions$has('setRefClass'))
    expect_false(pd_class_definitions$has('DefineANewClass'))
    expect_identical( pd_class_definitions$has(c('setClass', 'setRefClass'))
                    , c('setClass'=TRUE, 'setRefClass'=TRUE)
                    )
    expect_false(pd_class_definitions$has('not a class definition'))

    expect_false( pd_class_definitions$has('my_custom_class_definer'))
    expect_error( pd_class_definitions$add('my_custom_class_definer')
                , "function `my_custom_class_definer` not found."
                )
    expect_true(pd_class_definitions$add('my_custom_class_definer', .exists=FALSE))
    expect_true(pd_class_definitions$has('my_custom_class_definer'))
    expect_error( pd_class_definitions$add('my_custom_class_definer', .exists=FALSE)
                , "`my_custom_class_definer` already has a testing function defined."
                )

    pd <- get_parse_data(parse(text={"
        setClass('S4Test')
        setRefClass('RefTest')
        my_custom_class_definer('CustomTest')
    "}, keep.source=TRUE))

    roots <- roots(pd)

    names.of.definers <- pd_class_definitions$names()
    expect_identical( names.of.definers
                    , c("my_custom_class_definer", "setClass", "setRefClass")
                    )

    results.1.is <- pd_class_definitions$test_is(roots[[1]], pd=pd)
    expect_identical( results.1.is[names.of.definers]
                    , c("my_custom_class_definer"=FALSE, "setClass"=TRUE, "setRefClass"=FALSE)
                    )

    results.is <- pd_class_definitions$test_is(roots, pd=pd)
    expect_identical( dim(results.is), c(3L,3L) )
    expect_equal(rownames(results.is), as.character(roots) )
    expect_equal(sort(colnames(results.is)), sort(names.of.definers))


    expect_true(pd_class_definitions$test_is_in(.find_text("'S4Test'", pd), pd)['setClass'])
    expect_false(all(pd_class_definitions$test_is_in(.find_text("'S4Test'", pd), pd)))

    expect_true(pd_class_definitions$test_is_in(.find_text("'CustomTest'", pd), pd)['my_custom_class_definer'])
    expect_false(all(pd_class_definitions$test_is_in(.find_text("'CustomTest'", pd), pd)))

    expect_identical(pd_class_definitions$which(roots[[1]], pd), "setClass")
    expect_identical(pd_class_definitions$which(.find_text("'S4Test'", pd), pd), character(0))

    expect_identical(pd_class_definitions$in_which(roots[[1]], pd), "setClass")
    expect_identical(pd_class_definitions$in_which(.find_text("'S4Test'", pd), pd), "setClass")

    expect_null(pd_class_definitions$rm('my_custom_class_definer'))
    expect_false( pd_class_definitions$has('my_custom_class_definer'))
    expect_warning( pd_class_definitions$rm('my_custom_class_definer')
                  , "object 'my_custom_class_definer' not found"
                  )

    expect_error( pd_class_definitions$add_definition('another_custom')
                , "function `another_custom` not found."
                )

    expect_error( pd_class_definitions$add_definition('another_custom'
                        , test.is = NULL
                        , test.in = NULL
                        , .exists=FALSE)
                , "test.is must be a function."
                )
    expect_error( pd_class_definitions$add_definition('another_custom'
                        , test.is = function(){}
                        , test.in = NULL
                        , .exists=FALSE)
                , "test.in must be a function."
                )
    expect_error( pd_class_definitions$add_definition('another_custom'
                        , test.is = function(){}
                        , test.in = function(){}
                        , .exists=FALSE)
                , paste( "test.is function must accept arguments 'id' and 'pd'"
                       , "as the first two arguments."
                       )
                )
    expect_error( pd_class_definitions$add_definition('another_custom'
                        , test.is = function(id, pd){}
                        , test.in = function(){}
                        , .exists=FALSE)
                , "test.is function must accept argument .check or extra arguments `...`"
                )
    expect_error( pd_class_definitions$add_definition('another_custom'
                        , test.is = function(){}
                        , test.in = function(){}
                        , .exists=FALSE)
                , paste( "test.is function must accept arguments 'id' and 'pd'"
                       , "as the first two arguments."
                       )
                )
    expect_error( pd_class_definitions$add_definition('another_custom'
                        , test.is = function(id, pd, ...){return(TRUE)}
                        , test.in = function(id, pd){}
                        , .exists=FALSE)
                , "test.in function must accept argument .check or extra arguments `...`"
                )
    expect_error( pd_class_definitions$add_definition('another_custom'
                        , test.is = function(id, pd, ...){return(TRUE)}
                        , test.in = function(){}
                        , .exists=FALSE)
                , paste( "test.in function must accept arguments 'id' and 'pd'"
                       , "as the first two arguments."
                       )
                )
    expect_true( pd_class_definitions$add_definition('another_custom'
                        , test.is = function(id, pd, ...){return(TRUE)}
                        , test.in = function(id, pd, ...){return(TRUE)}
                        , .exists=FALSE)
                )
    expect_true(pd_class_definitions$has('another_custom'))
    expect_null(pd_class_definitions$rm('another_custom'))
    expect_false(pd_class_definitions$has('another_custom'))
})
#line 334 "R/pd_classes.R"
test_that('pd_is_class_definition', {#@test
    pd <- get_parse_data(parse(text='
                setClass( "testClass"
                        , slots = c( x="numeric" #< the x field
                                   , y="matrix"  #< the y field
                                   )
                        )
            ', keep.source=TRUE))
    expect_true(pd_is_class_definition(id = roots(pd), pd))
    expect_false(pd_is_class_definition(id = .find_text('"testClass"', pd), pd))

    pd <- get_parse_data(parse(text='
        setRefClass("mEdit",
                   fields = list( data = "matrix",
                   edits = "list"))
        ', keep.source=TRUE))
    expect_true(pd_is_class_definition(id = roots(pd), pd))
    expect_false(pd_is_class_definition(.find_text("fields", pd), pd))
    expect_identical( pd_is_class_definition(c(roots(pd), .find_text("fields", pd)), pd)
                    , c(TRUE, FALSE)
                    )
})
#line 367 "R/pd_classes.R"
test_that('object in setClass', {#@test object in setClass
    pd <- get_parse_data(parse(text='
                setClass( "testClass"
                        , slots = c( x="numeric" #< the x field
                                   , y="matrix"  #< the y field
                                   )
                        )
                setMethod("print", "testClass", function(){
                    cat("This is just a test.")
                })
            ', keep.source=TRUE))

    root.id <- roots(pd)

    id <- .find_text("#< the x field")

    pd_class_definitions$test_is_in(id, pd)

    expect_true(pd_is_in_class_definition(id, pd))

    id2 <- .find_text('"This is just a test."')

    expect_false(pd_is_in_class_definition(id2, pd))

    pd_class_definitions$test_is_in(c(id, id2), pd)

    expect_identical(pd_is_in_class_definition(c(id, id2), pd), c(TRUE, FALSE))
})
#line 402 "R/pd_classes.R"
test_that('pd_add_class_definition', {#@testing
    test.is <- pd_make_is_call('setTestClass')
    test.in <- pd_make_is_in_call('setTestClass', .is = test.is)
    pd_add_class_definition('setTestClass', test.is, test.in, FALSE, TRUE)

    pd <- get_parse_data(parse(text='
                setTestClass( MyClass, ...)
            ', keep.source=TRUE))
    expect_true(pd_is_class_definition(id = roots(pd), pd))
    expect_false(pd_is_class_definition(id = .find_text('MyClass', pd), pd))
    pd_class_definitions$rm('setTestClass')
})
#line 419 "R/pd_classes.R"
test_that('pd_add_class', {#@testing
    pd_add_class('setAnotherClass', FALSE, TRUE)

    pd <- get_parse_data(parse(text='
                setAnotherClass( "testAnotherClass", ...)
            ', keep.source=TRUE))
    expect_true(pd_is_class_definition(id = roots(pd), pd))
    expect_false(pd_is_class_definition(id = .find_text('"testAnotherClass"', pd), pd))

    pd_class_definitions$rm('setAnotherClass')
})
#line 456 "R/pd_classes.R"
test_that('closest_call', {#@testing
pd <- get_parse_data(parse(text={"
x <- 1
y <- rnorm(10)
z <- function(a,b){
    cat(a,b)
}

testClass <-
    setRefClass('testClass'
               , fields = list( f1 = 'integer'
                              , f2 = function(){
                                        return(f1)
                                    }
                              )
               , methods = list( hw = function(){
                                        print('hello world')
                                    }
                                )
               )
"}, keep.source=TRUE))

roots <- roots(pd)

id.10 <- pd[pd$text == '10','id']
expect_equal(text(call_symbol(pd_get_closest_call_id(id.10, pd=pd), pd=pd)), 'rnorm')

id.hw <- pd[pd$text == "'hello world'", 'id']

expect_equal(text(call_symbol(pd_get_closest_call_id(id.hw, pd=pd))), 'print')
expect_equal(text(call_symbol(pd_get_closest_call_id(id.hw, pd=pd, 'list'))), 'list')
expect_equal(text(call_symbol(pd_get_closest_call_id(id.hw, pd=pd, 'setRefClass'))), 'setRefClass')

expect_true(is.na(pd_get_closest_call_id(id.hw, pd=pd, 'setClass')))
})

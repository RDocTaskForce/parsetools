#! This file was automatically produced by documentation::extract_tests on  2018-05-04 11:22:55
#! changes will be overwritten.
context('tests extracted from file `pd_classes.R`')
#line 5 "/home/aredd/projects/rdtf/parsetools/R/pd_classes.R"
test_that('pd_is_class_definition', {#@test
'setClass( "testClass"
     , slots = c( x="numeric" #< the x field
                , y="matrix"  #< the y field
                )
     )' %>% 
    parse(text = .) %>%
    get_parse_data() -> pd
    
    expect_true(pd_is_class_definition(id = all_root_ids(pd), pd))
})
#line 19 "/home/aredd/projects/rdtf/parsetools/R/pd_classes.R"
test_that('object in setClass', {#@test object in setClass
'setClass( "testClass"
     , slots = c( x="numeric" #< the x field
                , y="matrix"  #< the y field
                )
     )
setMethod("print", "testClass", function(){
    cat("This is just a test.")
})
' %>% 
    parse(text = .) %>%
    get_parse_data() -> pd

    root.id <- all_root_ids(pd)
    
    id <- pd[pd$text=="#< the x field", 'id']
    
    expect_true(pd_is_in_class_definition(id, pd))
    
    id2 <- pd[pd$text=='"This is just a test."', 'id']
    expect_false(pd_is_in_class_definition(id2, pd))
    
    expect_identical(pd_is_in_class_definition(c(id, id2), pd), c(TRUE, FALSE))
})
#line 142 "/home/aredd/projects/rdtf/parsetools/R/pd_classes.R"
test_that('pd_class_definitions', {#@testing pd_class_definitions
    expect_identical(pd_class_definitions$has(), logical(0))
    expect_true(pd_class_definitions$has('setClass'))
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
    "}))
    
    roots <- all_root_ids(pd)
    
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
                , "test.is function must have two and only two arguments" %<<% 
                  "titled 'id' and 'pd', exclusively in that order"
                )
    expect_error( pd_class_definitions$add_definition('another_custom'
                        , test.is = function(id, pd){return(TRUE)}
                        , test.in = function(){}
                        , .exists=FALSE)
                , "test.in function must have two and only two arguments" %<<% 
                  "titled 'id' and 'pd', exclusively in that order"
                )
    expect_true( pd_class_definitions$add_definition('another_custom'
                        , test.is = function(id, pd){return(TRUE)}
                        , test.in = function(id, pd){return(TRUE)}
                        , .exists=FALSE)
                )
    expect_true(pd_class_definitions$has('another_custom'))
    expect_null(pd_class_definitions$rm('another_custom'))
    expect_false(pd_class_definitions$has('another_custom'))
})
#line 245 "/home/aredd/projects/rdtf/parsetools/R/pd_classes.R"
test_that('pd_get_closest_call', {#@testing
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
"}))
    
roots <- all_root_ids(pd)    

id.10 <- pd[pd$text == '10','id']
expect_equal(text(pd_get_call_symbol_id(pd_get_closest_call(id.10, pd=pd), pd=pd)), 'rnorm')

id.hw <- pd[pd$text == "'hello world'", 'id']

expect_equal(text(pd_get_call_symbol_id(pd_get_closest_call(id.hw, pd=pd))), 'print')
expect_equal(text(pd_get_call_symbol_id(pd_get_closest_call(id.hw, pd=pd, 'list'))), 'list')
expect_equal(text(pd_get_call_symbol_id(pd_get_closest_call(id.hw, pd=pd, 'setRefClass'))), 'setRefClass')

expect_true(is.na(pd_get_closest_call(id.hw, pd=pd, 'setClass')))
})

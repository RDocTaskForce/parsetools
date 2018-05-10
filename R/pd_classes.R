#' @include pd_make_is_in.R

.class.defining.functions <- c('setClass', 'setRefClass', 'R6Class')
pd_is_class_definition <- pd_make_is_call(.class.defining.functions)
if(FALSE){#@test
'setClass( "testClass"
     , slots = c( x="numeric" #< the x field
                , y="matrix"  #< the y field
                )
     )' %>%
    parse(text = .) %>%
    get_parse_data() -> pd

    expect_true(pd_is_class_definition(id = all_root_ids(pd), pd))
}

#' @internal
pd_is_in_class_definition <- pd_make_is_in_call(.class.defining.functions)
if(FALSE){#@test object in setClass
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
}


pd_class_definitions <- new.env(hash=TRUE)
local(envir=pd_class_definitions, {
.is    <- new.env(hash=TRUE, parent=emptyenv())
.is_in <- new.env(hash=TRUE, parent=emptyenv())

.call_test <- function(fun, ...)fun(...)

has <- function(name){
    if (missing(name) || length(name) == 0)
        return(logical(0))
    if (length(name) > 1L)
        return(sapply(name, has))
    exists(name, envir=.is, mode='function', inherits=FALSE)
}
add <- function(name, .exists=TRUE, .overwrite=FALSE){
    if (length(name) > 1L)
        return(invisible(structure( lapply( name, add
                                          , .exists=.exists
                                          , .overwrite=.overwrite
                                          )
                                  , names = name
                                  )))
    if (.exists && !exists(name))
        stop("function `", name, "` not found.")
    if (!.overwrite && has(name))
        stop("`", name, "` already has a testing function defined.")
    test.is <- pd_make_is_call(name)
    test.in <- pd_make_is_in_call(name, .is = test.is)
    assign(name, test.is, envir = .is)
    assign(name, test.in, envir = .is_in)
    return(invisible(TRUE))
}
add_definition <- function(name, test.is, test.in, .exists=TRUE, .overwrite=FALSE){
    if (.exists && !exists(name))
        stop("function `", name, "` not found.")
    if (!.overwrite && has(name)) stop("`", name, "` already has a testing function defined.")
    if (!inherits(test.is, 'function')) stop("test.is must be a function.")
    if (!inherits(test.in, 'function')) stop("test.in must be a function.")
    if (!identical(base::names(formals(test.is)), c('id', 'pd')))
        stop(paste( "test.is function must have two and only two arguments"
                  , "titled 'id' and 'pd', exclusively in that order"))
    if (!identical(base::names(formals(test.in)), c('id', 'pd')))
        stop(paste( "test.in function must have two and only two arguments"
                  , "titled 'id' and 'pd', exclusively in that order"))
    assign(name, test.is, envir = .is)
    assign(name, test.in, envir = .is_in)
    return(invisible(TRUE))
}
rm <- function(name){
    base::rm(list=name, envir=.is   , inherits = FALSE)
    base::rm(list=name, envir=.is_in, inherits = FALSE)
}
test_is <-
    function( id = pd$id
            , pd = get('pd', parent.frame())
            ){
        if (length(id) == 0) return(logical(0)) else
        if (length(id) > 1L) {
            value <- sapply(id, test_is, pd=pd)
            return(structure( t(value)
                            , dimnames = list(id = id, rownames(value))
                            ))
        } else
        sapply(.is, .call_test, id=id, pd=pd)
    }
test_is_in <-
    function( id = pd$id
            , pd = get('pd', parent.frame())
            ){
        if (length(id) == 0) return(logical(0)) else
        if (length(id) > 1L){
            value <- sapply(id, test_is_in, pd=pd)
            return(structure( t(value)
                            , dimnames = list(id = id, rownames(value))
                            ))
        } else
        sapply(.is_in, .call_test, id=id, pd=pd)
    }
names <- function(sorted=TRUE)objects(.is, sorted=sorted)
which <-
    function( id = pd$id
            , pd = get('pd', parent.frame())
            , all = FALSE
            ){


        i <- test_is_in(id=id, pd=pd)



    }




})
pd_class_definitions$add(c('setClass', 'setRefClass'))
if(FALSE){#@testing pd_class_definitions
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
                , paste( "test.is function must have two and only two arguments"
                       , "titled 'id' and 'pd', exclusively in that order"
                       )
                )
    expect_error( pd_class_definitions$add_definition('another_custom'
                        , test.is = function(id, pd){return(TRUE)}
                        , test.in = function(){}
                        , .exists=FALSE)
                , paste( "test.in function must have two and only two arguments"
                       , "titled 'id' and 'pd', exclusively in that order"
                       )
                )
    expect_true( pd_class_definitions$add_definition('another_custom'
                        , test.is = function(id, pd){return(TRUE)}
                        , test.in = function(id, pd){return(TRUE)}
                        , .exists=FALSE)
                )
    expect_true(pd_class_definitions$has('another_custom'))
    expect_null(pd_class_definitions$rm('another_custom'))
    expect_false(pd_class_definitions$has('another_custom'))
}

#TODO pd_get_class_definition
#' @internal
pd_get_closest_call <-
function( id = pd$id
        , pd = get('pd', parent.frame())
        , calls = NULL
        ){
    if (length(id) > 1L) return(sapply(id, pd_get_closest_call, pd=pd, calls=calls))
    ancestors <- get_ancestor_ids(id, pd, only.present=TRUE)
    ancestors <- ancestors[pd_is_symbol_call(ancestors)]
    if (length(ancestors) == 0 ) return(NA_integer_)
    if (length(calls))
        ancestors <- ancestors[text(pd_get_call_symbol_id(ancestors)) %in% calls]
    if (length(ancestors) == 0 ) return(NA_integer_)
    ancestors[[1]]
}
if(FALSE){#@testing
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
}

#' @include pd_make_is_in.R

#' @title Test for Class Definitions
#'
#' @usage
#'     pd_class_definitions
#' @param name name of the class defining function
#' @param .exists    require the function to exists to add.
#' @param .overwrite if TRUE allows for overwriting existing test functions.
#' @param test.is    function accepting arguments `id` and `pd` which
#'                   tests if given `id` is associated with the defined
#'                   class defining functions.
#' @param test.in    function accepting arguments `id` and `pd` which
#'                   tests if given `id` is contained in the defined
#'                   class defining functions.
#' @param id         id(s) to test.
#' @param pd         parse data which contains id.
#' @param .check     should the id, and pd be checked?
#'
#' @description
#' These function manage adding class defining functions and
#' testing if an id is associated with a class definition or
#' if is contained in the class definition.
#'
#' @details
#'
#' \subsection{\code{pd_class_definitions$has}}{
#'     \subsection{Usage}{\preformatted{
#'         pd_class_definitions$has(name)
#'     }}
#'     Check if a class defining function has
#'     'is' and 'in' function defined for it.
#' }
#' \subsection{\code{pd_class_definitions$add} or \code{pd_add_class}}{
#' \subsection{Usage}{\preformatted{
#' pd_class_definitions$add(name, .exists=TRUE, .overwrite=FALSE)
#'
#' pd_add_class(name, .exists=TRUE, .overwrite=FALSE)
#' }}
#'     Add a def with default 'is' and 'in' functions defined.
#' }
#' \subsection{\code{pd_class_definitions$add_definition} or \code{pd_add_class_definition}}{
#' \subsection{Usage}{\preformatted{
#' pd_class_definitions$add_definition(name, test.is, test.in, .exists=TRUE, .overwrite=FALSE)
#'
#' pd_add_class_definition(name, test.is, test.in, .exists=TRUE, .overwrite=FALSE)
#' }}
#'     Add a class defining function with custom 'is' and 'in' functions defined.
#' }
#' \subsection{\code{pd_class_definitions$rm}}{
#' \subsection{Usage}{\preformatted{
#' pd_class_definitions$rm(name)
#' }}
#'     Remove the testing functions for the class.
#' }
#' \subsection{\code{pd_class_definitions$names}}{
#' \subsection{Usage}{\preformatted{
#' pd_class_definitions$names()
#' }}
#'     Return a vector of the classed for which tests are defined.
#' }
#' \subsection{\code{pd_class_definitions$test_is}}{
#' \subsection{Usage}{\preformatted{
#' pd_class_definitions$test_is(id, pd, check=TRUE)
#' }}
#'     Test if \code{id} is associated with each of
#'     defined class definitions.
#' }
#' \subsection{\code{pd_class_definitions$test_is_in}}{
#' \subsection{Usage}{\preformatted{
#' pd_class_definitions$test_is_in(id, pd, check=TRUE)
#' }}
#'     Test if \code{id} is contained within each of
#'     defined class definitions.
#' }
#' \subsection{\code{pd_class_definitions$which}}{
#' \subsection{Usage}{\preformatted{
#' pd_class_definitions$which(id, pd, check=TRUE)
#' }}
#'     Return the name of the class, if any,
#'     which \code{id} corresponds to.
#' }
#' \subsection{\code{pd_class_definitions$in_which}}{
#' \subsection{Usage}{\preformatted{
#' pd_class_definitions$in_which(id, pd, check=TRUE)
#' }}
#'     Returns a vector of the classes, if any,
#'     of the classes which \code{id} is contained in.
#' }
#' \subsection{\code{pd_is_class_definition}}{
#'     Returns \code{TRUE} if the id corresponds to any of the
#'     class defining calls.
#' }
#'
#' @example inst/examples/example-pd.R
#' @example inst/examples/example-roots.R
#' @example inst/examples/example-classes.R
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
add <- function( name, .exists=TRUE, .overwrite=FALSE){
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
    if (!identical(match(c('id', 'pd'), base::names(formals(test.is))), 1:2))
        stop(paste( "test.is function must accept arguments 'id' and 'pd'"
                  , "as the first two arguments."))
    if (!('.check' %in% base::names(formals(test.is)) || "..." %in% base::names(formals(test.is))))
        stop( "test.is function must accept argument .check or extra arguments `...`")
    if (!identical(match(c('id', 'pd'), base::names(formals(test.in))), 1:2))
        stop(paste( "test.in function must accept arguments 'id' and 'pd'"
                  , "as the first two arguments."))
    if (!('.check' %in% base::names(formals(test.in)) || "..." %in% base::names(formals(test.in))))
        stop( "test.in function must accept argument .check or extra arguments `...`")
    assign(name, test.is, envir = .is)
    assign(name, test.in, envir = .is_in)
    return(invisible(TRUE))
}
rm <- function(name){
    base::rm(list=name, envir=.is   , inherits = FALSE)
    base::rm(list=name, envir=.is_in, inherits = FALSE)
}
test_is <- function( id, pd, .check=TRUE){
        if (.check) {
            pd <- ._check_parse_data(pd)
            id <- ._check_id(id, pd)
        }
        if (length(id) == 0) return(logical(0)) else
        if (length(id) > 1L) {
            value <- sapply(id, test_is, pd=pd)
            return(structure( t(value)
                            , dimnames = list(id = id, rownames(value))
                            ))
        } else
        sapply(.is, .call_test, id=id, pd=pd, .check=FALSE)[names()]
    }
test_is_in <- function( id, pd, .check=TRUE){
        if (.check) {
            pd <- ._check_parse_data(pd)
            id <- ._check_id(id, pd)
        }
        if (length(id) == 0) return(logical(0)) else
        if (length(id) > 1L){
            value <- sapply(id, test_is_in, pd=pd)
            return(structure( t(value)
                            , dimnames = list(id = id, rownames(value))
                            ))
        } else
        sapply(.is_in, .call_test, id=id, pd=pd, .check=FALSE)[names()]
    }
names <- function(sorted=TRUE) objects(.is, sorted=sorted)
which <- function( id, pd, .check = TRUE){
        if (.check) {
            pd <- ._check_parse_data(pd)
            id <- ._check_id(id, pd)
        }
        stopifnot(length(id) == 1L)
        base::names(base::which(test_is(id=id, pd=pd, .check=FALSE)))
    }
in_which <- function( id, pd, .check = TRUE){
        if (.check) {
            pd <- ._check_parse_data(pd)
            id <- ._check_id(id, pd)
        }
        stopifnot(length(id) == 1L)
        base::names(base::which(test_is_in(id=id, pd=pd, .check=FALSE)))
    }
})
lockEnvironment(pd_class_definitions, TRUE)

pd_class_definitions$add(c('setClass', 'setRefClass'))
if(FALSE){#@testing pd_class_definitions
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
}

pd_is_class_definition <- function(id, pd, .check=TRUE){
    #' @rdname pd_class_definitions
    if (.check) {
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    if (length(id) > 1L)
        return(sapply(id, pd_is_class_definition, pd=pd, .check=FALSE))
    any(pd_class_definitions$test_is(id, pd))
}
if(FALSE){#@test
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
}

pd_is_in_class_definition <- function(id, pd, .check = TRUE){
    #' @rdname pd_class_definitions
    if (.check) {
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    if (length(id) > 1L)
        return(sapply(id, pd_is_in_class_definition, pd=pd, .check=FALSE))
    any(pd_class_definitions$test_is_in(id, pd))
}
if(FALSE){#@test object in setClass
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
}

pd_add_class_definition <-
    function(name, test.is, test.in, .exists=TRUE, .overwrite=FALSE){
    #' @rdname pd_class_definitions
    pd_class_definitions$add_definition( name=name, test.is=test.is, test.in=test.in
                                       , .exists=.exists, .overwrite=.overwrite)
}
if(FALSE){#@testing
    test.is <- pd_make_is_call('setTestClass')
    test.in <- pd_make_is_in_call('setTestClass', .is = test.is)
    pd_add_class_definition('setTestClass', test.is, test.in, FALSE, TRUE)

    pd <- get_parse_data(parse(text='
                setTestClass( MyClass, ...)
            ', keep.source=TRUE))
    expect_true(pd_is_class_definition(id = roots(pd), pd))
    expect_false(pd_is_class_definition(id = .find_text('MyClass', pd), pd))
    pd_class_definitions$rm('setTestClass')
}

pd_add_class <- function(name, .exists=TRUE, .overwrite=FALSE){
    #' @rdname pd_class_definitions
    pd_class_definitions$add(name=name, .exists=.exists, .overwrite=.overwrite)
}
if(FALSE){#@testing
    pd_add_class('setAnotherClass', FALSE, TRUE)

    pd <- get_parse_data(parse(text='
                setAnotherClass( "testAnotherClass", ...)
            ', keep.source=TRUE))
    expect_true(pd_is_class_definition(id = roots(pd), pd))
    expect_false(pd_is_class_definition(id = .find_text('"testAnotherClass"', pd), pd))

    pd_class_definitions$rm('setAnotherClass')
}


#' @title Get the closest call ID.
#' @inheritParams pd_is_symbol_call
#' @param calls optional calls to limit consideration to.
#' @description
#' Get the id of the call that is closest to the \code{id} given.
#' Closest is defined as the innermost call that contains the \code{id}.
#'
pd_get_closest_call_id <-
function( id, pd, calls = NULL, .check=TRUE){
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot(is.null(calls) || is.character(calls))
    }
    if (length(id) > 1L) return(sapply(id, pd_get_closest_call_id, pd=pd, calls=calls)) # nocov
    all.ancestors <- ancestors(id, pd, only.present=TRUE)
    call.ancestors <- all.ancestors[is_symbol_call(all.ancestors)]
    if (length(call.ancestors) == 0 ) return(NA_integer_) # nocov
    if (length(calls))
        call.ancestors <- call.ancestors[text(call_symbol(call.ancestors)) %in% calls]
    if (length(call.ancestors) == 0 ) return(NA_integer_)
    call.ancestors[[1]]
}
closest_call <- internal(pd_get_closest_call_id)
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
"}, keep.source=TRUE))

roots <- roots(pd)

id.10 <- pd[pd$text == '10','id']
expect_equal(text(call_symbol(pd_get_closest_call_id(id.10, pd=pd), pd=pd)), 'rnorm')

id.hw <- pd[pd$text == "'hello world'", 'id']

expect_equal(text(call_symbol(pd_get_closest_call_id(id.hw, pd=pd))), 'print')
expect_equal(text(call_symbol(pd_get_closest_call_id(id.hw, pd=pd, 'list'))), 'list')
expect_equal(text(call_symbol(pd_get_closest_call_id(id.hw, pd=pd, 'setRefClass'))), 'setRefClass')

expect_true(is.na(pd_get_closest_call_id(id.hw, pd=pd, 'setClass')))
}

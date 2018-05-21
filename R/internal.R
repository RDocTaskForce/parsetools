#' make a function opperate internal to parsetools
#'
#' @param fun The function to make internal
#'
#' @description
#' Convert a function to look for pd object in the `parent.frame()`,
#' and the id to extract from the pd unless overwritten.
internal <- function(fun, id=pd$id){
    ..fun <- substitute(fun)
    ..id <- substitute(id)

    flist <- formals(fun)
    args <- list()
    if ('id' %in% names(flist))
        args$id <- ..id
    if ('pd' %in% names(flist))
        args$pd <- substitute(get('pd', parent.frame()))

    body.args <- lapply(names(args), as.name)
    names(body.args) <- names(args)

    if (length(setdiff(names(flist), c('id', 'pd', '.check')))){
        args <- c(args, alist(...=))
        body.args <- c(body.args, as.name('...'))
    }
    if ('.check' %in% names(flist))
        body.args$.check = FALSE

    body <- as.call(c(..fun, body.args))

    # body <- substitute(..fun(id=id, pd=pd), list(..fun=..fun))

    as.function( c(args, body)
               , envir = topenv()
               )
}
if(F){#@testing
    external_test <- function(id, pd){"do something"}

    test <- internal(external_test)
    expected <- function(id=pd$id, pd=get('pd', parent.frame()))external_test(id=id, pd=pd)
    environment(expected) <- asNamespace('parsetools')
    expect_identical(test, expected)

    external_test2 <- function(id, pd, .check=TRUE){"do something"}
    test2 <- internal(external_test2)
    expected2 <- function(id=pd$id, pd=get('pd', parent.frame()))external_test2(id=id, pd=pd, .check=FALSE)
    environment(expected2) <- asNamespace('parsetools')
    expect_identical(test2, expected2)

    external_test3 <- function(id, pd, N=1){"do something"}
    test3 <- internal(external_test3)
    expected3 <- function(id=pd$id, pd=get('pd', parent.frame()), ...)external_test3(id=id, pd=pd, ...)
    environment(expected3) <- asNamespace('parsetools')
    expect_identical(test3, expected3)

    external_test4 <- function(id, pd, N=1, .check=TRUE){"do something"}
    test4 <- internal(external_test4)
    expected4 <- function(id=pd$id, pd=get('pd', parent.frame()), ...)external_test4(id=id, pd=pd, ..., .check=FALSE)
    environment(expected4) <- asNamespace('parsetools')
    expect_identical(test4, expected4)
}



make_get_all <- function(fun){
    ..fun <- substitute(fun)
    body <- substitute(pd[..fun(id=pd$id, pd=pd), 'id'])
    as.function( c(alist(pd=get('pd', parent.frame())), body)
               , envir = topenv()
               )
}
if(F){
    expect_equal( body(make_get_all(pd_is_function))
                , substitute(pd[pd_is_function(id=pd$id, pd=pd), 'id'], emptyenv())
                )
}

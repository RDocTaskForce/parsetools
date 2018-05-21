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
    body <- substitute(..fun(id=id, pd=pd), list(..fun=..fun))
    as.function( list(id=..id, pd=substitute(get('pd', parent.frame())), body)
               , envir = topenv()
               ) 
}
if(F){#@testing 
    test <- internal(external)
    expected <- function(id=pd$id, pd=get('pd', parent.frame()))external(id=id, pd=pd)
    environment(expected) <- asNamespace('parsetools')
    expect_identical(test, expected)
}



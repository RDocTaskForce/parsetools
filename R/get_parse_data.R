{#######################################################################
# get_parse_data.R
# This file is part of the R package `parsetools`.
#
# Author: Andrew Redd
# Copyright: 2017 The R Consortium
#
# LICENSE
# ========
# The R package `parsetools` is free software: 
# you can redistribute it and/or modify it under the terms of the 
# GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) 
# any later version.
#
# This software is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License 
# along with this program. If not, see http://www.gnu.org/licenses/.
#
}#######################################################################

.pd.expected.names <-{c( 'line1', 'col1', 'line2', 'col2', 'id'
                       , 'parent', 'token', 'terminal', 'text'
                       )}

#' @export
valid_parse_data <-
function( df ){
    #' @title is a data.frame a valid parse-data?
    #' @param df a data.frame object.
    #' @description 
    #'   Test if the given `df` data.frame conformes to `<parse-data>` conventions.
    #' @return either TRUE if valid as parse-data or the reason why not.
    if (!inherits(df, "data.frame")) return("Not a data.frame object")
    if (!all(.pd.expected.names %in% names(df))) return("names of data do not conform.")
    return(TRUE)
}
if(F){#!@test
    df <- utils::getParseData(parse(text="rnorm(10,0,1)"))
    expect_true (valid_parse_data(df), 'parse-data')
    expect_equal(valid_parse_data(datasets::iris      ), "names of data do not conform.")
    expect_equal(valid_parse_data(stats::rnorm(10,0,1)), "Not a data.frame object")
}

as_parse_data <- function(df){
    #! Convert data.frame to `parse-data`
    is.valid <- valid_parse_data(df)
    if (!isTRUE(is.valid)) stop("Cannot convert to parse-data: ", is.valid)
    sort(structure( fix_eq_assign(classify_comment(df))
                  , class=c( 'parse-data', 'data.frame')
                  ))
}
if(FALSE){#!@testing
    df <- utils::getParseData(parse(text="rnorm(10,0,1)"))
    expect_is   (as_parse_data(df), 'parse-data')
    expect_error(as_parse_data(datasets::iris), "Cannot convert to parse-data: names of data do not conform.")
    expect_error(as_parse_data(stats::rnorm(10,0,1)), "Cannot convert to parse-data: Not a data.frame object")
}

#' @export
as.data.frame.parseData <- 
function( x, ...){
    x <- t(unclass(x))
    colnames(x) <- c( "line1", "col1", "line2", "col2"
                    , "terminal", "token.num", "id", "parent"
                    )
    x <- data.frame( x[, -c(5, 6), drop = FALSE]
                   , token    = attr(x, "tokens")
                   , terminal = as.logical(x[, "terminal"])
                   , text     = attr(x, 'text')
                   , stringsAsFactors = FALSE
                   )
    o <- order(x[, 1], x[, 2], -x[, 3], -x[, 4])
    x <- x[o, ]
    rownames(x) <- x$id
    x
}
if(FALSE){#!@testing
    if(F)
        debug(as.data.frame.parseData)
    p <- parse(text={"
    my_function <- function(object #< An object to do something with
            ){
        #' A title
        #' 
        #' A Description
        print(\"It Works!\")
        #< A return value.
    }"})
    srcfile <- attr(p, 'srcfile')
    x <- srcfile$parseData
    
    df1 <- as.data.frame.parseData(x, srcfile=srcfile)
    expect_true(valid_parse_data(df1))
}

#@internal
get_srcfile <- function(x){
    #! replicate of unexported function get_srcfile from utils.
    result <- attr(x, "srcfile")
    if (!is.null(result)) 
        return(result)
    srcref <- attr(x, "wholeSrcref")
    if (is.null(srcref)) {
        srcref <- utils::getSrcref(x)
        if (is.list(srcref) && length(srcref)) 
            srcref <- srcref[[length(srcref)]]
    }
    attr(srcref, "srcfile")
}



#' @export
#' @aliases parse-data
#' @title Get cleaned parse data 
#' 
#' @param x     an object to get parse-data from.
#' @param ...   passed on
#' 
#' @description
#' A customized version of `<getParseData>` that will return 
#' a cleaned up version of the parse data for a variety of objects.
#' This version also fails less often, even reparsing text when 
#' needed. 
get_parse_data <- function(x, ...)UseMethod("get_parse_data")

#' @export 
get_parse_data.srcfile <- 
function( x
        , ...                               #< discarded
        ){
    stopifnot(inherits(x, 'srcfile'))
    df <-    if (!is.null(x$parseData)) as.data.frame.parseData(x$parseData, x, ...)
        else if (!is.null(x$lines    ) && length(x$lines) ) utils::getParseData(parse(text=x$lines), ...)
        else if (!is.null(x$filename ) && x$filename != "") utils::getParseData(parse(x$filename  ), ...)
        else stop("could not retrieve parse-data for ", deparse(substitute(x)))
    structure(as_parse_data(df), srcfile = x)
}
if(FALSE){#!@testing
    text <- "    my_function <- function(object #< An object to do something with
            ){
        #' A title
        #' 
        #' A Description
        print(\"It Works!\")
        #< A return value.
    }"
    tmp <- tempfile()
    writeLines(text, tmp)
    
    readLines(tmp)
    source(tmp)
    
    srcref  <- utils::getSrcref(my_function) 
    srcfile <- attr(srcref, 'srcfile')
    expect_equal(srcfile$filename, tmp)
    expect_is(srcfile$parseData, 'parseData')
    pd <- get_parse_data.srcfile(srcfile)
    expect_is(pd, 'parse-data', info = "srcfile with parseData")
    expect_identical(attr(pd, 'srcfile'), srcfile, info='carried forward srcfile')
    
    remove('parseData', envir = srcfile)
    expect_null(srcfile$parseData)
    expect_is(srcfile$lines, 'character')
    pd <- get_parse_data.srcfile(srcfile)
    expect_is(pd, 'parse-data', info = "srcfile from lines")
    
    remove('lines', envir = srcfile)
    expect_null(srcfile$parseData)
    expect_null(srcfile$lines, 'character')
    pd <- get_parse_data.srcfile(srcfile)
    expect_is(pd, 'parse-data', info = "srcfile from file directly")
    
    remove('filename', envir = srcfile)
    expect_error(get_parse_data.srcfile(srcfile), "could not retrieve parse-data for srcfile")

    unlink(tmp)
}

#' @export 
get_parse_data.srcref <- 
function( x
        , ...                               #< passe to <getParseData>    
        , ignore.groups            = TRUE   #< see <ascend_to_root>
        , include.doc.comments     = TRUE   #< see <get_family>
        , include.regular.comments = FALSE  #< see <get_family>
        ){
    stopifnot(inherits(x, 'srcref'))
    pd <- get_parse_data.srcfile(attr(x, 'srcfile'), ...)
    id <- pd[ pd$line1 == utils::getSrcLocation(x, 'line', TRUE )
            & pd$line2 == utils::getSrcLocation(x, 'line', FALSE)
            & pd$col1  == utils::getSrcLocation(x, 'col' , FALSE)
            & pd$col2  == utils::getSrcLocation(x, 'col' , FALSE)
            , 'id' ]
    root <- ascend_to_root(id, pd, ignore.groups=ignore.groups)
    if  (!length(root)) return(NULL)
    structure(id = id, root=root,
    get_family( root, pd
              , include.doc.comments     = include.doc.comments
              , include.regular.comments = include.regular.comments
              ))
}
if(FALSE){#!@testing
    text <-{"my_function <- 
        function( object #< An object to do something with
                ){
            #' A title
            #' 
            #' A Description
            print('It Works!')
            #< A return value.
        }
        another_f <- function(){}
        if(F){}
    "}
    p <- parse(text=text)
    e <- new.env()
    eval(p, envir=e)
    srcref <- utils::getSrcref(e$my_function)
    srcfile <- get_srcfile(e$my_function)
    
    
    expect_is(srcref, 'srcref')
    pd <- get_parse_data.srcref(srcref)
    expect_is(pd, 'parse-data')
    expect_identical(attr(pd, 'srcfile'), srcfile)
}

#' @export 
get_parse_data.function <- 
function(x, ...){
    stopifnot(is.function(x))
    if (methods::isGeneric(fdef=x)) {
        default <- attr(x, 'default')
        if (is.null(default) || !is.function(default)) 
            stop(deparse(substitute(x)), " appears to be a generic, but could not find the default method, where parse data should be found.")
        return(Recall(default, ...))
    }
    get_parse_data.default(x, ...)
}
if(FALSE){#@test get_parse_data.function basic
test.text <-
"#' Roxygen Line Before
hw <-
function(x){
    #' line inside
    cat(\"hello world\")
}
another_fun <- function(){TRUE}
"
eval(parse(text=test.text))
x <- fun <- hw
pd.regular <- get_parse_data(hw)
expect_that(pd.regular, is_a("data.frame"))
expect_that(pd.regular[1,"text"], equals("#' Roxygen Line Before"))
}
if(FALSE){#@test get_parse_data.function grouped 
grouped.text <-
"{#' Roxygen Line Before
hw <-
function(x){
    #' line inside
    cat(\"hello world\")
}}"
eval(parse(text=grouped.text))
fun <- hw
pd <- get_parse_data(hw)
expect_is(pd, "parse-data")
expect_that(pd[1,"text"], equals("#' Roxygen Line Before"))
}
if(FALSE){#@test get_parse_data.function nested
nested.text <-{
"{# Section Block
#' Roxygen Line Before
nested <-
function(x){
    #' line inside
    cat(\"hello world\")
}
}
"}
eval(parse(text=nested.text))
x <- fun <- nested
pd <- get_parse_data(nested)
expect_is(pd, "data.frame")
expect_is(pd, "parse-data")

# pd <- get_parse_data(function(){})
# expect_that(pd, is_a("data.frame"))
}
if(FALSE){#@test get_parse_data.function S4 Generic
    # Note that testthat:::test_code will strip comments from code
    # this requires a parse & eval statement.
    p <- parse(text="setGeneric(\"my_generic\", 
        function(object #< An object to do something with
                ){
            #' A title
            #' 
            #' A Description
            print(\"It Works!\")
            #< A return value.
        })", keep.source=TRUE)
    eval(p)
    expect_null(utils::getParseData(my_generic))
    expect_true(isGeneric(fdef = my_generic))
    pd <- get_parse_data(my_generic)
    expect_is(pd, 'parse-data')
}
#' @export
get_parse_data.default <-
function( x, ...){
    #! the default get_parse_data method
    #! 
    #! This extracts the srcref and uses that to obtain the parse data.
    #! Currently I have only found srcrefs as attributes of functions.
    srcref <- utils::getSrcref(x)
    if (!is.null(srcref) && inherits(srcref, 'srcref')) {
            get_parse_data.srcref(srcref, ...)
    } else {
        srcfile <- get_srcfile(x)
        if (!is.null(srcfile))
            get_parse_data.srcfile(srcfile)
        else
            stop(deparse(substitute(x)), " does not have a valid srcref.")
    }
}
if(FALSE){#!@testing
    x <- 
    exprs <- parse(text=c('x <- rnorm(10, mean=0, sd=1)'
                         ,'y <- mean(x)'
                         ))
    pd <- get_parse_data(exprs, keep.source=TRUE)
    expect_is(pd, 'parse-data', info = "get_parse_datwa.default with srcfile")
}

fix_eq_assign <-
function( pd  #< The [parse-data] to fix
        ){
    #! Fix the parents for expressions associated with EQ_ASSIGN tokens.
    ids <- pd[pd[['token']] == "EQ_ASSIGN", 'id']

    for(id in rev(ids)){
        parent <- get_parent_id(pd, id)
        fam.pd <- get_child(pd, get_parent_id(pd, id))
        fam.pd <- fam.pd[order(fam.pd$id), ]
        fam.pd <- utils::head(fam.pd[fam.pd$id >= id, ], 3)

        new.id <- max(pd$id)+1L
        fam.pd$parent <- new.id

        line1   = min(fam.pd$line1)
        col1    = min(fam.pd[fam.pd$line1==line1, 'col1'])
        line2   = max(fam.pd$line2)
        col2    = max(fam.pd[fam.pd$line2==line2, 'col2'])

        pd <-
        rbind( pd[!(pd$id %in% c(fam.pd$id)), ]
             , data.frame( line1, col1
                         , line2, col2
                         , id      = new.id
                         , parent  = parent
                         , token   = 'expr'
                         , terminal= FALSE
                         , text    = ''
                         )
             , fam.pd
             )
    }
    pd[do.call(order, pd), ]
}
if(F){#! @testthat
pd <- utils::getParseData(parse(text="a=1"))
fixed.pd <- fix_eq_assign(pd)
expect_true(nrow(pd)+1 == nrow(fixed.pd))
expect_that(sum(fixed.pd$parent==0), equals(1))

pd <- utils::getParseData(parse(text="a=1\nb<-2\nc=3\nd<<-4"))
fixed.pd <- fix_eq_assign(pd)
expect_true(nrow(pd)+2 == nrow(fixed.pd))
expect_that(sum(fixed.pd$parent==0), equals(4))

pd <- utils::getParseData(parse(text="a=b=1"))
fixed.pd <- fix_eq_assign(pd)
expect_true(nrow(pd)+2 == nrow(fixed.pd))
expect_that(sum(fixed.pd$parent==0), equals(1))
}


#' @export
`subset.parse-data` <- function(x, ...)structure(NextMethod(), class=c('parse-data', 'data.frame'))
if(FALSE){#@testing
    pd <- get_parse_data(parse(text={
    "{# Section Block
    #' Roxygen Line Beore
    nested <-
    function(x){
        #' line inside
        cat(\"hello world\")
    }
    }
    "
    }))
    expect_is(pd, 'parse-data')
    pd2 <- pd[pd$line1 > 3, ]
    expect_is(pd2, 'parse-data')
    expect_equal(min(pd2$line1), 4)
}

#' @export
`[.parse-data` <- function(x, ...){
    result <- NextMethod()
    if(inherits(result, 'data-frame')) 
        structure(result, class=c('parse-data', 'data.frame'))
    else 
        result
}
if(FALSE){#!@testing
    pd       <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    expect_is(pd, 'parse-data')
    expect_is(pd[pd$parent==0, ], 'parse-data')
    expect_false(methods::is(pd[pd$parent==0, 'id'], 'parse-data'))
}



#' @export
`-.parse-data` <- function(e1, e2){
    stopifnot( inherits(e2, 'parse-data')
             , inherits(e1, 'parse-data')
             )
    subset(e1, !(e1$id %in% e2$id))
}
if(FALSE){#! @test `-.parse-data`
pd <- get_parse_data(parse(text={
"{# Section Block
#' Roxygen Line Beore
nested <-
function(x){
    #' line inside
    cat(\"hello world\")
}
}
"
}))
comments <- get_comments(pd)
expect_is(comments, 'parse-data')
clean.pd <- pd - comments

expect_is(clean.pd, 'parse-data')
expect_true(!any(comments$id %in% clean.pd$id))
}


#' @export
`sort.parse-data` <- function(x, decreasing=FALSE, ...){
    x[do.call(order, append(x, list(decreasing=decreasing))),]
}
if(FALSE){#TODO test for parse-data
    pd <- get_parse_data(parse(text={
        "{# Section Block
        #' Roxygen Line Beore
        nested <-
        function(x){
            #' line inside
            cat(\"hello world\")
        }
        }
        "
    }))
    sort(pd)
}

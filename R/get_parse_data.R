{#######################################################################
# get_parse_data.R
# This file is part of the R package `parsetools`.
#
# Author: Andrew Redd
# Copyright: 2017 University of Utah
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
function( df #< a [data.frame] object.
        ){
    #! Test if the given `df` data.frame conformes to `<parse-data>` conventions.
    #! @return either TRUE if valid as parse-data or the reason why not.
    if (!inherits(df, "data.frame")) return("Not a data.frame object")
    if (!all(.pd.expected.names %in% names(df))) return("names of data do not conform.")
    return(TRUE)
}
if(F){#!@test
    df <- getParseData(parse(text="rnorm(10,0,1)"))
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
    df <- getParseData(parse(text="rnorm(10,0,1)"))
    expect_is   (as_parse_data(df), 'parse-data')
    expect_error(as_parse_data(datasets::iris), "Cannot convert to parse-data: names of data do not conform.")
    expect_error(as_parse_data(stats::rnorm(10,0,1)), "Cannot convert to parse-data: Not a data.frame object")
}


#' @export
get_parse_data <-
function( x   #< object or function to retrive parse data for.
        , ... #< passed to `<getParseData>`
        ){
    #! customized version of `<getParseData>` that will return parse data for text objects.
    pd <- utils::getParseData(x, ...)

    if (is.null(pd) && methods::isGeneric(fdef=x)) {
        dflt <- attr(x, 'default')
        src <- utils::getSrcref(dflt)
        if(!is.null(src)){
            x  <- dflt
            pd <- utils::getParseData(src)
            expr.pd <- pd[ ( pd$line1 > src[1] | (pd$line1 == src[1] & pd$col1 >= src[2]))
                         & ( pd$line2 < src[3] | (pd$line2 == src[3] & pd$col2 <= src[4]))
                         , ]
            root <- unique(ascend_to_root( expr.pd, pd))
            return(structure( classify_comment(expr.pd)
                            , class=c("parse-data", "data.frame")
                            , header.pd = pd[pd$parent %in% -root, ]
                            ))
        }
    }

    if (is.null(pd)) {
        if( is.null(srcref <- utils::getSrcref(x)) )
            stop("x does not have srcref.")
        else {
            pd <- classify_comment(utils::getParseData(parse(text=as.character(srcref))))
            return(structure(pd, class=c("parse-data", "data.frame")))
        }
    }
    pd <- as_parse_data(pd)
    switch( mode(x)
          , 'function' = {
                index   <- ( pd$line1 == utils::getSrcLocation(x, 'line'  , TRUE )
                           & pd$col1  == utils::getSrcLocation(x, 'column', TRUE )
                           & pd$line2 == utils::getSrcLocation(x, 'line'  , FALSE)
                           & pd$col2  == utils::getSrcLocation(x, 'column', FALSE)
                           )
                id      <- pd[index, 'id']
                root    <- ascend_to_root(pd, id)
                expr.pd <- get_family(pd, root)
                return( structure( expr.pd
                                 , header.pd = pd[pd$parent == -root, ]
                                 )
                      )
          }
          , return(pd)
          )
}
if(F){# @testthat get_parse_data
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



nested.text <-
"{# Section Block
#' Roxygen Line Before
nested <-
function(x){
    #' line inside
    cat(\"hello world\")
}
}
"
eval(parse(text=nested.text))
x <- fun <- nested
pd <- get_parse_data(nested)
expect_that(pd, is_a("data.frame"))

pd <- get_parse_data(function(){})
expect_that(pd, is_a("data.frame"))

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
if(FALSE){
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
`transform.parse-data` <- function(`_data`, ...)structure(NextMethod(), class=c('parse-data', 'data.frame'))

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

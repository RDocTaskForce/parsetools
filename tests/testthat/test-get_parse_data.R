#! This file was automatically produced by lint on  2017-06-06 12:03:28
#! changes will be overwritten.
context('tests extracted from file `/mnt/data/projects/rdtf/parsetools/R/get_parse_data.R`')
test_that("'valid_parse_data'", {#!@test
    df <- utils::getParseData(parse(text="rnorm(10,0,1)"))
    expect_true (valid_parse_data(df), 'parse-data')
    expect_equal(valid_parse_data(datasets::iris      ), "names of data do not conform.")
    expect_equal(valid_parse_data(stats::rnorm(10,0,1)), "Not a data.frame object")
})
test_that("'as_parse_data'", {#!@testing
    df <- utils::getParseData(parse(text="rnorm(10,0,1)"))
    expect_is   (as_parse_data(df), 'parse-data')
    expect_error(as_parse_data(datasets::iris), "Cannot convert to parse-data: names of data do not conform.")
    expect_error(as_parse_data(stats::rnorm(10,0,1)), "Cannot convert to parse-data: Not a data.frame object")
})
test_that("'as.data.frame.parseData'", {#!@testing
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
})
test_that("'get_parse_data.srcfile'", {#!@testing
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
})
test_that("'get_parse_data.srcref'", {#!@testing
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
})
test_that("'get_parse_data.function'", {#!@testing
{# basic
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
{# grouped 
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
{# nested
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

pd <- get_parse_data(function(){})
expect_that(pd, is_a("data.frame"))
}
{# S4 Generic
setGeneric("my_generic", 
    function(object #< An object to do something with
            ){
        #' A title
        #' 
        #' A Description
        print("It Works!")
        #< A return value.
    })
expect_null(utils::getParseData(my_generic))
expect_true(isGeneric(fdef = my_generic))
pd <- get_parse_data(my_generic)
expect_is(pd, 'parse-data')

}
})
test_that("'get_parse_data.default'", {#!@testing
    x <- 
    exprs <- parse(text=c('x <- rnorm(10, mean=0, sd=1)'
                         ,'y <- mean(x)'
                         ))
    pd <- get_parse_data(exprs, keep.source=TRUE)
    expect_is(pd, 'parse-data', info = "get_parse_datwa.default with srcfile")
})
test_that("'fix_eq_assign'", {#! @testthat
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
})
test_that("'`[.parse-data`'", {#!@testing
    pd       <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)'))
    expect_is(pd, 'parse-data')
    expect_is(pd[pd$parent==0, ], 'parse-data')
    expect_false(methods::is(pd[pd$parent==0, 'id'], 'parse-data'))
})
test_that("'`-.parse-data`'", {#! @test `-.parse-data`
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
})

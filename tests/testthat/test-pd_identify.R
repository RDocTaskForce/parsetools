#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `pd_identify.R`')
#line 63 "R/pd_identify.R"
test_that('pd_identify.srcref', {#@testing
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
    source(file = textConnection(text), local=TRUE, keep.source = TRUE )
    parsed <- parse(text=text, keep.source=TRUE)
    pd <- get_parse_data(parsed)

    id <- pd_identify(pd, my_function)
    expected <- parent(.find_text('function')[1])
    expect_equal(id, expected)

    expect_error(pd_identify(pd, NULL))
})

#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `errors.R`')
#line 13 "R/errors.R"
test_that('errors', {#@testing errors
    pd <- get_parse_data(parse(text='
    classDef <- setClass( "testClass"
         , slots = c( x="numeric" #< the x field
                    , y="matrix"  #< the y field
                    )
         )', keep.source=TRUE))

    id <- pd[pd$text == "#< the x field", 'id']

    expect_error(line_error(id, 'testing', pd=pd)
                , "<text>:3:  testing")
    expect_error(line_error_if(TRUE, id, 'testing', pd=pd)
                , "<text>:3:  testing")
    expect_error( col_error(id, 'testing col error', pd=pd)
                , "<text>:3:35:  testing col error")
    expect_silent(line_error_if(FALSE, id, 'testing', pd=pd))
    expect_null(line_error_if(FALSE, id, 'testing', pd=pd))
})

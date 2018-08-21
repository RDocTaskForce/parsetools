{#######################################################################
# testing_blocks.R
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




.testing.tags <- c("test", "tests", "testing", "testthat")

#' @export
extract_test_block <-
function( id = all_tagged_iff_ids(pd, .testing.tags)
        , pd = get('pd', parent.frame())
        ){
    #' @title Extract testing blocks from the parse-data.
    #' @param pd a \link{parse-data} object.
    pd <- ._check_parse_data(pd)
    #' @param id iff block id, not the content
    id <- ._check_id(id)
    if (length(id) > 1){
        .l <- lapply(id, extract_test_block, pd=pd)
        return(structure( c(.l, recursive=TRUE)
                        , test.names      = sapply(.l, attr, 'name')
                        , start.locations = utils::head(cumsum(c(1, sapply(.l, length))),-1)
                        ))
    }
    #' @description
    #'   Extract the content of a testing block as a character vector of lines.
    #'   The name, which is attached as an attribute is taken from the info
    #'   string or inferred by location, see Details.
    #'
    stopifnot(pd_is_iff_block(id,pd))
    content.id  <- if_branch(id, pd)

    tag.comment <- children(content.id, pd)[[2]]
    info.string <- trimws(strip_doc_comment_leads(strip_tag(text(tag.comment), .testing.tags)))

    content     <- lines(content.id, pd)

    name <- if (!is.null(info.string) && info.string!='') {
        info.string
    } else {
        #! @details
        #! After the `@` tag you may provide an information
        #! string.  At the moment the information string is
        #! only used for two things. First to infer the `desc`
        #! argument of the generated `<test_that>` call.
        #! Second, the information string will be used in the
        #! absence of a provided `file.out` to name the output file,
        #! which will be prefixed by "test-" and placed in the `dir`
        #! directory.
        #!
        name <- iff_associated_name(id, pd)
        if(is.null(name))
            stop( "illformed block at "
                , paste( filename(pd), start_line(id), start_col(id), sep=':')
                )
        if (attr(name, 'type') == 'setGeneric')
            paste0("setGeneric(\"", name, "\", ...)")
        else if(attr(name, 'type') == 'setClass')
            paste0("setClass(\"", name, "\", ...)")
        else if(attr(name, 'type') == 'setAs')
            deparse(call("as", as.name(attr(name, 'from')), attr(name,"to")))
        else
            name
    }

    line.directive <- paste("#line", start_line(content.id), paste0('"', filename(pd), '"'))


    out.text <- if (length(content)<2)
                    sprintf("test_that('%s', %s)", name, content)
        else
            out.text <- c( sprintf("test_that('%s', %s", name, content[[1]])
                         , content[-c(1, length(content))]
                         , paste0(content[length(content)], ")"))
    out.text <- c( line.directive, out.text)
    structure(out.text, name = name)
    #' @return a character vector with the lines for the specific test(s)
    #'         with the name of the test included as an attribute.
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text={'
    if(F){#!@testing
        # a malplaced testing block
        FALSE
    }
    hello_world <- function(){
        print("hello world")
    }
    if(FALSE){#!@testthat
        expect_output(hello_world(), "hello world")
    }

    ldf <- data.frame(id = 1:26, letters)
    if(FALSE){#!@testing
        # not a function assignment
    }

    f2 <- function(){stop("this does nothing")}
    if(F){#! @example
        hw()
    }
    if(F){#! @test
        expect_error(f2())
    }

    setClass("A")
    if(F){#!@testing
        #testing a setClass
    }

    setMethod("print", "A")
    if(F){#!@testing
        #testing a setMethod
    }

    setGeneric("my_generic", function(x){x})
    if(F){#!@testing
        #testing a setClass
    }

    rnorm(10)
    if(F){#!@testing
        # no previous name
    }

    setAs("class1", "class2", function(from){new(from[[1]], "class2")})
    if(F){#!@testing
        #testing setAs
    }
    '}, keep.source=TRUE))
    iff.ids <- all_tagged_iff_ids(pd, c('testing', 'testthat', 'test'))

    expect_error( extract_test_block(iff.ids[[1L]], pd)
                , "illformed block at <text>:2:5"
                , info = "cannot find name for block"
                )

    expect_equal( extract_test_block(iff.ids[[2L]], pd)
                , structure(c( '#line 9 "<text>"'
                             , 'test_that(\'hello_world\', {#!@testthat'
                             , '        expect_output(hello_world(), "hello world")'
                             , '    })'
                             ), name=structure("hello_world", type = "function_assignment"))
                , info="testing after function assignment")
    expect_equal( extract_test_block(iff.ids[[3L]], pd)
                , structure(c( '#line 14 "<text>"'
                             , 'test_that(\'ldf\', {#!@testing'
                             , '        # not a function assignment'
                             , '    })'
                             ), name = structure("ldf", type = "assignment"))
                , info="testing after other assignment")
    expect_equal( extract_test_block(iff.ids[[4L]], pd)
                , structure(c( '#line 22 "<text>"'
                             , 'test_that(\'f2\', {#! @test'
                             , '        expect_error(f2())'
                             , '    })'
                             ), name=structure("f2", type = "function_assignment"))
                , info="testing after other iff")
    expect_equal( extract_test_block(iff.ids[[5L]], pd)
                , structure(c( '#line 27 "<text>"'
                             , 'test_that(\'setClass("A", ...)\', {#!@testing'
                             , '        #testing a setClass'
                             , '    })'
                             ), name="setClass(\"A\", ...)")
                , info="testing after setClass")
    expect_equal( extract_test_block(iff.ids[[6L]], pd)
                , structure(c( '#line 32 "<text>"'
                             , 'test_that(\'print,A-method\', {#!@testing'
                             , '        #testing a setMethod'
                             , '    })'
                             ), name=structure("print,A-method", type = "setMethod"))
                , info="testing after setMethod")
    expect_equal( extract_test_block(iff.ids[[7L]], pd)
                , structure(c( '#line 37 "<text>"'
                             , 'test_that(\'setGeneric("my_generic", ...)\', {#!@testing'
                             , '        #testing a setClass'
                             , '    })'
                             ), name="setGeneric(\"my_generic\", ...)")
                , info="testing after setGeneric")
    expect_error( extract_test_block(iff.ids[[8L]], pd)
                , info="following call")

    expect_equal( extract_test_block(iff.ids[2:3], pd)
                , structure(c( '#line 9 "<text>"'
                             , 'test_that(\'hello_world\', {#!@testthat'
                             , '        expect_output(hello_world(), "hello world")'
                             , '    })'
                             , '#line 14 "<text>"'
                             , 'test_that(\'ldf\', {#!@testing'
                             , '        # not a function assignment'
                             , '    })'
                             )
                           , test.names = c("hello_world", "ldf")
                           , start.locations = c(1, 5)
                           )
                , info = "multiple ids")

    pd <- get_parse_data(parse(text={"
        if(FALSE){#@testing An info string
            expect_true(T)
        }
    "}, keep.source = TRUE))
    expect_equal( extract_test_block(roots(pd), pd)
                , structure(c( "#line 2 \"<text>\""
                             , "test_that('An info string', {#@testing An info string"
                             , "            expect_true(T)"
                             , "        })"
                             )
                           , name = "An info string")
                , info = "using text string")

    expect_equal( extract_test_block(iff.ids[9], pd)
                , structure(c( '#line 47 "<text>"'
                             , 'test_that(\'as(class1, "class2")\', {#!@testing'
                             , '        #testing setAs'
                             , '    })'
                             )
                           , name = c("as(class1, \"class2\")")
                           )
                , info = "setAs")
}


#@internal
extract_test_blocks_parse_data <-
function( pd ){
    pd <- ._check_parse_data(pd)
    iff.ids <- all_tagged_iff_ids(pd, .testing.tags)
    .l <- lapply(iff.ids, extract_test_block, pd=pd)
    if (length(.l)==0) return(NULL)
    return(structure( c(.l, recursive=TRUE)
                    , test.names      = sapply(.l, attr, 'name')
                    , start.locations = utils::head(cumsum(c(1, sapply(.l, length))),-1)
                    ))
}

#' @export
extract_test_blocks <-
function( file ){
    #' @title extract tests from a file.
    #' @param file the file to retrieve tests from.
    #' @description
    #'    Convenience function for extracting all tests from a file.
    #'    This parses the file and passes the work to
    #'    \code{\link{extract_test_block}}.
    pd <- get_parse_data(parse(file=file, keep.source = TRUE))
    extract_test_blocks_parse_data(pd)
}
if(FALSE){#! @testthat
text <- {'hello_world <- function(){
    print("hello world")
}
if(FALSE){#!@testthat
    expect_output(hello_world(), "hello world")
}

f2 <- function(){stop("this does nothing")}
if(F){#! @test
    expect_error(f2())
}
if(F){#! example
    hw()
}
'}

tmp <- tempfile(fileext = ".R")
writeLines(text, tmp)

test.blocks <- extract_test_blocks(tmp)
expect_equal( test.blocks
            , structure(c( sprintf('#line 4 "%s"', tmp)
                         , 'test_that(\'hello_world\', {#!@testthat'
                         , '    expect_output(hello_world(), "hello world")'
                         , '})'
                         , sprintf('#line 9 "%s"', tmp)
                         , 'test_that(\'f2\', {#! @test'
                         , '    expect_error(f2())'
                         , '})'
                         )
                       , test.names = c("hello_world", "f2")
                       , start.locations = c(1, 5)
                       )
            , info = "Write to file and read back.")
}

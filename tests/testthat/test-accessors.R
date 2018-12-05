#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `accessors.R`')
#line 41 "R/accessors.R"
test_that('token', {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}, keep.source=TRUE))
    expect_equal(token(), pd$token)
    expect_equal(token(c(45,3, 58), pd), c("SYMBOL_FUNCTION_CALL", "SYMBOL", "expr"))
})
#line 56 "R/accessors.R"
test_that('text', {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}, keep.source=TRUE))
    expect_equal(text(pd$id, pd), pd$text)
    expect_equal(text(), pd$text)
    expect_equal(text(c(45,3, 58), pd), c("plot", "x", ""))
})
#line 72 "R/accessors.R"
test_that('nodes', {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}, keep.source=TRUE))
    expect_equal(nodes(pd$id, pd), pd)
    expect_equal(nodes(pd$id), pd)
    expect_equal(nodes(c(45,3, 58), pd), pd[c('45', '3', '58'), ])
})
#line 113 "R/accessors.R"
test_that('filename', {#@test
    pd <- get_parse_data(parse(text="1+1"))
    expect_identical(filename(pd), "<text>")

    attr(pd, 'srcfile') <- NULL
    expect_identical(filename(pd), "<UNKNOWN>")
})
#line 140 "R/accessors.R"
test_that('is_first_on_line', {#@testing
pd <- get_parse_data(parse(text="'

' -> a.multiline.string", keep.source=TRUE))

expect_true (is_first_on_line(1))
expect_false(is_first_on_line(2))

pd <- get_parse_data(parse(text={
"function(x, y){
x+
y+
1
}
"}, keep.source=TRUE))


})
#line 165 "R/accessors.R"
test_that('is_last_on_line', {#@testing
pd <- get_parse_data(parse(text="'

' -> a.multiline.string", keep.source=TRUE))

expect_false(is_last_on_line(1, pd))
expect_true(is_last_on_line(4, pd))
expect_false(is_last_on_line(6, pd))
})
#line 180 "R/accessors.R"
test_that('spans_multiple_lines', {#@testing
pd <- get_parse_data(parse(text="'

' -> a.multiline.string", keep.source=TRUE))
expect_true(spans_multiple_lines(1, pd))
expect_false(spans_multiple_lines(4, pd))
expect_true(spans_multiple_lines(pd_all_root_ids(pd), pd))
})
#line 194 "R/accessors.R"
test_that('terminal_ids_on_line', {#@testing
pd <- get_parse_data(parse(text="      {
         {1 + 3}
{2 + sin(pi)}
      }
", keep.source=TRUE))

expect_equal(terminal_ids_on_line(1), 1)
expect_equal(text(terminal_ids_on_line(2)), c('{', '1', '+', '3', '}'))

pd <- get_parse_data(parse(text="'

' -> a.multiline.string", keep.source=TRUE))
expect_equal(text(terminal_ids_on_line(1, pd)), "'\n\n'")
expect_equal(terminal_ids_on_line(2, pd), 1)
expect_equal(terminal_ids_on_line(4, pd), integer(0))
})
#line 223 "R/accessors.R"
test_that('ids_ending_on_line', {#@testing
pd <- get_parse_data(parse(text={"((1+
2)+
3)+
4"}, keep.source=TRUE))

expect_identical(ids_starting_on_line(1), head(pd$id, 10))
expect_identical(ids_starting_on_line(4), tail(pd$id, 2))
expect_identical(ids_ending_on_line(1), 1:5)
expect_identical(ids_ending_on_line(4), c(26L, 23L, 24L))

})
#line 247 "R/accessors.R"
test_that('prev_terminal', {#@testing
pd <- get_parse_data(parse(text="   rnorm( 10,  0,   3)", keep.source=TRUE))
    id <- 4
    expect_equal(prev_terminal(id, pd), 2L)

    expect_equal( prev_terminal(pd$id, pd=pd)
                , c(NA, NA, NA, 1, rep(2, 2), 4, 6, 6, 9, 11, 11, 14)
                )
})
#line 269 "R/accessors.R"
test_that('expr_text', {#@testing
    pd <- get_parse_data(parse(text="
        signature(x='hello', y='world')
    ", keep.source=TRUE))
    ids <- c( parent(.find_text("'hello'"))
            , parent(.find_text("'world'"))
            )
    expect_identical(expr_text(ids, pd), c("hello", "world"))
    expect_error( expr_text(pd_all_root_ids(pd))
                , "<text>:2:9:  a string constant is expected."
                )
})

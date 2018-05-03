#! This file was automatically produced by documentation::extract_tests on  2018-05-03 10:17:17
#! changes will be overwritten.
context('tests extracted from file `/home/aredd/projects/rdtf/parsetools/R/accessors.R`')
#line 40 "/home/aredd/projects/rdtf/parsetools/R/accessors.R"
test_that('token', {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    expect_equal(token(), pd$token)
    expect_equal(token(c(45,3, 58), pd), c("SYMBOL_FUNCTION_CALL", "SYMBOL", "expr"))
})
#line 55 "/home/aredd/projects/rdtf/parsetools/R/accessors.R"
test_that('text', {#!@testing 
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    expect_equal(text(pd$id, pd), pd$text)
    expect_equal(text(), pd$text)
    expect_equal(text(c(45,3, 58), pd), c("plot", "x", ""))
})
#line 71 "/home/aredd/projects/rdtf/parsetools/R/accessors.R"
test_that('nodes', {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    expect_equal(nodes(pd$id, pd), pd)
    expect_equal(nodes(pd$id), pd)
    expect_equal(nodes(c(45,3, 58), pd), pd[c('45', '3', '58'), ])
    
})
#line 157 "/home/aredd/projects/rdtf/parsetools/R/accessors.R"
test_that('is_last_on_line', {#@testing
"'

' -> a.multiline.string" %>% parse(text=.) %>% get_parse_data() -> pd

expect_false(is_last_on_line(1, pd))
expect_true(is_last_on_line(4, pd))
expect_false(is_last_on_line(6, pd))
})
#line 172 "/home/aredd/projects/rdtf/parsetools/R/accessors.R"
test_that('spans_multiple_lines', {#@testing
"'

' -> a.multiline.string" %>% parse(text=.) %>% get_parse_data() -> pd
expect_true(spans_multiple_lines(1, pd))
expect_false(spans_multiple_lines(4, pd))
expect_true(spans_multiple_lines(all_root_ids(pd), pd))
})
#line 185 "/home/aredd/projects/rdtf/parsetools/R/accessors.R"
test_that('terminal_ids_on_line', {#@testing
"      {
         {1 + 3}
{2 + sin(pi)}
      }
" %>% parse(text=.) %>%
get_parse_data -> pd
expect_equal(terminal_ids_on_line(1), 1)
expect_equal(text(terminal_ids_on_line(2)), c('{', '1', '+', '3', '}'))

"'

' -> a.multiline.string" %>% parse(text=.) %>% get_parse_data() -> pd
expect_equal(text(terminal_ids_on_line(1, pd)), "'\n\n'")
expect_equal(terminal_ids_on_line(2, pd), 1)
expect_equal(terminal_ids_on_line(4, pd), integer(0))
})
#line 211 "/home/aredd/projects/rdtf/parsetools/R/accessors.R"
test_that('ids_ending_on_line', {#@testing
pd <- {"((1+
2)+
3)+
4" %>% parse(text=.) %>%
get_parse_data}

expect_identical(ids_starting_on_line(1), head(pd$id, 10))
expect_identical(ids_starting_on_line(4), tail(pd$id, 2))
expect_identical(ids_ending_on_line(1), 1:5)
expect_identical(ids_ending_on_line(4), c(26L, 23L, 24L))

})
#line 235 "/home/aredd/projects/rdtf/parsetools/R/accessors.R"
test_that('get_prev_terminal_id', {#@testing
    pd <- "   rnorm( 10,  0,   3)" %>% parse(text=.) %>% get_parse_data()
    id <- 4
    expect_equal(get_prev_terminal_id(id, pd), 2L)

    expect_equal( get_prev_terminal_id(pd=pd)
                , c(NA, NA, NA, 1, rep(2, 2), 4, 6, 6, 9, 11, 11, 14)
                )
})

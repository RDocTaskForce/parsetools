#! This file was automatically produced by documentation::extract_tests on  2017-07-08 09:16:19
#! changes will be overwritten.
context('tests extracted from file `C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/accessors.R`')
#line 30 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/accessors.R"
test_that("token", {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    expect_equal(token(), pd$token)
    expect_equal(token(c(45,3, 58), pd), c("SYMBOL_FUNCTION_CALL", "SYMBOL", "expr"))
})
#line 44 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/accessors.R"
test_that("text", {#!@testing 
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    expect_equal(text(pd$id, pd), pd$text)
    expect_equal(text(), pd$text)
    expect_equal(text(c(45,3, 58), pd), c("plot", "x", ""))
})
#line 59 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/accessors.R"
test_that("nodes", {#!@testing
        pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    expect_equal(nodes(pd$id, pd), pd)
    expect_equal(nodes(pd$id), pd)
    expect_equal(nodes(c(45,3, 58), pd), pd[c('45', '3', '58'), ])
    
})
#line 135 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/accessors.R"
test_that("is_last_on_line", {#@testing
#TODO
})
#line 143 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/accessors.R"
test_that("spans_multiple_lines", {#@testing
"'

' -> a.multiline.string" %>% parse(text=.) %>% get_parse_data() -> pd
expect_true(spans_multiple_lines(1, pd))
expect_false(spans_multiple_lines(4, pd))
expect_true(spans_multiple_lines(all_root_ids(pd), pd))
})
#line 155 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/accessors.R"
test_that("terminal_ids_on_line", {#@testing
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
#line 179 "C:/Users/aredd/Box Sync/Projects/rdtf/parsetools/R/accessors.R"
test_that("ids_ending_on_line", {#@testing
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

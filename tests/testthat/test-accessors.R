#! This file was automatically produced by lint on  2017-06-06 12:03:28
#! changes will be overwritten.
context('tests extracted from file `/mnt/data/projects/rdtf/parsetools/R/accessors.R`')
test_that("'token'", {#!@testing
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    expect_equal(token(), pd$token)
    expect_equal(token(c(45,3, 58), pd), c("SYMBOL_FUNCTION_CALL", "SYMBOL", "expr"))
})
test_that("'text'", {#!@testing 
    pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    expect_equal(text(pd$id, pd), pd$text)
    expect_equal(text(), pd$text)
    expect_equal(text(c(45,3, 58), pd), c("plot", "x", ""))
})
test_that("'nodes'", {#!@testing
        pd <- get_parse_data(parse(text={"
        x <- rnorm(10, 0, 1)
        y <- runif(10)
        plot(x, y)
    "}))
    expect_equal(nodes(pd$id, pd), pd)
    expect_equal(nodes(pd$id), pd)
    expect_equal(nodes(c(45,3, 58), pd), pd[c('45', '3', '58'), ])
    
})

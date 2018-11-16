#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `internal.R`')
#line 35 "R/internal.R"
test_that('internal', {#@testing
    external_test <- function(id, pd){"do something"}

    test <- internal(external_test)
    expected <- function(id=pd$id, pd=get('pd', parent.frame()))external_test(id=id, pd=pd)
    environment(expected) <- asNamespace('parsetools')
    expect_identical(test, expected)

    external_test2 <- function(id, pd, .check=TRUE){"do something"}
    test2 <- internal(external_test2)
    expected2 <- function(id=pd$id, pd=get('pd', parent.frame()))external_test2(id=id, pd=pd, .check=FALSE)
    environment(expected2) <- asNamespace('parsetools')
    expect_identical(test2, expected2)

    external_test3 <- function(id, pd, N=1){"do something"}
    test3 <- internal(external_test3)
    expected3 <- function(id=pd$id, pd=get('pd', parent.frame()), ...)external_test3(id=id, pd=pd, ...)
    environment(expected3) <- asNamespace('parsetools')
    expect_identical(test3, expected3)

    external_test4 <- function(id, pd, N=1, .check=TRUE){"do something"}
    test4 <- internal(external_test4)
    expected4 <- function(id=pd$id, pd=get('pd', parent.frame()), ...)external_test4(id=id, pd=pd, ..., .check=FALSE)
    environment(expected4) <- asNamespace('parsetools')
    expect_identical(test4, expected4)
})
#line 86 "R/internal.R"
test_that('make_get_all', {#@test
    pd_is_test <- function(id, pd, n=Inf, .check=TRUE){"do something"}
    test_all <- make_get_all(pd_is_test)

    expected <- function(pd=get('pd', parent.frame()),...)pd[pd_is_test(id=pd$id, pd=pd, ..., .check=FALSE), "id"]
    expect_equal(test_all, expected)
})
#line 117 "R/internal.R"
test_that('external', {#@testing
    internal_test <- function(id=pd$id, pd=get('pd', parent.frame())){"do something"}
    test <- external(internal_test)
    expected <- function(id, pd)internal_test(id=id, pd=pd)
    environment(expected) <- asNamespace('parsetools')
    expect_identical(test, expected)

    internal_test <- function(id=pd$id, pd=get('pd', parent.frame()), .check=FALSE){"do something"}
    test <- external(internal_test)
    expected <- function(id, pd)internal_test(id=id, pd=pd, .check=TRUE)
    environment(expected) <- asNamespace('parsetools')
    expect_identical(test, expected)

    internal_test <- function(id=pd$id, pd=get('pd', parent.frame()), N=1){"do something"}
    test <- external(internal_test)
    expected <- function(id, pd, ...)internal_test(id=id, pd=pd, ...)
    environment(expected) <- asNamespace('parsetools')
    expect_identical(test, expected)

    internal_test <- function(id=pd$id, pd=get('pd', parent.frame()), N=1, .check=FALSE){"do something"}
    test <- external(internal_test)
    expected <- function(id, pd, ...)internal_test(id=id, pd=pd, ..., .check=TRUE)
    environment(expected) <- asNamespace('parsetools')
    expect_identical(test, expected)
})

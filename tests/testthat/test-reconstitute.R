#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `reconstitute.R`')
#line 102 "R/reconstitute.R"
test_that('reconstitute', {#@testing
    pd <- get_parse_data(parse(text='rnorm(10L, mean=0, sd=1)', keep.source=TRUE))
    id <- roots(pd)

    x <- substitute(rnorm(10L, mean=0, sd=1))
    expect_identical( reconstitute(id), x)

    pd <- get_parse_data(p <- parse(text="'pd'", keep.source=TRUE))
    expect_identical( reconstitute(roots(pd))
                    , substitute('pd')
                    )

    pd <- get_parse_data(p <- parse(text='get(\'pd\', parent.frame())', keep.source=TRUE))
    expect_identical( reconstitute(roots(pd))
                    , substitute(get('pd', parent.frame()), emptyenv())
                    )

    pd <- get_parse_data(parse(text={'
        function(id, pd=get(\'pd\', parent.frame())){
            fb <- firstborn(id)
            reconstitute(fb)
        }
        '}, keep.source=TRUE))
    id <- roots(pd)

    reconstituted <- reconstitute(id, pd)
    expect_is(reconstituted, 'call')

    expected <- quote(function(id, pd=get('pd', parent.frame())){
            fb <- firstborn(id)
            reconstitute(fb)
        })
    expect_equal(reconstituted, expected)
})
#line 136 "R/reconstitute.R"
test_that('reconstitute if statements}', {#@test reconstitute if statements}
    pd <- get_parse_data(parse(text={'
        if (TRUE) "YES" else "NO"
        '}, keep.source=TRUE))
    id <- roots(pd)

    reconstituted <- pd_reconstitute(id, pd)
    expect_is(reconstituted, 'if')

    expected <- quote(if (TRUE) "YES" else "NO")
    expect_equal(reconstituted, expected)

    pd <- get_parse_data(parse(text={'
        if (TRUE) "YES"
        '}, keep.source=TRUE))
    id <- roots(pd)

    reconstituted <- pd_reconstitute(id, pd)
    expect_is(reconstituted, 'if')

    expected <- quote(if (TRUE) "YES")
    expect_equal(reconstituted, expected)
})

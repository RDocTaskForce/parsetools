
#' Create a function to test if an id is contained in a type
#'
#' @param calls The tokens to test against
#' @param .is A function to test if a specific id is a valid
#'
#' @internal
pd_make_is_in_call <-
function(calls, .is=pd_make_is_call(calls)){
    force(.is)
    me <-
    function( id, pd, .check=TRUE){
        if (.check){
            pd <- ._check_parse_data(pd)
            id <- ._check_id(id, pd)
        }
        if (length(id) > 1) return(sapply(id, me, pd=pd))
        my.ancestors <- ancestors(id, pd, only.present=TRUE)
        any(.is(id=my.ancestors, pd=pd, .check=FALSE))
    }
}
#' @rdname pd_make_is_in_call
#' @internal
pd_make_is_call <-
function(calls){
    stopifnot( length(calls) > 0
             , is.character(calls)
             )
    me <-
    function(id, pd, .check=TRUE){
        if (.check){
            pd <- ._check_parse_data(pd)
            id <- ._check_id(id, pd)
        }
        if (length(id) > 1L) return(sapply(id, me, pd=pd, .check=FALSE))
        is_symbol_call(id, pd) && text(call_symbol(id, pd)) %in% calls
    }
}
if(FALSE){#@test pd_make_is_call & pd_make_is_in_call
pd <- get_parse_data(parse(text={"
    test <- function(msg){
        cat('test message:', msg, '\n')
    }
    test('my message')
"}, keep.source=TRUE))

    is_in_test <- pd_make_is_in_call('test')
    .is <- environment(is_in_test)[['.is']]
    calls <- environment(is_in_test)[['calls']]

    test.id <- pd_all_root_ids(pd)[[2]]
    id <- pd[pd$text=="'my message'",'id']

    expect_true(pd_is_symbol_call(test.id, pd))
    expect_identical(text(call_symbol(test.id, pd)), 'test')

    expect_true(.is(test.id, pd))
    expect_false(.is(id, pd))
    expect_true(is_in_test(id, pd))
    expect_true(is_in_test(id, pd))
    expect_identical(is_in_test(pd$id, pd=pd), ascend_to_root(pd=pd) == test.id)
}


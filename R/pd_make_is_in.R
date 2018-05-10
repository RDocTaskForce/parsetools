
#' Create a function to test if an id is contained in a type
#'
#' @param calls The tokens to test against
#' @param .is A function to test if a specific id is a valid
#'
#' @internal
pd_make_is_in_call <-
function(calls = character(0), .is=pd_make_is_call(calls)){
    me <-
    function( id = pd$id
            , pd = get('pd', parent.frame())
            ){
        if (length(id) > 1) return(sapply(id, me, pd=pd))
        ancestors <- get_ancestor_ids(id, pd, only.present=TRUE)
        any(.is(id=ancestors, pd=pd))
    }
}
#' @rdname pd_make_is_in_call
#' @internal
pd_make_is_call <-
function(calls = character(0)){
    if (length(calls) ==0)
        pd_is_symbol_call
    else
        function(id, pd)
            (pd_is_symbol_call(id, pd) & (text(pd_get_call_symbol_id(id, pd)) %in% calls))
}

if(FALSE){#@test pd_make_is_call & pd_make_is_in_call
pd <- get_parse_data(parse(text={"
    test <- function(msg){
        cat('test message:', msg, '\n')
    }
    test('my message')
"}))

    is_in_test <- pd_make_is_in_call('test')
    .is <- environment(is_in_test)[['.is']]
    calls <- environment(is_in_test)[['calls']]

    test.id <- all_root_ids(pd)[[2]]
    id <- pd[pd$text=="'my message'",'id']

    expect_true(pd_is_symbol_call(test.id, pd))
    expect_identical(text(pd_get_call_symbol_id(test.id, pd)), 'test')

    expect_true(.is(test.id, pd))
    expect_false(.is(id, pd))
    expect_true(is_in_test(id, pd))
    expect_true(is_in_test(id))
    expect_identical(is_in_test(), ascend_to_root(pd=pd) == test.id)
    expect_identical(is_in_test(pd=pd), ascend_to_root(pd=pd) == test.id)
}


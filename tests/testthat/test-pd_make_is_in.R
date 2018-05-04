#! This file was automatically produced by documentation::extract_tests on  2018-05-04 11:22:55
#! changes will be overwritten.
context('tests extracted from file `pd_make_is_in.R`')
#line 29 "/home/aredd/projects/rdtf/parsetools/R/pd_make_is_in.R"
test_that('pd_make_is_call & pd_make_is_in_call', {#@test pd_make_is_call & pd_make_is_in_call
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
})

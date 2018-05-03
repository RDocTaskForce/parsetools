#! This file was automatically produced by documentation::extract_tests on  2018-05-03 10:17:17
#! changes will be overwritten.
context('tests extracted from file `/home/aredd/projects/rdtf/parsetools/R/pd_classes.R`')
#line 13 "/home/aredd/projects/rdtf/parsetools/R/pd_classes.R"
test_that('pd_is_class_definition', {#@test
'setClass( "testClass"
     , slots = c( x="numeric" #< the x field
                , y="matrix"  #< the y field
                )
     )' %>% 
    parse(text = .) %>%
    get_parse_data() -> pd
    
    expect_true(pd_is_class_definition(pd, id = all_root_ids(pd)))
})
#line 35 "/home/aredd/projects/rdtf/parsetools/R/pd_classes.R"
test_that('object in setClass', {#@test object in setClass
'setClass( "testClass"
     , slots = c( x="numeric" #< the x field
                , y="matrix"  #< the y field
                )
     )
setMethod("print", "testClass", function(){
    cat("This is just a test.")
})
' %>% 
    parse(text = .) %>%
    get_parse_data() -> pd

    root.id <- all_root_ids(pd)
    
    id <- pd[pd$text=="#< the x field", 'id']
    
    expect_true(pd_is_in_class_definition(id, pd))
    
    id2 <- pd[pd$text=='"This is just a test."', 'id']
    expect_false(pd_is_in_class_definition(id2, pd))
    
    expect_identical(pd_is_in_class_definition(c(id, id2), pd), c(TRUE, FALSE))
})

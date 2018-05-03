#! This file was automatically produced by documentation::extract_tests on  2018-05-03 10:17:17
#! changes will be overwritten.
context('tests extracted from file `/home/aredd/projects/rdtf/parsetools/R/pd_comments.R`')
#line 40 "/home/aredd/projects/rdtf/parsetools/R/pd_comments.R"
test_that('function relative comments', {#@test function relative comments
'function( pd                    #< parse data
                                #< continuation comment
        , id = all_root_ids(pd) #< id number
        ){}' %>%
    parse(text = .) %>%
    get_parse_data() -> pd
    
    id <- get_relative_comments(pd)$id

    value <- associate_relative_comments(pd=pd)
    expect_identical(value[[1]], value[[2]])
    expect_identical(text(value, pd=pd), c('pd', 'pd', 'id'))

# while one argument documented and another not should be discouraged, 
# it is allowed.
'function( id, pd = get("pd", parent.frame()) #< parse data
        ){}' %>%
    parse(text = .) %>%
    get_parse_data() -> pd
    id <- get_relative_comments(pd)$id

    expect_identical(text(associate_relative_comments(id, pd), pd=pd), 'pd')

'function( id, #< traditional comma placement.
           pd = get("pd", parent.frame()) #< parse data
         ){}' %>%
    parse(text = .) %>%
    get_parse_data() -> pd
    id <- get_relative_comments(pd)$id

    value <- associate_relative_comments(id, pd)
    expected <- pd[ token(pd=pd)  ==  "SYMBOL_FORMALS"
                  & text(pd=pd)  %in% c("pd", "id")
                  , 'id']
    expect_identical(value, expected)
})
#line 77 "/home/aredd/projects/rdtf/parsetools/R/pd_comments.R"
test_that('class members', {#@test class members
'setClass( "testClass"
         , slots = c( x="numeric" #< the x field
                    , y="matrix"  #< the y field
                    )
         )' %>% 
    parse(text = .) %>%
    get_parse_data() -> pd

    
})

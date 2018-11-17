#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `pd_comments.R`')
#line 46 "R/pd_comments.R"
test_that('function relative comments', {#@test function relative comments
pd <- get_parse_data(parse(text='function( pd                    #< parse data
                                #< continuation comment
        , id = pd_all_root_ids(pd) #< id number
        ){}', keep.source=TRUE))
    id <- all_relative_comment_ids(pd)

    value <- pd_get_relative_comment_associated_ids(id, pd)
    expect_identical(value[[1]], value[[2]])
    expect_identical(text(value, pd=pd), c('pd', 'pd', 'id'))

# while one argument documented and another not should be discouraged,
# it is allowed.
pd <- get_parse_data(parse(text='function( id, pd = get("pd", parent.frame()) #< parse data
        ){}', keep.source=TRUE))
    id <- all_relative_comment_ids(pd)

    expect_identical(text(pd_get_relative_comment_associated_ids(id, pd), pd=pd), 'pd')

pd <- get_parse_data(parse(text='function( id, #< traditional comma placement.
           pd = get("pd", parent.frame()) #< parse data
         ){}', keep.source=TRUE))
    id <- all_relative_comment_ids(pd)

    value <- pd_get_relative_comment_associated_ids(id, pd)
    expected <- pd[ token(pd$id, pd=pd)  ==  "SYMBOL_FORMALS"
                  & text(pd$id, pd=pd)  %in% c("pd", "id")
                  , 'id']
    expect_identical(value, expected)
})
#line 76 "R/pd_comments.R"
test_that('class members', {#@test class members
pd <- get_parse_data(parse(text='
    classDef <- setClass( "testClass"
         , slots = c( x="numeric" #< the x field
                    , y="matrix"  #< the y field
                    )
         )', keep.source=TRUE))

    ids <- all_relative_comment_ids(pd)
    id <- ids[[1]]

    expect_true(pd_is_in_class_definition(id,pd))
    expect_identical( pd_is_in_class_definition(ids,pd), c(TRUE, TRUE))

    expect_false(pd_is_in_class_definition(.find_text('classDef',pd), pd))
})
#line 92 "R/pd_comments.R"
test_that('no possible relative.', {#@test no possible relative.
    pd <- get_parse_data(parse(text='
        #< not a valid relative comment.
        function(  #< also not valid
                  pd #< continuation comment
                , id = pd_all_root_ids(pd) #< id number
                ){}', keep.source=TRUE))
    id <- all_relative_comment_ids(pd)[[1]]
    expect_true(is.na(pd_get_relative_comment_associated_ids(id, pd)))

    id <- all_relative_comment_ids(pd)[[2]]
    expect_true(is.na(pd_get_relative_comment_associated_ids(id, pd)))
})

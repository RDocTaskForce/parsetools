

.class.defining.functions <- c('setClass', 'setRefClass', 'R6Class')
pd_is_class_definition <- 
function( id = pd$id
        , pd = get('pd', parent.frame())
        , funs = .class.defining.functions #< valid class defining functions 
        ){
    if (length(id)>1) return(sapply(id, pd_is_class_definition, pd=pd, funs=funs))
    pd_is_symbol_call(pd, id) &&
    text(get_pd_call_symbol_id(pd, id)) %in% funs
}
if(FALSE){#@test
'setClass( "testClass"
     , slots = c( x="numeric" #< the x field
                , y="matrix"  #< the y field
                )
     )' %>% 
    parse(text = .) %>%
    get_parse_data() -> pd
    
    expect_true(pd_is_class_definition(pd, id = all_root_ids(pd)))
}

#' @internal
pd_is_in_class_definition <- 
function( id = pd$id
        , pd = get('pd', parent.frame())
        , funs = .class.defining.functions #< valid class defining functions 
        ){
    if (length(id) > 1) return(sapply(id, pd_is_in_class_definition, pd=pd, funs=funs))
    ancestors <- get_ancestor_ids(pd, id, only.present=TRUE)
    any(pd_is_class_definition(ancestors, pd=pd, funs=funs))
}
if(FALSE){#@test object in setClass
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
}



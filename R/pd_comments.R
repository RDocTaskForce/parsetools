

#' Associate relative ducumentation comments
#' 
#' Relative comment created with `#<` comment tags document something
#' designated by the location of the comment.
#' In general, the comment documents the previous symbol.
#' A comment will not be associated with any parse id that does not have
#' the same parent as the comment. For example,
#' 
#'     function(x #< a valid comment
#'             ){
#' 
#' would associate `a valid comment` with `x`, but
#' 
#'     function(x){ #< not a valid comment     
#' 
#' would not.
#' 
#' @value Returns a vector of the same length as id.  Where the value is 
#'        either the id of the associated object or NA if it cannot be 
#'        associated.
associate_relative_comments <- 
function( id = get_relative_comments(pd)$id
        , pd = get('pd', parent.frame())
        ){
    if (length(id)>1L) return(sapply(id, associate_relative_comments, pd=pd))

    sibs <- get_sibling_ids(id, pd)
    possible <- sibs[token(sibs, pd) == 'SYMBOL_FORMALS']
    if (length(possible) == 0L) return(NA)
    possible <- possible[end_line(possible) <= start_line(id)]
    if (length(possible)==1L) return(possible)
    if (length(possible) == 0L) return(NA)
    possible <- possible[end_line(possible) == max(end_line(possible))]
    possible <- possible[end_col(possible) == max(end_col(possible))]
    stopifnot(length(possible) == 1)
    return(possible)
}
if(F){#@test function relative comments
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
}
if(F){#@test class members
'setClass( "testClass"
         , slots = c( x="numeric" #< the x field
                    , y="matrix"  #< the y field
                    )
         )' %>% 
    parse(text = .) %>%
    get_parse_data() -> pd

    
}

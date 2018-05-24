

#' Associate relative ducumentation comments
#'
#' Relative comment created with \code{\#\<} comment tags document something
#' designated by the location of the comment.
#' In general, the comment documents the previous symbol.
#' A comment will not be associated with any parse id that does not have
#' the same parent as the comment. For example,
#'
#'     function(x #< a valid comment
#'             ){}
#'
#' would associate \code{a valid comment} with \code{x}, but
#'
#'     function(x){ #< not a valid comment
#'                }
#'
#' would not.
#'
#' @return Returns a vector of the same length as id.  Where the value is
#'         either the id of the associated object or NA if it cannot be
#'         associated.
#' @export
pd_get_relative_comment_associated_ids <-
function( id, pd, .check=TRUE){
#' @inheritParams pd_get_children_ids
    if (.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot( all(pd_is_relative_comment(id, pd)))
    }
    if (length(id)>1L) return(sapply(id, pd_get_relative_comment_associated_ids, pd=pd))

    sibs <- siblings(id, pd)
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
relative_comment_associateds <- internal(pd_get_relative_comment_associated_ids)
if(F){#@test function relative comments
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
}
if(F){#@test class members
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

    expect_false(pd_is_in_class_definition(pd_find_text('classDef',pd), pd))
}

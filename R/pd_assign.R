

assignment.opperators <- c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN")

#' @export
pd_is_assignment <-
function( id = all_root_ids(pd)[1]
        , pd = get('pd', parent.frame())
        ){
    #' @title Check if it is an assignment
    #' @inheritParams get_children_ids
    #' @description
    #'   check if the provided parse-data is an assignment expression.
    id <- ._check_id(id)
    if (length(id)>1L) return(sapply(id, pd_is_assignment, pd=pd))

    token(id) == 'expr' &&
    any(token(children(id)) %in% assignment.opperators)
}
if(F){#! @testthat pd_is_assignment
    pd <- get_parse_data(parse(text="x <-  1", keep.source=TRUE))
    expect_true(pd_is_assignment(pd=pd))
    pd <- get_parse_data(parse(text="x <<- 1", keep.source=TRUE))
    expect_true(pd_is_assignment(pd=pd))
    pd <- get_parse_data(parse(text="1 ->  x", keep.source=TRUE))
    expect_true(pd_is_assignment(pd=pd))
    pd <- get_parse_data(parse(text="1 ->> x", keep.source=TRUE))
    expect_true(pd_is_assignment(pd=pd))
    pd <- get_parse_data(parse(text="x = 1", keep.source=TRUE))
    expect_true(pd_is_assignment(pd=pd))
}

#' @export
pd_get_assign_value_id <-
function( id = all_root_ids(pd)
        , pd = get('pd', parent.frame())
        ){
    #' @title Get the id for the value portion of an assignment
    #' @inheritParams pd_is_assignment
    #' @description
    #'    Gives the id of the value portion of the assignment, while correctly
    #'    accounting for the direction of the arrow.
    if(length(id) > 1)
        sapply(id, pd_get_assign_value_id, pd=pd)
    child.ids <- get_children_ids(id, pd, 1, FALSE)
    type <- pd[pd$id %in% child.ids & pd$token %in% assignment.opperators, 'token']
    switch( type
          , RIGHT_ASSIGN = min(child.ids)
          , max(child.ids)
          )
    #' @return an id integer.
}
if(FALSE){#!@testing
pd <- get_parse_data(parse(text="x<-1", keep.source=TRUE))
val.id <- pd_get_assign_value_id(pd=pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="x=1", keep.source=TRUE))
val.id <- pd_get_assign_value_id(pd=pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="x<<-1", keep.source=TRUE))
val.id <- pd_get_assign_value_id(pd=pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="1->x", keep.source=TRUE))
val.id <- pd_get_assign_value_id(pd=pd)
expect_equal(val.id, 2L)

pd <- get_parse_data(parse(text="1->>x", keep.source=TRUE))
val.id <- pd_get_assign_value_id(pd=pd)
expect_equal(val.id, 2L)
}

#' @export
pd_get_assign_value <-
function( pd #< The [parse-data] object, representing an assignment
        , id = all_root_ids(pd)
        ){
    #' @title get the value of an assignment operator expression.
    #' @inheritParams pd_get_assign_value_id
    #' @description
    #'   A convenience wrapper for getting the subset parse-data for
    #'   the value of an assignemtn expression.
    get_family(pd_get_assign_value_id(id), pd)
    #' @return a \code{\link{parse-data}} object.
}
if(FALSE){#! @testthat pd_get_assign_value
pd <- get_parse_data(parse(text="x<-1", keep.source=TRUE))

val.pd <- pd_get_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="x=1", keep.source=TRUE))
val.pd <- pd_get_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="x<<-1", keep.source=TRUE))
val.pd <- pd_get_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="1->x", keep.source=TRUE))
val.pd <- pd_get_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="1->>x", keep.source=TRUE))
val.pd <- pd_get_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)
}

#' @export
pd_get_assign_variable_id <-
function( pd #< The [parse-data] object, representing an assignment
        , id = all_root_ids(pd)
        ){
    #' @title Get the variable of an assignment
    #' @inheritParams pd_is_assignment
    #' @description
    #'   Gets the id for the variable portion of an assignment expression.
    #'   This accounts for the direction of the assignment arrow.
    #'
    if(length(id) > 1)
        sapply(id, pd_get_assign_variable_id, pd=pd)
    child.ids   <- get_children_ids(id, pd, 1, FALSE)
    assign.pd   <- pd[pd$id %in% child.ids & pd$token %in% assignment.opperators, ]
    switch( assign.pd$token
          , RIGHT_ASSIGN = max(child.ids)
          , min(setdiff(child.ids, assign.pd$id))
          )
    #' @return an id integer, typically pointing to an 'expr' node in pd.
}
if(F){#!@testthat
pd <- get_parse_data(parse(text="hello_world <- function(){
    print('hello world')
}
", keep.source=TRUE))

    expect_true(pd_is_assignment(pd=pd))

    var.id <- pd_get_assign_variable_id(pd=pd)
    expect_equal(var.id, parent(pd_find_text("hello_world")))
}




assignment.opperators <- c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN")

#' @export
pd_is_assignment <-
function( pd            #< parse data of assignemnt
        , id = all_root_ids(pd)[1] #< id of interest.
        ){
    #' @title Check if it is an assignment
    #' @inheritParams get_child_ids
    #' @description
    #'   check if the provided parse-data is an assignment expression.
    id <- ._check_id(id)
    if(pd[pd$id == id, 'token'] != 'expr')
        FALSE
    kids.pd <- get_children(id, pd, ngenerations=1)[[1]]
    kids.pd <- kids.pd[kids.pd[['token']] != 'expr', ]

    any(kids.pd[['token']] %in% assignment.opperators)
}
if(F){#! @testthat pd_is_assignment
    pd <- get_parse_data(parse(text="x <-  1"))
    expect_true(pd_is_assignment(pd=pd))
    pd <- get_parse_data(parse(text="x <<- 1"))
    expect_true(pd_is_assignment(pd=pd))
    pd <- get_parse_data(parse(text="1 ->  x"))
    expect_true(pd_is_assignment(pd=pd))
    pd <- get_parse_data(parse(text="1 ->> x"))
    expect_true(pd_is_assignment(pd=pd))
    pd <- get_parse_data(parse(text="x = 1"))
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
    child.ids <- get_child_ids(id, pd, 1, FALSE)
    type <- pd[pd$id %in% child.ids & pd$token %in% assignment.opperators, 'token']
    switch( type
          , RIGHT_ASSIGN = min(child.ids)
          , max(child.ids)
          )
    #' @return an id integer.
}
if(FALSE){#!@testing
pd <- get_parse_data(parse(text="x<-1"))
val.id <- pd_get_assign_value_id(pd=pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="x=1"))
val.id <- pd_get_assign_value_id(pd=pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="x<<-1"))
val.id <- pd_get_assign_value_id(pd=pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="1->x"))
val.id <- pd_get_assign_value_id(pd=pd)
expect_equal(val.id, 2L)

pd <- get_parse_data(parse(text="1->>x"))
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
pd <- get_parse_data(parse(text="x<-1"))

val.pd <- pd_get_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="x=1"))
val.pd <- pd_get_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="x<<-1"))
val.pd <- pd_get_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="1->x"))
val.pd <- pd_get_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="1->>x"))
val.pd <- pd_get_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)
}

#' @export
pd_get_assign_variable <-
function( pd #< The [parse-data] object, representing an assignment
        ){
    #' @title Get the variable of an assignment
    #' @inheritParams pd_is_assignment
    #' @description
    #'   Gets the variable portion of an assignment expression. This can be a
    #'   single variable or an expression for assignment to portions like
    #'   like assigning to an indexed element of a vector.
    kids.pd <- sort(get_child(id=all_root_ids(pd), pd, 1, FALSE))
    switch( kids.pd[2, 'token']
          , RIGHT_ASSIGN = get_family(id = utils::tail(kids.pd$id,1), pd)
          , LEFT_ASSIGN  = get_family(id = utils::head(kids.pd$id,1), pd)
          , EQ_ASSIGN    = get_family(id = utils::head(kids.pd$id,1), pd)
          )
    #' @return will return the \code{\link{parse-data}}, 
    #'         which is typically rooted by an 'expr' token.
}
if(F){#!@testthat
    pd <- get_parse_data(parse(text ={"hello_world <- function(){
        print('hello world')
    }
    "}))

    expect_true(pd_is_assignment(pd=pd))
    var.pd <- pd_get_assign_variable(pd=pd)
    expect_equal(getParseText(var.pd, all_root_ids(var.pd)), "hello_world")
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
    child.ids   <- get_child_ids(id, pd, 1, FALSE)
    assign.pd   <- pd[pd$id %in% child.ids & pd$token %in% assignment.opperators, ]
    switch( assign.pd$token
          , RIGHT_ASSIGN = max(child.ids)
          , min(setdiff(child.ids, assign.pd$id))
          )
    #' @return an id integer, typically pointing to an 'expr' node in pd.
}
if(F){#!@testthat
"hello_world <- function(){
    print('hello world')
}
" %>%
parse(text = .) %>%
get_parse_data() %>%
sort-> pd

    expect_true(pd_is_assignment(pd=pd))

    var.pd <- pd_get_assign_variable(pd=pd)
    var.id <- pd_get_assign_variable_id(pd=pd)
    expect_equal(var.id, all_root_ids(var.pd))
}


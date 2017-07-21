

assignment.opperators <- c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN")

#' @export
is_pd_assignment <-
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
    kids.pd <- get_children(pd, id, ngenerations=1)[[1]]
    kids.pd <- kids.pd[kids.pd[['token']] != 'expr', ]

    any(kids.pd[['token']] %in% assignment.opperators)
}
if(F){#! @testthat is_pd_assignment
    pd <- get_parse_data(parse(text="x <-  1"))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="x <<- 1"))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="1 ->  x"))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="1 ->> x"))
    expect_true(is_pd_assignment(pd))
    pd <- get_parse_data(parse(text="x = 1"))
    expect_true(is_pd_assignment(pd))
}

#' @export
get_pd_assign_value_id <-
function( pd
        , id = all_root_ids(pd)
        ){
    #' @title Get the id for the value portion of an assignment
    #' @inheritParams is_pd_assignment
    #' @description
    #'    Gives the id of the value portion of the assignment, while correctly
    #'    accounting for the direction of the arrow.
    if(length(id) > 1)
        sapply(id, get_pd_assign_value_id, pd=pd)
    child.ids <- get_child_ids(pd, id, 1, FALSE)
    type <- pd[pd$id %in% child.ids & pd$token %in% assignment.opperators, 'token']
    switch( type
          , RIGHT_ASSIGN = min(child.ids)
          , max(child.ids)
          )
    #' @return an id integer.
}
if(FALSE){#!@testing
pd <- get_parse_data(parse(text="x<-1"))
val.id <- get_pd_assign_value_id(pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="x=1"))
val.id <- get_pd_assign_value_id(pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="x<<-1"))
val.id <- get_pd_assign_value_id(pd)
expect_equal(val.id, 5L)

pd <- get_parse_data(parse(text="1->x"))
val.id <- get_pd_assign_value_id(pd)
expect_equal(val.id, 2L)

pd <- get_parse_data(parse(text="1->>x"))
val.id <- get_pd_assign_value_id(pd)
expect_equal(val.id, 2L)
}

#' @export
get_pd_assign_value <-
function( pd #< The [parse-data] object, representing an assignment
        , id = all_root_ids(pd)
        ){
    #' @title get the value of an assignment operator expression.
    #' @inheritParams get_pd_assign_value_id
    #' @description
    #'   A convenience wrapper for getting the subset parse-data for 
    #'   the value of an assignemtn expression.
    get_family(pd, get_pd_assign_value_id(pd, id))
    #' @return a \code{\link{parse-data}} object.
}
if(FALSE){#! @testthat get_pd_assign_value
pd <- get_parse_data(parse(text="x<-1"))

val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="x=1"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="x<<-1"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="1->x"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)

pd <- get_parse_data(parse(text="1->>x"))
val.pd <- get_pd_assign_value(pd)
expect_true("NUM_CONST" %in% val.pd$token)
}

#' @export
get_pd_assign_variable <-
function( pd #< The [parse-data] object, representing an assignment
        ){
    #' @title Get the variable of an assignment
    #' @inheritParams is_pd_assignment
    #' @description
    #'   Gets the variable portion of an assignment expression. This can be a
    #'   single variable or an expression for assignment to portions like
    #'   like assigning to an indexed element of a vector.
    kids.pd <- sort(get_child(id=all_root_ids(pd), pd, 1, FALSE))
    switch( kids.pd[2, 'token']
          , RIGHT_ASSIGN = get_family(pd, id = utils::tail(kids.pd$id,1))
          , LEFT_ASSIGN  = get_family(pd, id = utils::head(kids.pd$id,1))
          , EQ_ASSIGN    = get_family(pd, id = utils::head(kids.pd$id,1))
          )
    #' @return will return the \code{\link{parse-data}}, 
    #'         which is typically rooted by an 'expr' token.
}
if(F){#!@testthat
    pd <- get_parse_data(parse(text ={"hello_world <- function(){
        print('hello world')
    }
    "}))

    expect_true(is_pd_assignment(pd))
    var.pd <- get_pd_assign_variable(pd)
    expect_equal(getParseText(var.pd, all_root_ids(var.pd)), "hello_world")

}

#' @export
get_pd_assign_variable_id <-
function( pd #< The [parse-data] object, representing an assignment
        , id = all_root_ids(pd)
        ){
    #' @title Get the variable of an assignment
    #' @inheritParams is_pd_assignment
    #' @description
    #'   Gets the id for the variable portion of an assignment expression. 
    #'   This accounts for the direction of the assignment arrow.
    #'   
    if(length(id) > 1)
        sapply(id, get_pd_assign_variable_id, pd=pd)
    child.ids   <- get_child_ids(pd, id, 1, FALSE)
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

    expect_true(is_pd_assignment(pd))

    var.pd <- get_pd_assign_variable(pd)
    var.id <- get_pd_assign_variable_id(pd)
    expect_equal(var.id, all_root_ids(var.pd))
}


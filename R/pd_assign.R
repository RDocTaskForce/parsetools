

assignment.opperators <- c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN")

#' @export
is_pd_assignment <-
function( pd            #< parse data of assignemnt
        , id = all_root_ids(pd)[1] #< id of interest.
        ){
    #! check if the provided parse-data is an assignment expression.
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
    #! Get the id for the value portion of an assignment operator expression.
    if(length(id) > 1)
        sapply(id, get_pd_assign_value_id, pd=pd)
    child.ids <- get_child_ids(pd, id, 1, FALSE)
    type <- pd[pd$id %in% child.ids & pd$token %in% assignment.opperators, 'token']
    switch( type
          , RIGHT_ASSIGN = min(child.ids)
          , max(child.ids)
          )
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
    #! get the value of an assignment operator expression.
    #!
    #! This function assumes correct structure and does not check for compliance.
    get_family(pd, get_pd_assign_value_id(pd, id))
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
    #! get the value of an assignment operator expression.
    #!
    #! This function assumes correct structure and does not check for compliance.
    kids.pd <- sort(get_child(id=all_root_ids(pd), pd, 1, FALSE))
    switch( kids.pd[2, 'token']
          , RIGHT_ASSIGN = get_family(pd, id = utils::tail(kids.pd$id,1))
          , LEFT_ASSIGN  = get_family(pd, id = utils::head(kids.pd$id,1))
          , EQ_ASSIGN    = get_family(pd, id = utils::head(kids.pd$id,1))
          )
    #! @return will return the parse data, which is typically rooted by an 'expr' token.
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
    #! Get the id for the variable portion of an assignment operator expression.
    if(length(id) > 1)
        sapply(id, get_pd_assign_variable_id, pd=pd)
    child.ids   <- get_child_ids(pd, id, 1, FALSE)
    assign.pd   <- pd[pd$id %in% child.ids & pd$token %in% assignment.opperators, ]
    switch( assign.pd$token
          , RIGHT_ASSIGN = max(child.ids)
          , min(setdiff(child.ids, assign.pd$id))
          )
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

#' @export
is_pd_function <-
function( pd #< a [parse-data] object
        , id = all_root_ids(pd)
        ){
    #! test if parse data is a function
    kids.pd <- get_child(id=id, pd, ngenerations=1, FALSE)
    kids.pd[1, 'token'] == 'FUNCTION'
}
if(F){#! @testthat is_pd_function
    pd <- get_parse_data(parse(text="function(){}"))
    expect_true(is_pd_function(pd))

    pd <- get_parse_data(parse(text="fun <- function(){}"))
    expect_false(is_pd_function(pd))
}

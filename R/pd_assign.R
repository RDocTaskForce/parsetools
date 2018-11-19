#' @include internal.R

assignment.opperators <- c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN")

#' @name assignments
#' @title Assignment Node Navigation.
#' @description
#' These function help identify and navigate assignments in parse data.
#'
#' @details
#' These functions only deal with assignment operators.
#' Using [base::assign()] or [base::delayedAssign()] are considered
#' calls in terms of parse data.
#'
#' There are five assignment operators grouped into three categories.
#'
#' * Left assignment, the [base::assignOps](`<-`) and [base::assignOps](`<<-`),
#' * right assignment, [base::assignOps](`->`) and the rarely used [base::assignOps](`->>`)
#' * and the equals assignment [base::assignOps](`=`).
#'
#' @inheritParams pd_get_children_ids
NULL

#' @describeIn assignments Check if the node is an assignment expression.
pd_is_assignment <-
function( id, pd, .check=TRUE){
    if (.check) {
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
    }
    if (length(id)>1L) return(sapply(id, pd_is_assignment, pd=pd))

    token(id) %in% c('expr', 'equal_assign') &&
    any(token(children(id)) %in% assignment.opperators)
}
all_assignment_ids <- make_get_all(pd_is_assignment)
pd_all_assignment_ids <- external(all_assignment_ids)
is_assignment <- internal(pd_is_assignment)
if(FALSE){#@testing
    pd1 <- get_parse_data(parse(text="x <-  1", keep.source=TRUE))
    expect_true(pd_is_assignment(roots(pd1), pd=pd1))
    pd2 <- get_parse_data(parse(text="x <<- 1", keep.source=TRUE))
    expect_true(pd_is_assignment(roots(pd2), pd=pd2))
    pd3 <- get_parse_data(parse(roots(pd), text="1 ->  x", keep.source=TRUE))
    expect_true(pd_is_assignment(roots(pd3), pd=pd3))
    pd4 <- get_parse_data(parse(text="1 ->> x", keep.source=TRUE))
    expect_true(pd_is_assignment(roots(pd4), pd=pd4))
    pd5 <- get_parse_data(parse(text="x = 1", keep.source=TRUE))
    expect_true(pd_is_assignment(roots(pd5), pd=pd5))
}


#' @describeIn assignments Get the id for the value portion of an assignment.
pd_get_assign_value_id <-
function( id, pd, .check = TRUE){
    if(.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot(all(pd_is_assignment(id, pd)))
    }
    if(length(id) > 1)
        sapply(id, pd_get_assign_value_id, pd=pd) #nocov
    child.ids <- children(id, pd, 1, FALSE)
    type <- pd[pd$id %in% child.ids & pd$token %in% assignment.opperators, 'token']
    switch( type
          , RIGHT_ASSIGN = min(child.ids)
          , max(child.ids)
          )
}
assign_value <- internal(pd_get_assign_value_id, all_assignment_ids(pd))
if(FALSE){#!@testing
pd <- get_parse_data(parse(text="x<-1", keep.source=TRUE))
expect_equal(pd_get_assign_value_id(all_assignment_ids(pd), pd=pd), 5L)

pd <- get_parse_data(parse(text="x=1", keep.source=TRUE))
expect_equal(pd_get_assign_value_id(all_assignment_ids(pd), pd=pd), 5L)

pd <- get_parse_data(parse(text="x<<-1", keep.source=TRUE))
expect_equal(pd_get_assign_value_id(all_assignment_ids(pd), pd=pd), 5L)

pd <- get_parse_data(parse(text="1->x", keep.source=TRUE))
expect_equal(pd_get_assign_value_id(all_assignment_ids(pd), pd=pd), 2L)

pd <- get_parse_data(parse(text="1->>x", keep.source=TRUE))
expect_equal(pd_get_assign_value_id(all_assignment_ids(pd), pd=pd), 2L)
}

#' @describeIn assignments Get the variable of an assignment.
pd_get_assign_variable_id <-
function( id, pd, .check=TRUE){
    if(.check){
        pd <- ._check_parse_data(pd)
        id <- ._check_id(id, pd)
        stopifnot(all(pd_is_assignment(id, pd)))
    }
    if(length(id) > 1)
        sapply(id, pd_get_assign_variable_id, pd=pd) #nocov
    child.ids   <- children(id, pd, 1, FALSE)
    assign.pd   <- pd[pd$id %in% child.ids & pd$token %in% assignment.opperators, ]
    switch( assign.pd$token
          , RIGHT_ASSIGN = max(child.ids)
          , min(setdiff(child.ids, assign.pd$id))
          )
}
assign_variable <- internal(pd_get_assign_variable_id, all_assignment_ids(pd))
if(F){#!@testthat
pd <- get_parse_data(parse(text="hello_world <- function(){
    print('hello world')
}
", keep.source=TRUE))

    expect_true(pd_is_assignment(roots(pd), pd=pd))
    expect_equal( pd_get_assign_variable_id(roots(pd), pd=pd)
                , parent(.find_text("hello_world")))
}
if(F){#@test right_assign
    pd <- get_parse_data(parse(text="'hello_world' -> hw", keep.source=TRUE))
    expect_true(pd_is_assignment(roots(pd), pd))
    expect_equal( pd_get_assign_variable_id(roots(pd), pd=pd)
                , parent(.find_text("hw")))
}


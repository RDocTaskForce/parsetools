
line_error <- function(id, msg, ..., pd=get('pd', parent.frame()))
    stop(filename(pd), ':', start_line(id, pd), ':  ', msg, ...)

line_error_if <- function(test, id, msg, ..., pd=get('pd', parent.frame()))
    if (force(test))
        stop(filename(pd), ':', start_line(id, pd), ':  ', msg, ...)

col_error <- function(id, msg, ..., pd=get('pd', parent.frame()))
    stop(filename(pd), ':', start_line(id, pd), ':', start_col(id, pd), ':  ', msg, ...)


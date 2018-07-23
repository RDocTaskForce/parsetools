


#' @internal
.find_text <- function(text, pd=get('pd', parent.frame())){
    stopifnot(length(text)==1L)
    pd[pd$text == text, 'id']
}

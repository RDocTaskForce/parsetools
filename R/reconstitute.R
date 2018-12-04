
#' Reconstitute Expressions
#'
#' Creates expressions and calls from the given
#' `id` and parse-data, `pd`.
#' @inheritParams pd_get_children_ids
#' @example inst/examples/example-pd.R
#' @example inst/examples/example-roots.R
#' @example inst/examples/example-reconstitute.R
pd_reconstitute <- local({
    NUM_CONST <- function(id, pd=get('pd', parent.frame())){
        t <- text(id)
        if (t == "TRUE") return(TRUE) else
        if (t == "FALSE") return(FALSE) else
        if (substr(t, nchar(t), nchar(t)) == 'L')
            return(as.integer(substr(t, 0L, nchar(t) - 1L)))
        as.numeric(t)
    }
    STR_CONST <- function(id, pd=get('pd', parent.frame())) unquote(text(id))
    SYMBOL_FORMALS <- function(id, pd=get('pd', parent.frame()))
        as.name(text(id))
    SYMBOL_FUNCTION_CALL <- function(id, pd=get('pd', parent.frame())){
        call.id <- parent(parent(id))
        call.name <- text(call_symbol(call.id))
        args <- call_args(call.id)
        recon.args <- lapply(args, reconstitute, pd=pd)
        as.call(c(as.name(call.name), recon.args))
    }
    "'('" <- function(id, pd=get('pd', parent.frame())){
        reconstitute(next_sibling(id))
    }
    "'{'" <- function(id, pd=get('pd', parent.frame())){
        sibs <- utils::head(siblings(id), -1)
        exprs <- lapply(sibs[-1], reconstitute, pd=pd)
        as.call(c(as.name('{'), exprs))
    }
    LEFT_ASSIGN <- function(id, pd=get('pd', parent.frame())){
        kids <- children(id)
        name <- reconstitute(next_sibling(id))
        value <- reconstitute(next_sibling(next_sibling(id)))
        call('<-', name, value)
    }
    SYMBOL <- function(id, pd=get('pd', parent.frame())){
        as.symbol(text(id))
    }
    expr <- function(id, pd=get('pd', parent.frame())){
        reconstitute(firstborn(id))
    }
    FUNCTION <- function(id, pd=get('pd', parent.frame())){
        sibs <- siblings(id, pd)
        args <- sibs[2:(length(sibs)-2)]
        args <- split(args, cumsum(token(args) %in% c("'('", "','")))

        names <- sapply(args, function(.)text(.)[token(.) == 'SYMBOL_FORMALS'])

        recon.args <- sapply(args, function(.){
                if(length(.) <= 2)
                    alist(x=)$x
                else if(any(token(.) == 'SYMBOL_FORMALS'))
                    expr(.[token(.)=='expr'])
            })
        names(recon.args) <- names
        body.id <- sibs[length(sibs)]
        stopifnot(token(body.id) == 'expr')
        fun.body <- expr(body.id)
        call(text(id), as.pairlist(recon.args), fun.body)
    }
    "'!'" <- function(id, pd=get('pd', parent.frame())){
        call('!', reconstitute(next_sibling(id)))
    }
    IF <- function(id, pd=get('pd', parent.frame())){
        n <- length(siblings(id))
        if(n==5){
            call( 'if'
                , reconstitute(if_predicate(parent(id)))
                , reconstitute(if_branch(parent(id)))
                )
        } else
        if (n==7){
            call( 'if'
                , reconstitute(if_predicate(parent(id)))
                , reconstitute(if_branch(parent(id)))
                , reconstitute(if_alternate(parent(id)))
                )
        } else
            stop("ill-formed if statement")
    }
    recon.env <- environment()
    reconstitute <- function(id, pd=get('pd', parent.frame())){
        fun <- get(token(id), mode='function', envir=recon.env, inherits=FALSE)
        fun(id=id, pd=pd)
    }
    function(id, pd, .check=TRUE){
        if (.check) {
            pd <- ._check_parse_data(pd)
            id <- ._check_id(id, pd)
        }
        reconstitute(id=id, pd=pd)
    }
})
reconstitute <- internal(pd_reconstitute)
if(F){#@testing
    pd <- get_parse_data(parse(text='rnorm(10L, mean=0, sd=1)', keep.source=TRUE))
    id <- roots(pd)

    x <- substitute(rnorm(10L, mean=0, sd=1))
    expect_identical( reconstitute(id), x)

    pd <- get_parse_data(p <- parse(text="'pd'", keep.source=TRUE))
    expect_identical( reconstitute(roots(pd))
                    , substitute('pd')
                    )

    pd <- get_parse_data(p <- parse(text='get(\'pd\', parent.frame())', keep.source=TRUE))
    expect_identical( reconstitute(roots(pd))
                    , substitute(get('pd', parent.frame()), emptyenv())
                    )

    pd <- get_parse_data(parse(text={'
        function(id, pd=get(\'pd\', parent.frame())){
            fb <- firstborn(id)
            reconstitute(fb)
        }
        '}, keep.source=TRUE))
    id <- roots(pd)

    reconstituted <- reconstitute(id, pd)
    expect_is(reconstituted, 'call')

    expected <- quote(function(id, pd=get('pd', parent.frame())){
            fb <- firstborn(id)
            reconstitute(fb)
        })
    expect_equal(reconstituted, expected)
}
if(FALSE){#@test reconstitute if statements}
    pd <- get_parse_data(parse(text={'
        if (TRUE) "YES" else "NO"
        '}, keep.source=TRUE))
    id <- roots(pd)

    reconstituted <- pd_reconstitute(id, pd)
    expect_is(reconstituted, 'if')

    expected <- quote(if (TRUE) "YES" else "NO")
    expect_equal(reconstituted, expected)

    pd <- get_parse_data(parse(text={'
        if (TRUE) "YES"
        '}, keep.source=TRUE))
    id <- roots(pd)

    reconstituted <- pd_reconstitute(id, pd)
    expect_is(reconstituted, 'if')

    expected <- quote(if (TRUE) "YES")
    expect_equal(reconstituted, expected)
}

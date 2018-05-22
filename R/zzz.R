
formal_arg_is_missing <- function(x)identical(x, alist(x=)$x)

if(FALSE){#! test export compliance

    grep('^all_', getNamespaceExports('parsetools'), value=TRUE) %>%
        grep('_ids', ., value=TRUE, invert=TRUE)

    grep('^get_', getNamespaceExports('parsetools'), value=TRUE) %>%
        grep("ids$",., value=TRUE, invert = TRUE) %>%
        grep("^get_parse_data", ., value=TRUE, invert=TRUE)
    grep('^is_', getNamespaceExports('parsetools'), value=TRUE) %>%
        grep('\\.(data\\.frame|parse-data|character)$', ., invert=TRUE, value=TRUE)

    names.pd_is_ <- grep('pd_is_', getNamespaceExports('parsetools'), value=TRUE)
    for(i in seq_along(names.pd_is_)){
        name <- names.pd_is_[[i]]
        f <- get(name, asNamespace('parsetools'))
        args <- formals(f)

        id.present    <- match('id'    , names(args))
        pd.present    <- match('pd'    , names(args))
        check.present <- match('.check', names(args))

        if (!is.na(id.present)){
            expect_true( formal_arg_is_missing(args$id)
                       , paste0( "argument `id` for function '", names.pd_is_[[i]], "' "
                               , " should not have a default."
                               )
                       )
            expect_equal(id.present, 1L)
        }
        if (!is.na(pd.present)){
            expect_true( formal_arg_is_missing(args$pd)
                       , paste0( "argument `pd` for function '", names.pd_is_[[i]], "' "
                               , " should not have a default."
                               )
                       )
            expect_equal(pd.present, if (id.present) 2L else 1L)
        }
        if (!is.na(check.present)) {
            expect_true( identical(args$.check, TRUE)
                       , paste0( "argument `.check` for function '", names.pd_is_[[i]], "' "
                               , " should default to `TRUE`."
                               )
                       )
            if (pd.present) expect_true(pd.present < check.present)
        } else if (!is.na(pd.present))
            fail(paste0('if argument `pd` is present argument `.check` is required to be present'
                       , " for exported function '", names.pd_is_[[i]], "'."))


    }


}

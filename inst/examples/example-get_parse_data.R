    text <- "    my_function <- function(object #< An object to do something with
            ){
        #' A title
        #'
        #' A Description
        print(\"It Works!\")
        #< A return value.
    }"
    source(textConnection(text), keep.source = TRUE)

    # Get parse data from a function
    (pd <- get_parse_data(my_function))
    # which must have a srcref attribute.
    # You can call the get_parse data directly on the srcref object.
    src <- utils::getSrcref(my_function)
    pd2 <- get_parse_data(src)

    identical(pd, pd2)

    # Objects must have a srcref.
    utils::getSrcref(rnorm)
    tools::assertError(get_parse_data(rnorm), TRUE)

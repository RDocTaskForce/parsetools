# load example file and get_parse data
ex.file <- system.file("examples", "example.R", package="parsetools")
exprs <- parse(ex.file, keep.source = TRUE)
pd <- get_parse_data(exprs)

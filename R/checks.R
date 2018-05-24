{#######################################################################
# checks.R
# This file is part of the R package `parsetools`.
#
# Author: Andrew Redd
# Copyright: 2017 University of Utah
#
# LICENSE
# ========
# The R package `parsetools` is free software:
# you can redistribute it and/or modify it under the terms of the
# GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see http://www.gnu.org/licenses/.
#
}#######################################################################

._check_id <-
function( id = get('id', parent.frame())
        , pd = get('pd', parent.frame())
        , present = TRUE #< check if the id is present in pd.
        ){
    #! Verify and/or extract id is valid.
    if (is.numeric(id) && !is.integer(id))
        id <- as.integer(id)
    if (!is.integer(id))
        stop('id must be an object that can be coerced to an integer')
    if (present) {
        is.present <- id %in% pd$id
        if (!all(is.present | id == 0))
            stop('id(',  paste(id[!is.present], collapse=', '), ') is not present in given parse-data.')
    }
    return(invisible(id))
}
if(FALSE){#!@testing
    pd <- get_parse_data(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))

    expect_error(._check_id(pd)  , "id must be an object that can be coerced to an integer", info="Passing parse-data")
    expect_error(._check_id(iris), "id must be an object that can be coerced to an integer", info="data.frame but not parse-data")
    expect_identical(._check_id(1)  , 1L  , info="convert numeric to integer")
    expect_identical(._check_id(1.1), 1L  , info="convert numeric to integer")
    expect_error(._check_id(TRUE)         , info="passing logical that cannot be converted.")
    expect_error( ._check_id(1000, pd)
                , 'id\\(1000\\) is not present in given parse-data.'
                , info="passing logical that cannot be converted."
                )
}

._check_parse_data <- function(pd){
    #! check_validity of parse_data
    if(inherits(pd, 'parse-data')) return(pd)
    else if(inherits(pd, 'data.frame')){
        as_parse_data(pd)
    } else stop("Cannot convert to parse-data.")
}
if(FALSE){#!@testing
    df <- getParseData(parse(text='rnorm(10, mean=0, sd=1)', keep.source=TRUE))
    pd <- ._check_parse_data(df)
    expect_is(pd, "parse-data")
    expect_error(._check_parse_data(NULL), "Cannot convert to parse-data.", info="passing NULL")
    expect_error(._check_parse_data(iris), "names of data do not conform", info="passing non-conforming data.frame")
}

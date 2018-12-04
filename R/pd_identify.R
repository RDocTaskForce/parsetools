# pd_identify.R #####################################################
#                                                                     #
# This file is part of the R package `parsetools`.                    #
#                                                                     #
# Author: Andrew Redd                                                 #
# Copyright: 2018 The R Consortium                                    #
#                                                                     #
# LICENSE                                                             #
# ========                                                            #
# The R package `parsetools` is free software:                        #
# you can redistribute it and/or modify it under the terms of the     #
# GNU General Public License as published by the Free Software        #
# Foundation, either version 3 of the License, or (at your option)    #
# any later version.                                                  #
#                                                                     #
# This software is distributed in the hope that it will be useful,    #
# but WITHOUT ANY WARRANTY; without even the implied warranty of      #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the        #
# GNU General Public License for more details.                        #
#                                                                     #
# You should have received a copy of the GNU General Public License   #
# along with this program. If not, see http://www.gnu.org/licenses/.  #
#_____________________________________________________________________#

# pd_identify ==========================================================
#' Get the ID for an object
#'
#' Identify in pd the id for the object given.
#'
#' @param pd the parse data.
#' @param object an object that originated in pd,
#'               for which to obtain the ID.
#'
#' @export
pd_identify <-
function( pd       #< parse data
        , object   #< [srcref] object to identify
        ) UseMethod('pd_identify', object)

#' @export
#' @describeIn pd_identify Default method identifies by [base::srcref()].
pd_identify.default <-
function( pd, object) pd_identify(pd=pd, utils::getSrcref(object))

#' @export
#' @describeIn pd_identify Passing a NULL object will result in an error.
pd_identify.NULL <-
function( pd, object) stop("Invalid object.")

#' @export
#' @describeIn pd_identify Identify by explicit `srcref`.
pd_identify.srcref <-
function( pd, object){
    stopifnot( inherits(object, 'srcref')
             , inherits(pd, 'parse-data')
             )
    pd[ pd$line1 == utils::getSrcLocation(object, 'line', TRUE )
      & pd$line2 == utils::getSrcLocation(object, 'line', FALSE)
      & pd$col1  == utils::getSrcLocation(object, 'col' , TRUE)
      & pd$col2  == utils::getSrcLocation(object, 'col' , FALSE)
      , 'id' ]
}
if(FALSE){#@testing
    text <-{"my_function <-
        function( object #< An object to do something with
                ){
            #' A title
            #'
            #' A Description
            print('It Works!')
            #< A return value.
        }
        another_f <- function(){}
        if(F){}
    "}
    source(file = textConnection(text), local=TRUE, keep.source = TRUE )
    parsed <- parse(text=text, keep.source=TRUE)
    pd <- get_parse_data(parsed)

    id <- pd_identify(pd, my_function)
    expect_equal(id, 40)

    expect_error(pd_identify(pd, NULL))
}

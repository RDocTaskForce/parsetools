# example.R ###########################################################
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
# Foundation, either version 2 of the License, or (at your option)    #
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


hello_world <- function(greeting="hello", who="world"){
    message(paste(greeting, who))
}


myClass <- setClass("myClass", contains='list')

setMethod("initialize", "myClass", function(.Object, ...){
    l <- list(...)
    if (!all(sapply(l, is, 'character')))
        stop("Sorry you are not a winner.  Please try again.")
    else
        message("Congratulations!")
    S3Part(.Object) <- list(...)
})

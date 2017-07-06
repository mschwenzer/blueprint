##' Help function to split a df into two disjunct dfs
##'
##' select_spl splits a data.frame into two, one containing some specified variables and one not containing some variables. Like dplyr::select it returns the df with matching columns but additionally writes the other non-matching columns to a object in the calling environment. This is a potentially destructive function for its sideeffect of creating other.df.
##' @title select_spl
##' @param df A data.frame to split
##' @param .dots A string of variables to select (which has to be conform to inpute epected by select_).
##' @param odf Name of a newly created other df with the variables not matching. Defaulting to `odf` short for other data.frame.
##' @return The data.frame with matchin columns.
##' @author Marc Schwenzer
##' @importFrom dplyr select_
##' @importFrom stringr str_replace
##' @export
select_spl <- function(df,.dots,odf='odf'){
    e<- parent.env(environment())
    df %>% select_(.dots=.dots %>% stringr::str_replace('^','-')) %>% assign('odf',.,envir=e)
df %>% select_(.dots=.dots)
}

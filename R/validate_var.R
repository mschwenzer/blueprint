##' Compute statistics over a variable in a data.frame.
##'
##' Works also with grouped_df
##' @title validate_var
##' @param data A data.frame
##' @param var The variable to analyse
##' @param min Lower limit, values below are counted
##' @param max Upper limit, values above are counted
##' @return a data.frame with summary.
##' @author Marc Schwenzer
##' @export
##' @importFrom dplyr summarise_
##' @importFrom dplyr %>%
validate_var <- function(data,var,min=NA,max=NA){
    list('n()',
         paste0('sum(is.na(',var,'),na.rm=1)'),
         ifelse(is.na(min),NA,paste0('sum((',var,'<',min,'),na.rm=1)')),
         ifelse(is.na(max),NA,paste0('sum((',var,'>',max,'),na.rm=1)')))  -> funs
    c('n','missing',paste0('<',min),paste0('>',max)) ->     names(funs)
    data %>% dplyr::summarise_(.dots=funs)
}

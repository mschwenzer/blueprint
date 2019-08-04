##' des
##'
##' Summarise certain variables by group. 
##' @title des
##' @param df Either a single variable or a data.frame for which descriptive sumamry statistics will be generated. If df contains a variable named `weight` this will be used to weight the data. If df is of class `grouped_df` the descripitve statistics will be generated for every group.
##' @param ... 
##' @param hist 
##' @param tab 
##' @return A data.frame of class `tbl_df`.
##' @author Marc Schwenzer
##' @export
des<- function(df,...,hist=FALSE,tab=FALSE){
    if(is.data.frame(df)){
    des.data.frame(df) -> out
    return(out)}
#    if(is.numeric(x)){fre.numeric(x)
#    return()}
    if(is.atomic(df)){
        des.atomic(df) -> out
        }
    return(out)}


##' hist.des
##'
##' @title print histogram in des object
##' @param x 
##' @return df
##' @author Marc Schwenzer
##' @importFrom dplyr %>%
##' @export
hist.des <- function(x,n=10,...)
{
                                        # print histogram part meaning plotting it

#    try({
#        if(nrow(x)==1)
#        {
#            print('here')
#x %>% select(hist) %>% nth(1) %>% nth(1) %>% print
#                     }
                                        #   },silent=TRUE) -> out
     x[,'hist']  -> hists
                                        # adjust the title

    seq(along=hists) %>% map(~{
        x[.x,'hist']  %>% nth(1)-> the.plot
        paste0(the.plot$labels$title,' [',x[.x,'cnt'],']') -> the.plot$labels$title
        print(the.plot)
        invisible()
        })
}

##' print.des
##'
##' @title print.des
##' @param x 
##' @return df
##' @author Marc Schwenzer
##' @export
##' @importFrom dplyr select_
print.des<- function(x,...)
{
                                        # print histogram part meaning plotting it

#    try({
#        if(nrow(x)==1)
#        {
#            print('here')
#x %>% select(hist) %>% nth(1) %>% nth(1) %>% print
#                     }
#   },silent=TRUE) -> out
    try({x%>% select_(quote(-hist),quote(-tab))  %>% tbl_df-> x})
    c('tbl_df','tbl','data.frame') -> class(x)
    print(x)
cat('\nUse his on object to get histogram and tab to get the table stats.\n')
}

##' des.data.frame
##'
##' des.data.frame
##' @title des.data.frame
##' @param x 
##' @param hist 
##' @param tab 
##' @return data.frame with statistics in column.
##' @author Marc Schwenzer
##' @importFrom dplyr select
##' @importFrom purrr map_df
##' @importFrom dplyr do
##' @importFrom tidyr unnest
##' @importFrom dplyr ungroup
##' @importFrom dplyr one_of
##' @importFrom dplyr group_vars
##' @importFrom dplyr %>%
des.data.frame <- function(x,hist='',tab)
{
                                        # numeric variables
                                        #    x %>% group_vars %>% paste0(.,'collapse')-> groupingvars
                                        #        x %>%map_lgl(~is.numeric(.)) %>% which %>%
    names(x)[!(names(x)%in%c(group_vars(x),'weight'))] -> notgroupvars
                                        #    if(!(group_vars %>% has_length)){x %>% rowwise  -> x}
    notgroupvars    %>%  map_df(~{
        .x -> currentvar
        x        %>% do(
                                   var=currentvar,
                                  #     if(grouped){adf %>% select_(groupvars)  -> adf}
#                                       adf %>% names
#                                   },
                                   descriptives={
                                       (.) %>% ungroup %>%  select(one_of(c(currentvar,'weight')))   %>% numeric.descriptives
                                   })
    })   -> out
    out%>% ungroup %>% unnest(var) %>% unnest(descriptives)
    }


##' des.atomic
##'
##' Wrapper to deal with atomic values by encapsulating values into a data.frame.
##' @title des.atomic
##' @param x 
##' @return a data.frame
##' @author Marc Schwenzer
##' @importFrom dplyr %>%
des.atomic <- function(x)
{
paste0('data.frame(',substitute(x),'=x)') %>% parse(text=.) %>% eval %>% des.data.frame
    }


##' numeric.descriptives
##'
##' compute numeric descriptives. Acutally workhorse behind des.
##' @title numeric.descriptives
##' @param df 
##' @return a data.frame row of numeric descriptives
##' @author Marc Schwenzer
##' @importFrom e1071 kurtosis
##' @importFrom e1071 skewness
##' @importFrom laeken gini
##' @importFrom laeken weightedQuantile
##' @importFrom dplyr %>%
##' @importFrom dplyr summarise_if
##' @importFrom dplyr funs
##' @importFrom Hmisc wtd.mean
##' @importFrom TAM weighted_skewness
##' @importFrom TAM weighted_curtosis
##' @importFrom dplyr %>%
numeric.descriptives<- function(df)
{
#    print(class(df))
#    print(head(df))
                                        #    print(group_vars(df))
    if('weight'%in%ls(df))
        {
df %>%     summarise_if(names(.)!='weight',
                        funs(
                            'mean'=Hmisc::wtd.mean(.,na.rm=1,weights=weight) %>% round(2),
                            'median'=laeken::weightedMedian(.,na.rm=TRUE,weights=weight) %>% round(2),                            
                            'sd'=Hmisc::wtd.mean(.,weights=weight,na.rm=TRUE)  %>% sqrt %>% round(2),                              
                            'min'=min(.,na.rm=T) %>% round(2),
                            'max'=max(.,na.rm=T) %>% round(2),
                            'range'=max-min,
                            'gini'=laeken::gini(.,na.rm=TRUE,weights=weight) %>% nth(1) %>% round(2),
                            'skew'=TAM::weighted_skewness(.,w=weight) %>% round(2),
                            'curt'=TAM::weighted_curtosis(.,w=weight) %>% round(2),                            
                            'S80'=laeken::weightedQuantile(.,probs=.2,na.rm=TRUE,weights=weight) %>% round(2),
                            'S20'=laeken::weightedQuantile(.,probs=.8,na.rm=TRUE,weights=weight) %>% round(2),
                            'S80/S20'=(S80/S20) %>% round(2),
                            'N'=length,
                            'valid' = sum(!is.na(.),na.rm=TRUE),
                            'NA_' = sum(is.na(.),na.rm=TRUE),
                            'NA%' = round(NA_/N,2),
                           'Sumwts'=sum(weight,na.rm=TRUE)
))                        
}
else
{
    df %>% summarise_all(funs('mean'=mean(.,na.rm=T) %>% round(2),
                              'median'=median(.,na.rm=T) %>% round(2),
                              'sd'=sd(.,na.rm=TRUE)  %>% round(2),                              
                                'min'=min(.,na.rm=T) %>% round(2),
                              'max'=max(.,na.rm=T) %>% round(2),
                              'range'=max-min,
                              'gini'=laeken::gini(.,na.rm=TRUE) %>% nth(1) %>% round(2),
                            'skew'=e1071::skewness(.,na.rm=TRUE) %>% round(2),                              
                                'curt'=e1071::kurtosis(.,na.rm=TRUE) %>% round(2),
                                'S80'=quantile(.,.8,na.rm=TRUE) %>% round(2),
                              'S20'=quantile(.,.2,na.rm=TRUE) %>% round(2),
                                'S80/S20'=(quantile(.,.8,na.rm=TRUE)/quantile(.,.2,na.rm=TRUE)) %>% round(2),                                                              
                                'N'=length,
                                'valid' = sum(!is.na(.)),
                            'NA_' = sum(is.na(.)),
                                                        'NA%' = round(NA_/N,2)
                                )
                                )
                }

}


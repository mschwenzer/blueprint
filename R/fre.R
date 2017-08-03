## act_depending_on_class<- function(...)
##                                         # clone current environment and acti
## {
##     dots_list(...) -> funlist
##     map2(funlist %>% names2,funlist,~{
##         browser()
##         paste0(.x,'(x)') %>% parse(text=.) %>% eval -> predicate
##         if(predicate){
            
##             cat('matched');eval_tidy(.y)}
## })
## }


##' fre
##'
##' Compute a simple frequency summary and print it.
##' @title fre
##' @param x A vector or data.frame. If x is a data frame, it is iterated over its columns.
##' @param maxcat Maximum number of categories to be printed. Default: 50
##' @return data.frame (invisible)
##' @author Marc Schwenzer
##' @export
##' @importFrom dplyr bind_cols
##' @importFrom stargazer stargazer
##' @importFrom purrr map_dbl
fre<- function(x,groupingvars=NULL,maxcat=25,print=TRUE)
{
    
    if(is.grouped_df(x)){
        fre.grouped_df(x,maxcat=maxcat)
    return(invisible(NULL))}
    if(is.data.frame(x)){fre.data.frame(x,maxcat=maxcat,groupingvars=groupingvars,print=print) -> out
    return(invisible(NULL))}
#    if(is.numeric(x)){fre.numeric(x)
#    return()}
    if(is.atomic(x)){fre.atomic(x,maxcat=maxcat,groupingvars=groupingvars,print=print) -> out
    return(invisible(NULL))}
    }

fre.grouped_df<- function(x,maxcat='',print=TRUE)
{
    x %>% group_vars  -> groupingvar
            x %>% do(a={(.) -> df
                df %>% select_(.dots=groupingvar) %>% .[1,]  -> groupingval
                    seq_along(groupingvar) %>% map_chr(~{paste0(groupingvar[.x],':',groupingval[.])}) %>% paste0(collapse='|') -> groupingvars
df %>% select(groupingvar) %>% slice(1)
#            cat(paste0( (groupingvars %>% paste0(collapse=',')),': ',paste0((df %>% select_(groupingvars))[1,],collapse=','),'\n'))
            df %>% ungroup %>% select_(.dots=(groupingvar %>% paste0('-',.))) %>% fre(groupingvars=groupingvars,maxcat=maxcat,print=print)})
        return()
    }

fre.data.frame<- function(x,groupingvars=NULL,maxcat='',print=TRUE)
{
        x %>% as.data.frame  -> x
        1:ncol(x) %>% map(~{
            paste0('\n------------------------------------------------------------\n\n     ',names(x)[.x],'  ',if(!is.null(groupingvars)){paste0(' [',groupingvars,'] ')},
                   if(!is.null(attr(x[,.x],'label'))){paste0('[',attr(x[,.x],'label'),']')}
                  ,'\n')  -> atext
            if(print){cat(atext)}
try({ x[,.x] %>% fre(.,maxcat=maxcat,groupingvars=groupingvars,print=print)})
        })
            return()
    }


fre.atomic<- function(x,groupingvars,maxcat,print=TRUE)
{
        is.factor(x)-> isfac
        try({    if((unique(x) %>%length >maxcat)&(print)){cat('more than ',maxcat,' labels, Think about setting argument maxcat higher.\n');return()}})
        table( x, useNA='always')  -> tab
        tab[is.na(names(tab))] -> miss
        names(tab)[is.na(names(tab))]<- '-NA-'
        as.numeric (tab) -> tabnum
        names (tab) -> tabname
        # if x factor... add add level entires seperated by ' '
        if( is.factor(x) ) {paste0(levels(x) %>% seq_along,' ',tabname[-length(tabname)]) -> tabname[-length(tabname)]}
        else
    {
        attr(x,'labels') -> labattr
        if(!is.null(labattr)){
            if(!is.na(labattr)){
                try(
                {
                    labattr %>% {. ->.x; .x%>% names(.) ->> labattr;.x ->> names(labattr)}
                    tabname[-c(length(tabname))] %>% paste0(.,' [',labattr[.],']') -> tabname[-c(length(tabname))]
                })
            }
        }}
        sum(tab,na.rm=1) -> tabsum
    bind_cols(`Value`=tabname ,`Freq.`=tabnum,`Percent`={(tabnum/tabsum) *100},`Valid`={(tabnum/(tabsum-miss))*100 })    -> out
    out$Valid[nrow(out)]<- NA
    out$Valid %>% sum
    bind_cols(out,`Cum.`=seq_along(out$Valid) %>% map_dbl(~{(out$Valid[1:.x] %>% sum)})) -> out
    out[,1] -> nam
    out[,-1]  ->  vals
    vals %>% apply(2,sum,na.rm=1)  -> vals[(nrow(out)+1),]
    c(nam[[1]],'Total')  -> nam
    vals  %>% round(2) %>% bind_cols(data.frame(Value=nam),.) -> out
        NA -> out[nrow(out),5]
        if(print)
            {
                out %>%  prmatrix(quote=0,na.print=' ',rowlab=rep(' ',nrow(out)))
                }
    return(invisible(out))
}

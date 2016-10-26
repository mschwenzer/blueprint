attrs.as.factor <- function (x)
{
    require(forcats)
     the.label <- x %>% attr("label")
    from <- x %>% attr("labels")
#    print(from)
    if(from %>% is.numeric)
    {
        paste0('`',from,'`') -> from
    }
    else
    {
        paste0('"',from,'"') -> from
    }
#    print(from)
    x %>% attr("labels") %>% names -> to
    paste0('"',to,'"') -> to
    llply(1:length(from),function(id){paste0(from[id],'=',to[id])}) %>% paste0(collapse=',') -> recod
#    print(recod)
                                        #     rownames(cats) <- NULL
    paste0('x %>% dplyr::recode(',recod,') %>% as.factor-> x') -> code.to.eval
#    print(code.to.eval)
    eval(parse(text=code.to.eval))
    return(x)
#     attr(x, "label") <- the.label
 }




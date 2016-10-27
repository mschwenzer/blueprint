##' Run over attributes and try to create a vector.
##'
##' @title attrs.as.factor
##' @param x a vector object or data.fram.
##' @return Either a factor (if x is a vector) containing the attributes as levels or a data.frame with every column replaced by according factors.
##' @author Marc Schwenzer <m.schwenzer@uni-tuebingen.de>
##' @export
##' @importFrom dplyr recode
##' @importFrom plyr llply
##' @importFrom dplyr %>%
attrs.as.factor <- function (x)
{
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
    plyr::llply(1:length(from),function(id){paste0(from[id],'=',to[id])}) %>% paste0(collapse=',') -> recod
                                        #    print(recod)
                                        #     rownames(cats) <- NULL
    paste0('x %>% recode(',recod,') %>% as.factor-> x') -> code.to.eval
                                        #    print(code.to.eval)
                                        eval(parse(text=code.to.eval))
    return(x)
                                        #     attr(x, "label") <- the.label
}

##' \code{info_blue}
##'
##' Inform about a blueprint
##' @title info_blue
##' @param blueprint The Blueprint file.
##' @param waves The wave to inform about. By now only numeric vector of length 1 accepted. 
##' @param search An optional pattern to search for in the labels.
##' @return Information
##' @author Marc Schwenzer <m.schwenzer@uni-tuebingen.de>
##' @export
##' @importFrom stringr str_detect
##' @importFrom dplyr %>%
info_blue  <- function(blueprint=options('blueprint_file'),waves,searchstr=NULL){
    import(blueprint)  -> blueprint
    names(blueprint)  %>% str_detect('file') %>% which %>% .[waves] -> thecol
    blueprint[,thecol] %>% na.omit %>% .[1]  -> file
    cat(file,':\nOriginal Varnames:\n\n')
    file %>% import -> file
    file %>% names  -> varnams
    file %>% llply(function(x){attributes(x) %>% .$label})  %>% unlist  -> varlabs
    data.frame('Varname'=varnams,'Label'=varlabs)  -> adf
    NULL -> rownames(adf)
    if(is.character(searchstr))
    {
        adf[,'Label'] %>% str_detect(searchstr %>% ignore.case) %>% which  -> matches
        adf[matches,] %>% as.matrix %>% stargazer(type='text')
    }
    else
    {
        adf %>% as.matrix %>% stargazer(type='text')
        }
    invisible(file)
}

    

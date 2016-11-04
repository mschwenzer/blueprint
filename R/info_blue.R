##' \code{info_blue}
##'
##' Inform about a blueprint
##' @title info_blue
##' @param blueprint The Blueprint file.
##' @param chunks Numeric vector specifying the chunk(s) to inform about like shown in a blueprint file. If length(chunks)>1 the search will be vectorized over all chunks. Only if length(chunks)=1 the data is actually returned.
##' @param search An optional pattern to search for in the attribute labels of data.frame. Can be used to search through variable description if this attributes exist (they are by default imported when e.g. using Stata or SPSS file.
##' @return Returns the according file invisible. If you assign it or pipe it, it can be reused.
##' @author Marc Schwenzer <m.schwenzer@uni-tuebingen.de>
##' @export
##' @importFrom stringr str_detect
##' @importFrom dplyr %>%
info_blue  <- function(blueprint=options()$'blueprint_file',
                       chunks,
                       searchstr=NULL,
                       which=NULL){
    if(length(chunks)>1)
    {
        chunks %>% llply(function(wav){
            cat(paste0('--------------------------------------------------------------------------------\nchunk: ',wav,'\n--------------------------------------------------------------------------------\n\n'))

            info_blue(chunks=wav,searchstr=searchstr,which=which)
        NULL})
        return()
    }
    if(is.null(which)){1 -> which}
    import(blueprint,which=which)  -> blueprint
    names(blueprint)  %>% str_detect('file') %>% base::which(.) %>% .[chunks] -> thecol
    blueprint[,thecol] %>% na.omit %>% .[1]  -> file
    cat(file,':\nOriginal Varnames:\n\n')
    file %>% import -> file
    file %>% names  -> varnams
    file %>% llply(function(x){attributes(x) %>% .$label})  %>% unlist  -> varlabs
    data.frame('Varname'=varnams,'Label'=varlabs)  -> adf
    NULL -> rownames(adf)
    if(is.character(searchstr))
    {
        adf[,'Label'] %>% str_detect(searchstr %>% regex(ignore_case=1)) %>% base::which(.)  -> matches
        adf[matches,] %>% as.matrix %>% stargazer(type='text')
    }
    else
    {
        adf %>% as.matrix %>% stargazer(type='text')
        }
    invisible(file)
}

    

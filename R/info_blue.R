##' \code{info_blue}
##'
##' Inform about a blueprint
##' @title info_blue
##' @param blueprint The Blueprint file.
##' @param chunks Numeric vector specifying the chunk(s) to inform about like shown in a blueprint file. If length(chunks)>1 the search will be vectorized over all chunks. Only if length(chunks)=1 the data is actually returned.
##' @param searchstr The string to search for
##' @param force If TRUE cache file is rebuild. Defaults to FALSE
##' @param ... 
##' @param search An optional pattern to search for in the attribute labels of data.frame. Can be used to search through variable description if this attributes exist (they are by default imported when e.g. using Stata or SPSS file.
##' @return Returns the according file invisible. If you assign it or pipe it, it can be reused.
##' @author Marc Schwenzer <m.schwenzer@uni-tuebingen.de>
##' @export
##' @importFrom stringr str_detect
##' @importFrom stringr regex
##' @importFrom R.cache evalWithMemoization
##' @importFrom R.cache setCacheRootPath
##' @importFrom dplyr %>%
info_blue  <- function(blueprint=options()$'blueprint_file',
                       chunks=NULL,
                       searchstr=NULL,
                       force=FALSE,
                       ...){
    if(is.null(which)){1 -> which}
setCacheRootPath(paste0(getwd(),'/blueprint_cache_dir'))
evalWithMemoization(
        {
    import(file=blueprint,...)  -> blueprint
    names(blueprint)  %>% str_detect('file') %>% base::which(.)  -> thecol
    if(!is.null(chunks)){
        thecol %>% .[chunks] -> thecol
    }
    else
            {
                chunks=(1:length(thecol))
                }
    cat('\nSearching blueprint files...\n\n')
    
    data.frame(chunk=chunks,col=thecol) %>% group_by(chunk) %>% do(file={blueprint[,.$col] %>% na.omit %>% unique}) %>%  unnest(file) -> fileframe
    fileframe %>% group_by(chunk) %>% do(
                                          file=.$file,
                                          searchresults={
    .$file %>% import -> data
    data %>% names  -> varnams
    data %>% llply(function(x){attributes(x) %>% .$label})  %>% unlist  -> varlabs
                                              data.frame('Varname'=varnams,'Label'=varlabs)  -> adf
    adf})   -> adf
        },
    force=force
    )
                                        #        %>% ungroup %>% unnest(file) %>% unnest(searchresults) -> a
    adf %>% unnest(file)  %>% group_by(chunk) %>% do(searchresults={
                   .$searchresults[[1]] ->  adf
        if(is.character(searchstr))
            {
adf %>% filter(str_detect(Label,searchstr %>% regex(ignore_case=1))) -> adf
            }
adf
    },
file=.$file)%>% unnest(file) %>% unnest(searchresults) %>% select(chunk,Label,Varname,file)  -> a
    if(nrow(a)>0){
        cat(paste0('\n--------------------------------------------------------------------------------\nMatches of `',searchstr,'` in blueprint data files:\n\n'))        
        a %>% as.matrix %>% stargazer(type='text')
        
    }
    else{
        cat('\nNo matches of `',searchstr,'` in blueprint data files.\n\n')
        }
    invisible(a)
}

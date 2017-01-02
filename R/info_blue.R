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
            rio::import(file=blueprint,...)  %>% extract.blueprint.meta.statements(1) -> blueprints
            blueprints$blueprint %>%  validate.blueprint.file.and.return.list.of.valid.blueprints(blueprint=.,chunks=chunks) -> blueprint

            if(!is.null(chunks)){
                blueprint %>% filter(chunk%in%chunks) -> blueprint
            }
            cat('\nSearching blueprint files...\n\n')
            blueprint %>% unnest(blueprints)  %>%  select(chunk,file) %>% filter(!is.na(file),!(duplicated(file)))  -> fileframe
#            print(fileframe)
            fileframe %>% rowwise %>% do(                chunk=.$chunk,
                                         file=.$file,
                                         searchresults={
                                             .$chunk  -> chunk
                                             .$file  -> file                                             
                                             file %>% import -> data

                                             data %>% names  -> varnams
                                             data %>% llply(function(x){attributes(x) %>% .$label})  %>% unlist  -> varlabs
                                             if(is.null(varlabs))
                                             {
                                                 data.frame(chunk=chunk,'Varname'=varnams,'Label'=rep(NA,length(varnams)),file=file) -> adf}
                                             else 
                                             {
                                                 data.frame(chunk=chunk,'Varname'=varnams,'Label'=varlabs,file=file)  -> adf
                                             }
                                             adf}
                                         )  %>% unnest(searchresults) -> adf
        },
    force=force
)
                                        #        %>% ungroup %>% unnest(file) %>% unnest(searchresults) -> a

        if(is.character(searchstr))
        {
            ((str_detect(adf$Label,searchstr %>% regex(ignore_case=1)))|(str_detect(adf$Varname,searchstr %>% regex(ignore_case=1)))) -> matches
            matches[matches %>% is.na] <- FALSE
#                        print(matches)
adf %>% filter(matches) -> a
            }
        else{
                adf  -> a
                }

    if(nrow(a)>0){
        cat(paste0('\n--------------------------------------------------------------------------------\nMatches of `',searchstr,'` in blueprint data files:\n\n'))
        a  %>% as.matrix -> a
        (a %>% stargazer(type='text'))
    }
    else{
        cat('\nNo matches of `',searchstr,'` in blueprint data files.\n\n')
        }
    invisible(a)
}

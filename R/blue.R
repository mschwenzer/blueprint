##' @importFrom dplyr %>%
blueprint.check.every.specified.original.var.has.a.file  <- function(df)
{
    df$var %>% is.na %>% `!` %>% which  -> missing.in.var
    df$file[missing.in.var] %>% is.na %>% which -> also.missing.in.file
    if(length(also.missing.in.file)>0)
    {
        stop(paste0('Missing files for specified variables:\n'),paste0('var: ',df$var[(missing.in.var[also.missing.in.file])],' -> newvar: ',df$newvar[(missing.in.var[also.missing.in.file])],collapse='\n'))}
            return(df)
    }

##' @importFrom dplyr %>%
df.remove.non.standard.named.columns  <- function(df)
{
    ## remove empty colums
    standard.names <- c('newvar','link','var','file','fun')
    actual.names <- names(df)
    (names(df)%in%standard.names)  %>%  `!` %>% which  -> to.drop
    if(length(to.drop)>0){
        df[,-c(to.drop)] -> df
        # reset the names because default they get numbers after dropping
        actual.names[-to.drop] -> names(df)
    }
return(df)
    }


stop.if.no.var.column <- function(df)
{
    if(!('var'%in%names(df))) {
        stop('Error while importing the blueprint file: The file does not have specifications of original variable names that have to be indicated by a header row (row with variable names) that contains the string `var`. Please check the blueprint file. Maybe there is a non-comment row before the header or header is ignored during import?')
    }
    return(df)
    }





##' @importFrom dplyr transmute
##' @importFrom dplyr filter
##' @importFrom dplyr group_by
##' @importFrom dplyr do
##' @importFrom dplyr %>%
validate.blueprint.file.and.return.list.of.valid.blueprints <- function(blueprint,chunks)
    {
        blueprint %>%
            blueprint.remove.column.rows    %>%
            ## guarantee that all columns have the correct name, especially var used for cutting
            df.set.standard.names   %>% 
    df.remove.non.standard.named.columns %>% 
    stop.if.no.var.column -> blueprint
                                        # cut blueprint into chunks
        data.frame(startcol=(names(blueprint)=='var') %>% which,
                                        # end column 
                   endcol=c((names(blueprint) =='var')  %>% which %>% .[-1] %>% `-`(1),
                                        # + the last column containing everything
                            length(names(blueprint)))) %>% transmute(chunk=1:nrow(.),startcol,endcol)  -> blueprints.column.info
                                        # Subset over the chunks
        if(is.numeric(chunks)){
            blueprints.column.info %>% filter(.$chunk%in%c(chunks)) ->         blueprints.column.info
        }
        cat('- Validating blueprint for chunk')
        blueprints.column.info %>% group_by(chunk)   %>%
                                        # -> reduced to a single-line data.frame containing the selected chunks and the rows
            do(blueprints={
                cat(paste0('...',.$chunk))
            chunk.columns <- c(1,(.$startcol):(.$endcol))
            blueprint[,chunk.columns]  %>% 
                ## normalise to a data.frame with these variables / remove columns not named correct
                return.df.with.certain.vars(newvar,var,file,link,fun) %>%
                ## !!! to validate
                add.variables.specified.by.brackets %>%
                set.empty.values.to.NA  %>%
                ## Validate all blueprints before something is done actually.... 
                blueprint.chunk.validator})
    }


##' @importFrom plyr llply
##' @importFrom stringr str_detect
##' @importFrom dplyr arrange
##' @importFrom Hmisc describe
##' @importFrom stargazer stargazer
##' @importFrom logging loginfo
##' @importFrom dplyr %>%
return.diff.code <- function()
{
    "
blueprint.log <- function(message){

    loginfo(message, logger='blueprint.logger')
}

blueprint.log.formatter <- function(record) {
text <- paste(paste0(record$msg, sep=' '))
}

blueprint.variable.diff <- function(variable,funs,name='',chunk='')
{
    blueprint.log('')
    blueprint.log(Sys.time())
    blueprint.log('')
blueprint.log        (paste0('----Transformation of variable `',name,'`  (chunk ',chunk,'): ',funs,'  -----------------------------\n'))
    variable %>% duplicated %>% `!`  %>% which  -> old.pos
    variable[old.pos] -> kept.levels.of.variable
    class(variable) -> old.type
    extended <- length(kept.levels.of.variable)<40
if(extended)
    {
            plyr::llply(kept.levels.of.variable,function(x){stringr::str_detect(variable,x %>% as.character) %>% sum})  %>% unlist    -> old.count
            }
    eval(parse(text=paste0('variable %>% ',funs)))  -> variable
    class(variable) -> new.type
    if(extended){
    data.frame(old=kept.levels.of.variable,`. `=rep('|',length(kept.levels.of.variable)),`.  `=rep('v',length(kept.levels.of.variable)),new=variable[old.pos],`(n)`=old.count) %>% dplyr::arrange(old)  ->     printfr

capture.output(    printfr %>% as.matrix %>% t %>% stargazer::stargazer(type='text')  %>% paste0(.,'\n') %>% blueprint.log,file=NULL)  -> bla}
                            if(new.type!=old.type){
                                blueprint.log(paste0('!!! Type conversion from ',old.type,' to ',new.type,'. Was this intended?'))
                            }
    blueprint.log('')
    blueprint.log('')    
    blueprint.log('   >>> Distribution after recoding -----\n')                            
    capture.output(x=print(Hmisc::describe(variable)),file=NULL) %>% blueprint.log
    return(variable)
}"
}



##' @importFrom dplyr %>%
make.bind.code <- function(dfs,data.table=TRUE){
    if(data.table)
    {
        paste0('library(data.table)\nlist(',paste0(dfs,collapse=','),')  %>% 
         lapply(setattr, name = "class",
                         value = c("data.table",  "data.frame")) %>% 
         rbindlist(use.names=TRUE, fill=TRUE) -> final.df')
    }
}


blueprint.log.formatter <- function(record) {
    text <- paste(paste0(record$msg, sep=' '))
}


##' @importFrom dplyr %>%
normalised.path.and.dir.exists <- function(filepath)
{
    suppressWarnings(
        filepath %>% normalizePath  -> filepath
    )
    if(dir.exists(filepath)){stop(paste0('Filepath `',the.dir,'` is an exisiting directory. Specify a valid path where the blueprint template will be created. '))}
    filepath  %>% dirname  -> the.dir
                                        # Will stop and print error if directory does not exist            
    the.dir %>% dir.exists(.) %>% if(`!`(.)){stop(paste0('Directory `',the.dir,'` does not exist. Specify a valid path where the blueprint template will be created. '))}
    return(filepath)
}









##' @importFrom rio import
##' @importFrom dplyr transmute
##' @importFrom dplyr %>%
load.and.recode <- function(blueprint,fun=FALSE,chunk=1,extended=FALSE)
{
                                        # find non-empty fun (rec) columns  / !!! replace by stringr::str_match / find out why it can be string "NA" which is a bug
    if(fun)
    {
        ((blueprint$fun!='NA')&(!is.na(blueprint$fun))&(!is.nan(blueprint$fun)))  %>% which -> rep.pos
        if(extended){
                                        # if logging, execute the transformation in blueprint.variable.diff 
            paste0(blueprint$var[rep.pos],' %>% blueprint.variable.diff(fun="',blueprint$fun[rep.pos],'",name="',blueprint$var[rep.pos],'",chunk="',chunk,'")') -> blueprint$var[rep.pos]
        }
        else{
                                        # else direct
            paste0(blueprint$var[rep.pos],' %>% ',blueprint$fun[rep.pos]) -> blueprint$var[rep.pos]
        }
    }
    paste0(blueprint$newvar,'=',blueprint$var,collapse=',\n                         ') -> transmute.code
                                        # create a string that renames/selects with select and mutates afterwards
    paste0('rio::import("',blueprint$file[1],'")',paste0(' %>%\n        dplyr::transmute(',transmute.code,')')) -> code.to.execute
    return(code.to.execute)
}




##' @importFrom stringr str_replace_all
##' @importFrom stringr str_split
##' @importFrom stringr str_detect
##' @importFrom dplyr %>%
process.links <- function(links)        {
    links %>% str_replace_all('"','') %>% str_replace_all("'","") %>% 
        lapply(function(link)
        {
            if(is.na(link)){return(NA)}
            link %>% str_split(',') %>%  .[[1]]  -> link
            link %>% str_detect('=') %>% `!` %>% which  -> pos.no.equal.sign
                                        #                print(pos.no.equal.sign)
                                        # add equal sign for condition
                                        #                print(link[pos.no.equal.sign])
            link[pos.no.equal.sign]  %>% paste0(.,'=',.)  -> link[pos.no.equal.sign]
                                        #                print(link)
            link %>% str_replace_all('=','"="') %>% paste0('"',.,'"')  %>% paste0(collapse=',')-> link
            return(link)
        }) %>% unlist}


##' @importFrom plyr llply
##' @importFrom stringr str_detect
##' @importFrom stargazer stargazer
##' @importFrom Hmisc describe.vector
##' @importFrom utils capture.output
##' @importFrom dplyr %>%
blueprint.variable.diff <- function(variable,funs,name='',chunk='')
{
    blueprint.log('')
    blueprint.log(Sys.time())
    blueprint.log('')
    blueprint.log        (paste0('----Transformation of variable `',name,'`  (chunk ',chunk,'): ',funs,'  -----------------------------\n'))
    variable %>% duplicated %>% `!`  %>% which  -> old.pos
    variable[old.pos] -> kept.levels.of.variable
    class(variable) -> old.type
    extended <- length(kept.levels.of.variable)<40
    if(extended)
    {
        llply(kept.levels.of.variable,function(x){str_detect(variable,x %>% as.character) %>% sum})  %>% unlist    -> old.count
    }
    eval(parse(text=paste0('variable %>% ',funs)))  -> variable
    class(variable) -> new.type
    if(extended){
        data.frame(old=kept.levels.of.variable,`. `=rep('|',length(kept.levels.of.variable)),`.  `=rep('v',length(kept.levels.of.variable)),new=variable[old.pos],`(n)`=old.count) %>% arrange(old)  ->     printfr
        
        capture.output(    printfr %>% as.matrix %>% t %>% stargazer(type='text')  %>% paste0(.,'\n') %>% blueprint.log,file=NULL)  -> bla}
    if(new.type!=old.type){
        blueprint.log(paste0('!!! Type conversion from ',old.type,' to ',new.type,'. Was this intended?'))
    }
    blueprint.log('')
    blueprint.log('')    
    blueprint.log('   >>> Distribution after recoding -----\n')                            
    capture.output(x=print(describe.vector(variable)),file=NULL) %>% blueprint.log
    return(variable)
}

## make empty NA -----------------------------------------------------------
##' @importFrom dplyr mutate_all
##' @importFrom dplyr %>%
set.empty.values.to.NA <- function(blueprint)
{
    blueprint %>% mutate_all(.funs=funs(ifelse(.=='',NA,.)))
}

# Extract meta statements from blueprint
# Return a list of a blueprint and meta.statements indicated by ^@
extract.blueprint.meta.statements <- function(blueprint,varname.or.position)
{
    blueprint[,varname.or.position] %>% str_detect('^!')  %>% which  -> rows.with.meta.statements
    blueprint[rows.with.meta.statements,varname.or.position] -> meta.statements
    if(length(rows.with.meta.statements)>0){
    # Remove @ specifyer
    meta.statements %>% str_replace('^!','') -> meta.statements
    
                                        # add left join if blue statement is found evaluated
    names(blueprint)  %>% str_detect('link')  %>% which %>% .[1] -> linkcol
        blueprint[rows.with.meta.statements,linkcol]  -> links
        (links %>% str_detect(.,'=')|links %>% str_detect(.,','))  -> link.condition.exists.vector
    # fix NAs when link column is empty
FALSE -> link.condition.exists.vector[link.condition.exists.vector %>% is.na]
                                        # TODO: validate link condition
    sapply(1:length(link.condition.exists.vector),function(x)
        {
            ifelse(link.condition.exists.vector[x],
                   
                   paste0(meta.statements[x],'  ->  blueprint.to.add\nfinal.df  %>% left_join(blueprint.to.add,by=c(',blueprint[rows.with.meta.statements[x],linkcol] %>% process.links,')) -> final.df\nrm(blueprint.to.add)\n\n'),
                   paste0('final.df  %>% ',meta.statements[x],' -> final.df\n\n')
                   )
        }) -> meta.statements
    blueprint[-c(rows.with.meta.statements),] -> blueprint
    }
    else
    {
        meta.statements=''
    }
return(list(blueprint=blueprint,meta.statements=meta.statements))
    }
    


## add.variables.specified.by.brackets -----------------------------------------------------------
##' @importFrom stringr str_detect
##' @importFrom stringr str_replace_all
##' @importFrom plyr ldply
##' @importFrom dplyr %>%
add.variables.specified.by.brackets <- function(blueprint)
{
    ## Add [0:x]-specified interval to variablenames: create a row for every individual variable  -----------------------------------------------------------
    rowstoprocess <- blueprint[,1]
    blueprint %>% names -> column.names
    while(length(
        # Find ocurences of interval syntax [1:9]
    ((blueprint[,1]) %>% str_detect('\\[[0-9]*:[0-9]*\\]') %>% which)
    >0))
    {
        rowid <- ((blueprint[,'newvar']) %>% str_detect('\\[[0-9]*:[0-9]*\\]') %>% which %>% .[1])
        if((rowid-1)>0){
            frame.before <- blueprint[1:(rowid-1),]
        }
        if((rowid+1)<(nrow(blueprint)+1))
        {
            frame.after <- blueprint[(rowid+1):nrow(blueprint),]
        }
        to.process.vars <- blueprint[rowid,]
        ## !!! to change
        pattern <- regmatches(to.process.vars[,1],regexpr('\\[[0-9]*:[0-9]*\\]',to.process.vars[,1]))
        # parse interval -> numberic()
        nums <- eval(parse(
            text=(pattern %>% str_replace_all('\\[','') %>% str_replace_all('\\]',''))
        )
        )
        # escape pattern
        pattern %>% str_replace_all('\\[','\\\\[') %>% str_replace_all('\\]','\\\\]')  -> pattern
        
        frame.toadd <- ldply(nums,
                             function(x){

                                       to.process.vars %>% str_replace_all(pattern,x %>% as.character)
                                   })
        names(frame.toadd) <- column.names
        assign('blueprint',rbind(if((rowid-1)>0){frame.before},frame.toadd,              if((rowid+1)<(nrow(blueprint)+1)){frame.after}),-1)
    }
    return(blueprint)
}
###





## return.not.existing.files -----------------------------------------------------------
##' @importFrom stats na.omit
##' @importFrom plyr llply
##' @importFrom dplyr %>%
return.not.existing.files <- function(blueprint)
{
    blueprint[,'file'] %>% unlist %>% c %>% unique %>% na.omit  -> files.to.check
    plyr::llply(files.to.check,function(x){
        (!file.exists(x))|                    dir.exists(x)
    }) %>% unlist          %>% files.to.check[.]
}

###
##' @importFrom plyr llply
##' @importFrom stringr str_detect
##' @importFrom dplyr %>% 
blueprint.remove.column.rows <- function(blueprint)
{
    ## Remove comment rows -----------------------------------------------------------
                                        # Remove empty,NA or blank rows
    (
        is.na(blueprint[,1])|
        is.nan(blueprint[,1])|
        blueprint[,1] %>% str_detect('^ *$')|
        blueprint[,1] %>% str_detect('^ *#')
    )  %>% `!` %>% which -> not.column.rows
    if(length(not.column.rows)>0){
        blueprint[not.column.rows,] -> blueprint
    }
    return(blueprint)
}


## blueprint.log -----------------------------------------------------------
##' @importFrom logging loginfo
blueprint.log <- function(message){
    
    loginfo(message, logger="blueprint.logger")
}

##' @importFrom logging loginfo
blueprint.code.log <- function(message){
    loginfo(message, logger="blueprint.code.logger")
}

## return.df.with.certain.vars -----------------------------------------------------------
##' @importFrom dplyr transmute
##' @importFrom plyr llply
##' @importFrom dplyr %>%
return.df.with.certain.vars <- function(df,...)
{
                                        # Return a data.frame containing certain variables also ordered by vars.to.get
                                        # vars not in
    eval(substitute(alist(...)))  %>%  sapply(toString)   -> vars.to.get
    vars.to.get %>% llply(function(x){exists(x,df)}) %>% unlist   %>% `!` %>% which -> not.existing.pos
    if (length(not.existing.pos>0))
    {
        vars.to.get[not.existing.pos]  %>% paste0(.,'=rep_len(NA_real_,nrow(df))') -> vars.to.get[not.existing.pos]
    }
    vars.to.get %>% paste0(collapse=',\n                                     ') %>%                               paste0('df %>% dplyr::transmute(',.,') -> df')                      -> code.to.execute
    eval(parse(text=code.to.execute))
    return(df)
}

##' @importFrom dplyr transmute
##' @importFrom dplyr %>%
return.code.to.return.df.with.certain.vars_ <- function(all.vars,missing.var.pos)
{
                                        # Return a data.frame containing certain variables also ordered by vars.to.get
                                        # vars not in
    
    transmute.vars <- all.vars
    if (length(missing.var.pos>0))
    {
                                        #            missing.vars  %>% paste0(.,'=rep_len(NA_real_,nrow(.))') -> transmute.code
        transmute.vars[missing.var.pos]  %>% paste0(.,'=NA_real_') -> transmute.vars[missing.var.pos]
    }

    transmute.vars %>% paste0(collapse=',\n                               ') %>% paste0('dplyr::transmute(',.,')')                      -> code.to.execute
    return(code.to.execute)
}

## df.set.standard.names -----------------------------------------------------------
##' @importFrom stringr str_detect
##' @importFrom dplyr %>%
df.set.standard.names  <- function(df)
{
    names(df)  -> df.names
    df.names %>% str_detect('var') %>% which -> df.var.columns
    'var'  -> df.names[df.var.columns]
    df.names %>% str_detect('file') %>% which -> df.file.columns
    'file'  -> df.names[df.file.columns]
    df.names %>% str_detect('link') %>% which -> df.link.columns
    'link'  -> df.names[df.link.columns]    
    df.names %>% str_detect('fun') %>% which -> df.fun.columns
    'fun'  -> df.names[df.fun.columns]
                                        # lastly set newvar that has previous also been set to var
    'newvar'  -> df.names[1]
    df.names -> names(df)    
    return(df)
}

blueprint.check.for.missing.files <- function(blueprint)
{
    ## detect for missingness in files -----------------------------------------------------------
    blueprint %>% return.not.existing.files  -> not.existing.files
                                        # remove empty fields
    (not.existing.files=='') %>% which -> apos
    not.existing.files[-apos]  -> not.existing.files
    if(length(not.existing.files)>0){
        paste0('The following files do not exist:\n',paste0(not.existing.files,sep='\n')) -> warn.mess
        stop(warn.mess)
    }
    return(blueprint)
}


## check for duplicate variable names and stop -----------------------------------------------------------
blueprint.check.for.duplicate.variable.names <- function(blueprint)
{
    if(sum(duplicated(blueprint[,1]))>0){
        stop(paste0('Duplicate variablenames: ',blueprint[which(duplicated(blueprint[,1]))[1],1]),' rows:',paste0(which(duplicated(blueprint[,1])),collapse=','))}
    return(blueprint)
}


## blueprint.chunk.validator -----------------------------------------------------------
##' @importFrom dplyr %>%
blueprint.chunk.validator <- function(blueprint)
{
    blueprint %>%
        blueprint.check.for.duplicate.variable.names %>%
        blueprint.check.for.missing.files  %>% 
        blueprint.check.every.specified.original.var.has.a.file  -> blueprint
    return(blueprint)
}



##' \code{blue} - Read a blueprint file and return a merged data frame.
##' 
##' Use a blueprint-file to import a subset of variables from several data files, optionally transform them by specified functions (e.g. to recode values) and recombine them into a new wide data.frame.
##' @param blueprint A meta-data file that contains specifications what to do. The file format is taken from the suffix as defined in the \code{\link[=rio]{import}} function. See the vignette for details of the structure of a blueprint.
##' @param fun Logical vector wheter the functions from \code{fun} should be applied on the specified variables.
##' @param export_file Path to file the data is written after merging. The suffix determines the file type. In addition the data.frame is returned \code{\link{invisible}}. Note that if you choose to export to stata you have to choose final variable names (column newvar) that comply with the stata convention of stata names. You must use no dots (.) and length of a variable name may not excede a maximum number of 26? characters.
##' @param chunks A numeric vector specifying the chunks that shall be included from the blueprint file. If NULL every chunk will be merged.
##' @param logfile Either a logfile wheter to use an extended logfile. Or path where this extended logfile is written. The extended logfile will contain descriptive statistics and allow for inference to possible problems when transforming data. However the computation of descriptive statistics will take extra time which is why this argument is set to FALSE by default.
##' @param data.table Wheter to use the data.table package for merge process. (Minimal faster)
##' @param ... Optionally commands are passed to \code{\link[=rio]{import}}. Especially select the sheet of an Excel (.xlsx) files by the argument \code{which}.
##' @return New merged \code{data.frame} according to the blueprint. It is set to the class \code{\link[=dplyr]{tibble}}.
##' @author Marc Schwenzer <m.schwenzer@uni-tuebingen.de>
##' @export
##' @importFrom logging addHandler
##' @importFrom logging writeToFile
##' @importFrom stringr str_replace
##' @importFrom stringr str_detect
##' @importFrom stringr str_split
##' @importFrom rio export
##' @importFrom magrittr %T>%
##' @importFrom dplyr %>%
blue <- function(
                 blueprint=options()$'blueprint_file',
                 fun=TRUE,                 
                 export_file=NULL,
                 chunks=NULL,
                 logfile=FALSE,                      
                 data.table=TRUE,
                 ...
                 ){
                                        # requirements
                                        # Load Merge Data from XLS
    
    ## Logfile -----------------------------------------------------------
    ## Make default logfile path if missing logfile
                                        # Will be removed when alpha
    dots <- list(...)
    ifelse(
        names(dots) %>% str_detect('^w*') %>% which  %>% `>`(0),
            paste0('.',dots[         names(dots) %>% str_detect('^w*')  %>% which %>%  .[1]  ]),
        '') -> whichspecifier
    if(is.character(logfile)){extended=TRUE}
    if(is.logical(logfile)){
        if(logfile)
        {
            extended=TRUE
        }
        else
        {
        extended=FALSE            
        }
        str_replace(blueprint,'\\.....+$',paste0('.blueprint',whichspecifier,'.log.txt')) -> logfile
    }
    logfile %>% normalised.path.and.dir.exists -> logfile
    if(logfile==blueprint)
    {stop('You have to specify a path to a logfile since automatic replacement of the suffix was not possible. Try to set a logfile path argument or change it.')}
    if(file.exists(logfile)){unlink(logfile)}    
    addHandler(writeToFile, logger="blueprint.logger", file=logfile,formatter=blueprint.log.formatter)    
    start.message <- paste0('- Parsing blueprint file `',blueprint,'` (which: ',whichspecifier %>% str_replace('\\.',''),').','\n- Logging to file `',logfile,'`.\n')
    cat(start.message)
    blueprint.log(Sys.time())
    blueprint.log(start.message)
    str_replace(blueprint,'\\.....+$',paste0('.blueprint',whichspecifier,'.code.R')) -> codefile

                                        # !!! check for path consistency
    if(file.exists(codefile)){unlink(codefile)}
    
    addHandler(writeToFile, logger="blueprint.code.logger", file=codefile,formatter=blueprint.log.formatter)
    ## Import and validate blueprint -----------------------------------------------------------
    code.time <- Sys.time()
    rio::import(file=blueprint,...) %>% extract.blueprint.meta.statements(1)  -> blueprints
    blueprints$meta.statements -> global.meta.statements
    blueprints$blueprint %>%  validate.blueprint.file.and.return.list.of.valid.blueprints(blueprint=.,chunks=chunks) -> blueprints
    rm(blueprint)    
                                        # blueprints: a data.frame with columns 'chunk' and 'blueprints'
    ## Convert blueprints to code -----------------------------------------------------------
                                        # validator for this kind of data.frame
                                        # Actually get data to blueprints
    if(data.table){blueprint.code.log('suppressMessages(require(data.table,quietly = TRUE))')}
    blueprint.code.log('require(dplyr)')
    blueprint.code.log('data.frame() -> final.df')    
    blueprint.code.log(return.diff.code())
    blueprint.code.log(paste0('progress_estimated(',nrow(blueprints),') -> p'))
    blueprints %>% dplyr::do(dfs={
        (.) -> df_in
        blueprint <- df_in$blueprints
        chunk <- df_in$chunk
        blueprint.code.log(paste0('### chunk ',chunk))
        blueprint.code.log('\nprint(p$tick()$print())\n')
        
        blueprint$newvar -> all.vars
        is.na(blueprint$var) %>% which -> missing.var.pos
                                        #        cat('blueprint$newvar:\n',blueprint$newvar)        
                                        #        cat('blueprint$var:\n',blueprint$var)
         ###
        blueprint.log(paste('\n\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n>>> Processing chunk:',chunk,'\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n'))
                                        # get different unique filenames to process for each chunk
        blueprint.files <- unique(blueprint$file)
                                        # main file has to be the first specified file
        blueprint.main.file <- blueprint.files[1]
                                        # vars in main file that shall be kept
        vars.to.get <- blueprint$var
### Load main file ####################################################################################################
        blueprint.log(paste0('loading main file:',blueprint.main.file,'\n'))
        blueprint$file %>% str_detect(blueprint.main.file)  -> pos.main.file
        (pos.main.file & (pos.main.file %>% is.na %>% `!`)) -> pos.main.file
        blueprint[pos.main.file,] %>%
            load.and.recode(fun=fun,chunk=chunk,extended=extended) -> code.to.execute
        paste0(code.to.execute,'  -> main.data') -> code.to.execute
        blueprint$newvar %>% na.omit    -> main.data.var
        blueprint.code.log(code.to.execute)
        # !!! eval(parse(text=code.to.execute)) -> main.data
                                        # loaded and processed. remaining parts still to be processed
        blueprint[ !blueprint[,'file'] == blueprint.main.file & !is.na(blueprint$file),] -> blueprint
### restrict to variables that are specified by a file reference !!! should be: also a link
### Add the additional files  ####################################################################################################
                          if(length(blueprint$file)>0){
                              for(x in unique(blueprint$file) )
                              {
                                  blueprint.log(paste0('--- Adding additional variables from file:',x,'\n'))

                                        #x <- unique(blueprint$files)[1]
                                        # variabels to replace in original frame

                                  blueprint[blueprint$file==x,] -> add.blueprint
                                        # variables that replace in replacement frame
                                  the.vars <- add.blueprint[,'var']

                                  links <-add.blueprint[,'link']
                                  functions <-add.blueprint[,'fun']
                                  if(sum(is.na(links)&!is.na(the.vars))>0)
                                  {
                                      stop(paste('You have to specify links for the variables\n',paste(the.vars,collapse=',')))
                                  }
      
                          
                                  add.blueprint$link %>% str_split(',') %>% plyr::llply(.,function(x){x %>% str_replace('^.*=','')})  -> to.links                                      
# find link variables not specified in blueprints
                                  ((to.links %>% unlist %>% unique)    %in% add.blueprint$newvar) %>% `!` -> pos
                                  (to.links %>% unlist %>% unique)[pos] -> vars.to.add
                                  links %>% process.links -> link.condition
                                        #        paste0('"',from.links,'"="',to.links,'"',collapse=',') -> link.condition
         #                         link.condition
         #                         add vars.to.add to add.blueprint : 


                                  rbind(add.blueprint,data.frame(newvar=vars.to.add,var=vars.to.add,file=rep_len(NA,length(vars.to.add)),link=rep_len(NA,length(vars.to.add)),fun=rep_len(NA,length(vars.to.add)))) -> add.blueprint

#### merge the data
                                  add.blueprint %>% load.and.recode(fun=fun,chunk=chunk,extended=extended)  -> code.to.execute
                                  paste0(code.to.execute,' -> data.add') -> code.to.execute
                                  blueprint.code.log(code.to.execute)
                                  paste0('\n\ndplyr::left_join(main.data,data.add,by=c(',link.condition[1],'))  -> main.data\n\nrm(data.add)\n\n') -> code.to.execute
                                        # codelog
                                  blueprint.code.log(code.to.execute)
                                        #eval(parse(text=code.to.execute))
                                  }
                          }
        paste0('data.chunk',chunk) -> df.chunk.name
        return.code.to.return.df.with.certain.vars_(all.vars,missing.var.pos) -> select.code
                                        #        paste0('main.data  %>% ',select.code,' %>% mutate(chunk=',chunk,')  -> ',df.chunk.name) -> code.to.execute
                paste0('main.data  %>%  mutate(chunk=',chunk,')  -> ',df.chunk.name) -> code.to.execute
        # eval(parse(text=code.to.execute))
        blueprint.code.log(code.to.execute)
        if(chunk==1)
        {
            paste0(df.chunk.name,' -> final.df') -> code.to.execute
            }
            else
            {
                c('final.df',df.chunk.name) %>% make.bind.code(data.table=TRUE)  -> code.to.execute
            }
        code.to.execute %>% paste0(.,'\nrm(',df.chunk.name,')') -> code.to.execute
        blueprint.code.log(code.to.execute)
df.chunk.name
    }) -> blueprints.data

    blueprints.data$dfs  %>% unlist  -> dfs
    
                                        # paste0('rbind(',paste0(dfs,collapse=',          \n'),')   %>% tbl_df  -> final.df') -> code.to.execute

    
    blueprint.code.log('final.df %>% tbl_df -> final.df')
                                        # execute meta.statemets indicated by @ as previous extracted by extract.blueprint.meta.statements
    blueprint.code.log(paste0('\n',global.meta.statements,collapse=''))

#    cat(paste0('\nTime taken to produce code.file: ',format(round(Sys.time()- code.time,2),unit='sec'),'\n\n'))
    cat(paste0('\n- Starting iterations of import',ifelse(fun,', transformation',''),ifelse(extended,',',' and'),' merge',ifelse(extended,' and compution of extended stats',''),' ...\n'))
    eval.time <- Sys.time()
source(codefile,local=TRUE)
#source(codefile)
    blueprint.log('')        
    blueprint.log(Sys.time())
    blueprint.log('')    
    blueprint.log(paste('Finally ready. Merged data.frame has',dim(final.df)[1],'rows and',dim(final.df)[2],'columns.'))
                                        #    cat(paste0('\nTime elapsed for merging: ',format(Sys.time()- eval.time,unit='sec'),'\n\n\n'))
    
    cat(paste0('--- Creation of new data.frame [',nrow(final.df),' rows, ',ncol(final.df),' cols] took ',format(round(Sys.time()- code.time,1),unit='sec'),'.\n'))
    if(is.character(export_file)){
        cat(paste0('--- Writing the merged data to file `',export_file,'` '))
        write.time <- Sys.time()
        export(final.df,file=export_file)
        cat(paste0('took ',format(round((Sys.time()-write.time),1),unit='sec'),'.\n'))
        blueprint.log(paste0('--- Written data.frame to file:',export_file,'.'))
        return(invisible(final.df))
    }
    else{
        return(final.df)
        }
    }




##' open_blue creates or loads blueprint-files.
##' 
##' \code{"open_blue"} creates or loads blueprint-files of various file formats.
##' @param blueprint Path to blueprint (meta-data file) that contains specifications about the variables in data files that will be merged. The file format of data is taken from the suffix as defined in the \code{\link[=rio]{import}}. See the vignette for details of the structure.
##' @param chunks Number specifying the chunks to be included in the new blueprint file. This can be changed later manually by adding appropriate named columns.
##' @return Nothing. Just used for the side effect of generating or opening the specified blueprint file.
##' @author Marc Schwenzer <m.schwenzer@uni-tuebingen.de>
##' @export
##' @examples \dontrun{open_blue('/path/to/file.xlsx')}
##' @importFrom rio export
##' @importFrom utils browseURL
##' @importFrom dplyr %>%
open_blue <- function(
                          blueprint=options()$'blueprint_file',
                          chunks=20
                          )
{
    blueprint %>% normalised.path.and.dir.exists  -> blueprint
    if(!file.exists(blueprint)){
        c('var','file','link','fun') -> varnams
    c('newvar',paste0(rep(varnams,times=chunks),sort(rep((1:chunks),times=4))))-> varnams
        paste0(varnams,'=c("","")',collapse=',')-> varnams
        c('# The new name of the variable (that merges the data files of the chunk).','The original variable in the data.set','The original file that does contain this variable','A function or pipe of functions that is executed on the orignal variable var1 (e.g. for recoding)','Specifications of the variables that link the data') -> description
        eval(parse(text=paste0('data.frame(',varnams,',stringsAsFactors=FALSE)'))) -> a.df
        a.df[1,1] <- '# Chunk'
        a.df[1,c(2+4*((1:chunks)-1))]<-paste0('# ',1:chunks)
        a.df[1,c(5+4*((1:chunks)-1))]<-''
        a.df[2,1:5] <- description
    a.df %>% export(file=blueprint)
    cat(paste0('Written blueprint template to file ',blueprint,'.\n'))
    }
    browseURL(paste0('file://', blueprint))
    invisible(blueprint)
    }

## Local Variables:
## ess-r-package-info: ("blueprint" . "/doc/wissenschaft/rpackages/blueprint/")
## End:

##' blue_example Creates a folder with example files
##'
##' This folder will be named 'blueprint_example' and contains the example files 'INT_STU12_DEC03_synth.sav','INT_SCQ12_DEC03_synth.sav','example_blueprint1.xlsx','example_blueprint1.csv','example_blueprint2.xlsx','example_blueprint2.csv'.
##' @return NULL
##' @author Marc Schwenzer
##' @export
##' @importFrom rio export
##' @importFrom dplyr %>%
blue_example <- function()
{
    dir.create('blueprint_example')
    load(
        base::system.file("extdata", "examples.rda", package = "blueprint")
    )
    INT_STU12_DEC03_synth  %>% export('blueprint_example/INT_STU12_DEC03_synth.sav')
    INT_SCQ12_DEC03_synth  %>% export('blueprint_example/INT_SCQ12_DEC03_synth.sav')
    example_blueprint1 %>% export('blueprint_example/example_blueprint1.xlsx')
    example_blueprint1 %>% export('blueprint_example/example_blueprint1.csv')
    example_blueprint2 %>% export('blueprint_example/example_blueprint2.xlsx')
    example_blueprint2 %>% export('blueprint_example/example_blueprint2.csv')
    invisible(NULL)
}

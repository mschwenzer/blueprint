

blueprint.check.every.specified.original.var.has.a.file  <- function(df)
{
    df$var %>% is.na %>% `!` %>% which  -> missing.in.var
    df$file[missing.in.var] %>% is.na %>% which -> also.missing.in.file
    if(length(also.missing.in.file)>0)
    {
        stop(paste0('Missing files for specified variables:\n'),paste0('var: ',df$var[(missing.in.var[also.missing.in.file])],' -> newvar: ',df$newvar[(missing.in.var[also.missing.in.file])],collapse='\n'))}
            return(df)
    }

df.remove.non.standard.named.columns  <- function(df)
{
    ## remove empty colums
    standard.names <- c('newvar','link','var','file','fun')
    (names(df)%in%standard.names)  %>%  `!` %>% which  -> to.drop
    #print(to.drop)
    if(length(to.drop)>0){
        df[,-c(to.drop)] -> df
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



        
validate.blueprint.file.and.return.list.of.valid.blueprints <- function(blueprint,debug=FALSE,waves)
    {
        blueprint %>%
            blueprint.remove.column.rows    %>%
            ## guarantee that all columns have the correct name, especially var used for cutting
            df.set.standard.names %>%
            df.remove.non.standard.named.columns %>%
            stop.if.no.var.column -> blueprint
                                        # cut blueprint into waves
        data.frame(startcol=(names(blueprint)=='var') %>% which,
                                        # end column 
                   endcol=c((names(blueprint) =='var')  %>% which %>% .[-1] %>% `-`(1),
                                        # + the last column containing everything
                            length(names(blueprint)))) %>% dplyr::transmute(wave=1:nrow(.),startcol,endcol)  -> blueprints.column.info
        #print(blueprints.column.info)
                                        # Subset over the waves
        if(is.numeric(waves)){
            blueprints.column.info %>% dplyr::filter(.$wave%in%c(waves)) ->         blueprints.column.info
        }
        cat('- Validating blueprint file for wave')
        blueprints.column.info %>% dplyr::group_by(wave)   %>%
                                        # -> reduced to a single-line data.frame containing the selected waves and the rows
            dplyr::do(blueprints={
                cat(paste0('...',.$wave))
            chunk.columns <- c(1,(.$startcol):(.$endcol))
            blueprint[,chunk.columns]  %>% 
                ## normalise to a data.frame with these variables / remove columns not named correct
                return.df.with.certain.vars(newvar,var,file,link,fun) %>%
                ## ❗️ to validate
                add.variables.specified.by.brackets %>%
                set.empty.values.to.NA  %>%
                ## Validate all blueprints before something is done actually.... 
                blueprint.wave.validator})
    }


return.diff.code <- function()
{
    "
blueprint.log <- function(message){

    loginfo(message, logger='blueprint.logger')
}

blueprint.log.formatter <- function(record) {
text <- paste(paste0(record$msg, sep=' '))
}

blueprint.variable.diff <- function(variable,funs,name='',wave='')
{
    blueprint.log('')
    blueprint.log(Sys.time())
    blueprint.log('')
blueprint.log        (paste0('----Transformation of variable `',name,'`  (wave ',wave,'): ',funs,'  -----------------------------\n'))
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
    data.frame(old=kept.levels.of.variable,`. `=rep('|',length(kept.levels.of.variable)),`.  `=rep('v',length(kept.levels.of.variable)),new=variable[old.pos],`(n)`=old.count) %>% dplyr::arrange(old)  ->     printfr
    
capture.output(    printfr %>% as.matrix %>% t %>% stargazer(type='text')  %>% paste0(.,'\n') %>% blueprint.log,file=NULL)  -> bla}
                            if(new.type!=old.type){
                                blueprint.log(paste0('!!! Type conversion from ',old.type,' to ',new.type,'. Was this intended?'))
                            }
    blueprint.log('')
    blueprint.log('')    
    blueprint.log('   >>> Distribution after recoding -----\n')                            
    capture.output(x=print(Hmisc::describe.vector(variable)),file=NULL) %>% blueprint.log
    return(variable)
}"
}




make.bind.code <- function(dfs,data.table=TRUE){
    if(data.table)
    {
        paste0('library(data.table)\nlist(',paste0(dfs,collapse=','),')  %>% 
         lapply(setattr, name = "class",
                         value = c("data.table",  "data.frame")) %>% 
         rbindlist(use.names=TRUE, fill=TRUE) -> final.df')
    }
}



## require(plyr)
## require(dplyr)
## require(stringr)  
## require(rio)
## require(logging)
## require(stargazer)
## require(Hmisc)    

blueprint.log.formatter <- function(record) {
    text <- paste(paste0(record$msg, sep=' '))
}


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
    paste0('x %>% dplyr::recode(',recod,') %>% as.factor-> x') -> code.to.eval
                                        #    print(code.to.eval)
                                        #eval(parse(text=code.to.eval))
    return(x)
                                        #     attr(x, "label") <- the.label
}


load.and.recode <- function(blueprint,fun=FALSE,wave=1,debug=FALSE,extended=FALSE)
{
                                        #                         cat('recs:\n')
                                        #                          print(recs)
                                        # find non-empty fun (rec) columns  / ❗️ replace by stringr::str_match / find out why it can be string "NA" which is a bug
    if(fun)
    {
        ((blueprint$fun!='NA')&(!is.na(blueprint$fun))&(!is.nan(blueprint$fun)))  %>% which -> rep.pos
        if(debug){   print(rep.pos)}
        if(extended){
                                        # if logging, execute the transformation in blueprint.variable.diff 
            paste0(blueprint$var[rep.pos],' %>% blueprint.variable.diff(fun="',blueprint$fun[rep.pos],'",name="',blueprint$var[rep.pos],'",wave="',wave,'")') -> blueprint$var[rep.pos]
        }
        else{
                                        # else direct
            paste0(blueprint$var[rep.pos],' %>% ',blueprint$fun[rep.pos]) -> blueprint$var[rep.pos]
        }
    }
    paste0(blueprint$newvar,'=',blueprint$var,collapse=',\n                         ') -> transmute.code
                                        # create a string that renames/selects with select and mutates afterwards
    paste0('rio::import("',blueprint$file[1],'")',paste0(' %>%\n        dplyr::transmute(',transmute.code,')')) -> code.to.execute
                                        #                              print(eval.code)
                                        # execute and return
                                        #                              print(eval.code)
    return(code.to.execute)
}





process.links <- function(links)        {
    links %>% str_replace_all('"','') %>% str_replace_all("'","") %>% 
        lapply(function(link)
        {
            if(is.na(link)){return(NA)}
            link %>% stringr::str_split(',') %>%  .[[1]]  -> link
            link %>% stringr::str_detect('=') %>% `!` %>% which  -> pos.no.equal.sign
                                        #                print(pos.no.equal.sign)
                                        # add equal sign for condition
                                        #                print(link[pos.no.equal.sign])
            link[pos.no.equal.sign]  %>% paste0(.,'=',.)  -> link[pos.no.equal.sign]
                                        #                print(link)
            link %>% stringr::str_replace_all('=','"="') %>% paste0('"',.,'"')  %>% paste0(collapse=',')-> link
            return(link)
        }) %>% unlist}



blueprint.variable.diff <- function(variable,funs,name='',wave='')
{
    blueprint.log('')
    blueprint.log(Sys.time())
    blueprint.log('')
    blueprint.log        (paste0('----Transformation of variable `',name,'`  (wave ',wave,'): ',funs,'  -----------------------------\n'))
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
        data.frame(old=kept.levels.of.variable,`. `=rep('|',length(kept.levels.of.variable)),`.  `=rep('v',length(kept.levels.of.variable)),new=variable[old.pos],`(n)`=old.count) %>% dplyr::arrange(old)  ->     printfr
        
        capture.output(    printfr %>% as.matrix %>% t %>% stargazer(type='text')  %>% paste0(.,'\n') %>% blueprint.log,file=NULL)  -> bla}
    if(new.type!=old.type){
        blueprint.log(paste0('!!! Type conversion from ',old.type,' to ',new.type,'. Was this intended?'))
    }
    blueprint.log('')
    blueprint.log('')    
    blueprint.log('   >>> Distribution after recoding -----\n')                            
    capture.output(x=print(Hmisc::describe.vector(variable)),file=NULL) %>% blueprint.log
    return(variable)
}

## make empty NA -----------------------------------------------------------
set.empty.values.to.NA <- function(blueprint)
{
    blueprint %>% mutate_all(.funs=funs(ifelse(.=='',NA,.)))
}
## add.variables.specified.by.brackets -----------------------------------------------------------
add.variables.specified.by.brackets <- function(blueprint)
{
    ## Add [0:x]-specified interval to variablenames: create a row for every individual variable  -----------------------------------------------------------
    rowstoprocess <- blueprint[,1]
    blueprint %>% names -> column.names
    while(length(
    ((blueprint[,1]) %>% stringr::str_detect('\\[[0-9]*:[0-9]*\\]') %>% which)
    >0))
    {
        rowid <- ((blueprint[,'newvar']) %>% stringr::str_detect('\\[[0-9]*:[0-9]*\\]') %>% which)
        if((rowid-1)>0){
            frame.before <- blueprint[1:(rowid-1),]
        }
        if((rowid+1)<(nrow(blueprint)+1))
        {
            frame.after <- blueprint[(rowid+1):nrow(blueprint),]
        }
        to.process.vars <- blueprint[rowid,]
        ## ❗️ to change
        pattern <- regmatches(to.process.vars[,1],regexpr('\\[[0-9]*:[0-9]*\\]',to.process.vars[,1]))
        nums <- eval(parse(
            text=(pattern %>% stringr::str_replace_all('\\[','') %>% stringr::str_replace_all('\\]',''))
        )
        )
        pattern %>% stringr::str_replace_all('\\[','\\\\[') %>% stringr::str_replace_all('\\]','\\\\]')  -> pattern
        frame.toadd <- plyr::ldply(nums,
                                   function(x){
                                       to.process.vars %>% stringr::str_replace_all(pattern,x)
                                   })
        names(frame.toadd) <- column.names
        assign('blueprint',rbind(if((rowid-1)>0){frame.before},frame.toadd,              if((rowid+1)<(nrow(blueprint)+1)){frame.after}),-1)
    }
    return(blueprint)
}
###





## return.not.existing.files -----------------------------------------------------------
return.not.existing.files <- function(blueprint)
{
    blueprint[,'file'] %>% unlist %>% c %>% unique %>% na.omit  -> files.to.check
    plyr::llply(files.to.check,function(x){
        (!file.exists(x))|                    dir.exists(x)
    }) %>% unlist          %>% files.to.check[.]
}
###

blueprint.remove.column.rows <- function(blueprint,debug=FALSE)
{
                                        #    print(blueprint[,1])
    ## Remove comment rows -----------------------------------------------------------
    llply(blueprint[,1],function(x){stringr::str_detect(x,'^ *#')})  %>% unlist %>% `!`  %>% which-> not.commentrows
                                        #    print(not.commentrows)
    if(debug){cat('commentrows:',which(not.commentrows),'\n')}
    if(length(not.commentrows)>0){
        blueprint[not.commentrows,] -> blueprint
    }
    return(blueprint)
}


## blueprint.log -----------------------------------------------------------
blueprint.log <- function(message){
    
    loginfo(message, logger="blueprint.logger")
}
blueprint.code.log <- function(message){
    loginfo(message, logger="blueprint.code.logger")
}

## return.df.with.certain.vars -----------------------------------------------------------
return.df.with.certain.vars <- function(df,...,debug=0)
{
                                        # Return a data.frame containing certain variables also ordered by vars.to.get
                                        # vars not in
    eval(substitute(alist(...)))  %>%  sapply(toString)   -> vars.to.get
                                        #    print(vars.to.get)
    vars.to.get %>% plyr::llply(function(x){exists(x,df)}) %>% unlist   %>% `!` %>% which -> not.existing.pos
                                        #    print(vars.to.get[not.existing.pos])
    if (length(not.existing.pos>0))
    {
        vars.to.get[not.existing.pos]  %>% paste0(.,'=rep_len(NA_real_,nrow(df))') -> vars.to.get[not.existing.pos]
    }
                                        #    print(vars.to.get)
    vars.to.get %>% paste0(collapse=',\n                                     ') %>%                               paste0('df %>% dplyr::transmute(',.,') -> df')                      -> code.to.execute
                                        #    print(code.to.execute)
    eval(parse(text=code.to.execute))
    return(df)
}


return.code.to.return.df.with.certain.vars_ <- function(all.vars,missing.var.pos,debug=0)
{
                                        # Return a data.frame containing certain variables also ordered by vars.to.get
                                        # vars not in
    
                                        #    print(vars.to.get[not.existing.pos])
    transmute.vars <- all.vars
    if (length(missing.var.pos>0))
    {
                                        #            missing.vars  %>% paste0(.,'=rep_len(NA_real_,nrow(.))') -> transmute.code
        transmute.vars[missing.var.pos]  %>% paste0(.,'=NA_real_') -> transmute.vars[missing.var.pos]
    }
                                        #    print(vars.to.get)
    transmute.vars %>% paste0(collapse=',\n                               ') %>% paste0('dplyr::transmute(',.,')')                      -> code.to.execute
                                        #   print(code.to.execute)
    return(code.to.execute)
}

## df.set.standard.names -----------------------------------------------------------
df.set.standard.names  <- function(df)
{
    names(df)  -> df.names
    df.names %>% stringr::str_detect('var') %>% which -> df.var.columns
    'var'  -> df.names[df.var.columns]
    df.names %>% stringr::str_detect('file') %>% which -> df.file.columns
    'file'  -> df.names[df.file.columns]
    df.names %>% stringr::str_detect('link') %>% which -> df.link.columns
    'link'  -> df.names[df.link.columns]    
    df.names %>% stringr::str_detect('fun') %>% which -> df.fun.columns
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
        stop(paste('Duplicate variablenames:',blueprint[which(duplicated(blueprint[,1]))[1],1]),' rows:',paste0(which(duplicated(blueprint[,1])),collapse=','))}
    return(blueprint)
}


## blueprint.wave.validator -----------------------------------------------------------
blueprint.wave.validator <- function(blueprint)
{
    blueprint %>%
        blueprint.check.for.duplicate.variable.names %>%
        blueprint.check.every.specified.original.var.has.a.file %>%         
        blueprint.check.for.missing.files  -> blueprint
    return(blueprint)
}
## blueprint -----------------------------------------------------------

blue <- function(
                 blueprint='/Users/eur/Documents/140_Datenaufbereitung/pisa.xlsx',
                 export.file=NULL,
                 waves=NULL,
                 debug=FALSE,
                 logfile=FALSE,                      
                 fun=TRUE,
                 extended=FALSE,
                 data.table=TRUE,
                 ...
                 ){
                                        # requirements
                                        # Load Merge Data from XLS
    
                                        # if(debug){cat(paste0('file: ',blueprint))}
    ## Logfile -----------------------------------------------------------
    ## Make default logfile path if missing logfile
    if(!logfile)
    {
        stringr::str_replace(blueprint,'\\.....+$','.log.txt') -> logfile
    }
    if(logfile==blueprint)
    {stop('You have to specify a logfile since automatic replacement of the suffix was not able. Try to set a logfile argument or change it.')}
    if(file.exists(logfile)){unlink(logfile)}    
    addHandler(writeToFile, logger="blueprint.logger", file=logfile,formatter=blueprint.log.formatter)    
                                        # if(debug){print('logger created')}
    start.message <- paste0('- Parsing blueprint file: ',blueprint,'.',if(extended){paste0('\nlogging to file: ',logfile)},'\n')
    cat(start.message)
    blueprint.log(Sys.time())
    blueprint.log(start.message)
    stringr::str_replace(blueprint,'\\.....+$','.code.R') -> codefile
                                        # ❗️ check for path consistency
    if(file.exists(codefile)){unlink(codefile)}
    
    addHandler(writeToFile, logger="blueprint.code.logger", file=codefile,formatter=blueprint.log.formatter)
    ## Import and validate blueprint -----------------------------------------------------------
    code.time <- Sys.time()    
    rio::import(file=blueprint,...) %>% validate.blueprint.file.and.return.list.of.valid.blueprints(blueprint=.,debug=debug,waves=waves) -> blueprints
    rm(blueprint)    
                                        # blueprints: a data.frame with columns 'wave' and 'blueprints'
    ## Convert blueprints to code -----------------------------------------------------------
                                        #➤ if(debug){print(blueprint)}
    
    
                                        #➤ if(debug){print(blueprints) }
                                        # validator for this kind of data.frame
                                        # Actually get data to blueprints
    if(data.table){blueprint.code.log('suppressMessages(require(data.table,quietly = TRUE))')}
    blueprint.code.log('require(dplyr)')
    blueprint.code.log(return.diff.code())
    blueprint.code.log(paste0('progress_estimated(',nrow(blueprints),') -> p'))
    blueprints %>% dplyr::do(dfs={
        blueprint <- .$blueprints
        wave <- .$wave
        blueprint.code.log(paste0('### wave ',wave))
        blueprint.code.log('\nprint(p$tick()$print())\n')
        
        blueprint$newvar -> all.vars
        is.na(blueprint$var) %>% which -> missing.var.pos
                                        #        cat('blueprint$newvar:\n',blueprint$newvar)        
                                        #        cat('blueprint$var:\n',blueprint$var)
         ###
        blueprint.log(paste('\n\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n>>> Processing wave:',wave,'\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n'))
                                        # get different unique filenames to process for each wave
        blueprint.files <- unique(blueprint$file)
                                        # main file has to be the first specified file
        blueprint.main.file <- blueprint.files[1]
                                        # vars in main file that shall be kept
        vars.to.get <- blueprint$var
                                                # print(vars.to.get)
### Load main file ####################################################################################################
        blueprint.log(paste0('loading main file:',blueprint.main.file,'\n'))
        blueprint$file %>% stringr::str_detect(blueprint.main.file)  -> pos.main.file
        (pos.main.file & (pos.main.file %>% is.na %>% `!`)) -> pos.main.file
                                        #➤ if(debug)        {print(blueprint$wave                 )}
        blueprint[pos.main.file,] %>%
            load.and.recode(fun=fun,wave=wave,extended=extended) -> code.to.execute
        paste0(code.to.execute,'  -> main.data') -> code.to.execute
        blueprint$newvar %>% na.omit    -> main.data.var
        blueprint.code.log(code.to.execute)
        # 🔴 eval(parse(text=code.to.execute)) -> main.data
                                        # loaded and processed. remaining parts still to be processed
        blueprint[ !blueprint[,'file'] == blueprint.main.file & !is.na(blueprint$file),] -> blueprint
### restrict to variables that are specified by a file reference ❗️ should be: also a link
### Add the additional files  ####################################################################################################
                if(debug){       print(all.vars)}
                if(debug) {print(blueprint$file)}
                          if(length(blueprint$file)>0){
                              for(x in unique(blueprint$file) )
                              {
                                  blueprint.log(paste0('--- Adding additional variables from file:',x,'\n'))

                                        #x <- unique(blueprint$files)[1]
                                        #print(blueprint)
                                        # variabels to replace in original frame

                                  blueprint[blueprint$file==x,] -> add.blueprint
                                        # variables that replace in replacement frame
                                  the.vars <- add.blueprint[,'var']
                                        #print(the.vars)##
                                  links <-add.blueprint[,'link']
                                        #print(blueprint)
                                  functions <-add.blueprint[,'fun']
                                  if(sum(is.na(links)&!is.na(the.vars))>0)
                                  {
                                      stop(paste('You have to specify links for the variables\n',paste(the.vars,collapse=',')))
                                  }
      
                          
                                  add.blueprint$link %>% stringr::str_split(',') %>% plyr::llply(.,function(x){x %>% stringr::str_replace('^.*=','')})  -> to.links                                      
# find link variables not specified in blueprints
                                  ((to.links %>% unlist %>% unique)    %in% add.blueprint$newvar) %>% `!` -> pos
                                  (to.links %>% unlist %>% unique)[pos] -> vars.to.add
                                  if(debug){cat('vars.to.add:\n',vars.to.add)}
                                  links %>% process.links -> link.condition
                                        #        paste0('"',from.links,'"="',to.links,'"',collapse=',') -> link.condition
         #                         link.condition
         #                         add vars.to.add to add.blueprint : 


                                  rbind(add.blueprint,data.frame(newvar=vars.to.add,var=vars.to.add,file=rep_len(NA,length(vars.to.add)),link=rep_len(NA,length(vars.to.add)),fun=rep_len(NA,length(vars.to.add)))) -> add.blueprint

#### merge the data
                                  add.blueprint %>% load.and.recode(fun=fun,wave=wave,extended=extended)  -> code.to.execute
                                  paste0(code.to.execute,' -> data.add') -> code.to.execute
                                  blueprint.code.log(code.to.execute)
                                  paste0('\n\ndplyr::left_join(main.data,data.add,by=c(',link.condition[1],'))  -> main.data\n\nrm(data.add)\n\n') -> code.to.execute
                                        # codelog
                                  blueprint.code.log(code.to.execute)
                                        #🔴 eval(parse(text=code.to.execute))
                                  }
                                      ## codestoexecute <- which(blueprint$file==x&(grepl('`',blueprint$vars,perl=TRUE)))
                                      ## for(a.row in codestoexecute)
                                      ##     {
                                      ##         print(paste('data','$',vars[a.row,1],' <- ', gsub('`','',blueprint$vars[a.row]),sep=''))
                                      ##         eval(parse(text=paste('data','$',vars[a.row,1],' <- ', gsub('`','',blueprint$vars[a.row]),sep='')))
                                      ##     }
                                        #print(head(data))
                                      

                                        # what is done here?
                                        #   print(vars[,1])
                                        #nv <- length(vars[,1])
                                        #names(data)[c(1:(nv))] <- vars[,1]
                                        #names(data)[length(names(data))] <- 'year'
                          ## Reocde data over here -----------------------------------------------------------

                                        #                                                    vars.to.get
                          ## Execute the code from the code -----------------------------------------------------------
#                                  cat('####################################################################################################\n')
         
                          }
        paste0('data.wave',wave) -> df.wave.name
        return.code.to.return.df.with.certain.vars_(all.vars,missing.var.pos) -> select.code
                                        #        paste0('main.data  %>% ',select.code,' %>% mutate(wave=',wave,')  -> ',df.wave.name) -> code.to.execute
                paste0('main.data  %>%  mutate(wave=',wave,')  -> ',df.wave.name) -> code.to.execute
        # eval(parse(text=code.to.execute))
        blueprint.code.log(code.to.execute)
        if(wave==1)
        {
            paste0(df.wave.name,' -> final.df') -> code.to.execute
            }
            else
            {
                c('final.df',df.wave.name) %>% make.bind.code(data.table=TRUE)  -> code.to.execute
            }
        code.to.execute %>% paste0(.,'\nrm(',df.wave.name,')') -> code.to.execute
        blueprint.code.log(code.to.execute)
        return(df.wave.name)
    }) -> blueprints.data

                                        # if(debug){print(blueprints.data)}
    blueprints.data$dfs  %>% unlist  -> dfs
    
                                        # paste0('rbind(',paste0(dfs,collapse=',          \n'),')   %>% tbl_df  -> final.df') -> code.to.execute

    
#    blueprint.code.log(code.to.execute)
    blueprint.code.log('final.df %>% tbl_df -> final.df')
                                        #    dfs%>% do.call(rbind,.) %>% tbl_df     -> final.df
#    cat(paste0('\nTime taken to produce code.file: ',format(round(Sys.time()- code.time,2),unit='sec'),'\n\n'))
    cat(paste0('\n- Starting import and merge processes...\n'))
    eval.time <- Sys.time()
    source(codefile)
    blueprint.log('')        
    blueprint.log(Sys.time())
    blueprint.log('')    
    blueprint.log(paste('Finally ready. Merged data.frame has',dim(final.df)[1],'rows and',dim(final.df)[2],'columns.'))
                                        #    cat(paste0('\nTime elapsed for merging: ',format(Sys.time()- eval.time,unit='sec'),'\n\n\n'))
    cat(paste0('--- Ready (after ',format(round(Sys.time()- code.time,1),unit='sec'),').\n'))    
    if(is.character(export.file)){
        cat(paste0('\nWriting file to file: ',export.file,'\n'))
        rio::export(final.df,file=export.file)
        blueprint.log(paste0('--- Written data.frame to file:',export.file,'.'))
        return(invisible(final.df))
    }
    else{
        return(final.df)
        }
    }


open.blue <- function(
                          blueprint=paste0(getwd(),'/blueprint.xlsx'),
                          waves=1,
                          debug=FALSE
                          )
{
    blueprint %>% normalised.path.and.dir.exists  -> blueprint
    if(!file.exists(blueprint)){
        c('var','file','link','fun') -> varnams
    c('newvar',paste0(rep(varnams,times=waves),sort(rep((1:waves),times=4))))-> varnams
        paste0(varnams,'=c("","")',collapse=',')-> varnams
        c('# The new name of the variable (that merges the waves).','The original variable in the data.set','The original file that does contain this variable','A function or pipe of functions that is executed on the orignal variable var1 (e.g. for recoding)','Specifications of the variables that link the data') -> description
        eval(parse(text=paste0('data.frame(',varnams,',stringsAsFactors=FALSE)'))) -> a.df
        a.df[1,1] <- '#'
        a.df[1,c(2+4*((1:waves)-1))]<-paste0(c('wave'),1:waves)
        a.df[1,c(5+4*((1:waves)-1))]<-'^\n
|\n
v\n'
        a.df[2,1:5] <- description
        if(debug){return(a.df)}
    a.df %>% rio::export(file=blueprint)
    cat(paste0('Written blueprint template to file ',blueprint,'.\n'))
    }
    browseURL(paste0('file://', blueprint))
    invisible(blueprint)
    }


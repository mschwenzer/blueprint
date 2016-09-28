
blueprint.variable.diff <- function(variable,funs,name='',wave='')
{
    blueprint.log('')
    blueprint.log('')
blueprint.log        (paste0('----Transformation. Variable `',name,'`  (wave ',wave,'): ',funs,'  -----------------------------\n'))
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
    data.frame(old=kept.levels.of.variable,`. `=rep('|',length(kept.levels.of.variable)),`.  `=rep('v',length(kept.levels.of.variable)),new=variable[old.pos],`(n)`=old.count) %>% arrange(old)  ->     printfr
    
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
  while(length(
((blueprint[,1]) %>% str_detect('\\[[0-9]*:[0-9]*\\]') %>% which)
        >0))
          {
              rowid <- ((blueprint[,1]) %>% str_detect('\\[[0-9]*:[0-9]*\\]') %>% which)
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
    text=(pattern %>% str_replace_all('\\[','') %>% str_replace_all('\\]',''))
                   )
    )
              pattern %>% str_replace_all('\\[','\\\\[') %>% str_replace_all('\\]','\\\\]')  -> pattern
          frame.toadd <- ldply(nums,
                               function(x){
                                   to.process.vars %>% str_replace_all(pattern,x)
                })
               names(frame.toadd) <- names(frame.before)
             assign('blueprint',rbind(if((rowid-1)>0){frame.before},frame.toadd,              if((rowid+1)<(nrow(blueprint)+1)){frame.after}),-1)
          }
    return(blueprint)
    }
###
## return.not.existing.files -----------------------------------------------------------
return.not.existing.files <- function(blueprint)
      {
          blueprint$file %>% unlist %>% c %>% unique %>% na.omit  -> files.to.check
          llply(files.to.check,file.exists) %>% unlist %>% `!` %>% files.to.check[.]
      }
###
blueprint.remove.column.rows <- function(blueprint)
    {
        ## Remove comment rows -----------------------------------------------------------
        str_detect(blueprint[,1],'^#.*') %>% which  -> commentrows
      if (length(commentrows)>0){    blueprint[-commentrows,]  -> blueprint   }                                    
                                                     }
## blueprint.log -----------------------------------------------------------
blueprint.log <- function(message){

    loginfo(message, logger="blueprint.logger")
}
## return.df.with.certain.vars -----------------------------------------------------------
return.df.with.certain.vars <- function(df,vars.to.get)
{
    # Return a data.frame containing certain variables also ordered by vars.to.get
                          vars.to.get%in%names(df) %>% `!`  -> posis
                          vars.to.get -> vars.to.get.new
                          vars.to.get.new[posis]  %>% str_replace('$','=rep_len(NA_real_,nrow(df))') -> vars.to.get.new[posis]
                          vars.to.get.new %>% paste0(collapse=',') %>%                               paste0('df %>% transmute(',.,')')                      -> code.to.execute
                          eval(parse(text=code.to.execute))
}
## df.set.standard.names -----------------------------------------------------------
df.set.standard.names  <- function(df)
{
    names(df)  -> df.names
    df.names %>% str_detect('var') %>% which -> df.var.columns
    'var'  -> df.names[df.var.columns]
    'newvar'  -> df.names[1]            
    df.names %>% str_detect('file') %>% which -> df.file.columns
    'file'  -> df.names[df.file.columns]
    df.names %>% str_detect('link') %>% which -> df.link.columns
    'link'  -> df.names[df.link.columns]    
    df.names %>% str_detect('fun') %>% which -> df.fun.columns
    'fun'  -> df.names[df.fun.columns]
    df.names %>% str_detect('newvaw|var|fun|file|link|""') %>% `!` -> df.var.columns
    'var'  -> df.names[df.var.columns]
    df.names -> names(df)
    return(df)
    }
## constant -----------------------------------------------------------
constant <- function(x,y){y}
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


## blueprint.validator -----------------------------------------------------------
blueprint.validator <- function(blueprint)
{
    blueprint %>% 
        blueprint.check.for.missing.files %>%
        blueprint.check.for.duplicate.variable.names 
    }
## blueprint -----------------------------------------------------------
blueprint <- function(
                      blueprint='/Users/eur/Documents/140_Datenaufbereitung/pisa.xlsx',
                      out_file=NULL,
                      waves=NULL,
                      debug=FALSE,
                      logfile=FALSE,                      
                      fun=TRUE,
                      logging=TRUE,
                      ...
    ){
   # requirements  
    require(plyr)

    require(gdata)
    require(stringr)  
    require(rio)
    require(logging)    
                                        # Load Merge Data from XLS

    if(debug){cat(paste0('file: ',blueprint))}
    if(!logfile)
        {
            str_replace(blueprint,'\\.....+$','.log.txt') -> logfile
            }
    if(logfile==blueprint)
    {stop('You have to specify a logfile since automatic replacement of the suffix was not able. Try to set a logfile argumet or change it.')}
    blueprint.log.formatter <- function(record) {
        text <- paste(paste0(' ',record$msg, sep=' '))
        }
    addHandler(writeToFile, logger="blueprint.logger", file=logfile,formatter=blueprint.log.formatter)
    if(debug){print('logger created')}
    cat(paste0('Parsing file: ',blueprint,'.',if(logging){paste0('\nlogging to file: ',logfile)},'  \nStarting merge processes...'))
    import(file=blueprint,...) %>% blueprint.remove.column.rows %>% 
        blueprint.set.standard.names  -> blueprint
    if(debug){print(blueprint)}
                                        # cut blueprint into waves


    data.frame(startcol=(names(blueprint)=='var') %>% which,
               # end column 
               endcol=c((names(blueprint) =='var')  %>% which %>% .[-1] %>% `-`(1),
                        # + the last column containing everything
                        length(names(blueprint)))) %>% transmute(wave=1:nrow(.),startcol,endcol)    %>%
        filter(wave%in%c(waves)) %>%
        group_by(wave)   %>% 
        do(blueprints={blueprint[,c(1,(.$startcol):(.$endcol))]  %>%
                                        # normalise to a data.frame with these variables
                      return.df.with.certain.vars(c('newvar','var','file','link','fun')) %>%
                      add.variables.specified.by.brackets %>%
                      set.empty.values.to.NA  %>%
                      blueprint.validator})             -> blueprints
    if(debug){print(bluepring)}
    rm(blueprint)
## Validate all blueprints before something is done actually....     -----------------------------------------------------------q
                                        # validator for this kind of data.frame
    if(debug){print(blueprints) }
                                        # Actually get data to blueprints
    blueprints %>% group_by(wave) %>% do(dfs={
        blueprint <- .$blueprints[[1]]
        wave <- .$wave[[1]]
        all.vars <-.$blueprints[[1]]$newvar
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
                          blueprint$file %>% str_detect(blueprint.main.file)                          -> pos.main.file
                          (pos.main.file & (pos.main.file %>% is.na %>% `!`))                           -> pos.main.file
#df <- blueprint[pos.main.file,]
if(debug)        {print(blueprint$wave                 )}
                          blueprint[pos.main.file,] %>%
                              load.and.recode(fun=fun,wave=wave,logging=logging) -> main.data
cat('main.file...')         
                                        # loaded and processed. remaining parts still to be processed
                          blueprint <- blueprint[ !blueprint[,'file'] == blueprint.main.file & !is.na(blueprint$file),]

### restrict to variables that are specified by a file reference ❗️ should be: also a link

### Add the additional files  ####################################################################################################
         if(debug){       print(all.vars)}
                          if(length(blueprint$file)>0){
                              for(x in unique(blueprint$file) )
                              {
                                  blueprint.log(paste0('--- Adding additional variables from file:',x,'\n'))
cat('add.file...')

                                        #x <- unique(blueprint$files)[1]
                                        #print(blueprint)
                                        # variabels to replace in original frame

                                      add.blueprint <- blueprint[blueprint$file==x,]
                                      
#                                      new.vars <- ,'newvars']
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
      
                                      from.links <- strsplit(links,';')[[1]][1]
                                      ifelse (length(strsplit(links,';')[[1]])>1,            to.links <- strsplit(links,';')[[1]][2],to.links <- from.links)
                                      from.links <-  strsplit(from.links,',')[[1]]
                                      to.links <-  strsplit(to.links,',')[[1]]

                                  paste0('"',from.links,'"="',to.links,'"',collapse=',') -> link.condition
         #                         link.condition
         #                         add.blueprint
#### merge the data
                                  rbind(add.blueprint                           ,data.frame(newvar=to.links,var=to.links,file=rep_len(NA,length(to.links)),link=rep_len(NA,length(to.links)),fun=rep_len(NA,length(to.links)))) -> add.blueprint


                                  add.blueprint %>% load.and.recode(fun=fun,wave=wave,logging=logging)  -> data.add

#                                  paste0('left_join(current.data,data.add,by=c(',link.condition,')) %>% select(',paste0('-',to.links,collapse=','),')') %>% parse(text=.) %>% eval  -> current.data
                                  paste0('left_join(main.data,data.add,by=c(',link.condition,'))') -> code.to.execute
                                  eval(parse(text=code.to.execute))  -> main.data
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
         
                              }}
         main.data %>% mutate(wave)  %>% return.df.with.certain.vars(c('wave',all.vars))  -> main.data
        return(main.data)
    }) -> blueprints.data
            if(debug){print(blueprints.data)}
                                        #    blueprints.data    %>% do.call(rbind,.$dfs) -> final.df
    blueprints.data$dfs  -> dfs
    dfs%>% do.call(rbind,.) %>% as_data_frame     -> final.df
    blueprint.log(Sys.time())
    blueprint.log('')    
    blueprint.log(paste('Finally ready. Merged data.frame has',dim(final.df)[1],'rows and',dim(final.df)[2],'columns.'))

                                        #    b  cat('Building data.frame for variables (newnames):\n')
#
                               # do for every wave (additional colums)
    
## ldply(
##                        # first wave vars are in second column , every addtional wave is 4 columns right of this column
##                       2+(0:(waves-1))*4,
##                       function(wavecolumn){
##                                         # isolate a data.frame, that contains the variable identifier (col1) and the columns of the current wave

    ##                       }  %>% as_data_frame   -> data}})
    
    if(is.character(out_file)){
        cat(paste0('exporting to file: ',out_file,'\n'))
        export(final.df,file=out_file)
        blueprint.log(paste0('Written data.frame to file:',out_file,'.'))
    }
    return(final.df )}
?export
blueprint(which='MergeESS',waves=1:3,loggin=FALSE)  -> tes
blueprint(which='MergePisa',waves=1:5,logging=1)
library(microbenchmark)
microbenchmark(blueprint(which='MergePisa',waves=1:5,logging=0))

tmp <- tempfile()
Rprof(tmp, interval = 0.1)

Rprof(NULL)
summaryRprof(tmp)

open.blue <- function(
                          file=paste0(getwd(),'/blueprint.xlsx'),
                          waves=2,
                          type=NULL
                          )
{
    if(!file.exists(file)){
        c('var','file','fun','link') -> varnams
    c('newvar',paste0(rep(varnams,times=waves),sort(rep((1:waves),times=4))))-> varnams
        paste0(varnams,'=c("","")',collapse=',')-> varnams
        c('# The new name of the variable (that merges the waves).','The original variable in the data.set','The original file that does contain this variable','A function or pipe of functions that is executed on the orignal variable var1 (e.g. for recoding)','Specifications of the variables that link the data') -> description
        eval(parse(text=paste0('data.frame(',varnams,')'))) -> a.df
        a.df[1,1] <- '#'
        a.df[1,c(2+4*((1:waves)-1))]<-paste0(c('wave'),1:waves)
        a.df[1,c(5+4*((1:waves)-1))]<-'^\n
|\n
v\n'
    a.df[3,1:5] <- description
    a.df %>% export(file=file)
    cat(paste0('Written blueprint template to file ',file,'.\n'))
    }
    openFILE <- function(x) browseURL(paste0('file://', file.path(getwd(), x)))
    openFILE(file)
    invisible(file)
    }


import('my.blueprint.name.csv')
%>%slice(1)%>%export('my.blueprint.name.csv')
data.frame(a=Sys.time()) %>% export(file='/dsk/test.R')

blueprint(which='MergePisa',waves=1:5,logging=0,out_file='/dsk/test.RData')  -> tes
;
Sys.time()
blueprint(which='MergePisa',waves=1:5,logging=1)  -> tes
Sys.time()

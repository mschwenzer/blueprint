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
if(debug){          print(pattern)}
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
                      debug=0,
                      filter=TRUE,
                      ...
    ){
   # requirements  
    require(plyr)
    require(gdata)
    require(stringr)  
    require(rio)
                                        # Load Merge Data from XLS
    cat('Parsing file: ',blueprint,'\n')
    import(file=blueprint,...) %>% blueprint.remove.column.rows %>% 
        blueprint.set.standard.names  -> blueprint
    # cut blueprint into waves
    data.frame(startcol=(names(blueprint)=='var') %>% which,
               # end column 
               endcol=c((names(blueprint) =='var')  %>% which %>% .[-1] %>% `-`(1),
                        # + the last column containing everything
                        length(names(blueprint)))) %>% transmute(wave=1:nrow(.),startcol,endcol)    %>%
        filter(wave==waves) %>% 
        group_by(wave)   %>% 
        do(blueprints={blueprint[,c(1,(.$startcol):(.$endcol))]  %>%
                                        # normalise to a data.frame with these variables
                      return.df.with.certain.vars(c('newvar','var','file','link','fun')) %>%
                      add.variables.specified.by.brackets %>%
                      set.empty.values.to.NA  %>%
                      blueprint.validator})             -> blueprints
    rm(blueprint)
## Validate all blueprints before something is done actually....     -----------------------------------------------------------q
                                        # validator for this kind of data.frame
    print(blueprints) 
                                        # Actually get data to blueprints
    blueprints %>% group_by(wave) %>% do(dfs={
        blueprint <- .$blueprints[[1]]
        wave <- .$wave[[1]]
        all.vars <-.$blueprints[[1]]$newvar
         ###
                          cat(paste('Processing wave:',wave,'\n'))
                                        # get different unique filenames to process for each wave
                          blueprint.files <- unique(blueprint$file)
                                        # main file has to be the first specified file
                          blueprint.main.file <- blueprint.files[1]
                                        # vars in main file that shall be kept
                          vars.to.get <- blueprint$var
        
                                        # print(vars.to.get)
### Load main file ####################################################################################################
                          cat('loading main file:',blueprint.main.file,'\n')
                          blueprint$file %>% str_detect(blueprint.main.file)                          -> pos.main.file
                          (pos.main.file & (pos.main.file %>% is.na %>% `!`))                           -> pos.main.file
#df <- blueprint[pos.main.file,]
        print(blueprint$wave                 )
                          blueprint[pos.main.file,] %>%
                              load.and.recode(filter=filter) -> main.data
         
                                        # loaded and processed. remaining parts still to be processed
                          blueprint <- blueprint[ !blueprint[,'file'] == blueprint.main.file & !is.na(blueprint$file),]

### restrict to variables that are specified by a file reference ❗️ should be: also a link

### Add the additional files  ####################################################################################################
                print(all.vars)
                          if(length(blueprint$file)>0){
                              for(x in unique(blueprint$file) )
                              {
                                  cat('adding variables from file:',x,'\n')
                                  

                                        #x <- unique(blueprint$files)[1]
                                        #print(blueprint)
                                        # variabels to replace in original frame

                                      add.blueprint <- blueprint[blueprint$files==x,]
                                      
#                                      new.vars <- ,'newvars']
                                        # variables that replace in replacement frame
                                      the.vars <- add.blueprint[,'var']
                                        #print(the.vars)##
                                      links <-add.blueprint[,'link']
                                        #print(blueprint)
                                      functions <-add.blueprint[,'fun']
                                        #print(functions)
                                        #print(links)##
                                      if(sum(is.na(links)&!is.na(the.vars))>0)
                                          {
                                              stop(paste('You have to specify links for the variables\n',paste(the.vars,collapse=',')))
                                          }
      
                                      from.links <- strsplit(links,';')[[1]][1]
                                      ifelse (length(strsplit(links,';')[[1]])>1,            to.links <- strsplit(links,';')[[1]][2],to.links <- from.links)
                                      from.links <-  strsplit(from.links,',')[[1]]
                                        # DEBUG> print(from.links)
                                      to.links <-  strsplit(to.links,',')[[1]]

                                        #print(to.links)
                                        #print(the.vars)
                                        #print(head(data))
                                        #         print(head(replace.vars.from.file(data,rep.file=x,
                                        #                                           orig.vars=the.vars,
                                        #                                           rep.vars=the.vars,
                                        #                                           orig.id=from.links,rep.id=to.links)))
                                        # DEBUG  
                                        #print('here')
                                        #
                                      
#### merge the data
                                  paste0('"',from.links,'"="',to.links,'"',collapse=',') -> link.condition
                                  link.condition
                                  add.blueprint

                                  rbind(add.blueprint                           ,data.frame(newvar=to.links,var=to.links,file=rep_len(NA,length(to.links)),link=rep_len(NA,length(to.links)),fun=rep_len(NA,length(to.links)))) -> add.blueprint


                                  add.blueprint %>% load.and.recode(filter)  -> data.add

#                                  paste0('left_join(current.data,data.add,by=c(',link.condition,')) %>% select(',paste0('-',to.links,collapse=','),')') %>% parse(text=.) %>% eval  -> current.data
                                  paste0('left_join(main,data.add,by=c(',link.condition,'))') -> code.to.execute
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
                                  cat('####################################################################################################\n')
         
                              }}
         main.data %>% mutate(wave)  %>% return.df.with.certain.vars(c('wave',all.vars))  -> main.data
        return(main.data)
    }) -> blueprints.data
            if(debug){print(blueprints.data)}
                                        #    blueprints.data    %>% do.call(rbind,.$dfs) -> final.df
    blueprints.data$dfs  -> dfs
    dfs%>% do.call(bind_rows,.) %>% as_data_frame     -> final.df

                                        #    b  cat('Building data.frame for variables (newnames):\n')
#
                               # do for every wave (additional colums)
    
## ldply(
##                        # first wave vars are in second column , every addtional wave is 4 columns right of this column
##                       2+(0:(waves-1))*4,
##                       function(wavecolumn){
##                                         # isolate a data.frame, that contains the variable identifier (col1) and the columns of the current wave

    ##                       }  %>% as_data_frame   -> data}})
    
    if(is.character(out_file)){export(final.df,file=out_file)}
    return(final.df )}

blueprint(which='MergeESS',waves=1:2)  -> tes



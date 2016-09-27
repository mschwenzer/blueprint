blueprint.remove.column.rows <- function(df)
    {
        ## Remove comment rows -----------------------------------------------------------
        str_detect(df[,1],'^#.*') %>% which  -> commentrows
      if (length(commentrows)>0){    blueprint[-commentrows,]  -> blueprint   }                                    
                                                     }


blueprint.log <- function(message){
    
    }

return.df.with.certain.vars <- function(df,vars.to.get)
{
    # Return a data.frame containing certain variables also ordered by vars.to.get
                          vars.to.get%in%names(df) %>% `!`  -> posis
                          vars.to.get -> vars.to.get.new
                          vars.to.get.new[posis]  %>% str_replace('$','=rep_len(NA_real_,nrow(df))') -> vars.to.get.new[posis]
                          vars.to.get.new %>% paste0(collapse=',') %>%                               paste0('df %>% transmute(',.,')')                      -> code.to.execute
                          eval(parse(text=code.to.execute))
}
    

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


constant <- function(x,y){y}

blueprint.validator <- function(blueprint)
{
return(    blueprint)
    }




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
        group_by(wave)   %>% 
        do(waves={blueprint[,c(1,(.$startcol):(.$endcol))]  %>%
                                        # normalise to a data.frame with these variables
                      return.df.with.certain.vars(c('newvar','var','file','link','fun')) %>%
                                                     # validator for this kind of data.frame
                                    blueprint.validator
            #%>%
         
#        
        }) -> blueprints
        if(debug){print(blueprints)}
## Validate all blueprints before something is done actually....     -----------------------------------------------------------
#    blueprint  %>% group_by(wave) %>% do(
    
    
            ## ifelse(is.null(blueprint.wave$file)){)}
            ##    if(is.null(blueprint.wave$link)){blueprint.wave$link <- NA}
            ## if(is.null(blueprint.wave$fun)){blueprint.wave$fun <- NA}                
           ## blueprint.wave %>% transmute(newvar,var,file,link,fun)
           
##:ess-bp-start::browser@nil:##
browser(expr=is.null(.ESSBP.[["@16@"]]));##:ess-bp-end:##
    
    
    

    ncol(blueprint)  -> blueprint.ncols

 #  names(blueprint)
  #was :
                                        #blueprint <- read.csv(a.file,sep=';')
                                        # check files


    
    #%>% `!` %>% which -> blueprint.func.columns    

    
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
  return.not.existing.files <- function(df)
      {
          blueprint %>% names %>% str_detect('file') -> filevarpos
          blueprint[,filevarpos] %>% unlist %>% c %>% unique  -> files.to.check
          llply(files.to.check,file.exists) %>% unlist %>% `!` %>% files.to.check[.]
      }
    blueprint %>% return.not.existing.files  -> not.existing.files
    # remove empty fields
        (not.existing.files=='') %>% which -> apos
    not.existing.files[-apos]  -> not.existing.files
  if(length(not.existing.files)>0){
          paste0('The following files do not exist:\n',paste0(not.existing.files,sep='\n')) -> warn.mess
          stop(warn.mess)
      }

  cat('Building data.frame for variables (newnames):\n')
blueprint[,1]  -> all.vars

#rowstoprocess
  #row names of all variables
    varsnam <- names(blueprint)
  ## check for duplicate variable names and stop -----------------------------------------------------------

  if(sum(duplicated(blueprint[,1]))>0){
      stop(paste('Duplicate variablenames:',blueprint[which(duplicated(blueprint[,1]))[1],1]),' rows:',paste0(which(duplicated(blueprint[,1])),collapse=','))}
                                          #  print(varsnam)
  
  
  wave <- 1
  #add additional years
  
                                        #yea
names(blueprint)[  seq(2,by=4,length=waves)] %>%
    str_replace_all('X','') %>%
    as.numeric -> years
  #print(years)
  

  blueprint <- do.call(data.frame,llply(blueprint,function(x){
        #Just add missing to empty strings / make empty cells NA
    is.na(x) <- x %in% c('')
    return(x)}))
    names(blueprint) <- varsnam
  #print(vars)
  #print(vars)
  #n waves
#  print(waves)
#vars  
                                        # do for every wave (additional colums)
    
ldply(
                       # first wave vars are in second column , every addtional wave is 4 columns right of this column
                      2+(0:(waves-1))*4,
                      function(wavecolumn){
                                        # isolate a data.frame, that contains the variable identifier (col1) and the columns of the current wave
                          current.wave <- blueprint[,c(1,wavecolumn:(wavecolumn+3))]
                          names(current.wave) <- c('newvars','vars','files','links','rec')
###
                          cat(paste('Processing wave:',wave,'\n'))
                                        # get different unique filenames to process for each wave
                          current.wave.files <- unique(current.wave$files)
                                        # main file has to be the first specified file
                          current.wave.main.file <- current.wave.files[1]
                                        # vars in main file that shall be kept
                          vars.to.get <- current.wave$vars
                                        # print(vars.to.get)
### Load main file ####################################################################################################
                          cat('loading main file:',current.wave.main.file,'\n')
                          current.wave$files %>% str_detect(current.wave.main.file)                          -> pos.main.file
                          (pos.main.file & (pos.main.file %>% is.na %>% `!`))                           -> pos.main.file
#df <- current.wave[pos.main.file,]

                          current.wave[pos.main.file,] %>% load.and.recode(filter) -> current.data

                          current.wave <- current.wave[ !current.wave[,'files'] == current.wave.main.file & current.wave[,'files']!='',]
### restrict to variables that are specified by a file reference ❗️ should be: also a link
                          current.wave <- current.wave[!is.na(current.wave$files),]

### Add the additional files  ####################################################################################################
                          if(length(current.wave$files)>0){
                              for(x in unique(current.wave$files) )
                              {
                                  cat('adding variables from file:',x,'\n')

                                        #x <- unique(current.wave$files)[1]
                                        #print(current.wave)
                                        # variabels to replace in original frame

                                      add.current.wave <- current.wave[current.wave$files==x,]
                                      
#                                      new.vars <- ,'newvars']
                                        # variables that replace in replacement frame
                                      the.vars <- add.current.wave[,'vars']
                                        #print(the.vars)##
                                      links <-add.current.wave[,'links']
                                        #print(current.wave)
                                      recs <-add.current.wave[,'rec']
                                        #print(recs)
                                        #print(links)##
                                      if(sum(is.na(links)&!is.na(the.vars))>0)
                                          {
                                              stop(paste('You hav;;e to specify links for the variables\n',paste(the.vars,collapse=',')))
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
                                  add.current.wave

                                  rbind(add.current.wave                           ,data.frame(newvars=to.links,vars=to.links,files=rep_len(NA,length(to.links)),links=rep_len(NA,length(to.links)),rec=rep_len(NA,length(to.links)))) -> add.current.wave


                                  add.current.wave %>% load.and.recode(filter)  -> data.add

#                                  paste0('left_join(current.data,data.add,by=c(',link.condition,')) %>% select(',paste0('-',to.links,collapse=','),')') %>% parse(text=.) %>% eval  -> current.data
                                  paste0('left_join(current.data,data.add,by=c(',link.condition,'))') -> code.to.execute
                                  eval(parse(text=code.to.execute))  -> current.data
                                      ## codestoexecute <- which(current.wave$file==x&(grepl('`',current.wave$vars,perl=TRUE)))
                                      ## for(a.row in codestoexecute)
                                      ##     {
                                      ##         print(paste('data','$',vars[a.row,1],' <- ', gsub('`','',current.wave$vars[a.row]),sep=''))
                                      ##         eval(parse(text=paste('data','$',vars[a.row,1],' <- ', gsub('`','',current.wave$vars[a.row]),sep='')))
                                      ##     }
                                        #print(head(data))
                                      
                                  }}
                                        # what is done here?
                                        #   print(vars[,1])
                                        #nv <- length(vars[,1])
                                        #names(data)[c(1:(nv))] <- vars[,1]
                                        #names(data)[length(names(data))] <- 'year'
                          ## Reocde data over here -----------------------------------------------------------

                                        #                                                    vars.to.get
                          ## Execute the code from the code -----------------------------------------------------------

                          ## Add wave specifier -----------------------------------------------------------

                          cat('####################################################################################################\n')
                          current.data %>% mutate(wave)  %>% return.df.with.certain.vars(c('wave',all.vars))  -> current.data
                          wave <<- wave+1
                          return(current.data)
                      })  %>% as_data_frame   -> data
    if(is.character(out_file)){export(data,file=out_file)}
    return(data)
}
blueprint(which='MergeESS')

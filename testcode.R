library(tibble)
## Tests -----------------------------------------------------------
## ✅ normalised.path.and.dir.exists -----------------------------------------------------------
expect_error(normalised.path.and.dir.exists('/dska/bla'))
expect_error(normalised.path.and.dir.exists('/dsk'))
expect_error(normalised.path.and.dir.exists('/dsk/'))
expect_error(normalised.path.and.dir.exists('/dsk/../../'))

## attrs.as.factor -----------------------------------------------------------


## load.and.recode -----------------------------------------------------------

## process.links -----------------------------------------------------------
expect_equal("'asd'='asdf'"  %>% process.links,"\"asd\"=\"asdf\"")
expect_equal('"asd"="asdf"'  %>% process.links,"\"asd\"=\"asdf\"")
# Time enhancement when the checks replacement are part of the blueprint validator

## 🔵 blueprint.variable.diff -----------------------------------------------------------

## set.empty.values.to.NA -----------------------------------------------------------
expect_equal(data.frame(a=c('a','b','',""))  %>% set.empty.values.to.NA, data.frame(a=c('a','b',NA,NA)))
expect_equal(data.frame(a=c(1,2,'',""))  %>% set.empty.values.to.NA, data.frame(a=c('1','2',NA,NA)))


## add.variables.specified.by.brackets -----------------------------------------------------------
data.frame(newvar=c('tos','bla[1:10]','heinz'),var=c('ras','aha','bha'),fun=c(1,2,2),file=c('','',''),link=c('','',''))%>% add.variables.specified.by.brackets
data.frame(newvar=c('bla[1:10]','heinz'),var=c('aha','bha'),fun=c(1,2),file=c('',''),link=c('',''))%>% add.variables.specified.by.brackets
# >>>>>>>>>>
data.frame(newvar=c('bla[1:10]'),var=c('aha'),fun=c(1),file=c(''),link=c('','')) %>%     add.variables.specified.by.brackets
# ❗️ to fix: more than one bracket variables leads to error



## ✅ return.not.existing.files -----------------------------------------------------------
expect_equal(return.not.existing.files(data.frame(file=c('/dsk/','heinz.pdf','/dsk','~/.Rprofile'))),c('/dsk/','heinz.pdf','/dsk'))

## blueprint.remove.column.rows -----------------------------------------------------------
data.frame(newvar='bla',var='heinz',file='ahaha.xlsx',link='a=b',fun=' %>%  %>% #') -> blueprint

tibble::tribble(
            ~newvar, ~var, ~file, ~link, ~fun,
            'heinz', 'hoho', '/dsk/bla', 'a=4', 'asfjkljf %>% ',
            '#', 'asd#a', '//#','#okay', '#',
            ' #', 'asd#a', '//#','#okay', '#',
            's#', 'asd#a', '//#','#okay', '#'                        
        ) -> blueprint.a
blueprint.a%>%blueprint.remove.column.rows


## blueprint.log -----------------------------------------------------------

## return.df.with.certain.vars -----------------------------------------------------------

tibble::tribble(
            ~newvar, ~var, ~file, ~link, ~fun,
            'heinz', 'hoho', '/dsk/bla', 'a=4', 'asfjkljf %>% ',
            '#', 'asd#a', '//#','#okay', '#',
            ' #', 'ajsd#a', '//#','#okay', '#',
            's#', 'asd#a', '//#','#okay', '#'                        
        ) -> blueprint.a

blueprint.a%>%return.df.with.certain.vars


## df.set.standard.names -----------------------------------------------------------

## blueprint.check.for.missing.files -----------------------------------------------------------

## blueprint.check.for.duplicate.variable.names -----------------------------------------------------------

## blueprint.validator -----------------------------------------------------------




## blue -----------------------------------------------------------

## open.blue -----------------------------------------------------------


    

## #################### Benchmarking and Optimisation -----------------------------------------------------------
blueprint(which='MergeESS',waves=1:3,loggin=FALSE)  -> tes
blueprint(which='MergePisa',waves=1,extended=1)
blue(blueprint='/doc/140_Datenaufbereitung/pisa.xlsx',waves=1:5,extended=0,which='MergePisa') -> pisa
library(microbenchmark)
microbenchmark(blue(which='MergePisa',waves=1,extended=0),times=2)  -> dest
dest %>% str


### ✳️  blueprint.variable.diff

tmp <- tempfile()
Rprof(tmp, interval = 0.1)
blue(which='MergePisa',waves=1,extended=0)
Rprof(NULL)
summaryRprof(tmp)


library(blueprint)
import('my.blueprint.name.csv')
%>%slice(1)%>%export('my.blueprint.name.csv')
data.frame(a=Sys.time()) %>% export(file='/dsk/test.R')

blueprint(which='MergePisa',waves=1:5,logging=0,out_file='/dsk/test.RData')  -> tes

Sys.time()
blueprint(which='MergePisa',waves=1:5,logging=1)  -> tes
Sys.time()

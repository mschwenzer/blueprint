## Tests -----------------------------------------------------------

## attrs.as.factor -----------------------------------------------------------


context("load.and.recode")
structure(list(newvar = c("asa", "school"), var = c("COUNTRY",  "SCHOOLID"), file = c("/doc/wissenschaft/data/pisa/pisa2000/data r/intstud_read_v3.Rdata",  "/doc/wissenschaft/data/pisa/pisa2000/data r/intstud_read_v3.Rdata" ), link = c("", ""), fun = c("", "")), .Names = c("newvar", "var",  "file", "link", "fun"), class = c("tbl_df", "tbl", "data.frame" ), row.names = c(NA, -2L)) -> blueprint.with.file1

blueprint.with.file1 %>% load.and.recode %>% tbl_df   %>% 


context('process.links')
expect_equal("'asd'='asdf'"  %>% process.links,"\"asd\"=\"asdf\"")
expect_equal('"asd"="asdf"'  %>% process.links,"\"asd\"=\"asdf\"")
# Time enhancement when the checks replacement are part of the blueprint validator



context('set.empty.values.to.NA')
expect_equal(data.frame(a=c('a','b','',""))  %>% set.empty.values.to.NA, data.frame(a=c('a','b',NA,NA)))
expect_equal(data.frame(a=c(1,2,'',""))  %>% set.empty.values.to.NA, data.frame(a=c('1','2',NA,NA)))


## add.variables.specified.by.brackets -----------------------------------------------------------
context('add.variables.specified.by.brackets')
data.frame(newvar=c('tos','bla[1:10]','heinz'),var=c('ras','aha','bha'),fun=c(1,2,2),file=c('','',''),link=c('','',''))%>% add.variables.specified.by.brackets
data.frame(newvar=c('bla[1:10]','heinz'),var=c('aha','bha'),fun=c(1,2),file=c('',''),link=c('',''))%>% add.variables.specified.by.brackets
# >>>>>>>>>>
data.frame(newvar=c('bla[1:10]'),var=c('aha'),fun=c(1),file=c(''),link=c('','')) %>%     add.variables.specified.by.brackets
# ❗️ to fix: more than one bracket variables leads to error



## ✅ return.not.existing.files -----------------------------------------------------------
context('return.not.existing.files')
expect_equal(return.not.existing.files(data.frame(file=c('/dsk/','heinz.pdf','/dsk','~/.Rprofile'))),c('/dsk/','heinz.pdf','/dsk'))

## blueprint.remove.column.rows -----------------------------------------------------------


tibble::tribble(
            ~newvar, ~var, ~file, ~link, ~fun,
            'heinz', 'hoho', '/dsk/bla', 'a=4', 'asfjkljf %>% ',
            '#', 'asd#a', '//#','#okay', '#',
            ' #', 'asd#a', '//#','#okay', '#',
            's#', 'asd#a', '//#','#okay', '#'                        
        ) -> blueprint.a
tibble::tribble(
            ~newvar, ~var, ~file, ~link, ~fun,
            'heinz', 'hoho', '/dsk/bla', 'a=4', 'asfjkljf %>% ',
            '#', 'asd#a', '//#','#okay', '#',
            ' #', 'asd#a', '//#','#okay', '#',
            's#', 'asd#a', '//#','#okay', '#'                        
        ) -> blueprint
blueprint.a%>%blueprint.remove.column.rows
data.frame(1:10,1:10,1:10,1:10,1:10,1:10) -> blueprint.b
c('var','var','link','link','newvar','fun') -> names(blueprint.b)
blueprint.b%>%blueprint.remove.column.rows
## blueprint.log -----------------------------------------------------------

## return.df.with.certain.vars -----------------------------------------------------------

tibble::tribble(
            ~newvar, ~var, ~file, ~link, ~fun, ~er,
            'heinz', 'hoho', '/dsk/bla', 'a=4', 'asfjkljf','bla',
            'a', 'asd#a', '//#','#okay', '#','o'
            'a#', 'ajsd#a', '//#','#okay', '#','f',
            'a', 'asd#a', '#','#okay', '#'      ,'4d'                  
        ) -> blueprint.a


expect_equal(    blueprint.a%>%return.df.with.certain.vars(file,link,fun,newvar,var) , blueprint.a[,c('file','link','fun','newvar','var')])



## df.set.standard.names -----------------------------------------------------------
expect_equal(blueprint.a %>% df.set.standard.names %>% names,c('newvar','var','file','link','fun'))
expect_equal(blueprint.a %>% df.set.standard.names,blueprint.a)
data.frame(a=1:10) %>% return.df.with.certain.vars(newvar2,var3,file,link3,fun3) %>% df.set.standard.names %>% names %>% expect_equal(c('newvar','var','file','link','fun'))
data.frame(a=1:10) %>% return.df.with.certain.vars(newvar2,var3,file,link3,myfun,old,brain,lose) %>% df.set.standard.names


### non-standard chars
data.frame(a=1:10) %>% return.df.with.certain.vars(newvar22,var3,file,link3,fun3)  %>% rename('🔴'=newvar22)%>% df.set.standard.names %>% names %>% expect_equal(c('newvar','var','file','link','fun'))

## blueprint.check.for.missing.files -----------------------------------------------------------

## blueprint.check.for.duplicate.variable.names -----------------------------------------------------------

## blueprint.validator -----------------------------------------------------------
# ✳️: : validator for funs

blue(blueprint='/doc/140_Datenaufbereitung/pisa.xlsx',waves=1,which='MergePisa',codefile='/dsk/code.txt') -> pisa

## blue -----------------------------------------------------------


blue(blueprint='/doc/140_Datenaufbereitung/pisa.xlsx',waves=1,which='MergePisa',codefile='/dsk/code.txt') -> pisa


## open.blue -----------------------------------------------------------


    




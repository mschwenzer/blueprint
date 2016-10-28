## #################### Benchmarking and Optimisation -----------------------------------------------------------
blueprint(which='MergeESS',waves=1:3,loggin=FALSE)  -> tes
blueprint(which='MergePisa',waves=1,extended=1)

install.packages('profvis')
library(profvis)

p <- profvis(
    blue(blueprint='/doc/140_Datenaufbereitung/pisa.xlsx',waves=1:5,extended=0,which='MergePisa',fun=FALSE) -> pisa
)

library(profvis)

p <- profvis(
blue(blueprint='/doc/140_Datenaufbereitung/pisa.xlsx',waves=1:5,extended=0,which='MergePisa') -> pisa
)
save(p,file='enhanced.by.removing.mutate.and.use.rbindrows.and.diffs.R')
p

load('/dsk/rbind_data.table_on_the_flysession.Rdata')

                                        #save(p,file='/dsk/rbind_data.table_rbind.at.the.end.Rdata')
p
pisa
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


blueprint(which='MergePisa',waves=1:5,logging=1)  -> tes

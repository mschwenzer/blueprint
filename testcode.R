?export
blueprint(which='MergeESS',waves=1:3,loggin=FALSE)  -> tes
blueprint(which='MergePisa',waves=1,extended=1)
blue(blueprint='/doc/140_Datenaufbereitung/pisa.xlsx',waves=1:5,extended=0,which='MergePisa') -> pisa
library(microbenchmark)
microbenchmark(blueprint(which='MergePisa',waves=1:5,logging=0))

tmp <- tempfile()
Rprof(NULL)
summaryRprof(tmp)
Rprof(tmp, interval = 0.1)
library(blueprint)
import('my.blueprint.name.csv')
%>%slice(1)%>%export('my.blueprint.name.csv')
data.frame(a=Sys.time()) %>% export(file='/dsk/test.R')

blueprint(which='MergePisa',waves=1:5,logging=0,out_file='/dsk/test.RData')  -> tes

Sys.time()
blueprint(which='MergePisa',waves=1:5,logging=1)  -> tes
Sys.time()

?export
blueprint(which='MergeESS',waves=1:3,loggin=FALSE)  -> tes
blueprint(which='MergePisa',waves=1,logging=1)
blueprint(which='MergePisa',waves=1)
library(microbenchmark)
microbenchmark(blueprint(which='MergePisa',waves=1:5,logging=0))

tmp <- tempfile()
Rprof(NULL)
summaryRprof(tmp)
Rprof(tmp, interval = 0.1)

import('my.blueprint.name.csv')
%>%slice(1)%>%export('my.blueprint.name.csv')
data.frame(a=Sys.time()) %>% export(file='/dsk/test.R')

blueprint(which='MergePisa',waves=1:5,logging=0,out_file='/dsk/test.RData')  -> tes

Sys.time()
blueprint(which='MergePisa',waves=1:5,logging=1)  -> tes
Sys.time()

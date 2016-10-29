
library(synthpop)

load('/doc/wissenschaft/blueprint/R/sysdata.rda')
import('/doc/wissenschaft/data/pisa/pisa2012/data r/INT_STU12_DEC03.Rdata')  -> p2012
p2012%>% filter(CNT=='DEU'|CNT=='GBR')%>% sample_n(2000) -> p2012
p2012 %>% dplyr::select(W_FSTUWT,ST04Q01,AGE,PV1MATH,PV2MATH,PV3MATH,PV4MATH,PV5MATH) %>% syn(seed=123)  %>% .$syn -> p2012s
p2012$SCHOOLID -> p2012s$SCHOOLID
p2012$CNT -> p2012s$CNT
p2012$StIDStd -> p2012s$StIDStd
p2012s  -> INT_STU12_DEC03_synth
import('/Users/eur/Documents/wissenschaft/data/pisa/pisa2012/data r/INT_SCQ12_DEC03.Rdata')  -> p2012sch
p2012sch%>% filter(CNT=='DEU'|CNT=='GBR') -> p2012sch
p2012sch %>% dplyr::select(SCHSIZE,CLSIZE) %>% syn(seed=123)  %>% .$syn -> p2012schs
p2012sch$SCHOOLID -> p2012schs$SCHOOLID
p2012sch$CNT -> p2012schs$CNT
p2012schs  -> INT_SCQ12_DEC03_synth

#example_blueprint1$file  %>% str_replace('/doc/wissenschaft/blueprint/inst/extdata','blueprint_example')  -> example_blueprint1$file
                                        #example_blueprint2$file  %>% str_replace('/doc/wissenschaft/blueprint/inst/extdata','blueprint_example')  -> example_blueprint2$file
example_blueprint1 %>% select(newvar,var1,file1=file,link1,fun1) -> example_blueprint1
example_blueprint2 %>% select(newvar,var1,file1=file,link1,fun1) -> example_blueprint2
save(list=c('INT_STU12_DEC03_synth','INT_SCQ12_DEC03_synth','example_blueprint1','example_blueprint2'),file='/doc/wissenschaft/blueprint/inst/extdata/examples.rda')


paste(path.package("blueprint"),"/inst/extdata/INT_STU12_DEC03.sav",sep="") %>% import



import('/doc/wissenschaft/data/pisa/pisa2012/data r/INT_STU12_DEC03.Rdata')  -> p2012
p2000%>% filter(CNT=='DEU'|CNT=='GBR')%>% sample_n(2000) -> p2012
p2000 %>% dplyr::select(W_FSTUWT,ST03Q01,ST11Q03,AGE,misced) %>% syn(seed=123)  %>% .$syn -> p2012s
p2000s %>% export('/doc/wissenschaft/blueprint/inst/extdata/INT_STU12_DEC03.sav')


import() -> test

p2012s$syn  -> p2012s


p2012 %>% dplyr::select(W_FSTUWT,ST03Q01,ST11Q03,AGE,misced) %>% hist(.$W_FSTUWT)
p2012s$syn %>% dplyr::select(W_FSTUWT,ST03Q01,ST11Q03,AGE,misced) %>% hist(.$W_FSTUWT)



## Local Variables:
## ess-r-package-info: ("blueprint" . "/doc/wissenschaft/blueprint")
## End:

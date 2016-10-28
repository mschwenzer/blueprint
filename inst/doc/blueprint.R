## ----xtable, echo=FALSE, results='asis'----------------------------------
library(tibble)
library(xtable)
tribble(~newvar,~var1,~file1,~link1,~fun1,~var2,~file2,~link2,~fun2,~`...`) -> a
a[1,] <- '                '
a %>% as.data.frame %>% xtable  %>% print(type='html')


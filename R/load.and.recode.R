load.and.recode <-
function(df)
                          {
                                        #                         cat('recs:\n')
                                        #                          print(recs)
                              # find non-empty filter (rec) columns  / ❗️ replace by str_match / find out why it can be string "NA" which is a bug
                              ((df$rec!='')&(df$rec!='NA')&(!is.na(df$rec))&(!is.nan(df$rec)))  %>% which -> rep.pos
                              print(rep.pos)
paste0(df$vars[rep.pos],' %>% ',df$rec[rep.pos]) -> df$vars[rep.pos]
                              paste0(df$newvars,'=',df$vars,collapse=',')  -> transmute.code





                              # create a string that renames/selects with select and mutates afterwards
                              paste0('import("',df$files[1],'") %>% transmute(',transmute.code,')') -> eval.code
                              eval.code
                                        # execute and return
#                              print(eval.code)
                              eval(parse(text=eval.code))
                          }

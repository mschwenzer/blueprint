load.and.recode <-
function(df,fun,debug=FALSE)
                          {
                                        #                         cat('recs:\n')
                                        #                          print(recs)
                              # find non-empty fun (rec) columns  / ❗️ replace by str_match / find out why it can be string "NA" which is a bug
                              ((df$fun!='NA')&(!is.na(df$fun))&(!is.nan(df$fun)))  %>% which -> rep.pos
                           if(debug){   print(rep.pos)}
paste0(df$var[rep.pos],' %>% ',df$fun[rep.pos]) -> df$var[rep.pos]
                              paste0(df$newvar,'=',df$var,collapse=',')  -> transmute.code
                              # create a string that renames/selects with select and mutates afterwards
                              paste0('import("',df$file[1],'")',if(fun){paste0(' %>% transmute(',transmute.code,')')}) -> eval.code
                              eval.code
                                        # execute and return
#                              print(eval.code)
                              eval(parse(text=eval.code))
                          }

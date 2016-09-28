attrs.as.factor <- function (x)
 {
     the.label <- x %>% attr("label")
     from <- x %>% attr("labels") %>% as.numeric
     to <- x %>% attr("labels") %>% names
     cats <- bind_cols(from = data.frame(from), to = data.frame(to))
     rownames(cats) <- NULL
     x %>% rec(recodes = cats) %>% as.factor -> x
     attr(x, "label") <- the.label
     return(x)
 }

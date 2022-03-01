## FUN for transforming NA 
name2label_question <- function(tool.survey, tool.choices, col){
  if (str_detect(col, "/")) {
    q.name <- str_split(col, "/")[[1]][1]
    c.name <- paste0(tail(str_split(col, "/")[[1]], -1), collapse="/")
  } else {
    q.name <- col
    c.name <- NA
  }
  if (q.name %in% tool.survey$name){
    q <- tool.survey[tool.survey$name==q.name,]
    q.label <- q$`label`
    if (is.na(q.label) | q$q.type %in% c("note")) q.label <- q.name
    if (!is.na(c.name)){
      q.list_name=ifelse(q$list_name=="NA", NA, q$list_name)
      c.label <- tool.choices[tool.choices$list_name==q.list_name & tool.choices$name==c.name, "label"]
    } else c.label <- NA
    label <- ifelse(is.na(c.label), q.label, paste0(q.label, "/", c.label))
  } else label <- q.name
  return(label)
}


name2label_question <- function(tool_survey, tool_choices, col){
  if (str_detect(col, "/")) {
    q.name <- str_split(col, "/")[[1]][1]
    c.name <- paste0(tail(str_split(col, "/")[[1]], -1), collapse="/")
  } else {
    q.name <- col
    c.name <- NA
  }
  if (q.name %in% tool_survey$name){
    q <- tool_survey[tool_survey$name==q.name,]
    q.label <- q$`label::english`
    if (is.na(q.label) | q$type %in% c("note")) q.label <- q.name
    if (!is.na(c.name)){
      q.list_name=ifelse(q$list_name=="NA", NA, q$list_name)
      c.label <- tool_choices[tool_choices$list_name==q.list_name & tool_choices$name==c.name, "label::english"]
    } else c.label <- NA
    label <- ifelse(is.na(c.label), q.label, paste0(q.label, "/", c.label))
  } else label <- q.name
  return(label)
}

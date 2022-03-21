name2label_question <- function(tool_survey, tool_choices, col){
  # for each column check if it is a select multiple
  if (str_detect(col, "/")) {
    q.name <- str_split(col, "/")[[1]][1]
    c.name <- paste0(tail(str_split(col, "/")[[1]], -1), collapse="/")
  } else {
    q.name <- col
    c.name <- NA
  }
  
  # returning the label and make sure to include the note and NAs
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

name2label_choices_one <- function(tool_survey, tool_choices, data, col) {
  # select the type column from each select_one question
  q.type <- tool_survey$type[tool_survey$name==col]
  
  # take the id of the choices to get the list name
  q.list_name <- str_split(q.type, " ")[[1]][2]
  
  # export the choices relevant to each select_one question
  choices <- tool_choices %>% filter(list_name == q.list_name) %>% 
      select(name, `label::english`) %>% rename(label=`label::english`)
  
  # replace the xml with label using left_join
  d.join <- data.frame(col=as.character(data[[col]])) %>% 
    left_join(choices, by=c("col"="name")) %>% select(label)
  
  # return only the new label column and replace it in the for loop using vectors 
  return(d.join$label)
}

name2label_choices_multiple <- function(tool_survey, tool_choices, data, col) {
  # select all the columns with all the options for each select_multiple
  d.join <- data %>% 
    select(contains(paste0(col,"/")))
  col_internal <- colnames(d.join)
  
  # for each column with options
  for(j in 1:length(col_internal)){
    # change all 1's to the xml answer
    xml_answer <- str_split(col_internal[j], "/")[[1]][2]
    d.join <- d.join %>% 
      mutate(!!sym(col_internal[j]) := ifelse(!!sym(col_internal[j]) == "1", xml_answer, NA))
    
    # get the list of the xml and label options for each select multiple questions
    choice_id <- filter(tool_survey, str_starts(name, str_split(col_internal[j],"/")[[1]][1])) %>% 
      select(list_name)
    choice_id <- choice_id$list_name
    t.choices <- tool_choices %>% 
      filter(list_name == choice_id) %>% 
      select(name, `label::english`) %>% rename(label = `label::english`)
    
    # replace the xml with label using left_join
    d.new.join <- data.frame(col=as.character(d.join[[col_internal[j]]])) %>%
      left_join(t.choices, by=c("col"="name")) % >% select(label)
    d.join[col_internal[j]] <- d.new.join$label
  }
  
  # concatenate all the answers, removing NAs in one cell and separated by a ';' 
  d.join <- d.join %>% 
    unite("Merged", everything(), sep= ";", na.rm = T)
  
  # return only the new label column and replace it in the for loop using vectors
  return(d.join$Merged)
}





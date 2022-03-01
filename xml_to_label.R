setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

library(tidyverse)
library(readxl)
library(openxlsx)
source("src/utils.R")
# Input
filename_tool <- "resources/Supply_Chain_Analysis_Retailers_tool.xlsx"
filename_cleaned_data <- "resources/data_cleaned.xlsx"

# Output
filename_labeled_output <- "output/dataset_labels.xlsx"

# First loading tool
tool_survey <- read_excel(filename_tool, sheet = "survey", col_types = "text") %>% 
  filter(!is.na(name)) %>% 
  mutate(q.type = as.character(lapply(type, function(x) str_split(x, " ")[[1]][1])),
         list_name = as.character(lapply(type, function(x) str_split(x, " ")[[1]][2])))

tool_choices <- read_excel(filename_tool, sheet = "choices", col_types = "text") %>% 
  filter(!is.na(list_name))

data_labeled_list <- list()

# Loading the data
data <- read_excel(filename_cleaned_data, col_types = "text")
data_labeled <- data

# XML to Label
col_names <- colnames(data)
for (i in 1:length(col_names)) {
  colnames(data_labeled)[i] <- name2label_question(tool_survey, tool_choices, col_names[i])
  # code responses of select_one questions
  if (col_names[i] %in% tool_survey$name){
    q.type <- tool_survey$type[tool_survey$name==col_names[i]]
    if (str_starts(q.type, "select_one ")){
      q.list_name <- str_split(q.type, " ")[[1]][2]
      choices <- tool_choices %>% filter(list_name==q.list_name) %>%
        select(name, `label::english`) %>% rename(label=`label::english`)
      d <- data.frame(col=as.character(data[[col_names[i]]])) %>% 
        left_join(choices, by=c("col"="name")) %>% select(label)
      data_labeled[[colnames(data_labeled)[i]]] <- d$label
    }
  }
}






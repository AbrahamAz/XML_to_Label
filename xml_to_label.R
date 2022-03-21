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

# data_labeled_list <- list()

# Loading the data
data <- read_excel(filename_cleaned_data, col_types = "text")
data_labeled <- data

# Select_One choice - XML to Label

tool_one <- tool_survey %>% 
  filter(str_starts(type, "select_one "))

col_one <- tool_one$name

for (i in 1:length(col_one)){
  if(!is.null(data_labeled[[col_one[i]]])){
    data_labeled[[col_one[i]]] <- name2label_choices_one(tool_survey,tool_choices,data,col_one[i])
  }
}

# Select_Multiple choices - XML to Label

tool_multi <- tool_survey %>% 
  filter(str_starts(type, "select_multiple "))
col_multi <- tool_multi$name

for (i in 1:length(col_multi)){
  if(!is.null(data_labeled[[col_multi[i]]])){
    data_labeled[[col_multi[i]]] <- name2label_choices_multiple(tool_survey,tool_choices,data,col_multi[i])
  }
}

# Questions - XML to Label
col_names <- colnames(data)

for (i in 1:length(col_names)) {
  colnames(data_labeled)[i] <- name2label_question(tool_survey, tool_choices, col_names[i])
  
}





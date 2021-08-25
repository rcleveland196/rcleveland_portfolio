###################################### Enrollment Trends
######################################

library(tidyverse)
library(data.table)

current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("..")
rawdata <- paste0(getwd(), "/Raw Data/")
save <- paste0(getwd(), "/Output/")
setwd(dirname(current_path))

### Set variables
savefile1 <- "Student_List_All"
savefile2 <- "Enrollment_List"
savefile3 <- "Demographics_List"
savefile4 <- "School_Enrollment"
savedate <- paste0("_", format(as.Date(Sys.Date()), "%Y%m%d"), ".csv")

### Read and Append Enrollment files
master <- list.files(rawdata, pattern = "csv", full.names = TRUE) %>%
  map_df(function(x) read_csv(x, col_names = T))

### Recode Necessary Variables
students <- master %>% 
  mutate(grade_level = as.numeric(grade_level),
         gender = ifelse(gender == "F", "Female", "Male"),
         special_ed = ifelse(special_ed == 1, "Special Ed", "Not Special Ed"))

### Create Group
list <- bind_rows(
  students %>% mutate(group = "All Students"),
  students %>% mutate(group = gender),
  students %>% mutate(group = ethnicity),
  students %>% mutate(group = special_ed)) %>% 
  select(-ethnicity, -gender, -special_ed) %>% 
  filter(!grepl("^Not", group))

### Create category
list <- list %>% 
  mutate(category = case_when(group == "All Students" ~ "All",
                              group %in% c("Female", "Male") ~ "Gender",
                              group == "Special Ed" ~  "SpecEd",
                              TRUE ~ "Ethnicity"))

### Aggregate at the School Level
schools <- bind_rows(
  list %>% 
    group_by(year, school_name, grade_level, category, group) %>%
    summarize(total_students = n()) %>% 
    mutate(grade_level = as.character(grade_level)),
  list %>% 
    group_by(year, school_name, category, group) %>%
    summarize(total_students = n()) %>% 
    mutate(grade_level = "All Grades"))

### Aggregate at the District Level
district <- bind_rows(
  list %>% 
    group_by(year, grade_level, category, group) %>%
    summarize(total_students = n()) %>% 
    mutate(grade_level = as.character(grade_level)),
  list %>% 
    group_by(year, category, group) %>%
    summarize(total_students = n()) %>% 
    mutate(grade_level = "All Grades")) %>% 
  mutate(school_name = "Presidential School District")

### Combine and Re-shape to Long for School Sorter    
all <- bind_rows(schools, district) %>% 
  spread(year, total_students)

### Create Enrollment Change Columns
all <- all %>% 
  mutate(key = paste(school_name, grade_level, category, group, sep = "-")) %>% 
  select(key, everything())

### Create Student Enrollment List for Dashboard
enroll <- master %>% 
  select(year, student_id, grade_level, school_name)

### Create Student Demographics List for Dashboard
demo <- master %>% 
  distinct(student_id, gender, ethnicity, special_ed)

### Save Raw Student List
write.csv(master,
          paste0(save, savefile1, savedate),
          row.names = F)

### Save Enrollment List
write.csv(enroll,
          paste0(save, savefile2, ".csv"),
          row.names = F)

### Save Demographics List
write.csv(demo,
          paste0(save, savefile3, ".csv"),
          row.names = F)

### Save Schools List for School Sorter
write.csv(all,
          paste0(save, savefile4, savedate),
          row.names = F)

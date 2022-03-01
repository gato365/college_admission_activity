

## 0) Load Libriaries
library(tidyverse)
library(readxl)

## 2) Load data
df = read_xlsx('typical_ca_colleges.xlsx',sheet = 'typical_ca_colleges')

## Task 1: Make column names easier to use based on selected 10

## 1) More than 0 enrollment
city_college_df <- df %>% 
  filter(`Enrolled Total` > 0,
         Urban %in% c(11,12,13)) %>% ## Provides only city colleges
  mutate(City_Size = case_when(
    Urban == 11 ~ 'Large',
    Urban == 12 ~ 'Midsize',
    Urban == 13 ~ 'Small'
  )) %>% 
  select(-ID,
         -`Price In-State Living off Campus`,
         -`Price Out-of-State Living off Campus`, 
         -GPA,
         -`Enrolled Total`,
         -`SAT Reading 75th Percentile`,
         -`ACT English 75th Percentile`,
         -`ACT Math 75th Percentile`,
         -Private,
         -Urban,
         -Major,
         -Address) %>% 
  filter(!(is.na(`ACT Composite 75th Percentile`) & is.na(`SAT Math 75th Percentile`))) %>% 
  mutate(State = case_when(
    str_detect(`School Name`,'California') ~ 'IS',
    str_detect(`School Name`, c('San Jose|San Francisco|Chapman University|University of the Pacific|University of Redlands|Simpson University|Santa Clara University|San Diego State University|Point Loma Nazarene University|Mills College|Mount Saint Mary\'s University|Occidental College|Loyola Marymount University|La Sierra University|Fresno Pacific University|Concordia University-Irvine' ))~ 'IS', 
    TRUE ~ 'OOS'
  ),
  Acceptance_Rate =round( `Admissions Total`/`Applicants Total`,3)
  )



new_column_names <- str_replace_all(colnames(city_college_df),' ','_') %>% 
  str_replace_all('Living_on_Campus','LOC') %>% 
  str_remove_all('_75th_Percentile') %>% 
  str_replace_all('Out-of-State','OOS') %>% 
  str_replace_all('In-State','IS') 

colnames(city_college_df) <- new_column_names 

##Pacakge Installation##

install.packages("janitor")
install.packages("tidyverse")
install.packages("dplyr")

##Data Clean-up##
library(janitor)
library(tidyverse)
library(dplyr)

raw_data <- read.csv("AAsHduQg.csv")
dict <- readLines("gss_dict.txt", skip = 18)
labels_raw <- read_file("gss_labels.txt")

variable_descriptions <- as_tibble(dict) %>%
  filter(value!="}") %>%
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))

labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)

cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))

#select variables#
gss <- raw_data %>% 
  select(CASEID, 
         sex,
         acompstc,
         ede_01a,
         ede_01b,
         ede_01c,
         ede_02,
         ehg3_01b,
         ehg3_04b,
         ehg3_05b,
         eop_210,
         eop_220,
         eop_230,
         eop_240,
         ree_02,
         slm_01,
         srh_110,
         srh_115,
         toe_240,
         toe_241,
         uhw_16gr,
         ups_500,
         uws_230) %>% 
  mutate_at(vars(sex:uws_230), .funs = funs(ifelse(.>=96, NA, .))) %>% 
  mutate_at(.vars = vars(sex:uws_230),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull()))))

#rename variables#
gss <- gss %>% 
  clean_names() %>% 
  rename(sex=sex,
         age_when_completed=acompstc,
         elementary_high_school_type=ede_01a,
         trade_school_type=ede_01b,
         university_type=ede_01c,
         enrollment_status=ede_02,
         highest_certificate_diploma_degree=ehg3_01b,
         college_cegep_other_nonuniversity_type=ehg3_04b,
         university_diploma_type=ehg3_05b,
         high_school=eop_210,
         trade_school=eop_220,
         college_cegep_other_nonuniversity=eop_230,
         university=eop_240,
         religious_activities=ree_02,
         feelings_life=slm_01,
         self_rate_health=srh_110,
         self_rate_mental=srh_115,
         job_permanent=toe_240,
         reason_not_permanent_job=toe_241,
         average_work_hour_week=uhw_16gr,
         program_mediation=ups_500,
         usual_work_schedule_description=uws_230)

#data cleaning#
gss <- gss %>% 
  mutate_at(vars(sex:usual_work_schedule_description),
            .fun = funs(ifelse(.=="Valid skip"|.=="Don't know"|.=="Refusal"|.=="Not stated", "NA", .)))

gss <- gss %>% 
  mutate(is_male = ifelse(sex=="Male", 1, 0)) 

gss <- gss %>% 
  mutate_at(vars(elementary_high_school_type:university_type), .funs = funs(case_when(
    .=="Yes"~1,
    .=="No"~0,
    .=="NA"~as.numeric(NA)
  )))

gss <- gss %>% 
  mutate_at(vars(feelings_life), .funs = funs(case_when(.=="Very satisfied"~1,.=="|"~0,)))

working_status <- raw_data %>% 
  mutate(main_activity = case_when(
    mpl_105a=="Yes"~ "Working at a paid job/business",
    mpl_105b=="Yes" ~ "Looking for paid work",
    mpl_105c=="Yes" ~ "Going to school",
    mpl_105d=="Yes" ~ "Caring for children",
    mpl_105e=="Yes" ~ "Household work", 
    mpl_105i=="Yes" ~ "Other", 
    TRUE~ "NA")) %>% 
  select(main_activity) %>% 
  pull()

gss <- gss %>% 
  mutate(main_activity = working_status)

write_csv(gss, "gss.csv")

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(dataverse)
library(ggplot2)
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")

#-------------Load V-Dem data (Coppedge et al. 2022)----------------------------
dataset <- get_dataset("10.7910/DVN/YQPN8D")
files <- dataset$files[c("filename", "contentType")]

# Load V-Dem data
vdem <- get_dataframe_by_name(
  "V-Dem-CY-Full+Others-v12.csv",
  "10.7910/DVN/YQPN8D",
  .f = function(x) read.delim(x, sep = ","))

# subset to relevant variables
vdem_sub <- vdem %>%
  select(year, cowcode = COWcode, country_text_id, country_name, country_id,
         v2x_regime, v2x_polyarchy, v2x_libdem,
         v2cademmob, v2cademmob_osp_codelow, v2cademmob_osp_codehigh, v2cacamps,
         v2cademmob_nr, v2cademmob_osp, v2cademmob_ord, v2cademmob_sd,v2cagenmob,
         v2caautmob, v2caautmob_nr, v2caautmob_osp, v2caautmob_ord,
         v2caautmob_sd, v2caautmob_osp_codelow, v2caautmob_osp_codehigh,
         e_pop, v2mecenefm, e_area) %>% 
  filter(!is.na(v2cademmob) & !is.na(v2caautmob))

#-------------Load Carnegie Tracker---------------------------------------------
carnegie <- get_dataframe_by_name(
  "Global Protest Tracker - View Data.tab",
  "10.7910/DVN/YQPN8D",
  .f = function(x) read.delim(x, sep = "\t"))

carnegie <- carnegie %>% # read data
  janitor::clean_names() %>% # clean names
  drop_na(country) %>% # drop obs with NA for country
  mutate(cowcode = countrycode::countrycode(country, "country.name", "cown"),
         year = as.numeric(paste0("20", str_sub(start_date, -2, -1)))) %>% # add year
  group_by(cowcode, year) %>% # Aggregate at year level
  summarise(protest_name = paste0(protest_name, collapse = ", ")) %>% 
  drop_na(cowcode) %>% 
  ungroup() 

#-------------Comparison with Carnegie (Figure 4)-------------------------------

# Subset V-Dem data
vdem_sub2017 <- vdem_sub %>% 
  filter(year > 2016)

# Merge Carnegie and V-Dem
vdem_carnegie <- left_join(vdem_sub2017, carnegie, by = c("cowcode", "year"))

# Rename protest issues
vdem_carnegie <- vdem_carnegie %>% 
  mutate(carnegie = ifelse(is.na(protest_name), 0, 1),
         protest_name = case_when(protest_name == "Cybersecurity / special economic zones protests" ~
                                    "Cybersecurity/ \n economic zones protests",
                                  protest_name == "Coronavirus lockdown protest, Coronavirus protest" ~
                                    "Coronavirus protest",
                                  protest_name == "Racial equality protests" ~ "George Floyd solidarity protests",
                                  protest_name == "Corruption protests, Coronavirus lockdown protest" ~ "Corruption, Coronavirus protests",
                                  T ~ protest_name
                                  
         ))

# Identify top 30 mass mob values in Carnegie (Hellmeier and Bernhard originally look at top 10)
top30 <- vdem_carnegie %>% filter(carnegie == 1) %>% 
  select(country_name, country_text_id, year, protest_name, contains("v2cademmob_osp")) %>% 
  arrange(-v2cademmob_osp) %>% 
  slice(1:30) %>% 
  mutate(name_new = paste0(protest_name, " (",country_name, " ",year, ") " ),
         name_new =  iconv(name_new, "UTF-8", "ASCII", sub = ""))

write_excel_csv(top30, "data/output/hellbern_protests.csv")

top30aut <- vdem_carnegie %>% filter(carnegie == 1) %>% 
  select(country_name, v2x_polyarchy, country_text_id, year, protest_name, contains("v2cademmob_osp")) %>% 
  filter(v2x_polyarchy <.5) %>%
  arrange(-v2cademmob_osp) %>% 
  slice(1:30) %>% 
  mutate(name_new = paste0(protest_name, " (",country_name, " ",year, ") " ),
         name_new =  iconv(name_new, "UTF-8", "ASCII", sub = ""))

write_excel_csv(top30aut, "data/output/hellbern_protests_aut.csv")

#-------------Load Beissinger data on revolutions-------------------------------
revol_goals <- get_dataframe_by_name(
  "Revolutionary episodes_v_1.0.xlsm",
  "10.7910/DVN/YQPN8D",
  .f = function(x) readxl::read_xlsx(x, sheet = "2-Goals"))

revols_general <- get_dataframe_by_name(
  "Revolutionary episodes_v_1.0.xlsm",
  "10.7910/DVN/YQPN8D",
  .f = function(x) readxl::read_xlsx(x, sheet = "1-Timing & location"))

# merge both data sets and subset to relevant variables
revols <- left_join(revol_goals, revols_general) %>%
  select(revid, nameofrevolution, year = startyear,  democrat, leftist,
         rightwing, anticolonial, antimonarch, cowcode, isoabb)

#Merge with V-Dem
vdem_revol <- left_join(revols, vdem_sub)

vdem_revoldem <- vdem_revol %>%
  filter(democrat==1) %>%
  filter(year >=1990) %>%
  select(revid, nameofrevolution, year, country_text_id, country_name)

write_excel_csv(vdem_revoldem, "data/output/vdem_beissdem.csv")

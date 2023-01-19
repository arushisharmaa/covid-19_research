 # Automated Web Scraping in R
#last updated 
#every identification & characteristics 


#use free profiles 
#name, bed count 
#install.packages("rvest")
library(tidyverse)
library(rvest)

# #find the specific website by using the search bar 
# ahd_search <- read_html("https://www.ahd.com/search.php")
# 
# #grab the state name from the html_elements 
# state <- ahd_search %>% html_elements("option") # mstate[]
# state
# #take in the specific TX state element 
# texasSelection <- state  %>% 
#   html_text()
# View(texasSelection)
# 
# #large text file to find a specific value
# tx_location = grep("TX \\(Texas\\)", texasSelection)[1]

##########################################
# Get the url for each TX hospital profile
##########################################

#grab in the results from the specific location 
# I have to manually open link in Chrome and select "I'm not a robot" for this to work
tx_search = read_html("https://www.ahd.com/list_cms.php?mstate%5B%5D=TX&listing=1&viewmap=0#") %>% 
  html_elements("a")

# turn list of urls into a dataframe
df = bind_rows(lapply(xml2::xml_attrs(tx_search), 
                      function(x) 
                        data.frame(as.list(x), 
                                   stringsAsFactors=FALSE)))

# get only urls for the free profiles
df_sub = df %>%
  select(href) %>%
  filter(grepl("free_profile", href)) %>%
  distinct()

# separate urls into useful info about each hospital
tx_hosp_names = df_sub %>%
  separate(href, into=c("n1", "prof", "CCN", "NAME", "CITY", "STATE", "n2"), sep="/", remove=F) %>% # , remove=F
  select(-n1, -n2, -prof) %>%
  mutate(across('NAME', str_replace_all, "_", " ")) %>%
  mutate(across('NAME', str_replace_all, "%26", "&")) %>%
  mutate(across('NAME', str_replace_all, "%27", "'"))

write.csv(tx_hosp_names, "all_TX_hospital_urls.csv", row.names=F)

# https://www.ahd.com/free_profile/450890/_Baylor_Scott_%26_White_Medical_Center__Plano/Plano/Texas/
open_hosp_free_profile = read_html(paste0("https://www.ahd.com/free_profile", CCN, + "/" + NAME
                                          + "/" + CITY + "/" + CITY + "/" + STATE))


# get info about hospital and put in a data frame with column header
# turn list of urls into a dataframe
hosp_data = bind_rows(lapply(xml2::xml_attrs(test), 
                             function(x) 
                               data.frame(as.list(x), 
                                          stringsAsFactors=FALSE)))



df[grep("Hospital>", df$href), 1]

#must be https://www.ahd.com + what is in df_sub 
#attempt to grab the name & city of the hospital 
df_sub_name = df[grep("Hospital>", df$href), 1]
df_sub_city = df[grep("<td>", df$href), 1]


#choose from each profile: 
ahd_spec_search <- read_html("https://www.ahd.com/profile.php?hcfa_id=3a8b9dbfa10b05a69002415f1c684e6f&ek=146539cb7a912eab199d255646dff909")


#grab the name, city, ICU beds 
name <- ahd_spec_search %>% html_elements("name= top")
cmsnum <- ahd_spec_search %>% html_elements("abbr")
icubeds_search <- ahd_spec_search %>% html_elements("noborder nomargin")

#grab in the results from the specifc location 
specifcHospital <- icubeds_search %>% 
  html_elements("valign=top align=right")






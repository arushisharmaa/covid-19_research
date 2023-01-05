# Automated Web Scraping in R
#last updated 
#every indentification & characteristics 


#use free profiles 
#name, bed count 
#install.packages("rvest")
library(tidyverse)
library(rvest)

#find the specific website by using the search bar 
ahd_search <- read_html("https://www.ahd.com/search.php")

#grab the state name from the html_elements 
state <- ahd_search %>% html_elements("option") # mstate[]
state
#take in the specific TX state element 
texasSelection <- state  %>% 
  html_text()
View(texasSelection)

#large text file to find a specific value
tx_location = grep("TX \\(Texas\\)", texasSelection)[1]

tx_search = read_html("https://www.ahd.com/list_cms.php?submitted=Submit&mname=&mcity=&mstate%5B0%5D=TX")

#grab in the results from the specifc location 
specifcHospital <- tx_search %>% 
  html_elements("a")
specifcHospital

#hosp = specifcHospitalSearch[grep("profile.php", specifcHospitalSearch)]
df = bind_rows(lapply(xml2::xml_attrs(specifcHospital), 
                 function(x) 
                   data.frame(as.list(x), 
                              stringsAsFactors=FALSE)))


df_sub = df[grep("profile.php", df$href), 1]

#must be https://www.ahd.com + what is in df_sub 
#attempt to grab the name & city of the hospital 
df_sub_name = df[grep("Hospital>", df$href), 1]
df_sub_city = df[grep("<td>", df$href), 1]


#choose from each profile: 
ahd_spec_search-> read_html("https://www.ahd.com/profile.php?hcfa_id=3a8b9dbfa10b05a69002415f1c684e6f&ek=146539cb7a912eab199d255646dff909")


#grab the name, city, ICU beds 
name <- ahd_spec_search %>% html_elements("name= top")
cmsnum <- ahd_spec_search %>% html_elements("abbr")
icubeds_search <- ahd_spec_search %>% html_elements("noborder nomargin")

#grab in the results from the specifc location 
specifcHospital <- icubeds_search %>% 
  html_elements("valign=top align=right")







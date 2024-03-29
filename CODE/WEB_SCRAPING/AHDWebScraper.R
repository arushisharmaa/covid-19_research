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

################################################
# Get the url for each TX hospital FREE profile
################################################
if(!file.exists("all_TX_hospital_urls.csv")){ # update path when we make sub-folders
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
  
  ##REPLACE WITH THE SIGNED IN NAMES AND URLS 
  # Last ran on Feb. 8, 2023
  write.csv(tx_hosp_names, "all_TX_hospital_urls.csv", row.names=F)
}


###################################################################
# Instead of running code above just open what we previously saved
tx_hosp_names = read_csv("all_TX_hospital_urls.csv")

# https://www.ahd.com/free_profile/450890/_Baylor_Scott_%26_White_Medical_Center__Plano/Plano/Texas/
# tx_hosp_names[row, column] in a data frame
#make sure to run the verification before running the scraper 
hosp_features_free = list()
for(i in 22:40){ # nrow(tx_hosp_names)
  open_hosp_free_profile = read_html( paste0("https://www.ahd.com", tx_hosp_names[i, "href"]) ) # 
  
  # logged in link: "https://www.ahd.com/profile.php?hcfa_id=d6356f47ff83e8b7e8959d1828d32614&ek=4d97a78b10bbb844da7f61504e8eaa01"
  
  hosp_features_free[[i]] <- open_hosp_free_profile %>% 
    html_elements("tr") %>% 
    html_text2()
  
  if(length(hosp_features_free[[i]]) < 10){
    print(paste0("need to verify not a robot to continue /n Rerun from i = ", i) )
    break 
  }
  
  print(paste0("i = ", i, " finsihed"))
} # end for i

#NAME, ADDRESS, PHONE NUMBER, TYPE OF HOSPITAL, Total Staffed Beds, NOTES, CMS Certification Number, Special Care 
#Routine Services, Hospital Website, 


nameOfHospital = (temp_feat[2,])
View(nameOfHospital)

clean_hosp_features = data.frame()
for(i in 1:5){
  # change the hospital
  temp_feat = as_tibble(hosp_features[[i]])
  
  
  cmsNumCol = temp_feat$value[grep("CMS Certification Number:", temp_feat$value, ignore.case = T )[3]]
  
  zipCodeCol = temp_feat$value[grep("ZIP Code", temp_feat$value, ignore.case = T )[2]]
  loop_names = c("address", "phone")
  
  nameAndAddressCol = temp_feat$value[grep("address", temp_feat$value, ignore.case = T )[2]]
  split = unlist(str_split(nameAndAddressCol, "\t"))
  data = unlist(str_split(split[2], "\n"))
  
  #temp_df = data.frame(name=data[1],  address=data[2])
  
  phoneNumCol = temp_feat$value[grep("phone", temp_feat$value, ignore.case = T )[2]]
  phoneSplit = unlist(str_split(phoneNumCol, "\t"))[2]
  
  websiteCol = temp_feat$value[grep("Hospital Website:", temp_feat$value, ignore.case = T )[2]]
  typeOfHospitalCol = temp_feat$value[grep("type of facility", temp_feat$value, ignore.case = T )[2]]
  totalStaffedBedsCol = temp_feat$value[grep("Total Staffed Beds:", temp_feat$value, ignore.case = T )[2]]
  
  notesCol1 = temp_feat$value[grep("N O T E S", temp_feat$value, ignore.case = T )[2]]
  notesCol1 = temp_feat$value[grep("N O T E S", temp_feat$value, ignore.case = T )[3]]
  
  temp = data.frame(name=data[1], address=data[2], phoneNum = phoneSplit)
  
  clean_hosp_features = rbind(clean_hosp_features, temp)
} # end for i



# str_split
# str_replace

#saddressOfHospitalCol

############ Old code #############

# get info about hospital and put in a data frame with column header
# turn list of urls into a dataframe
hosp_data = bind_rows(lapply(xml2::xml_attrs(hosp_features), 
                             function(x) 
                               data.frame(as.list(x), 
                                          stringsAsFactors=FALSE)))


df[grep("Hospital>", df$href), 1]

#must be https://www.ahd.com + what is in df_sub 
#attempt to grab the name & city of the hospital 
df_sub_name = df[grep("Hospital>", df$href), 1]
df_sub_city = df[grep("<td>", df$href), 1]


#grab the name, city, ICU beds 
name <- ahd_spec_search %>% html_elements("name= top")
cmsnum <- ahd_spec_search %>% html_elements("abbr")
icubeds_search <- ahd_spec_search %>% html_elements("noborder nomargin")
write.csv(icuebds_search, "Desktop/R-Studio/csv/beds.csv”, row.names=F")

#grab in the results from the specifc location 
specifcHospital <- icubeds_search %>% 
  html_elements("valign=top align=right")

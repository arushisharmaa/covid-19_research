####################################
# Get private hospital profile urls
# Emily - 02.22.23 - ATX
# This is only returning a test site
# Abandoning task for now
####################################

# Load library
library(rvest)

# all the free profile urls made in AHDWebScraper.R
free_prof_url = read_csv("all_TX_hospital_urls.csv")

# Get all elements that are in <a\> blocks (rows of table)
tx_search = read_html("https://www.ahd.com/list_cms.php?mstate%5B%5D=TX&listing=1&viewmap=0#") %>%
  html_elements("a")

# Turn list of urls into a dataframe
df = bind_rows(lapply(xml2::xml_attrs(tx_search), 
                      function(x) 
                        data.frame(as.list(x), 
                                   stringsAsFactors=FALSE)))

# Subset to only urls of private profiles
df_sub = df %>%
  select(href) %>%
  filter(grepl("profile.php", href)) %>%
  distinct()

i=2
hosp_features = list()
#for(i in 1:40){ # nrow(tx_hosp_names)
  open_hosp_profile = read_html( paste0("https://www.ahd.com", df_sub[i, "href"]) ) # 
  
  # This appears to only open the test profile despite the url being correct >._.<
  hosp_features[[i]] <- open_hosp_profile %>% 
    html_elements("tr") %>% 
    html_text2()
  
  print(paste0("i = ", i, " finsihed"))
#} # end for i




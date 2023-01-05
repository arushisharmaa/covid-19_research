library(tidyverse)

#data2020q4=read_csv("Provider_of_Services_File_Hospital_Non_Hospital_Facilities_Dataset_2020_Q4.csv")
#data2021q4=read_csv("Provider_of_Services_File_Hospital_Non_Hospital_Facilities_Dataset_2021_Q4.csv")
# col_names = data.frame(name=names(data))
#write.csv(tx_hosp_data, paste0("tx_hosp_", date_range[1], ".csv"), row.names = F )

year_quarter = c("2020_Q4", "2021_Q4")
all_year_quarters = data.frame()
for(i in 1:length(year_quarter)){
  
  temp_df = read_csv(paste0("Provider_of_Services_File_Hospital_Non_Hospital_Facilities_Dataset_", year_quarter[i], ".csv")) %>%
    select( BED_CNT, CITY_NAME, FAC_NAME, FIPS_STATE_CD, FIPS_CNTY_CD, PRVDR_NUM ) %>%
    filter(FIPS_STATE_CD == "48") %>% 
    select( BED_CNT, CITY_NAME, FAC_NAME, FIPS_STATE_CD, FIPS_CNTY_CD, PRVDR_NUM ) %>%
    filter(CITY_NAME %in% c("AUSTIN", "HOUSTON", "DALLAS") ) %>%
    mutate(TIME = year_quarter[i]) %>%
    group_by(PRVDR_NUM, CITY_NAME, FIPS_STATE_CD, FIPS_CNTY_CD ) %>%
    slice(1) %>%
    ungroup()
  
  
  all_year_quarters = rbind(all_year_quarters, temp_df)
}


# ##2020 Data
# 
# 
# ##2021 Data
# tx_hosp_data_2021q4 = read_csv("Provider_of_Services_File_Hospital_Non_Hospital_Facilities_Dataset_2021_Q4.csv") %>%
#   select( BED_CNT, CITY_NAME, FAC_NAME, FIPS_STATE_CD, FIPS_CNTY_CD, PRVDR_NUM ) %>%
#   filter(FIPS_STATE_CD == "48")
# 
# sub_hosp_data_2020q4 = tx_hosp_data_2020q4 
# 
# sub_hosp_data_2021q4 = tx_hosp_data_2021q4 %>% 
#   select( BED_CNT, CITY_NAME, FAC_NAME, FIPS_STATE_CD, FIPS_CNTY_CD, PRVDR_NUM ) %>%
#   filter(CITY_NAME %in% c("AUSTIN", "HOUSTON", "DALLAS") ) %>%
#   mutate(TIME = "2021q4") %>%
#   group_by(PRVDR_NUM, CITY_NAME, FIPS_STATE_CD, FIPS_CNTY_CD ) %>%
#   slice(1) %>%
#   ungroup() %>%
#   rename(BED_CNT = BED_CNT_2021q4) %>%
#   mutate(TIME = year_quarter[2])
# 
# # joined_data = sub_hosp_data_2020q4 %>%
# #   full_join(sub_hosp_data_2021q4, 
# #             by=c("CITY_NAME", "FIPS_STATE_CD", "FIPS_CNTY_CD", "PRVDR_NUM")) %>%
# #   select(starts_with("BED_C"), PRVDR_NUM, everything()) %>% 
# #   filter(!(is.na(BED_CNT_2020q4) & is.na(BED_CNT_2021q4) ) )
# 
# temp = rbind(sub_hosp_data_2020q4, sub_hosp_data_2021q4) 

 count_group_city = all_year_quarters %>% 
  mutate(BED_CNT_NEW = ifelse(is.na(BED_CNT), 0, BED_CNT)) %>%
  group_by(TIME, CITY_NAME) %>%
  summarise(cnty_sum = sum(BED_CNT_NEW) ) %>%
  ungroup() 

ggplot(count_group_city, 
       aes(x = TIME, y = cnty_sum, 
           group = CITY_NAME, color = CITY_NAME ))+
  geom_line()+
  geom_point()+
  theme_bw()


                      





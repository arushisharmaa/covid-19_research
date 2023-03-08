#####################################################################################
# Example for how to calculate Enhanced 2-Step Floating Catchment Area (E2SFCA)
# This algorithm is a gravity model that assumed bigger hospitals "attract"
#  more patients from the surrounding ZCTAs (ZIP Code Tabulated Areas)
# Emily - 03/08/23 - ATX
#####################################################################################

library(tidyverse)

# Download data and put in INPUT_DATA/PUDF_DATA/ from UT BOX
# https://utexas.box.com/s/ni4iuw6z725s2evlwxdxeiiwbsigofwa
# These files are too large for GitHub and cannot be made available to the public

# All of Texas, so filter to 
tx_hosp_zcta_pairs = read_csv("INPUT_DATA/PUDF_DATA/cov_flu_patient_hosp_zcta_pairs.csv") 

sub_zcta_hosp_pairs = tx_hosp_zcta_pairs %>%
  filter(ZCTA_CITY_NAME %in% c("Houston", "Austin", "Dallas"))

austin_zcta_hosp_pairs = tx_hosp_zcta_pairs %>%
  filter(ZCTA_CITY_NAME == "Austin")
#pull the names of the columns 
names(tx_hosp_zcta_pairs)

# Get the longest drive time experienced by any patient
d0=90 #max(pos_cases$drive_time) # 86 # threshold driving time from i=origin=zip_centroid to j=hospitals

all_dt = rep(pos_cases$drive_time, pos_cases$cases) # repeat all drive times for as many cases went from ZCTA i to hospital j
#write.csv(all_dt, "produced_data/drive_time_example.csv", row.names = F)
cum_prob = rep(0, d0+1) # initialize the cumulative prob vector
for(i in 0:d0){ #i=0
  sub_dt = all_dt[all_dt>=i]
  cum_prob[i+1] = length(sub_dt)/length(all_dt) # proportion of visits that have a certain drive time 
  #print(paste0(sub_dt, " ", cum_prob[i+1]) )
} # end for to get cumulative probability
obs_decay_df = data.frame(d = seq(0, d0, 1), cum_prob = cum_prob)


#######################
#### Set variables ####
#######################
catch_type = "E2SFCA" # Two Step Floating Catchment Area (2SFCA), binary inside or outside
# Enhanced Two Step FCA (E2SFCA), distance/duration decay function
# Add Three Step FCA (3SFCA) and Modified Two Step FCA (M2SFCA)
decay_type = "GAUS"   # EXPonential # GAUSsian # DLL # LCDF # OBServed data

####################################################################
#### Choose weight matrix based on floating catchment algorithm ####
####################################################################

if(catch_type == "2SFCA"){
  # Weight matrix as most basic binary of being inside or outside the catchment range
  W = matrix(as.integer(as.logical(duration<=d0)), nrow = nrow(duration), ncol = ncol(duration))
}else if(catch_type == "E2SFCA"){
  # Weight matrix is now based on a distance/duration decay function rather than binary
  if(decay_type == "EXP"){
    # Exponential decay function
    alpha = as.double(coef(exponential))
    #alpha = -log(0.00001)/d0
    W=exp(-alpha*duration)
    W[duration>d0] = 0
  }else if(decay_type == "GAUS"){
    # Gaussian decay function
    alpha = as.double(coef(gaus))
    #alpha = -(d0^2)/log(0.00001) # 1% prob by the time d0 threshold is reached
    W=exp(-(duration)^2/alpha)
    W[duration>d0] = 0
  }else if(decay_type == "DLL"){
    alpha = as.double(coef(dll))[1]
    beta = as.double(coef(dll))[2]
    W=1/(1+(duration/alpha)^beta)
    W[duration>d0] = 0
  }else if(decay_type == "LCDF"){
    alpha = as.double(coef(lcdf))[1]
    beta = as.double(coef(lcdf))[2]
    W=(1+exp(-alpha/beta))/(1+exp((duration-alpha)/beta))
    W[duration>d0] = 0
  }else if(decay_type == "OBS"){
    
    # This assignment of weights is messed up 
    
    temp_W = duration %>%
      as.data.frame() %>%
      rownames_to_column("ZIP") %>%
      gather(HOSPITAL, d, -ZIP) %>% # d is drive time
      # rename(ZIP=Var1,
      #        HOSPITAL=Var2,
      #        d=value) %>% 
      left_join(obs_decay_df, by="d") %>%
      select(ZIP, HOSPITAL, cum_prob) %>%
      spread(key=HOSPITAL, value=cum_prob, fill=0) # anything missing would be 0, but nothing should be
    
    # convert the first column of ZIPs to rownames then remove from matrix
    rownames(temp_W) = temp_W[,1]
    W = as.matrix(temp_W[,-1])
    
  }# end if for decay funtion
} # end if for algorithm choice


#### Calculate accessibility score for each zip and normalize from 0 to 100 ####

D = t(W) %*% t(t(zip_pop$ADULT_POP)) # Population (D)emand for hospitals; weighted sum of pops in catchment area of each hospital
S = t(t( hosp_address$icu_beds_AHD)) # (S)ervice offered by each hospital; currently staffed beds per hospital
L = S/D # (L)evel of sevice offered; proportion of staffed beds to pop hosp serves
A = W %*% L # (A)ccessibility of zip codes to staffed beds at hospitals within the d0 drive time catchment area
A_df = data.frame(ZIP = rownames(duration), access=A) # Accessibility as data frame of zip codes
A_df = merge(A_df, zip_cases, by="ZIP", all.x = TRUE) # add on cases per zip
A_df$total_cases[is.na(A_df$total_cases)]=0 # zips without cases have 0
A_df = A_df %>%
  mutate(norm_access = (access - min(access))/(max(access) - min(access))*100 )

zip_polygons_new = zip_polygons %>%
  mutate(ZIP = as.character(ZIP)) %>%
  left_join(A_df, by="ZIP")


# Accessibility scores by ZCTA
zip_access = ggplot() +
  geom_sf(data=zip_polygons_new, mapping=aes(geometry=POLYGON, fill=norm_access),
          color="black", size=0.2) +
  scale_fill_gradient(low = "gray96", high = "royalblue2", space = "Lab", na.value = "red",
                      guide = "colourbar", aesthetics = "fill", name="Accessibility")+
  geom_point(data = hosp_address, aes(x=long_x, y=lat_y, size=icu_beds_AHD), shape=10 ) + #shape="triangle",
  #geom_point(data = zip_cent, aes(x=ZCTA_CENT_LONG, y=ZCTA_CENT_LAT), shape=20 )+
  #geom_label(data = zip_cent, aes(x=ZCTA_CENT_LONG, y=ZCTA_CENT_LAT, label=ZIP))+
  labs(size = "ICU Beds", shape="ICU Beds")+
  # scale_shape_manual(labels=c("Hospital"), values = c(1),
  #                    breaks = c("triangle"), name="")+
  theme_void(base_size = 12)+
  theme(plot.margin=unit(c(.5, .7, .5, .5),"cm"))

if(catch_type == "E2SFCA"){
  png(file=paste0("figures/", decay_type, "_", "zcta_accessibility_", d0, ".png"),
      width=8.25,height=8.25, units = "in", res=1200)
  plot(zip_access)
  dev.off()
}else{
  png(file=paste0("figures/", catch_type, "_", "zcta_accessibility_", d0, ".png"),
      width=8.25,height=8.25, units = "in", res=1200)
  plot(zip_access)
  dev.off()
}

# p is the predicted probability people living in zip i will visit hosp j
# (Accessibility of i to j)/(Access i)
p=matrix(0, nrow = nrow(duration), ncol = ncol(duration)) 
for(i in 1:nrow(duration)){ # 
  if(A[i] != 0){
    p[i,] = (W[i,]*L)/A[i] # calculate entire row of predicted probs
  } # end if
} # end for
# p is super sparse but small, so not a problem right now
rownames(p) = rownames(duration)
colnames(p) = colnames(duration)

prob_df_temp = p %>%
  as.data.frame() %>%
  rownames_to_column("org") %>%
  gather(dest, prob_predicted, -org)
#prob_df = setNames(melt(p), c('org', 'dest', 'prob_predicted')) # melt is apparently getting deprecated
hosp_zip_count_old = merge(hosp_zip_count, prob_df, by=c("org", "dest"))

# RI is the observed use of hospitals by zips, the proportion of total cases in zip to each hospital
# pred_cases is the predicted number of cases from zip i to hosp j based on the probability (p)
hosp_zip_count_new=hosp_zip_count_old %>%
  group_by(org) %>%
  mutate(RI_observed = (cases)/(sum(cases)),
         temp_pred_cases = prob_predicted*sum(cases),
         pred_cases = round_preserve_sum(temp_pred_cases)) 
hosp_zip_count_new$RI_observed[is.na(hosp_zip_count_new$RI_observed)]=0














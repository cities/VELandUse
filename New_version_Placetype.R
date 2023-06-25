# Load Sources
#======================
library(sf)
library(dplyr)

# Load SLD database 
SLD3<-st_read("data/SmartLocationDatabase.gdb")
SLD2<-st_read("data/SmartLocationDb.gdb") %>%
  st_transform(st_crs(SLD3))

# Check the nuit of the map
#st_crs(SLD3, parameters = TRUE)$units_gdal
#st_crs(SLD2, parameters = TRUE)$units_gdal

SLD2_sf <- st_as_sf(SLD2)
SLD3_sf <- st_as_sf(SLD3)

#Add Centroid Locations to Smart Location Database
#=================================================
SLD2_point <- st_centroid(SLD2_sf)
SLD3_point <- st_centroid(SLD3_sf)

#Simplify the data
#=================
SLD2_point <- SLD2_point %>% select(c("GEOID10", "SFIPS", "CBSA", "CBSA_Name", "HH","EMPTOT","TOTPOP10",
                                      "E5_RET10","E5_SVC10", "D1D","D2A_JPHH","D3amm","D3apo","D4a", 
                                      "D4c", "D4d"))
SLD3_point <- SLD3_point %>% select(c("GEOID10", "GEOID20", "STATEFP", "CBSA", "CBSA_Name", "HH","TotEmp",
                                      "TotPop","E5_Ret","E5_Svc", "D1D","D2A_JPHH","D3AMM","D3APO",
                                      "D4A", "D4C", "D4D"))

#Tabulate values within specified distances of each block group
#==============================================================
#Some factors that affect place type designations are based on the
#the characteristics of the census tracts surrounding each census tracts. These
#values are calculated for each urban area and added to the datasets.

SLD2_buf_5mile<- st_buffer(SLD2_point, dist = 8046.72) #5 mile buffer
SLD2_buf_2mile<- st_buffer(SLD2_point, dist = 3218.69) #2 mile buffer
SLD3_buf_5mile<- st_buffer(SLD3_point, dist = 8046.72) #5 mile buffer
SLD3_buf_2mile<- st_buffer(SLD3_point, dist = 3218.69) #2 mile buffer

## temp <- aggregate(x = points, by = test, FUN = sum)

Pop_SLD2_buf_5mile <- st_join(SLD2_buf_5mile[,c("GEOID10")], SLD2_point[,c("TOTPOP10")]) %>% 
  st_drop_geometry() %>% #st_drop_geometry() produce the same result using the st_set_geometry(NULL) function
  group_by(GEOID10) %>%
  summarise(Pop_SLD2_buf_5mile = sum(TOTPOP10))

Emp_SLD2_buf_2mile <- st_join(SLD2_buf_2mile[,c("GEOID10")], SLD2_point[,c("EMPTOT")]) %>% 
  st_drop_geometry() %>%
  group_by(GEOID10) %>%
  summarise(Emp_SLD2_buf_2mile = sum(EMPTOT))

Pop_SLD3_buf_5mile <- st_join(SLD3_buf_5mile[,c("GEOID20")], SLD3_point[,c("TotPop")]) %>% 
  st_drop_geometry() %>%
  group_by(GEOID20) %>%
  summarise(Pop_SLD3_buf_5mile = sum(TotPop))

Emp_SLD3_buf_2mile <- st_join(SLD3_buf_2mile[,c("GEOID20")], SLD3_point[,c("TotEmp")]) %>% 
  st_drop_geometry() %>%
  group_by(GEOID20) %>%
  summarise(Emp_SLD3_buf_2mile = sum(TotEmp))

# Extract data.frame from simple features
# Join data.frame from SLD2 and SLD3 with the data of sum of population and employment within the buffer areas 
V2_type <- SLD2_point %>% st_drop_geometry() %>%
  left_join(Pop_SLD2_buf_5mile, by ='GEOID10') %>%
  left_join(Emp_SLD2_buf_2mile, by ='GEOID10')

V3_type <- SLD3_point %>% st_drop_geometry() %>%
  left_join(Pop_SLD3_buf_5mile, by ='GEOID20') %>%
  left_join(Emp_SLD3_buf_2mile, by ='GEOID20')


## split activity density(AD) into 4 levels
##===============================================================================
V2_type$AD_lev<-ifelse(V2_type$D1D<=0.5,"VL",
                       ifelse(V2_type$D1D>0.5 & V2_type$D1D<=5, "L",
                              ifelse(V2_type$D1D>5 & V2_type$D1D<=10, "M","H")))
#table(V2_type$AD_lev)

V3_type$AD_lev<-ifelse(V3_type$D1D<=0.5,"VL",
                       ifelse(V3_type$D1D>0.5 & V3_type$D1D<=5, "L",
                              ifelse(V3_type$D1D>5 & V3_type$D1D<=10, "M","H")))

## destination accessibility (DA)
### calculate harmonic mean of the population within 5 miles and employment within 2 miles
###=======================================================================================
V2_type$V2_DA <- 2/(1/V2_type$Pop_SLD2_buf_5mile + 1/V2_type$Emp_SLD2_buf_2mile)
V3_type$V3_DA <- 2/(1/V3_type$Pop_SLD3_buf_5mile + 1/V3_type$Emp_SLD3_buf_2mile)

### split DA into 4 levels
###=============================================
V2_type$DA_lev<-ifelse(V2_type$V2_DA<=2000,"VL",
                       ifelse(V2_type$V2_DA>2000 & V2_type$V2_DA<=10000, "L",
                              ifelse(V2_type$V2_DA>10000 & V2_type$V2_DA<=50000, "M","H")))

V3_type$DA_lev<-ifelse(V3_type$V3_DA<=2000,"VL",
                       ifelse(V3_type$V3_DA>2000 & V3_type$V3_DA<=10000, "L",
                              ifelse(V3_type$V3_DA>10000 & V3_type$V3_DA<=50000, "M","H")))

## 16 combinations of Area type
##============================================
Areatype_key<-read.csv("data/areatype_key.csv")
V2_type <- merge(V2_type, Areatype_key, by=c("AD_lev", "DA_lev"), all=T)
V3_type <- merge(V3_type, Areatype_key, by=c("AD_lev", "DA_lev"), all=T)


# Development Type
#================================================================
V2_type$dev_type <- ifelse(V2_type$D2A_JPHH >2, 'emp', 
                           ifelse(1/V2_type$D2A_JPHH >=2,'res', 'mix'))

V3_type$dev_type <- ifelse(V3_type$D2A_JPHH >2, 'emp', 
                           ifelse(1/V3_type$D2A_JPHH >=2,'res', 'mix'))

saveRDS(V2_type, file="output/V2_type_new.rds")
saveRDS(V3_type, file="output/V3_type_new.rds")


# Blockgroup Crosswalk
bg_cw<-read.csv("data/nhgis_bg2010_bg2020.csv")
bg_cw$GEOID10 = str_pad(as.character(bg_cw$GEOID10), 15, pad="0")
bg_cw$GEOID20 = str_pad(as.character(bg_cw$GEOID20), 15, pad="0")

bg_cw_1<-bg_cw %>% mutate(GEOID10_bg = substr(GEOID10, 1,12), GEOID20_bg =substr(GEOID20,1,12))



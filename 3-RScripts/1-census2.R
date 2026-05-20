# Data Download -----------------------------------------------------------

da_raw <- download_shp("https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lda_000b21a_e.zip",
                       "input/da_raw.zip")

# download census data
census_raw <- download_csv("https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger/comp/GetFile.cfm?Lang=E&FILETYPE=CSV&GEONO=006_Quebec",
                           1)


quartiers <- download_shp('https://donnees.montreal.ca/dataset/c8f37ad6-16ff-4cdc-9e5a-e47898656fc9/resource/d342d18e-f710-4991-a259-0092bac3d62c/download/quartiers_sociologiques_2014.zip',
                          'input/quartiers.zip')



# Data Prep ---------------------------------------------------------------
da_raw <- da_raw %>% mutate(area_total = set_units(st_area(geometry), km^2))


study_da <- st_transform(quartiers, st_crs(da_raw)) %>% 
  st_intersection(da_raw) %>% 
  mutate(DAUID = as.integer(DAUID),
         areaint = st_area(geometry)/1000000) # area of the DA that intersects w the buffer (sq km)

census_da_f <- census_raw %>%
  select(c("ALT_GEO_CODE","CHARACTERISTIC_ID","C1_COUNT_TOTAL")) %>%
  rename(DAUID = "ALT_GEO_CODE",
         sofac = "CHARACTERISTIC_ID",
         sonum = "C1_COUNT_TOTAL") %>%
  right_join(study_da, by = "DAUID") %>%
  filter(sofac %in% c(1, 6:7, 115, 384:387, 1527:1529, 1544, 1545, 1557, 1574, 1585, 1603, 1683:1684, 1687, 2014:2017, 41:49, 57, 1414:1416))

# we are interested in education, language, income, and ethnicity 

# 1   Population 2021
# 6   Pop density per sq km
# 7   Land area sq km

# Income statistics in 2020 for the population aged 15 years and over in private households - 100% data (10)
# 115   Median after-tax income in 2020 among recipients ($)

# Knowledge of official languages for the total population excluding institutional residents - 100% data (36)
# 384	  English only
# 385	  French only
# 386	  English and French
# 387	  Neither English nor French

# 1527    Total - Immigrant status and period of immigration for the population in private households - 25% sample data (79)
# 1528	  Non-immigrants (80)
# 1529	  Immigrants (81)

# 1544    Total - Place of birth for the immigrant population in private households - 25% sample data (85)
# 1545	  Americas
# 1557	  Europe
# 1574	  Africa
# 1585	  Asia
# 1603	  Oceania and other places of birth (93)

# 2014    Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data (165)
# 2015	  No certificate, diploma or degree
# 2016	  High (secondary) school diploma or equivalency certificate (167)
# 2017	  Postsecondary certificate, diploma or degree

# 1683	Total - Visible minority for the population in private households - 25% sample data (117)
# 1684	  Total visible minority population (118)
# 1697	  Not a visible minority (120)

# 41 Total - Occupied private dwellings by structural type of dwelling - 100% data
# 42 Single-detached house
# 43 Semi-detached house
# 44 Row house
# 45 Apartment or flat in a duplex
# 46 Apartment in a building that has fewer than five storeys
# 47 Apartment in a building that has five or more storeys
# 48 Other single-attached house
# 49 Movable dwelling (4)
# 57 Average household size
# 1414 Total - Private households by tenure - 25% sample data (50)
# 1415 Owner
# 1416 Renter

census_da_w <- census_da_f %>% pivot_wider(names_from = sofac, values_from = sonum)

census_da_r <- census_da_w %>%
  rename(totpop = "1") %>%
  rename(area = "7") %>%
  rename(popdens = "6") %>%
  rename(medinc = "115") %>% 
  rename(en = "384") %>% 
  rename(fr = "385") %>% 
  rename(fr_en = "386") %>% 
  rename(no_fr_en = "387") %>% 
  rename(tot_imm = "1527") %>% 
  rename(non_imm = "1528") %>% 
  rename(imm = "1529") %>% 
  rename(tot_orig = "1544") %>% 
  rename(imm_amer = "1545") %>% 
  rename(imm_eur = "1557") %>% 
  rename(imm_afr = "1574") %>% 
  rename(imm_asia = "1585") %>% 
  rename(imm_oth = "1603") %>% 
  rename(tot_edu = "2014") %>% 
  rename(edu_no = "2015") %>% 
  rename(edu_sec = "2016") %>%
  rename(edu_postsec = "2017") %>% 
  rename(tot_vismin = "1683") %>% 
  rename(vismin = "1684") %>%
  rename(nonvismin = "1687") %>%
  rename(tot_dwel = "41") %>%
  rename(sdhouse = "42") %>%
  rename(smhouse = "43") %>%
  rename(rhouse = "44") %>% 
  rename(duplex = "45") %>% 
  rename(applow = "46") %>% 
  rename(apphigh = "47") %>% 
  rename(ohouse = "48") %>% 
  rename(mov = "49") %>% 
  rename(househ_size = "57") %>% 
  rename(tot_owner_renter = "1414") %>% 
  rename(tot_owner = "1415") %>% 
  rename(tot_renter = "1416") 


census_da_sf <- st_as_sf(census_da_r, sf_column_name = c("geometry"), crs = st_crs(study_da))

census_da_na <- census_da_sf %>% 
  drop_na(Q_socio)  %>%
  filter(area > 0) %>%
  mutate(da = as.factor(DAUID)) %>%
  mutate(across(c(totpop:nonvismin), ~as.numeric(.))) %>%
  select(-c(DGUID, LANDAREA, PRUID, DAUID))

# save for later use 
saveRDS(census_da_na %>% select(c(Q_socio, geometry, areaint, totpop, area, da)), 'output/studyDAs.rds')

# population percentages
can_cen_pp <- census_da_na %>% 
  mutate(per_apart = (applow+apphigh)/totpop,
         per_renter = tot_renter/tot_owner_renter)


# need to calculate the area of the DA that is within the neigbourhood (areaint)
study_cen_pop <- can_cen_pp %>%
  # to calculate the approximate population within the neighbourhood bounds (assuming equal density throughout the DA)
  # divide the intersected area/total area of DA and multiply the population by that 
  # can then use this population as weight for weighted means
  mutate(popwithin = (as.numeric(areaint)/as.numeric(area))*as.numeric(totpop)) %>% 
  select(c("Q_socio","da","geometry","totpop", "popwithin", "area", "areaint",
           "per_house", "per_apart", "per_renter"))

# population weighted mean
study_cen <- study_cen_pop %>%
  group_by(da) %>%   
  summarize(DAcount = n(),
            totarea = sum(areaint),
            geometry = st_union(geometry),
            per_house = weighted.mean(as.numeric(per_house), as.numeric(popwithin), na.rm = T),
            per_apart = weighted.mean(as.numeric(per_apart), as.numeric(popwithin), na.rm = T),
            per_duplex = weighted.mean(as.numeric(per_duplex), as.numeric(popwithin), na.rm = T),
            per_mov = weighted.mean(as.numeric(per_mov), as.numeric(popwithin), na.rm = T),
            per_owner = weighted.mean(as.numeric(per_owner), as.numeric(popwithin), na.rm = T),
            per_renter = weighted.mean(as.numeric(per_renter), as.numeric(popwithin), na.rm = T),
            househ_size = weighted.mean(as.numeric(househ_size), as.numeric(popwithin), na.rm = T),
            popwithin = sum(as.numeric(popwithin))
  ) %>%
  distinct(Q_socio, .keep_all = TRUE)

saveRDS(study_cen, 'output/quartiers_census_new_method.rds')
write.csv(study_cen %>% st_drop_geometry(), 'output/quartiers_census_new_method.csv', row.names = FALSE)

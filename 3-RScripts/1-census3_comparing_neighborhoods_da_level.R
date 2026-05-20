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
#  filter(sofac %in% c(1, 6:7, 115, 384:387, 1527:1529, 1544, 1545, 1557, 1574, 1585, 1603, 1683:1684, 1687, 2014:2017, 41:49, 57, 1414:1416))
  filter(sofac %in% c(1, 7, 115, 1683, 1684, 2014, 2017, 41, 46, 47, 1414, 1416))

# we are interested in median income, % post-sec ed, % visible minority, % living in apartments, % renters

# 1   Population 2021
# 7   Land area sq km

# Income statistics in 2020 for the population aged 15 years and over in private households - 100% data (10)
# 115   Median after-tax income in 2020 among recipients ($)

# 2014    Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data (165)
# 2015	  No certificate, diploma or degree

# 1683	Total - Visible minority for the population in private households - 25% sample data (117)
# 1684	  Total visible minority population (118)

# 41 Total - Occupied private dwellings by structural type of dwelling - 100% data
# 46 Apartment in a building that has fewer than five storeys
# 47 Apartment in a building that has five or more storeys

# 1414 Total - Private households by tenure - 25% sample data (50)
# 1416 Renter

census_da_w <- census_da_f %>% pivot_wider(names_from = sofac, values_from = sonum)

census_da_r <- census_da_w %>%
  rename(totpop = "1") %>%
  rename(area = "7") %>%
  rename(medinc = "115") %>% 
  rename(tot_edu = "2014") %>% 
  rename(edu_postsec = "2017") %>% 
  rename(tot_vismin = "1683") %>% 
  rename(vismin = "1684") %>%
  rename(tot_dwel = "41") %>%
  rename(applow = "46") %>% 
  rename(apphigh = "47") %>% 
  rename(tot_owner_renter = "1414") %>% 
  rename(tot_renter = "1416") 


census_da_sf <- st_as_sf(census_da_r, sf_column_name = c("geometry"), crs = st_crs(study_da))

census_da_na <- census_da_sf %>% 
  drop_na(Q_socio)  %>%
  filter(area > 0) %>%
  mutate(da = as.factor(DAUID)) %>%
#  mutate(across(c(totpop:nonvismin), ~as.numeric(.))) %>%
  select(-c(DGUID, LANDAREA, PRUID, DAUID))

# save for later use 
saveRDS(census_da_na %>% select(c(Q_socio, geometry, areaint, totpop, area, da)), 'output2/studyDAs2.rds')

# population percentages
can_cen_pp <- census_da_na %>% 
  mutate(per_apart = (applow+apphigh)/totpop,
         per_edu_postsec = edu_postsec/tot_edu,
         per_renter = tot_renter/tot_owner_renter,
         per_vismin = vismin/tot_vismin)

saveRDS(can_cen_pp, 'output/dataset_for_maps.rds')
write.csv(can_cen_pp %>% st_drop_geometry(), 'output/dataset_for_maps.csv', row.names = FALSE)

#Building maps ---------------------------------------------------------------------------------------
df_long <- pivot_longer(data = dataset_for_maps, cols = c(per_apart, per_edu_postsec, per_renter, per_vismin))
ggplot() + 
#  basemap_gglayer(ext = st_bbox(dataset_for_maps), map_service = "carto", map_type = "light") 
  geom_sf(aes(fill = value), linewidth = 0, data = df_long) + #color is the outline and fill is the inside
  geom_sf(fill = NA, colour = "black", data = quartiers, linewidth = 0.75) +
  scale_fill_viridis_c(option = "A", direction = -1) + #viridis is forcolor blindness and fill changes the fill categ where aes is
  theme_classic() +
  facet_wrap(~name) 

ggsave('output/maps.png', width = 12, height = 12, units = 'in')

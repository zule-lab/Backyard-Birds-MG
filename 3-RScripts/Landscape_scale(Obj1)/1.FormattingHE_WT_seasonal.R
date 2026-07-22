library(tidyverse)




file_path <- "1-Input/2025_all_results" #this links to the folder with all your resulting hawkears txt files. They should all be in one folder
file_list <- list.files(path = file_path, pattern = "\\.txt$", full.names = TRUE)

column_names <- c("Start (s)", "End (s)", "Common name")

species_list_df <- read.csv("1-Input/species_list.csv", sep = "_")
species_list <- c(species_list_df$Common.name)



all_HEdata <- lapply(file_list, function(file) {
  # Check if the file is empty
  if (file.info(file)$size == 0) {
    message("Skipping empty file: ", file)
    return(NULL)
  }
  # Read the current file, without headers, assuming columns are space/tab separated
  temp_data <- read.table(file, header = FALSE, sep = "\t", fill = TRUE, quote = "", stringsAsFactors = FALSE)
  
  # Assign the specified column names
  colnames(temp_data) <- column_names
  
  # Split the "Common name" column (3rd column) into two new columns based on the ";" separator
  # Create two new columns: "Common name part 1" and "Common name part 2"
  temp_data <- temp_data %>%
    tidyr::separate("Common name", into = c("Common name", "Confidence"), sep = ";", extra = "merge", fill = "right")
  
  # Add the filename as a new column
  temp_data$filename <- basename(file)  # Extract just the file name (without path)
  
  return(temp_data)
}) %>%
  bind_rows() %>%
  mutate(location=sub("_.*", "", filename)) %>% #this substracts only the portion of the filename that comes before the underscore
  mutate(datetime_software = sub("^[^_]*_", "", filename))  %>% #this substracts only the portion of the filename that comes after the underscore
  ##date formatting
  mutate(year=substr(datetime_software,1,4))%>% #this is based on the length of file names which is why I transformed first everything to standard 4 letter site names
  mutate(month=substr(datetime_software,5,6))%>%
  mutate(day=substr(datetime_software,7,8))%>%
  mutate(date=as.Date(paste0(year,"/",month,"/",day)))%>%
  mutate(rec_time=paste0(substr(datetime_software,10,11), ":", substr(datetime_software,12,13), ":", substr(datetime_software,14,15)))%>%
  mutate(recordingDate= paste0(date, " ", rec_time)) %>%
  dplyr::rename(startTime='Start (s)',end='End (s)',
                common='Common name',confidence=Confidence)  %>%
  filter(common%in%species_list)

all_HEdata$location_code <- all_HEdata$location


#==========================================================#
                         # SPRING #
#==========================================================#

spring_HE_2025 = dplyr::filter(all_HEdata, as.numeric(month) <= 05) %>% 
  mutate(tag_duration = end - startTime)


banding_codes <- read.csv("1-Input/species_codes_WT.csv")

spring_HE_2025 <- spring_HE_2025 %>%
  left_join(banding_codes, by = "common")

unique(spring_HE_2025$species)

spring_HE_2025_sub <- subset(spring_HE_2025, confidence >= 0.9)

#keep only the 3 highest confidence detections for each species per ARU location, and remove repeated detections of species within the same recording
spring2025_TOP3 <- spring_HE_2025_sub %>% arrange(desc(confidence)) %>% distinct(filename, common, .keep_all = TRUE)%>%
  group_by(location_code, common) %>%
  slice(1:3)

# Cutting the sites that I'm not using for Spring

spring2025_TOP3 <- spring2025_TOP3 %>% subset(location != "WILB" | 
                                                location != "GEOR" |
                                                location != "BENN" |
                                                location != "RY49" |
                                                location != "RY45" )


write.csv(spring2025_TOP3, file = "3-Output/spring2025_TOP3.csv", row.names=FALSE)

spring2025_TOP3 <- spring2025_TOP3 %>%
  rename(tag_start_time = startTime, recording_date_time = recordingDate, species_code = species )

spring2025_TOP3$task_method = "1SPT"
spring2025_TOP3$task_duration = "599.85"
spring2025_TOP3$observer = "Not Assigned"
spring2025_TOP3$individual_number = "1"
spring2025_TOP3$vocalization = "Song"
spring2025_TOP3$abundance = "1"
spring2025_TOP3$min_tag_freq = "0"
spring2025_TOP3$max_tag_freq = "12000"
spring2025_TOP3$species_individual_comments =""
spring2025_TOP3$internal_tag_id = ""
spring2025_TOP3$tag_is_hidden_for_verification = "f"
spring2025_TOP3$recording_sample_frequency = "44100"

WT_SPRINGupload_2025 <- spring2025_TOP3
WT_SPRINGupload_2025$end <- NULL
WT_SPRINGupload_2025$common <- NULL
WT_SPRINGupload_2025$confidence <- NULL
WT_SPRINGupload_2025$filename <- NULL
WT_SPRINGupload_2025$datetime_software <- NULL
WT_SPRINGupload_2025$year <- NULL
WT_SPRINGupload_2025$month <- NULL
WT_SPRINGupload_2025$day <- NULL
WT_SPRINGupload_2025$date <- NULL
WT_SPRINGupload_2025$rec_time <- NULL
WT_SPRINGupload_2025$location_code <- NULL
WT_SPRINGupload_2025$site <- NULL

 

WT_SPRINGupload_2025 <- WT_SPRINGupload_2025%>%
  mutate(tag_duration = ifelse(tag_duration > 30, 29, tag_duration))


WT_SPRINGupload_2025 <- WT_SPRINGupload_2025 %>%
  anti_join(
    WT_ALLupload_2025,
    by = c(
      "location",
      "species_code",
      "recording_date_time",
      "tag_start_time"))


write.csv(WT_SPRINGupload_2025, "3-Output/WT_SPRINGupload_2025.csv", row.names=FALSE )






#====================================================================#
                           # SUMMER #
#====================================================================#


summer_HE_2025 = dplyr::filter(all_HEdata, as.numeric(month) >= 06) %>% 
  mutate(tag_duration = end - startTime)



summer_HE_2025 <- summer_HE_2025 %>%
  left_join(banding_codes, by = "common")

unique(summer_HE_2025$species)

summer_HE_2025_sub <- subset(summer_HE_2025, confidence >= 0.9)

#keep only the 3 highest confidence detections for each species per ARU location, and remove repeated detections of species within the same recording
summer2025_TOP3 <- summer_HE_2025_sub %>% arrange(desc(confidence)) %>% distinct(filename, common, .keep_all = TRUE)%>%
  group_by(location_code, common) %>%
  slice(1:3)

# Cutting the sites that I'm not using for Summer

summer2025_TOP3 <- summer2025_TOP3 %>% subset(location != "WILB" | 
                                                location != "GEOR" |
                                                location != "BENN" |
                                                location != "RY49" |
                                                location != "RY45" |
                                                location != "LOYP" |
                                                location != "TREN" |
                                                location != "WILH" |
                                                location != "GILB" |
                                                location != "Y51" )

write.csv(summer2025_TOP3, file = "3-Output/summer2025_TOP3.csv", row.names=FALSE)

summer2025_TOP3 <- summer2025_TOP3 %>%
  rename(tag_start_time = startTime, recording_date_time = recordingDate, species_code = species )

summer2025_TOP3$task_method = "1SPT"
summer2025_TOP3$task_duration = "599.85"
summer2025_TOP3$observer = "Not Assigned"
summer2025_TOP3$individual_number = "1"
summer2025_TOP3$vocalization = "Song"
summer2025_TOP3$abundance = "1"
summer2025_TOP3$min_tag_freq = "0"
summer2025_TOP3$max_tag_freq = "12000"
summer2025_TOP3$species_individual_comments =""
summer2025_TOP3$internal_tag_id = ""
summer2025_TOP3$tag_is_hidden_for_verification = "f"
summer2025_TOP3$recording_sample_frequency = "44100"

WT_SUMMERupload_2025 <- summer2025_TOP3
WT_SUMMERupload_2025$end <- NULL
WT_SUMMERupload_2025$common <- NULL
WT_SUMMERupload_2025$confidence <- NULL
WT_SUMMERupload_2025$filename <- NULL
WT_SUMMERupload_2025$datetime_software <- NULL
WT_SUMMERupload_2025$year <- NULL
WT_SUMMERupload_2025$month <- NULL
WT_SUMMERupload_2025$day <- NULL
WT_SUMMERupload_2025$date <- NULL
WT_SUMMERupload_2025$rec_time <- NULL
WT_SUMMERupload_2025$location_code <- NULL
WT_SUMMERupload_2025$site <- NULL



WT_SUMMERupload_2025 <- WT_SUMMERupload_2025%>%
  mutate(tag_duration = ifelse(tag_duration > 30, 29, tag_duration))

WT_SUMMERupload_2025 <- WT_SUMMERupload_2025 %>%
  anti_join(
    WT_ALLupload_2025,
    by = c(
      "location",
      "species_code",
      "recording_date_time",
      "tag_start_time"))


write.csv(WT_SUMMERupload_2025, "3-Output/WT_SUMMERupload_2025.csv", row.names=FALSE )








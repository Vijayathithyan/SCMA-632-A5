# Cleaned data set of the assigned state Bihar is used 

setwd("D:\\MDA\\Course\\Boot Camp\\SCMA 632\\Assignments\\A5")
bhr = read.csv("BHR.csv")

list(bhr)

View(bhr$state_1)

unique(bhr['state_1'])

is.na(bhr)

install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA")
lapply(libraries, install_and_load)


# Display dataset info
cat("Dataset Information:\n")
print(names(bhr))
print(head(bhr))
print(dim(bhr))

# Sub-setting the data
bhrnew <- bhr %>%
  select(state_1, District, Region, Sector, State_Region, fv_tot, 
         nonvegtotal_q, fruitstt_q, sugartotal_q, spicetot_q, teatotal_q, 
         Beveragestotal_q, pulsestt_q
         )

View(bhrnew)

# Check for missing values in the subset
cat("Missing Values in Subset:\n")
print(colSums(is.na(bhrnew)))

#Total consumption 
bhrnew$total_consumption <- rowSums(bhrnew[, c("fv_tot", "nonvegtotal_q", "fruitstt_q", "sugartotal_q",
                                               "spicetot_q", "teatotal_q", 
                                               "Beveragestotal_q", "pulsestt_q")], na.rm = TRUE)

bhrnew$total_consumption

# Summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- bhrnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
View(district_summary)

unique(bhrnew$District)

# Rename districts and sectors
district_mapping <- c("1" = "Pashchim Champaran","2" = "Purba Champaran",
                      "3" = "Sheohar","4" = "Sitamarhi",
                      "5" = "Madhubani","6" = "Supaul","7" = "Araria","8" = "Kishanganj","9" = "Purnia",
                      "10" = "Katihar","11" = "Madhepura","12" = "Saharsa","13" = "Darbhanga",
                      "14" = "Muzaffarpur", "15" = "Gopalganj", "16" = "Siwan", "17" = "Saran",
                      "18" = "Vaishali","19" = "Samastipur","20" = "Begusarai",
                      "21" = "Khagaria","22" = "Bhagalpur","23" = "Banka","24" = "Munger",
                      "25" = "Lakhisarai","26" = "Sheikhpura","27" = "Nalanda","28" = "Patna",
                      "29" = "Bhojpur","30" = "Buxar","31" = "Kaimur","32" = "Rohtas",
                      "33" = "Jehanabad","34" = "Aurangabad","35" = "Gaya","36" = "Nawada",
                      "37" = "Jamui","38" = "Arwal")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

bhrnew$District <- as.character(bhrnew$District)
bhrnew$Sector <- as.character(bhrnew$Sector)
bhrnew$District <- ifelse(bhrnew$District %in% names(district_mapping), district_mapping[bhrnew$District], bhrnew$District)
bhrnew$Sector <- ifelse(bhrnew$Sector %in% names(sector_mapping), sector_mapping[bhrnew$Sector], bhrnew$Sector)

View(bhrnew)

library(ggplot2)

bhr_consumption <- aggregate(total_consumption ~ District, data = bhrnew, sum) 
View(bhr_consumption)

#Ploting
barplot(bhr_consumption$total_consumption, 
        names.arg = bhr_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'lightpink', 
        border = 'lightblue', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption in Quantities per District",
        cex.names = 0.5) # Adjust the size of district names if needed

# b) Plot on the Bihar state map using NSSO68.csv data

library(ggplot2) 
library(sf) # mapping
library(dplyr) 
Sys.setenv("SHAPE_RESTORE_SHX" = "YES") 

data_map <- st_read("D:\\MDA\\Course\\Boot Camp\\SCMA 632\\Assignments\\A5\\BIHAR_DISTRICTS.geojson") 
View(data_map)

data_map <- data_map %>% 
  rename(District = dtname) 

colnames(data_map) 

data_map_data <- merge(bhr_consumption,data_map,by = "District") 

colnames(data_map_data)

View(data_map_data)

ggplot(data_map_data) + 
  geom_sf(aes(fill =total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption_by_District") 

ggplot(data_map_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption by District") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")


source("resident_text_function.R")

#read in data on grads. this data is from CMF application. Update this to accommodate graduate data
residents <- read.csv("awardees.csv", stringsAsFactors = FALSE)

#remove the combined programs and only keep the individual discipline
residents <- residents %>% filter(Remove != "Yes")

#Group by county to get overall county data
resident_county <- residents %>% 
  group_by(County) %>% 
  summarize(n = sum(Total.Positions.Awarded))

#group grads by county and discipline to get discipline level data
resident_pt <- residents %>%
  group_by(County, Discipline) %>% 
  summarize(Positions = sum(Total.Positions.Awarded))

#get totals by discipline
fm_Residents <- sum(resident_pt$Positions[resident_pt$Discipline == "FM"])
im_Residents <- sum(resident_pt$Positions[resident_pt$Discipline == "IM"])
em_Residents <- sum(resident_pt$Positions[resident_pt$Discipline == "EM"])
ob_Residents <- sum(resident_pt$Positions[resident_pt$Discipline == "OBGYN"])
peds_Residents <- sum(resident_pt$Positions[resident_pt$Discipline == "Peds"])

#rm(Grad_cali)

# create table for grad text
counties <- unique(resident_pt$County)

residenttexts <- NULL


for (i in counties) {
  
  d2 <- filter(resident_pt, County == i)
  
  residenttexts[i] <- resident_text(d2)
  
}

df <- data.frame(County = counties, mytext = residenttexts)

#remove any unnecessary data
rm(list = c("d2", "i", "residenttexts", "counties", "resident_pt"))

# read in cali map
cali <- counties("California", cb = TRUE)

colnames(cali)[6] <- "County"

# merge cali map with total_set

resident_cali <- left_join(cali, df, by = "County")

resident_cali <- left_join(resident_cali, resident_county, by = "County")

rm(list = c("df", "cali", "resident_county", "resident_text"))

#join the county map with grad discipline data
resident_cali$mytext[is.na(resident_cali$mytext)] <- "No resident spots awarded in this County"
#grads_cali$n[is.na(grads_cali$n)] <- 0

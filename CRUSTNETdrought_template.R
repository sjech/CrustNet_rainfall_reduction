# Project: CrustNet Rainfall Reduction (Tier 3)
# Code Author: Sierra Jech
# Date: May 2025
# Version: 1

# Purpose: Calculate the amount of precipitation that is equivalent to a 1 in 100 year drought for your CrustNet site. Data may be obtained through the SC-ACIS website (https://scacis.rcc-acis.org/). Download data for the period of record for the Daily Data Listing, using the closest weather station to your site or the station with the longest period of record if there are two close stations. 

# Copy the data to Excel, expand the csv to columns, modify the date to a usable format, and modify the year to be in its own column. There is a note at the end of each data file on the website indicating the level of certainty in the data up to a certain year. Consider removing data points before that year if necessary.


#Load Libraries
library(dplyr)
library(ggplot2)

# Inport Data
node <- read.csv("SC_ACIS_USC00247894_por_precip.csv") # change this to your csv file name

#Change data types
str(node)
node$Precipitation <- as.numeric(node$Precipitation)

# Summarize to year level
node_sum <- node %>%
  group_by(Year) %>%
  summarise(an_precip = sum(Precipitation, na.rm = T))

# Plot it
node_sum %>% 
  filter(!Year %in% c("1911","2024","2025")) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = Year, y = an_precip))

#histogram
node_sum %>% 
  filter(!Year %in% c("1911","2024","2025")) %>% 
  ggplot() +
  geom_histogram(mapping = aes(an_precip), binwidth = 0.25)

# calculate quantiles
quantile(node_sum$an_precip)

# calculate the 1% quantile
quantile(node_sum$an_precip, probs = 0.01, na.rm = TRUE) #1% quantile is represented as 0.01
quantile(node_sum$an_precip) #gives several common quantile cutoffs
quantile(node_sum$an_precip, probs = .25, na.rm = TRUE) #25% is represented as 0.25
quantile(node_sum$an_precip, probs = .1, na.rm = TRUE) #10% is represented as 0.1
# 1 in 100 should be 1% I think


# The 1% quantile is 2.9 inches
quantile(node_sum$an_precip)
 
#histogram with 1% quantile
node_sum %>% 
  filter(!Year %in% c("1911","2024","2025")) %>% #you may need to adjust these to your data. I removed these years that had partial data, and thus an incorrect annual precipitation value which could not be included in the dataset at the year-level
  ggplot() +
  geom_histogram(mapping = aes(an_precip), binwidth = 0.25)+
  geom_vline(xintercept = 2.95, linetype = "dotted")

# The mean annual precipitation is:
mean(node_sum$an_precip)

# So if the mean annual precipitation is 11.89 and the 1% drought event is 2.9 then we want to let through:
2.9/11.89
# 24.4 % which means you need to block:
100-24.4
# 75.6% of the precipitation

# determine the number of slats needed to cover 75.6% of 74 cm (length of the roof)
0.756 * 74
# 56 cm, but each slat is 1.8 cm, so we need
56/1.8
# 31 slats in that 74 cm space. 

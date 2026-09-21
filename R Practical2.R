# practical 1: FILTERING DATA

getwd()
## load library
library(tidyverse)

# Import the Example-02 CSV dataset
example02 <- read.csv("Example-02.csv")
example02

# FILTER DATA BY LOCATION
# Select observations from Nambour OR RedlandBay
filter(example02, loc == "Nambour" | loc == "RedlandBay")

# FILTERING USING MULTIPLE CONDITIONS
# Select G01, G57, and G58
# Only from Brookstead in the year 1970
filter(example02, gen == "G01" | gen == "G57" | gen == "G58", loc == "Brookstead",
       year == 1970)

# Select observations where: Yield is between 2 and 3,Oil content is greater than 22 and Location is Lawes
filter(example02, yield >=2, yield <=3, oil > 22, loc == "Lawes")

# practical 2: # PRACTICAL 2: DATA IMPORT, EXPLORATION AND TRANSFORMATION
#Load libraries
library(tidyverse)
library(readxl)
# Import Example-03 
example03 <- read_xlsx("Example-03.xlsx")
example03
# Import Example-03 
str(example03)

# Group the data by location, then count the number of observations in each location
example03 %>%     
  group_by(loc) %>%     
  summarise(n = n())
sum(sapply(example03, is.numeric))
sum(!sapply(example03, is.numeric))
example03$loc <- as.factor(example03$loc)
example03$gen <- as.factor(example03$gen)
length(unique(example03$loc))
sort(unique(example03$loc))
length(unique(example03$gen))
sort(unique(example03$gen))
filter(example03, yield < 150)
example03 %>%
  group_by(gen) %>%
  summarise(earht_genotype = mean(earht, na.rm = TRUE)) %>%
  arrange(desc(earht_genotype))
example03.short <- example03 %>% 
  select(loc, gen,yield,flower) %>%
  mutate(flower_new = flower - 10)
head(example03.short)

# practical 3: DATA SUMMARY AND DESCRIPTIVE STATISTICS

#Load library
library(tidyverse)
##Import the data
example02 <- read.csv("Example-02.csv")
example02

# Group data by year and Count the number of different locations in each year
example02 %>%     
  group_by(year) %>%     
  summarise(n_locations = n_distinct(loc))

# Select unique combinations of year and location
# Arrange them in ascending order of year and location
example02 %>%
  distinct(year,loc) %>%
  arrange(year,loc)

# Group data by location and year
# Calculate several descriptive statistics for size
example02 %>%
group_by(loc,year) %>%
summarise(observations = n(),
          min_size = min(size, na.rm = TRUE),
          max_size = max(size, na.rm = TRUE),
          mean_size = mean(size, na.rm = TRUE),
          variance = var(size, na.rm = TRUE),
          sd = sd(size, na.rm = TRUE),
          .groups = "drop")

# Select observations from Brookstead where:Oil > 20,Lodging < 3 and Yield > 3
## Then:Arrange observations from highest to lowest height and Select only the required columns
example02 %>%
  filter(loc == "Brookstead", oil > 20, lodging < 3, yield > 3) %>%
  arrange(desc(height)) %>%
  select(env,year,gen,yield,height,lodging,oil)

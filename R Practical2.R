# practical 1

getwd()
library(tidyverse)

example02 <- read.csv("C:/Users/Stephen/Desktop/OLUCHI/Example-02.csv")
example02
filter(example02, loc == "Nambour" | loc == "RedlandBay")
filter(example02, gen == "G01" | gen == "G57" | gen == "G58", loc == "Brookstead",
       year == 1970)
filter(example02, yield >=2, yield <=3, oil > 22, loc == "Lawes")

# practical 2

library(tidyverse)
install.packages("readxl")

example03 <- read_xlsx("C:/Users/Stephen/Desktop/OLUCHI/Example-03.xlsx")
example03
str(example03)

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

# practical 3

library(tidyverse)
example02 <- read.csv("C:/Users/Stephen/Desktop/OLUCHI/Example-02.csv")
example02
example02 %>%     
  group_by(year) %>%     
  summarise(n_locations = n_distinct(loc))
example02 %>%
  distinct(year,loc) %>%
  arrange(year,loc)
example02 %>%
group_by(loc,year) %>%
summarise(observations = n(),
          min_size = min(size, na.rm = TRUE),
          max_size = max(size, na.rm = TRUE),
          mean_size = mean(size, na.rm = TRUE),
          variance = var(size, na.rm = TRUE),
          sd = sd(size, na.rm = TRUE),
          .groups = "drop")
example02 %>%
  filter(loc == "Brookstead", oil > 20, lodging < 3, yield > 3) %>%
  arrange(desc(height)) %>%
  select(env,year,gen,yield,height,lodging,oil)

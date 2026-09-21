getwd()
# session 3 first practical
# practical 1: Australia soybean Data set

# Question 1: Import the Australia Soybean Data set

soybean_data <- read.csv("C:/Users/Stephen/Desktop/OLUCHI/australia.soybean.csv")
soybean_data

# Question 2: Scatter Plot of Oil Content vs Protein Content
# by Location
library(tidyverse)
library(ggplot2)
p <- ggplot(soybean_data) +
  aes(x = oil, y = protein, colour = loc) +
  geom_point(shape = 17, size = 3) +
  labs(
    title = "Relationship Between Oil and Protein Content",
    subtitle = "Soybean observations by location",
    caption = "Data: agridat::Australia.soybean",
    x = "Oil Content (%)",
    y = "Protein Content (%)"
  )
p

# Question 3: Scatter Plot of Oil Content vs Protein Content
# Using Separate Panels for Each Location

library(ggplot2)

p <- ggplot(soybean_data, aes(x = oil, y = protein)) +
  geom_point(shape = 17, size = 3, colour = "steelblue") +
  facet_wrap(~ loc) +
  labs(
    title = "Relationship Between Oil and Protein by Location",
    subtitle = "Scatter plots for each experimental location",
    caption = "Data: agridat::Australia.soybean",
    x = "Oil Content (%)",
    y = "Protein Content (%)"
  )

p

# practical 2: White corn Dataset
# Question 1: Import the white corn Dataset

white_corn <- read.csv("C:/Users/Stephen/Desktop/OLUCHI/ars.earlywhitecorn96.csv")
white_corn

# Question 2: Scatter Plot of Ear Height and Yield
# by Location

library(ggplot2)

ggplot(white_corn, aes(x = earht, y = yield, colour = loc)) +
  geom_point(shape = 17, size = 3) +
  labs(
    title = "Relationship Between Earth Height and Yield",
    subtitle = "White corn yield by location",
    caption = "Data: White corn dataset",
    x = "Earth Height",
    y = "Yield"
  )

# Question 3: Histogram and Density Plot of Yield
# by Location

ggplot(white_corn, aes(x = yield)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 15,
    fill = "skyblue",
    colour = "black"
  ) +
  geom_density(colour = "red", linewidth = 1) +
  facet_wrap(~ loc) +
  labs(
    title = "Distribution of Yield by Location",
    subtitle = "Histogram and density curve",
    caption = "Data: White corn dataset",
    x = "Yield",
    y = "Density"
  )

# Question 3: Boxplot of Yield by Location

ggplot(white_corn, aes(x = loc, y = yield, fill = loc)) +
  geom_boxplot() +
  labs(
    title = "Yield by Location",
    subtitle = "Boxplots of yield across locations",
    caption = "Data: White corn dataset",
    x = "Location",
    y = "Yield"
  ) +
  theme(legend.position = "none")


library(tidyverse)
# practical 3: Iris Dataset
# Question 1: Import the Iris Dataset
iris_data <- read_xlsx("C:/Users/Stephen/Desktop/OLUCHI/iris.xlsx")
iris_data

# QUESTION 2: Summarize the Iris Dataset by Species
# Calculate the mean, standard deviation, variance,
# and number of observations for each species

library(dplyr)
iris_data %>%
  group_by(Species) %>%
  summarise(
    Mean_Sepal_Length = mean(Sepal.Length),
    SD_Sepal_Length = sd(Sepal.Length),
    Var_Sepal_Length = var(Sepal.Length),
    
    Mean_Sepal_Width = mean(Sepal.Width),
    SD_Sepal_Width = sd(Sepal.Width),
    Var_Sepal_Width = var(Sepal.Width),
    
    Mean_Petal_Length = mean(Petal.Length),
    SD_Petal_Length = sd(Petal.Length),
    Var_Petal_Length = var(Petal.Length),
    
    Mean_Petal_Width = mean(Petal.Width),
    SD_Petal_Width = sd(Petal.Width),
    Var_Petal_Width = var(Petal.Width),
    
    Number_of_Observations = n()
  )

# QUESTION 3: Scatter Plot of Sepal Length and Petal Length
# by Species
library(tidyverse)

library(ggplot2)

ggplot(iris_data,
       aes(x = Sepal.Length,
           y = Petal.Length,
           colour = Species)) +
  geom_point(size = 3) +
  labs(
    title = "Relationship Between Sepal Length and Petal Length",
    subtitle = "Iris flower species",
    caption = "Data: Iris dataset",
    x = "Sepal Length (cm)",
    y = "Petal Length (cm)"
  )

# QUESTION 4: Scatter Plot with Regression Lines
# for Each Iris Species

ggplot(iris_data,
       aes(x = Sepal.Length,
           y = Petal.Length,
           colour = Species)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship Between Sepal Length and Petal Length",
    subtitle = "Regression line for each species",
    caption = "Data: Iris dataset",
    x = "Sepal Length (cm)",
    y = "Petal Length (cm)"
  )


# QUESTION 5: Boxplot of Sepal Width by Species

ggplot(iris_data,
       aes(x = Species,
           y = Sepal.Width,
           fill = Species)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Sepal Width by Species",
    x = "Species",
    y = "Sepal Width (cm)"
  ) +
  theme(legend.position = "none")

# linear model Regression practical

library(tidyverse)
# Load the required packages
library(agridat)


# Save the dataset as an object
barley_data <- read.csv("C:/Users/Stephen/Desktop/OLUCHI/steptoe.morex.pheno.csv")
barley_data

# Convert the data frame to a tibble
barley_data <- as_tibble(barley_data)

# Display the tibble
barley_data

library(ggplot2)

# Scatter plot of Height against Yield
ggplot(barley_data, aes(x = height, y = yield)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship Between Height and Yield",
    x = "Height",
    y = "Yield"
  )

# Fit a simple linear regression model
Yield_height <- lm(yield ~ height, data = barley_data)

# Display the regression results
summary(Yield_height)

# Fit a multiple linear regression model
Yield_height_protein <- lm(yield ~ height + protein, data = barley_data)

# Display the regression results
summary(Yield_height_protein)


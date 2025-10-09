

library(tinytex)

library(tidyverse)

library(dslabs)

library(dplyr)

library(ggplot2)


library(tibble)

library(modelsummary)

library(broom)

library(here)

# install.packages("devtools")       
# devtools::install_github("username/repo")



# Basics of R/R Studio


setwd("C:/Users/sonan/OneDrive/Documents/ECON-320-Fall-2025-GE")



here()

#data_file_path <- here("data")

#print(data_file_path)

penguins <- here("ECON-320-Fall-2025-GE", "data", "penguins.csv")


data <- read.csv(penguins)

head(data)

data("mtcars")

head(mtcars)





runs <- c(12, 65, 8, 55, 27, 15, 4, 5, 22, 30, 17, 
          10, 40, 7, 29, 38, 5,
          23, 18, 20, 9, 25, 22, 0, 0)


summary(runs)

avg <- mean(runs)


which.max(runs)

# sorting: 
sort(runs, decreasing = TRUE)

# Position of Sorted Numbers:
order(runs, decreasing = TRUE)



## Basic Vector Operations


runs[c(1:20)]/20

average_first_20 <- mean(runs[c(1:20)])

average_first_5 <- mean(runs[c(1:5)])

# Percentiles:

quantile(runs, c(0.1, 0.5, 0.9, 0.99))

cat("Batting Average In First 20 Matches:", 
    average_first_20, "\n")

cat("Batting Average In First 5 Matches:", 
    average_first_5, "\n")



data <- read.csv("US_GDP_NX.csv")


head(data)

tail(data)

data <- ts(data, start = c(1947,1), frequency = 3)

data <- data.frame(data)




data$Quarter <- seq(from = as.Date("1947-01-01"), 
                    to = as.Date("2020-12-01"), 
                    by = 'quarter')


colnames(data) <- c("GDP", "NX", "index", "Quarter")


View(data)



# Create new variable with natural log

data$log_gdp <- log(data$GDP)


write.csv(data, "output.csv", row.names = FALSE)



## Binding 1
  
 
country <- c("USA", "India", "Argentina", "Sudan")

WB_classification <- c("High Income", 
                       "Lower Middle Income",
                       "Upper Middle Income", "Low Income")

rbind(country, WB_classification)

cbind(country, WB_classification)

cc <- cbind(country, WB_classification)



chdi <- cbind(country, hdi_rank = c(17, 130, 47, 176))
chdi

combined <- cbind(cc, chdi) # does capture column name of latter

combined <- combined[, !duplicated(colnames(combined))] 
# deleting duplicates


rbind(cc, chdi) #doesn't capture column names of latter.




## Binding 2



country_class1 <- tribble(
  ~Country,     ~Income_Classification,
  "USA",        "High income",
  "India",      "Lower-middle income",
  "Argentina",  "Upper-middle income",
  "Sudan",      "Low income"
)

country_class2 <- tribble(
  ~Country,  ~Income_Classification,
  "USA",     "High income",
  "China",   "Upper-middle income",
  "Germany", "High income"
)



## Binding 2



cc_tib <- as_tibble(cc)

chdi_tib <- as_tibble(chdi)

bind_cols(cc_tib, chdi_tib)

bind_rows(cc_tib, chdi_tib) # creates missing values

union(country_class1, country_class2) 
# similar to bind rows but unions will remove duplicates.


intersect(country_class1, country_class2)


setdiff(country_class1, country_class2)
# unique of first




## Mutating Joins



country_class3 <- tribble(
  ~Country,     ~HDI,
  "USA",        17,
  "India",      130,
  "Argentina",  47,
  "Ethiopia",    180
)


left_join(country_class1, country_class3, by = "Country")

left_join(country_class3, country_class1, by = "Country")

inner_join(country_class1, country_class3, by = "Country")

full_join(country_class1, country_class3, by = "Country")


## Filtering Joins


country_class3 <- tribble(
  ~Country,     ~HDI,
  "USA",        17,
  "India",      130,
  "Argentina",  47,
  "Ethiopia",    180
)


semi_join(country_class1, country_class3) 

#keeps all rows of first where key matches with second.

anti_join(country_class1, country_class3) 

#keeps rows of first which don't match with the second.



## Outliers


data <- read.csv("output.csv")


stats <- summary(data$log_gdp)
stats

low <- 1.25
high <- 5.5

filtered_gdp <- filter(data, log_gdp > low, 
                       log_gdp < high)

# Simple Regressions:


set.seed(1)

data <- tibble(x = runif(1000,0, 10),
               e = rnorm(1000,0,1),
               z = runif(1000, -20, -10),
               y = 10 + 1.5*x - 10*z + e)



model1 <- lm(data = data, y ~ x)

model2 <- lm(data = data, y ~ x + z)


modelsummary(
  list("Regression X" = model1, "Regression X and Z" = model2),
  stars = TRUE,
  statistic = "std.error",
  output = "tinytable",
  title = "Results From Simulated Data",
  gof_omit = ".*"  # remove AIC, BIC, etc.
)












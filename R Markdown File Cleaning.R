head(GDP)
head(Internet_Users)

summary(GDP)
summary(Internet_Users)

colSums(is.na(GDP))
colSums(is.na(Internet_Users))

library(tidyverse)
install.packages("dplyr")

year_cols <- colnames(GDP)[-(1:2)]
GDP[year_cols] <- lapply(GDP[year_cols], function(x) as.numeric(as.character(x)))


GDP_long <- GDP %>%
  pivot_longer(
    cols = -c(`Data Source`, `World Development Indicators`),  # pivot all year columns
    names_to = "Year",
    values_to = "GDP_Value"
  )


year_cols <- colnames(Internet_Users)[-(1:2)]
Internet_Users[year_cols] <- lapply(Internet_Users[year_cols], function(x) as.numeric(as.character(x)))

Internet_Users_long <- Internet_Users %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "Year",
    values_to = "Internet_Value"
  )


GDP_long$Year <- as.numeric(GDP_long$Year)


File cleaning to get rid of missing datasets and also just using the years of 2005 - 2024

 packages <- c("readxl", "dplyr", "janitor", "writexl")
install.packages(setdiff(packages, rownames(installed.packages())))

library(readxl)
library(dplyr)
library(janitor)
library(writexl)

raw <- read_excel("GDP.xlsx", col_names = FALSE)
header <- as.character(unlist(raw[5, ]))
df <- raw[-(1:5), ] 
colnames(df) <- header

df <- df %>% select(where(~ !all(is.na(.))))

years <- as.character(2005:2024)

df_filtered <- df %>%
  select(
    `Country Name`,
    `Indicator Name`,
    all_of(years)
  )

write_xlsx(df_filtered, "GDP_2005_2024_no_codes.xlsx") 

 raw <- read_excel("Internet Users.xlsx", col_names = FALSE)
header <- as.character(unlist(raw[5, ]))
df <- raw[-(1:5), ] 
colnames(df) <- header 

df <- df %>% select(where(~ !all(is.na(.))))

years <- as.character(2005:2024)

df_filtered <- df %>%
  select(
    `Country Name`,
    `Indicator Name`,
    all_of(years)
  )

write_xlsx(df_filtered, "InternetUsers_2005_2024_clean.xlsx")


gdp <- read_excel("GDP_2005_2024_no_codes.xlsx")
internet <- read_excel("InternetUsers_2005_2024_clean.xlsx")

gdp_long <- gdp %>%
  pivot_longer(cols = `2005`:`2024`,
               names_to = "Year",
               values_to = "Value") %>%
  mutate(Year = as.numeric(Year))

internet_long <- internet %>%
  pivot_longer(cols = `2005`:`2024`,
               names_to = "Year",
               values_to = "Value") %>%
  mutate(Year = as.numeric(Year))   

  gdp_trend <- gdp_long %>%
  group_by(Year) %>%
  summarize(Global_Average = mean(Value, na.rm = TRUE))

internet_trend <- internet_long %>%
  group_by(Year) %>%
  summarize(Global_Average = mean(Value, na.rm = TRUE))

ggplot(gdp_trend, aes(x = Year, y = Global_Average)) +
  geom_line(size = 1.2, color = "orange") +
  geom_point(size = 2) +
  labs(
    title = "Global GDP Trend (2005–2024)",
    x = "Year",
    y = "Average GDP"
  ) +
  theme_minimal()             

ggplot(internet_trend, aes(x = Year, y = Global_Average)) +
  geom_line(size = 1.2, color = "orange") +
  geom_point(size = 2) +
  labs(
    title = "Global Internet Users Trend (2005–2024)",
    x = "Year",
    y = "Average Internet Users (%)"
  ) +
  theme_minimal()                                    

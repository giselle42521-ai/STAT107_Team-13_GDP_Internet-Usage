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
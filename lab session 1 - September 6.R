# GVPT622 Lab Session - September 6

library(tidyverse)
library(polisciols)
library(wbstats)
library(janitor)
library(skimr)
library(countrycode)
library(scales)

polisciols::nes
nes|> distinct(education) # to look at a specific variable
nes|> distinct(income_gap)
skim(nes$income_gap) # a significant portion of US voters believe the income gap have become larger
tabyl(nes,income_gap)

ggplot(nes, aes(y = income_gap)) + 
  geom_bar() +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot") + 
  labs(
    title = "Do you think the difference in incomes between rich people and poor people in the United States today is larger, \nsmaller, or about the same as it was 20 years ago?", 
    x = "Count of respondents",
    y = NULL,
    caption = "Source: ANES 2020 Survey"
  ) + 
  scale_x_continuous(labels = scales::label_comma())


perc_edu <- wb_data(
  "SE.XPD.TOTL.GD.ZS", start_date = 2020, end_date = 2020, return_wide = F
) |> 
  transmute(
    country, 
    region = countrycode(country, "country.name", "region"),
    year = date,
    value
  )

perc_edu

ggplot(perc_edu, aes(x = value)) + 
  geom_histogram(binwidth = 1) + 
  theme_minimal() + 
  labs(
    x = "Expenditure on education as a proportion of GDP",
    y = "Number of countries"
  )

ggplot(perc_edu, aes(x = value)) + 
  geom_histogram(binwidth = 0.25, fill="lightblue") + 
  theme_minimal() + 
  labs(
    x = "Expenditure on education as a proportion of GDP",
    y = "Number of countries"
  ) # smaller binwidth to get a clearer idea

ggplot(perc_edu, aes(x = value)) + 
  geom_density() + 
  theme_minimal() + 
  labs(
    x = "Expenditure on education as a proportion of GDP",
    y = "Density"
  )

mean(perc_edu$value, na.rm = T)
median(perc_edu$value, na.rm = T)

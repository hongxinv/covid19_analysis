library(tidyverse)
by_country <- read.csv("country_wise_latest.csv")
by_day <- read.csv("day_wise.csv")
clean <- read.csv("covid_19_clean_complete.csv")
#head(by_country)
#View(by_country)
#View(by_day)
#View(clean)

max(by_country$Recovered)

#Data is approximately from 2020-07-27 latest

#-------------------------------------------------------------------------------

#cummulative monthly infections total based on regions
january_infections <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "January") %>%
  group_by(WHO.Region) %>% #groups the regions
  summarise(Total_infections = sum(Confirmed)) #sums the infections of each region

by_country_africa_jan <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "January" & WHO.Region == "Africa") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_americas_jan <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "January" & WHO.Region == "Americas") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_east_med_jan <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "January" & WHO.Region == "Eastern Mediterranean") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed)) #funfact: 14

by_country_europe_jan <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "January" & WHO.Region == "Europe") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_sea_jan <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "January" & WHO.Region == "South-East Asia") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_western_pacific_jan <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "January" & WHO.Region == "Western Pacific") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

ggplot(january_infections, aes(x = WHO.Region, Total_infections))+
  geom_bar(stat="identity")+
  labs(title = "Infected count in January",
       x = "Region",
       y = "Infection Count")+
  scale_y_continuous(labels = scales::comma) #continuous instead of exponents

#means
mean_africa_jan <- mean(by_country_africa_jan$country_infections)
#print(mean_africa_jan)
mean_americas_jan <- mean(by_country_americas_jan$country_infections)
#print(mean_americas_jan)
mean_east_med_jan <- mean(by_country_east_med_jan$country_infections)
#print(mean_east_med_jan)
mean_europe_jan <- mean(by_country_europe_jan$country_infections)
#print(mean_europe_jan)
mean_sea_jan <- mean(by_country_sea_jan$country_infections)
#print(mean_sea_jan)
mean_western_pacific_jan <- mean(by_country_western_pacific_jan$country_infections)
#print(mean_western_pacific_jan)

#--------------------------------------------------------------------------------

february_infections <- clean %>%
  mutate(Month = format(as.Date(Date), "%B")) %>%
  filter(Month == "February") %>%
  group_by(WHO.Region) %>%
  summarise(Total_infections = sum(Confirmed))

by_country_africa_feb <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "February" & WHO.Region == "Africa") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_americas_feb <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "February" & WHO.Region == "Americas") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_east_med_feb <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "February" & WHO.Region == "Eastern Mediterranean") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_europe_feb <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "February" & WHO.Region == "Europe") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_sea_feb <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "February" & WHO.Region == "South-East Asia") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_western_pacific_feb <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "February" & WHO.Region == "Western Pacific") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

print(february_infections)

ggplot(february_infections, aes(x = WHO.Region, Total_infections))+
  geom_bar(stat="identity")+
  labs(title = "Infected count in February",
       x = "Region",
       y = "Infection Count")+
  scale_y_continuous(labels = scales::comma)

mean_africa_feb <- mean(by_country_africa_feb$country_infections)
#print(mean_africa_feb)
mean_americas_feb <- mean(by_country_americas_feb$country_infections)
#print(mean_americas_feb)
mean_east_med_feb <- mean(by_country_east_med_feb$country_infections)
#print(mean_east_med_feb)
mean_europe_feb <- mean(by_country_europe_feb$country_infections)
#print(mean_europe_feb)
mean_sea_feb <- mean(by_country_sea_feb$country_infections)
#print(mean_sea_feb)
mean_western_pacific_feb <- mean(by_country_western_pacific_feb$country_infections)
#print(mean_western_pacific_feb)

#-------------------------------------------------------------------------------

march_infections <- clean %>%
  mutate(Month = format(as.Date(Date), "%B")) %>%
  filter(Month == "March") %>%
  group_by(WHO.Region) %>%
  summarise(Total_infections = sum(Confirmed))

by_country_africa_mar <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "March" & WHO.Region == "Africa") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_americas_mar <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "March" & WHO.Region == "Americas") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_east_med_mar <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "March" & WHO.Region == "Eastern Mediterranean") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_europe_mar <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "March" & WHO.Region == "Europe") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_sea_mar <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "March" & WHO.Region == "South-East Asia") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_western_pacific_mar <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "March" & WHO.Region == "Western Pacific") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

print(march_infections)

ggplot(march_infections, aes(x = WHO.Region, Total_infections))+
  geom_bar(stat="identity")+
  labs(title = "Infected count in March",
       x = "Region",
       y = "Infection Count")+
  scale_y_continuous(labels = scales::comma)

mean_africa_mar <- mean(by_country_africa_mar$country_infections)
print(mean_africa_mar)
mean_americas_mar <- mean(by_country_americas_mar$country_infections)
print(mean_americas_mar)
mean_east_med_mar <- mean(by_country_east_med_mar$country_infections)
print(mean_east_med_mar)
mean_europe_mar <- mean(by_country_europe_mar$country_infections)
print(mean_europe_mar)
mean_sea_mar <- mean(by_country_sea_mar$country_infections)
print(mean_sea_mar)
mean_western_pacific_mar <- mean(by_country_western_pacific_mar$country_infections)
print(mean_western_pacific_mar)

#-------------------------------------------------------------------------------

april_infections <- clean %>%
  mutate(Month = format(as.Date(Date), "%B")) %>%
  filter(Month == "April") %>%
  group_by(WHO.Region) %>%
  summarise(Total_infections = sum(Confirmed))

by_country_africa_apr <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "April" & WHO.Region == "Africa") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_americas_apr <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "April" & WHO.Region == "Americas") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_east_med_apr <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "April" & WHO.Region == "Eastern Mediterranean") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_europe_apr <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "April" & WHO.Region == "Europe") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_sea_apr <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "April" & WHO.Region == "South-East Asia") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_western_pacific_apr <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "April" & WHO.Region == "Western Pacific") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

print(april_infections)

ggplot(april_infections, aes(x = WHO.Region, Total_infections))+
  geom_bar(stat="identity")+
  labs(title = "Infected count in April",
       x = "Region",
       y = "Infection Count")+
  scale_y_continuous(labels = scales::comma)

mean_africa_apr <- mean(by_country_africa_apr$country_infections)
print(mean_africa_apr)
mean_americas_apr <- mean(by_country_americas_apr$country_infections)
print(mean_americas_apr)
mean_east_med_apr <- mean(by_country_east_med_apr$country_infections)
print(mean_east_med_apr)
mean_europe_apr <- mean(by_country_europe_apr$country_infections)
print(mean_europe_apr)
mean_sea_apr <- mean(by_country_sea_apr$country_infections)
print(mean_sea_apr)
mean_western_pacific_apr <- mean(by_country_western_pacific_apr$country_infections)
print(mean_western_pacific_apr)

#-------------------------------------------------------------------------------

may_infections <- clean %>%
  mutate(Month = format(as.Date(Date), "%B")) %>%
  filter(Month == "May") %>%
  group_by(WHO.Region) %>%
  summarise(Total_infections = sum(Confirmed))

by_country_africa_may <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "May" & WHO.Region == "Africa") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_americas_may <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "May" & WHO.Region == "Americas") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_east_med_may <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "May" & WHO.Region == "Eastern Mediterranean") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_europe_may <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "May" & WHO.Region == "Europe") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_sea_may <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "May" & WHO.Region == "South-East Asia") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_western_pacific_may <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "May" & WHO.Region == "Western Pacific") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

print(may_infections) #prints regional infections for may

ggplot(may_infections, aes(x = WHO.Region, Total_infections))+
  geom_bar(stat="identity")+
  labs(title = "Infected count in May",
       x = "Region",
       y = "Infection Count")+
  scale_y_continuous(labels = scales::comma)

mean_africa_may <- mean(by_country_africa_may$country_infections)
print(mean_africa_may)
mean_americas_may <- mean(by_country_americas_may$country_infections)
print(mean_americas_may)
mean_east_med_may <- mean(by_country_east_med_may$country_infections)
print(mean_east_med_may)
mean_europe_may <- mean(by_country_europe_may$country_infections)
print(mean_europe_may)
mean_sea_may <- mean(by_country_sea_may$country_infections)
print(mean_sea_may)
mean_western_pacific_may <- mean(by_country_western_pacific_may$country_infections)
print(mean_western_pacific_may)

#-------------------------------------------------------------------------------

june_infections <- clean %>%
  mutate(Month = format(as.Date(Date), "%B")) %>%
  filter(Month == "June") %>%
  group_by(WHO.Region) %>%
  summarise(Total_infections = sum(Confirmed))

by_country_africa_jun <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "June" & WHO.Region == "Africa") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_americas_jun <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "June" & WHO.Region == "Americas") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_east_med_jun <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "June" & WHO.Region == "Eastern Mediterranean") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_europe_jun <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "June" & WHO.Region == "Europe") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_sea_jun <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "June" & WHO.Region == "South-East Asia") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_western_pacific_jun <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "June" & WHO.Region == "Western Pacific") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

print(june_infections) #prints regional infections for june

ggplot(june_infections, aes(x = WHO.Region, Total_infections))+
  geom_bar(stat="identity")+
  labs(title = "Infected count in June",
       x = "Region",
       y = "Infection Count")+
  scale_y_continuous(labels = scales::comma)

mean_africa_jun <- mean(by_country_africa_jun$country_infections)
print(mean_africa_jun)
mean_americas_jun <- mean(by_country_americas_jun$country_infections)
print(mean_americas_jun)
mean_east_med_jun <- mean(by_country_east_med_jun$country_infections)
print(mean_east_med_jun)
mean_europe_jun <- mean(by_country_europe_jun$country_infections)
print(mean_europe_jun)
mean_sea_jun <- mean(by_country_sea_jun$country_infections)
print(mean_sea_jun)
mean_western_pacific_jun <- mean(by_country_western_pacific_jun$country_infections)
print(mean_western_pacific_jun)

#-------------------------------------------------------------------------------

july_infections <- clean %>%
  mutate(Month = format(as.Date(Date), "%B")) %>%
  filter(Month == "July") %>%
  group_by(WHO.Region) %>%
  summarise(Total_infections = sum(Confirmed))

by_country_africa_jul <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "July" & WHO.Region == "Africa") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_americas_jul <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "July" & WHO.Region == "Americas") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_east_med_jul <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "July" & WHO.Region == "Eastern Mediterranean") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_europe_jul <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "July" & WHO.Region == "Europe") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_sea_jul <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "July" & WHO.Region == "South-East Asia") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

by_country_western_pacific_jul <- clean %>%
  mutate(Month = format(as.Date(Date), "%B"), country = Country.Region) %>%
  filter(Month == "July" & WHO.Region == "Western Pacific") %>% 
  group_by(country) %>% 
  summarise(country_infections = sum(Confirmed))

print(july_infections) #prints regional infections for july

ggplot(july_infections, aes(x = WHO.Region, Total_infections))+
  geom_bar(stat="identity")+
  labs(title = "Infected count in July",
       x = "Region",
       y = "Infection Count")+
  scale_y_continuous(labels = scales::comma)

mean_africa_jul <- mean(by_country_africa_jul$country_infections)
print(mean_africa_jul)
mean_americas_jul <- mean(by_country_americas_jul$country_infections)
print(mean_americas_jul)
mean_east_med_jul <- mean(by_country_east_med_jul$country_infections)
print(mean_east_med_jul)
mean_europe_jul <- mean(by_country_europe_jul$country_infections)
print(mean_europe_jul)
mean_sea_jul <- mean(by_country_sea_jul$country_infections)
print(mean_sea_jul)
mean_western_pacific_jul <- mean(by_country_western_pacific_jul$country_infections)
print(mean_western_pacific_jul)

#-------------------------------------------------------------------------------
#To plot regional mean line charts

#months
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")

#africa
mean_africa <- c(mean_africa_jan, mean_africa_feb, mean_africa_mar, mean_africa_apr, mean_africa_may, mean_africa_jun, mean_africa_jul)

#creates dataframe for mean of africa and months
data_africa <- data.frame(month = factor(months, levels = months), mean_infections = mean_africa) 

ggplot(data_africa, aes(x = month, y = mean_infections)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Mean Infection Counts in Africa (Jan-Jul)", x = "Month", y = "Mean Infection Counts") +
  scale_y_continuous(labels = scales::comma)

#americas
mean_americas <- c(mean_americas_jan, mean_americas_feb, mean_americas_mar, mean_americas_apr, mean_americas_may, mean_americas_jun, mean_americas_jul)

data_americas <- data.frame(month = factor(months, levels = months), mean_infections = mean_americas)

ggplot(data_americas, aes(x = month, y = mean_infections)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Mean Infection Counts in Americas (Jan-Jul)", x = "Month", y = "Mean Infection Counts") +
  scale_y_continuous(labels = scales::comma)

#east med
mean_east_med <- c(mean_east_med_jan, mean_east_med_feb, mean_east_med_mar, mean_east_med_apr, mean_east_med_may, mean_east_med_jun,  mean_east_med_jul)

data_east_med <- data.frame(month = factor(months, levels = months),
                            mean_infections = mean_east_med)

ggplot(data_east_med, aes(x = month, y = mean_infections)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Mean Infection Counts in Eastern Mediterranean (Jan-Jul)", x = "Month", y = "Mean Infection Counts") +
  scale_y_continuous(labels = scales::comma)

#europe
mean_europe <- c(mean_europe_jan, mean_europe_feb, mean_europe_mar, mean_europe_apr, mean_europe_may, mean_europe_jun, mean_europe_jul)

data_europe <- data.frame(month = factor(months, levels = months),
                          mean_infections = mean_europe)

ggplot(data_europe, aes(x = month, y = mean_infections)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Mean Infection Counts in Europe (Jan-Jul)", x = "Month", y = "Mean Infection Counts") +
  scale_y_continuous(labels = scales::comma)

#sea
mean_infections_sea <- c(mean_sea_jan, mean_sea_feb, mean_sea_mar, mean_sea_apr, mean_sea_may, mean_sea_jun, mean_sea_jul)

data_sea <- data.frame(month = factor(months, levels = months), mean_infections = mean_infections_sea)

ggplot(data_sea, aes(x = month, y = mean_infections)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Mean Infection Counts in South-East Asia (Jan-Jul)", x = "Month", y = "Mean Infection Counts") +
  scale_y_continuous(labels = scales::comma)

#west pac
mean_western_pacific <- c(mean_western_pacific_jan, mean_western_pacific_feb, mean_western_pacific_mar, mean_western_pacific_apr, mean_western_pacific_may, mean_western_pacific_jun, mean_western_pacific_jul)

data_western_pacific <- data.frame(month = factor(months, levels = months), mean_infections = mean_western_pacific)

ggplot(data_western_pacific, aes(x = month, y = mean_infections)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Mean Infection Counts in Western Pacific (Jan-Jul)",
       x = "Month",
       y = "Mean Infection Counts") +
  scale_y_continuous(labels = scales::comma)

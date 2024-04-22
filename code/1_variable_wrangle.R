# install.packages("ggcorrplot")
# install.packages("FactoMineR")
# install.packages("factoextra")

library(dplyr)
library(tidycensus)
library(sf)
library(tidyverse)
library(reshape2)
library("readxl")
library(ggcorrplot)
library("FactoMineR")
library(plotly)
library(stats)
library(cluster)
library(factoextra)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read hateful tweets
hate <- read.csv("../data/hate_counts_counties.csv")
names(hate) <- c("GEOID", "Hate")
hate$GEOID <- str_pad(hate$GEOID, 5, pad="0")

#read non-hateful tweets
nohate <- read.csv("../data/nohate_counts_counties.csv")
names(nohate) <- c("GEOID", "Nohate")
nohate$GEOID <- str_pad(nohate$GEOID, 5, pad="0")

#get your own key!
census_api_key("[get your own key]")

#get census data
#total population
pop <- subset(get_decennial(geography = "county", 
                      variables = "P1_001N", 
                      year = 2020),
              select = -c(NAME,variable))
names(pop) <- c("GEOID", "Total_population")

#foreign born population
foreign <- subset(get_acs(geography = "county", 
                           variables = c(var = "B05007_001"), 
                           year = 2020),
                   select = -c(NAME,variable, moe))
names(foreign) <- c("GEOID", "Foreign_born_population")

#NH White pop
white <- subset(get_decennial(geography = "county", 
                     variables = "P2_005N", 
                     year = 2020),
       select = -c(NAME,variable))
names(white) <- c("GEOID", "NH_White_population")

#Poverty
poverty <- subset(get_acs(geography = "county", 
                          variables = c(var = "B17010_002"), 
                          year = 2020),
                  select = -c(NAME,variable,moe))
names(poverty) <- c("GEOID", "Below_poverty_population")

#population density
geom <- st_read("../data/GIS/us_counties.shp")
geom$area <-  units::set_units(st_area(geom), "km^2")
geom$area <- units::drop_units(geom$area)
geom$geometry <- NULL

#COVID-19cases
url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
cases <- read.csv(url(url))
cases <- subset(cases, select = c("FIPS", "X8.1.22")) #drop columns
names(cases) <- c("GEOID", "Cases")
cases$GEOID <- str_pad(cases$GEOID, 5, pad="0")

#COVID-19deaths
url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
deaths <- read.csv(url(url))
deaths <- subset(deaths, select = c("FIPS", "X8.1.22")) #drop columns
names(deaths) <- c("GEOID", "Deaths")
deaths$GEOID <- str_pad(deaths$GEOID, 5, pad="0")

#income inequality
#----------------------------------------------------------------------------------------
url = "https://www.countyhealthrankings.org/sites/default/files/media/document/2020%20County%20Health%20Rankings%20Data%20-%20v2.xlsx"
temp = tempfile(fileext = ".xlsx")
download.file(url, destfile=temp, mode='wb')
inequality <- readxl::read_excel(temp, sheet =4)
inequality <- cbind(inequality[,1], inequality[,166])
inequality <- inequality[-1:-2,]
names(inequality)<-c("GEOID","Ineq")
inequality$Ineq <- as.numeric(inequality$Ineq)
inequality$Ineq[is.na(inequality$Ineq)] <- mean(inequality$Ineq, na.rm = TRUE) 
#----------------------------------------------------------------------------------------


#Ratio of % Asian-White population in poverty
#----------------------------------------------------------------------------------------
#Poverty Status in the Past 12 Months” (Asian Alone)” 
poverty_Asian <- subset(get_acs(geography = "county", 
                                 variables = c(var = "S1701_C02_016E"), #S1701_C01_016E, S1701_C03_016E
                                 year = 2020),
                         select = -c(NAME,variable,moe))
names(poverty_Asian) <- c("GEOID", "Poverty_Asian")

#Poverty Status in the Past Two Months” (non-hispanic White Alone)” 
poverty_White <- subset(get_acs(geography = "county", 
                                variables = c(var = "S1701_C02_021E"), #S1701_C03_021E
                                year = 2020),
                        select = -c(NAME,variable,moe))
names(poverty_White) <- c("GEOID", "Poverty_White")
#----------------------------------------------------------------------------------------


#concentrated disadvantage
#----------------------------------------------------------------------------------------
#total number of households
households_total <- subset(get_acs(geography = "county", 
                                              variables = c(var = "B19057_001E"), 
                                              year = 2020),
                                      select = -c(NAME,variable,moe))
names(households_total) <- c("GEOID", "Households_total")

#households on cash assistance
households_assistance <- subset(get_acs(geography = "county", 
                                   variables = c(var = "B19057_002E"), 
                                   year = 2020),
                           select = -c(NAME,variable,moe))
names(households_assistance) <- c("GEOID", "Households_on_public_assistance")

#female-headed families
households_female <- subset(get_acs(geography = "county", 
                                        variables = c(var = "S1101_C04_001E"), 
                                        year = 2020),
                                select = -c(NAME,variable,moe))
names(households_female) <- c("GEOID", "Female_headed_families")

#unemployment
unemployed <- subset(get_acs(geography = "county", 
                             variables = c(var = "B23025_005"), 
                             year = 2020),
                     select = -c(NAME,variable,moe))
names(unemployed) <- c("GEOID", "Unemployed")

#population under 18 years old
u18 <- subset(get_acs(geography = "county", 
                      variables = c(var = "S0101_C01_022E"), 
                      year = 2020),
              select = -c(NAME,variable,moe)) 
names(u18) <- c("GEOID", "Under_18")

#no high school diploma
nohighschool <- subset(get_acs(geography = "county", 
                      variables = c(var = "S1501_C01_008E"), 
                      year = 2020),
              select = -c(NAME,variable,moe)) 
names(nohighschool) <- c("GEOID", "No_high_school_diploma")

# logged median family income
income <- subset(get_acs(geography = "county", 
                      variables = c(var = "S1901_C01_012E"), 
                      year = 2020),
                      select = -c(NAME,variable,moe)) 
names(income) <- c("GEOID", "Median_family_income") 
income$Median_family_income[is.na(income$Median_family_income)] <- mean(income$Median_family_income, na.rm = TRUE)
#----------------------------------------------------------------------------------------


#Asian-White population in managerial positions
#----------------------------------------------------------------------------------------
manager_asian_male <- subset(get_acs(geography = "county", 
                         variables = c(var = "C24010D_003E"), 
                         year = 2020),
                 select = -c(NAME,variable,moe)) 
names(manager_asian_male) <- c("GEOID", "Managerial_positions_male_Asian")
manager_asian_female <- subset(get_acs(geography = "county", 
                                     variables = c(var = "C24010D_009E"), 
                                     year = 2020),
                             select = -c(NAME,variable,moe)) 
names(manager_asian_female) <- c("GEOID", "Managerial_positions_female_Asian")
workforce_asian <- subset(get_acs(geography = "county", 
                                     variables = c(var = "C24010D_001E"), 
                                     year = 2020),
                             select = -c(NAME,variable,moe)) 
names(workforce_asian) <- c("GEOID", "Workforce_Asian")

manager_white_male <- subset(get_acs(geography = "county", 
                                     variables = c(var = "C24010A_003E"), 
                                     year = 2020),
                             select = -c(NAME,variable,moe)) 
names(manager_white_male) <- c("GEOID", "Managerial_positions_male_White")
manager_white_female <- subset(get_acs(geography = "county", 
                                       variables = c(var = "C24010A_009E"), 
                                       year = 2020),
                               select = -c(NAME,variable,moe)) 
names(manager_white_female) <- c("GEOID", "Managerial_positions_female_White")
workforce_white <- subset(get_acs(geography = "county", 
                                  variables = c(var = "C24010A_001E"), 
                                  year = 2020),
                          select = -c(NAME,variable,moe)) 
names(workforce_white) <- c("GEOID", "Workforce_White")
#----------------------------------------------------------------------------------------


#Asian destination
#----------------------------------------------------------------------------------------

#Asian population 2010
asian_2010 <- subset(get_decennial(geography = "county", 
                                   variables = "P003005", 
                                   year = 2010, 
                                   sumfile = "sf1"),
                          select = -c(NAME,variable)) 
names(asian_2010) <- c("GEOID", "Asian_population_2010")

#Asian population 2020: 
asian_2020 <- subset(get_decennial(geography = "county", 
                                   variables = "P1_006N", 
                                   year = 2020),
                     select = -c(NAME,variable)) 
names(asian_2020) <- c("GEOID", "Asian_population_2020")

#------------------------------------------------------------------------------------------------------------------------
#bind together
df <- left_join(hate, nohate, by = c("GEOID")) %>%
  left_join(., pop, by = c("GEOID")) %>%
  left_join(., cases, by = c("GEOID")) %>%
  left_join(., deaths, by = c("GEOID")) %>%
  left_join(., foreign, by = c("GEOID")) %>%
  left_join(., white, by = c("GEOID")) %>%
  left_join(., geom, by = c("GEOID")) %>%
  left_join(., inequality, by = c("GEOID")) %>%
  left_join(., poverty, by = c("GEOID")) %>%
  left_join(., poverty_Asian, by = c("GEOID")) %>%
  left_join(., poverty_White, by = c("GEOID")) %>%
  left_join(., households_total, by = c("GEOID")) %>%
  left_join(., households_female, by = c("GEOID")) %>%
  left_join(., households_assistance, by = c("GEOID")) %>%
  left_join(., unemployed, by = c("GEOID")) %>%
  left_join(., u18, by = c("GEOID")) %>%
  left_join(., nohighschool, by = c("GEOID")) %>%
  left_join(., income, by = c("GEOID")) %>%
  left_join(., manager_asian_male, by = c("GEOID")) %>%
  left_join(., manager_asian_female, by = c("GEOID")) %>%
  left_join(., workforce_asian, by = c("GEOID")) %>%
  left_join(., manager_white_male, by = c("GEOID")) %>%
  left_join(., manager_white_female, by = c("GEOID")) %>%
  left_join(., workforce_white, by = c("GEOID")) %>%
  left_join(., asian_2010, by = c("GEOID")) %>%
  left_join(., asian_2020, by = c("GEOID"))


#check for NAs
any(is.na(df))
which(is.na(df), arr.ind=TRUE)

#Oglala Lakota County, SD
df[223, 27] <- asian_2010$Asian_population_2010[asian_2010$GEOID == 46113]
any(is.na(df))


#compute rates, ratios
df$Hate_percent <- (df$Hate/(df$Nohate+1)) * 100
df$Case_rate <- (df$Cases/df$Total_population) * 1000
df$Death_rate <- (df$Deaths/df$Total_population) * 1000
df$Percent_Asian_2010 <- (df$Asian_population_2010/(df$Total_population)) * 100
df$Percent_Asian_2020 <- (df$Asian_population_2020/(df$Total_population)) * 100
df$Percent_foreign_born <- (df$Foreign_born_population/(df$Total_population)) * 100
df$Percent_NH_White <- (df$NH_White_population/(df$Total_population)) * 100
df$Population_density <- (df$Total_population/(df$area))
df$Income_inequality <- df$Ineq
df$Percent_below_poverty <- (df$Below_poverty_population/(df$Total_population)) * 100
df$Asian_White_poverty_ratio <- (df$Poverty_Asian/(1+df$Asian_population_2020))/(df$NH_White_population/(1+df$Poverty_White))
df$Percent_public_assistance <- (df$Households_on_public_assistance/df$Households_total) * 100
df$Percent_unemployed <- (df$Unemployed/df$Total_population) * 100
df$Percent_under_18 <- (df$Under_18/df$Total_population) * 100
df$Percent_female_headed_families <- (df$Female_headed_families/df$Households_total) * 100
df$No_high_school_diploma <- (df$No_high_school_diploma/df$Households_total) * 100
df$Median_family_income <- log(df$Median_family_income)
df$Asian_White_population_in_managerial_positions <- ((df$Managerial_positions_male_Asian+df$Managerial_positions_female_Asian)/(1+df$Workforce_Asian))/(1+((df$Managerial_positions_male_White+df$Managerial_positions_female_White)/(1+df$Workforce_White)))

#Concentrated disadvantage
#----------------------------------------------------------------------------------------
#PCA
pca <- prcomp(subset(df, select = c(Percent_below_poverty, Percent_public_assistance, Percent_female_headed_families, 
                                    No_high_school_diploma, Percent_unemployed, Median_family_income)), scale = FALSE, retx = TRUE)
summary(pca)
pca$rotation
df$Concentrated_disadvantage <- -1 * pca$x[,1]
#plot(df$`Median family income`, df$`Concentrated disadvantage`)
#abline(lm(`Concentrated disadvantage`~`Median family income`,data=df),col='red')
# high values of the Concentrated disadvantage score mean high disadvantage
#----------------------------------------------------------------------------------------


#Emerging Asian destination
#----------------------------------------------------------------------------------------
df$Asian_population_change <- df$Percent_Asian_2020 - df$Percent_Asian_2010

kmeans.data <- subset(df, select = c(Asian_population_2010, Asian_population_change))
kmeans.data$Asian_population_2010 <- scale(df$Asian_population_2010, center = TRUE)
kmeans.data$Asian_population_change <- scale(df$Asian_population_change, center = TRUE)

# #k-medioids: optimal k
# fviz_nbclust(kmeans.data, pam, method = "wss")
# 
# #k-medioids
# df$`Asian destination` <- pam(
#   kmeans.data,
#   k = 5,
#   nstart = 5,
# )$cluster
# plot(kmeans.data$`Asian population 2010`, kmeans.data$`Asian population change`,pch=16,col=df$`Asian destination`)
# table(df$`Asian destination`)

#kmeans: optimal k
fviz_nbclust(kmeans.data, kmeans, method = "wss")

#kmeans (k = 7)
df$Asian_destination <- kmeans(
  kmeans.data,
  centers = 7,
  nstart = 5,
  iter.max = 10)$cluster
plot(kmeans.data$Asian_population_2010, kmeans.data$Asian_population_change,pch=16,col=df$Asian_destination)
table(df$Asian_destination)

df$Asian_destination <- as.factor(df$Asian_destination)

#----------------------------------------------------------------------------------------


df <- subset(df, select = c(GEOID, Hate, Nohate, Case_rate, Death_rate, Percent_Asian_2020, Percent_foreign_born,
                            Percent_NH_White, Population_density, Asian_White_poverty_ratio, 
                            Asian_White_population_in_managerial_positions, Concentrated_disadvantage, Income_inequality, 
                            Asian_destination))

save(df, file = "../data/df.RData")


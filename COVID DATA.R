
# Plot 1: Infant vs Non-infant


# Initial cleaning

# reading in the raw data
raw_data_url = "https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/demographic_data/demographics_by_state_raw.csv"

library(janitor)
library(dplyr)
library(ggplot2)
library(readr)
library(wesanderson)
library(UsingR)
library("scales")
library(data.table)
library(maptools)
library(rgdal)

# cleaning raw_demo_data
raw_demo_data <- read.csv(url(raw_data_url))
raw_demo_data2 <- clean_names(raw_demo_data)

#removes all rows and cols that are only composed of NA values
raw_demo_data3 <- remove_empty(raw_demo_data2, which = c("rows", "cols"), 
                               quiet = TRUE)

#getting rid of the columns containing 2022 data
rawdata_5 <- subset(raw_demo_data3, select = -c(x1_14_22, x1_28_22, x2_11_22, 
                                                x2_25_22, x3_11_22, x3_25_22, 
                                                x4_8_22))

percentages_data <- rawdata_5[which(rawdata_5$estimate_type == "rate_percent"),]
all_ages_data <- percentages_data[which(percentages_data$demo_cat_0 == "age"),]
confirmed_data <- all_ages_data[which(all_ages_data$metric == "confirmed"),]


# Infants' data

# INFANTS data (ages 0-4)

infants_data <- confirmed_data[which(confirmed_data$demo_cat_1 == "0_4"),]

# infants data - deaths 
infants_data_deaths <- infants_data[which(infants_data$category == "Deaths"),]

#removing NA columns
infants_data_deaths <- infants_data_deaths[, -c(12, 14, 16, 18, 20, 22, 24)]

# infants data - cases
infants_data_cases <- infants_data[which(infants_data$category == "Cases"),]

#removing NA columns
infants_data_cases <- infants_data_cases[, -c(12, 14, 16, 18, 20, 22, 24)]


# Non-Infants' data

#NON_INFANTS DATA
non_infants_data <- confirmed_data[which(confirmed_data$demo_cat_1 == "5_17"),]

# non-infants data - deaths
non_infants_deaths <- non_infants_data[which(
  non_infants_data$category == "Deaths"),]

# non-infants data - cases
non_infants_cases <- non_infants_data[which(
  non_infants_data$category == "Cases"),]


# Calculating the means of infant cases & deaths and non-infant cases & deaths

infant_cases_avg <- colMeans(infants_data_cases[,7:18], na.rm = TRUE)
infant_case <- mean(infant_cases_avg)

infant_death_avg <- colMeans(infants_data_deaths[,7:18], na.rm = TRUE)
infant_death <- mean(infant_death_avg)

non_infant_cases_avg <- colMeans(non_infants_cases[,7:18], na.rm = TRUE)
non_infant_case <- mean(non_infant_cases_avg, na.rm = TRUE)

non_infant_death_avg <- colMeans(non_infants_deaths[,7:18], na.rm = TRUE)
non_infant_death <- mean(non_infant_death_avg, na.rm = TRUE)


# Data frame with the averages for infant cases/deaths and non-infant cases/deaths

final_data <- data.frame(matrix(ncol = 2, nrow = 4))
final_data$percentage <- c(infant_case, infant_death, non_infant_case, 
                           non_infant_death)
final_data$category <- c("Cases", "Deaths", "Cases", "Deaths")
final_data$age <- c("Infants (0 - 4 years)", "Infants (0 - 4 years)", 
                    "Non-Infants (5 - 17 years)", "Non-Infants (5 - 17 years)")
final_data <- final_data[, -c(1, 2)]


# Infant & Non-Infant plot

ggplot(data = final_data) +
  geom_bar(aes(x = age, y = percentage, fill = category), stat = "identity", 
           position = position_dodge()) +
  facet_wrap(~category, scales="free_y") +
  scale_fill_manual(values = c("#87CEEB", "#FF7F7F")) +
  labs(title = "Cases / Deaths as seen in Infants and Non-Infants", 
       x = "Age Group", y = "Percentage")  +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


# Plot 2: Vaccination and Mortality Totals by Age Group



raw_demo_data <- read.csv("demographics_by_state_raw.csv")

# getting rid of the columns containing 2022 data
rawdata_5 <- raw_demo_data[, 1:25]

# Obtain rows for total, cumulative number of cases, vaccines, 
# and cases by age groups

newdata <- rawdata_5[which(rawdata_5$Estimate_type == "total_cumulative"), ]

age_cumulative <- newdata[which(newdata$Demo_cat_0 == "age"), ]
age_cumulative <- newdata[which(newdata$Category == "Deaths" | 
                                  newdata$Category == "Vaccines"), ]
age_cumulative[is.na(age_cumulative)] <- 0

age_cumulative <- age_cumulative[which(
  age_cumulative$Demo_cat_1 == "65_older" | 
    age_cumulative$Demo_cat_1 == "65_74" | 
    age_cumulative$Demo_cat_1 == "75_older" | 
    age_cumulative$Demo_cat_1 == "65_69" | 
    age_cumulative$Demo_cat_1 == "70_74" |
    age_cumulative$Demo_cat_1 == "50_64" | 
    age_cumulative$Demo_cat_1 == "55_64" |
    age_cumulative$Demo_cat_1 == "25_49" | 
    age_cumulative$Demo_cat_1 == "30_49" | 
    age_cumulative$Demo_cat_1 == "25_34" | 
    age_cumulative$Demo_cat_1 == "35_44" |
    age_cumulative$Demo_cat_1 == "18_25" |
    age_cumulative$Demo_cat_1 == "18_24" |
    age_cumulative$Demo_cat_1 == "5_17" | 
    age_cumulative$Demo_cat_1 == "5_11" | 
    age_cumulative$Demo_cat_1 == "12_17" | 
    age_cumulative$Demo_cat_1 == "11_17"), ]

# Break up age groups 
age_cumulative$Demo_cat_1[which(
  age_cumulative$Demo_cat_1 == "65_older" | 
    age_cumulative$Demo_cat_1 == "65_74" |      
    age_cumulative$Demo_cat_1 == "75_older" | 
    age_cumulative$Demo_cat_1 == "65_69" |    
    age_cumulative$Demo_cat_1 == "70_74")] <- "65+"

age_cumulative$Demo_cat_1[which(
  age_cumulative$Demo_cat_1 == "50_64" | 
    age_cumulative$Demo_cat_1 == "55_64")] <- "50-64"

age_cumulative$Demo_cat_1[which(
  age_cumulative$Demo_cat_1 == "25_49" | 
    age_cumulative$Demo_cat_1 == "30_49" | 
    age_cumulative$Demo_cat_1 == "25_34" | 
    age_cumulative$Demo_cat_1 == "35_44")] <- "25-49"

age_cumulative$Demo_cat_1[which(
  age_cumulative$Demo_cat_1 == "18_25" | 
    age_cumulative$Demo_cat_1 == "18_29" | 
    age_cumulative$Demo_cat_1 == "18_24")] <- "18-24"

age_cumulative$Demo_cat_1[which(
  age_cumulative$Demo_cat_1 == "5_17" | 
    age_cumulative$Demo_cat_1 == "5_11" | 
    age_cumulative$Demo_cat_1 == "12_17" | 
    age_cumulative$Demo_cat_1 == "11_17")] <- "5-17"

# vax by age
vax <- age_cumulative[which(age_cumulative$Category == "Vaccines"), ]

vax_65 <- vax[which(vax$Demo_cat_1 == "65+"), ]
vax_50 <- vax[which(vax$Demo_cat_1 == "50-64"), ]
vax_25 <- vax[which(vax$Demo_cat_1 == "25-49"), ]
vax_18 <- vax[which(vax$Demo_cat_1 == "18-24"), ]
vax_5 <- vax[which(vax$Demo_cat_1 == "5-17"), ]

sums_vax65 <- colSums(vax_65[, 7:25])
sums_vax50 <- colSums(vax_50[, 7:25])
sums_vax25 <- colSums(vax_25[, 7:25])
sums_vax18 <- colSums(vax_18[, 7:25])
sums_vax5 <- colSums(vax_5[, 7:25])

#Deaths by age

deaths <- 
  age_cumulative[which(age_cumulative$Category == "Deaths"), ]

deaths_65 <- deaths[which(deaths$Demo_cat_1 == "65+"), ]
deaths_50 <- deaths[which(deaths$Demo_cat_1 == "50-64"), ]
deaths_25 <- deaths[which(deaths$Demo_cat_1 == "25-49"), ]
deaths_18 <- deaths[which(deaths$Demo_cat_1 == "18-24"), ]
deaths_5 <- deaths[which(deaths$Demo_cat_1 == "5-17"), ]

sums_deaths65 <- colSums(deaths_65[, 7:25])
sums_deaths50 <- colSums(deaths_50[, 7:25])
sums_deaths25 <- colSums(deaths_25[, 7:25])
sums_deaths18 <- colSums(deaths_18[, 7:25])
sums_deaths5 <- colSums(deaths_5[, 7:25])

# Add variable "Distinction" to facet plot by deaths and vaccines

distinction <- c("Vaccines", "Vaccines", "Vaccines", "Vaccines", "Vaccines",
                 "Deaths", "Deaths", "Deaths", "Deaths", "Deaths")
distinction <- factor(distinction, ordered = FALSE, levels = c("Vaccines", 
                                                               "Deaths"))

# Dates and age group labels for plot

dates <- c("04-02-21", "04-23-21", "05-07-21", "05-21-21", "06-04-21", 
           "06-17-21", "07-02-21", "07-16-21", "07-30-21", "08-13-21", 
           "08-27-21", "09-10-21", "09-25-21", "10-08-21", "10-22-21", 
           "11-05-21", "11-19-21", "12-03-21", "12-17-21")
dates <- factor(dates, ordered = TRUE, levels = dates)

age_group <- c("05-17", "18-24", "25-49", "50-64", "65+", "05-17", "18-24", 
               "25-49", "50-64", "65+")

# Separating columns by date

x4.2.21 <- c(sums_vax5[1], sums_vax18[1], sums_vax25[1], sums_vax50[1], 
             sums_vax65[1], sums_deaths5[1], sums_deaths18[1], sums_deaths25[1], 
             sums_deaths50[1], sums_deaths65[1]) 
x4.23.21 <- c(sums_vax5[2], sums_vax18[2], sums_vax25[2], sums_vax50[2], 
              sums_vax65[2], sums_deaths5[2], sums_deaths18[2], 
              sums_deaths25[2], sums_deaths50[2], sums_deaths65[2])
x5.7.21 <- c(sums_vax5[3], sums_vax18[3], sums_vax25[3], sums_vax50[3], 
             sums_vax65[3], sums_deaths5[3], sums_deaths18[3], sums_deaths25[3], 
             sums_deaths50[3], sums_deaths65[3])
x5.21.21 <- c(sums_vax5[4], sums_vax18[4], sums_vax25[4], sums_vax50[4], 
              sums_vax65[4], sums_deaths5[4], sums_deaths18[4], 
              sums_deaths25[4], sums_deaths50[4], sums_deaths65[4])
x6.4.21 <- c(sums_vax5[5], sums_vax18[5], sums_vax25[5], sums_vax50[5], 
             sums_vax65[5], sums_deaths5[5], sums_deaths18[5], sums_deaths25[5], 
             sums_deaths50[5], sums_deaths65[5])
x6.17.21<- c(sums_vax5[6], sums_vax18[6], sums_vax25[6], sums_vax50[6], 
             sums_vax65[6], sums_deaths5[6], sums_deaths18[6], sums_deaths25[6], 
             sums_deaths50[6], sums_deaths65[6])
x7.2.21 <- c(sums_vax5[7], sums_vax18[7], sums_vax25[7], sums_vax50[7], 
             sums_vax65[7], sums_deaths5[7], sums_deaths18[7], sums_deaths25[7], 
             sums_deaths50[7], sums_deaths65[7])
x7.16.21 <- c(sums_vax5[8], sums_vax18[8], sums_vax25[8], sums_vax50[8], 
              sums_vax65[8], sums_deaths5[8], sums_deaths18[8], 
              sums_deaths25[8], sums_deaths50[8], sums_deaths65[8])
x7.30.21 <- c(sums_vax5[9], sums_vax18[9], sums_vax25[9], sums_vax50[9], 
              sums_vax65[9], sums_deaths5[9], sums_deaths18[9], 
              sums_deaths25[9], sums_deaths50[9], sums_deaths65[9])
x8.13.21 <- c(sums_vax5[10], sums_vax18[10], sums_vax25[10], sums_vax50[10], 
              sums_vax65[10], sums_deaths5[10], sums_deaths18[10], 
              sums_deaths25[10], sums_deaths50[10], sums_deaths65[10])
x8.27.21 <- c(sums_vax5[11], sums_vax18[11], sums_vax25[11], sums_vax50[11], 
              sums_vax65[11], sums_deaths5[11], sums_deaths18[11], 
              sums_deaths25[11], sums_deaths50[11], sums_deaths65[11])
x9.10.21 <- c(sums_vax5[12], sums_vax18[12], sums_vax25[12], sums_vax50[12], 
              sums_vax65[12],
              sums_deaths5[12], sums_deaths18[12], sums_deaths25[12], 
              sums_deaths50[12], sums_deaths65[12])
x9.25.21 <- c(sums_vax5[13], sums_vax18[13], sums_vax25[13], sums_vax50[13], 
              sums_vax65[13],
              sums_deaths5[13], sums_deaths18[13], sums_deaths25[13], 
              sums_deaths50[13], sums_deaths65[13])
x10.8.21 <- c(sums_vax5[14], sums_vax18[14], sums_vax25[14], sums_vax50[14], 
              sums_vax65[14],
              sums_deaths5[14], sums_deaths18[14], sums_deaths25[14], 
              sums_deaths50[14], sums_deaths65[14])
x10.22.21 <- c(sums_vax5[15], sums_vax18[15], sums_vax25[15], sums_vax50[15], 
               sums_vax65[15],
               sums_deaths5[15], sums_deaths18[15], sums_deaths25[15], 
               sums_deaths50[15], sums_deaths65[15])
x11.5.21 <- c(sums_vax5[16], sums_vax18[16], sums_vax25[16], sums_vax50[16], 
              sums_vax65[16],
              sums_deaths5[16], sums_deaths18[16], sums_deaths25[16], 
              sums_deaths50[16], sums_deaths65[16])
x11.19.21 <- c(sums_vax5[17], sums_vax18[17], sums_vax25[17], sums_vax50[17], 
               sums_vax65[17],
               sums_deaths5[17], sums_deaths18[17], sums_deaths25[17], 
               sums_deaths50[17], sums_deaths65[17])
x12.3.21 <- c(sums_vax5[18], sums_vax18[18], sums_vax25[18], sums_vax50[18], 
              sums_vax65[18],
              sums_deaths5[18], sums_deaths18[18], sums_deaths25[18], 
              sums_deaths50[18], sums_deaths65[18])
x12.17.21 <- c(sums_vax5[19], sums_vax18[19], sums_vax25[19], sums_vax50[19], 
               sums_vax65[19],
               sums_deaths5[19], sums_deaths18[19], sums_deaths25[19], 
               sums_deaths50[19], sums_deaths65[19])


# New data frame for sums by date, age group, and vaccine or death toll

end_data <- data.frame(age_group, distinction, x4.2.21, x4.23.21, x5.7.21, 
                       x5.21.21, x6.4.21, x6.17.21, x7.2.21, x7.16.21, x7.30.21, x8.13.21, 
                       x8.27.21, x9.10.21, x9.25.21, x10.8.21, x10.22.21, x11.5.21, 
                       x11.19.21, x12.3.21, x12.17.21)

end_data[is.na(end_data)] <- 0

# Plot

ggplot(data = end_data, mapping = aes(color = age_group)) + 
  geom_point(mapping = aes(x = dates[1], y = end_data[1:10, 3])) + 
  geom_point(mapping = aes(x = dates[2], y = end_data[1:10, 4])) + 
  geom_point(mapping = aes(x = dates[3], y = end_data[1:10, 5])) + 
  geom_point(mapping = aes(x = dates[4], y = end_data[1:10, 6])) + 
  geom_point(mapping = aes(x = dates[5], y = end_data[1:10, 7])) + 
  geom_point(mapping = aes(x = dates[6], y = end_data[1:10, 8])) + 
  geom_point(mapping = aes(x = dates[7], y = end_data[1:10, 9])) + 
  geom_point(mapping = aes(x = dates[8], y = end_data[1:10, 10])) + 
  geom_point(mapping = aes(x = dates[9], y = end_data[1:10, 11])) + 
  geom_point(mapping = aes(x = dates[10], y = end_data[1:10, 12])) + 
  geom_point(mapping = aes(x = dates[11], y = end_data[1:10, 13])) + 
  geom_point(mapping = aes(x = dates[12], y = end_data[1:10, 14])) + 
  geom_point(mapping = aes(x = dates[13], y = end_data[1:10, 15])) + 
  geom_point(mapping = aes(x = dates[14], y = end_data[1:10, 16])) + 
  geom_point(mapping = aes(x = dates[15], y = end_data[1:10, 17])) + 
  geom_point(mapping = aes(x = dates[16], y = end_data[1:10, 18])) + 
  geom_point(mapping = aes(x = dates[17], y = end_data[1:10, 19])) + 
  geom_point(mapping = aes(x = dates[18], y = end_data[1:10, 20])) + 
  geom_point(mapping = aes(x = dates[19], y = end_data[1:10, 21])) + 
  theme(axis.text.x = element_text(angle = 90, size = 5, hjust = 1), 
        plot.title = element_text(hjust = .5)) +
  labs(x = "Date (April to December 2021)", 
       y = "Count",
       title = "Total Cumulative Covid-19 Vaccinations and Deaths 
as of Date Across USA")+
  facet_wrap(vars(end_data[,2]), scales = "free_y")  + 
  scale_y_continuous(labels = comma)



# Plots 3 and 4: US Politcal Maps vs Conf. Cases


#store the initial-raw datasheet for data-cleaning
my_data <- fread("RAW_us_deaths.csv")

#remove unnecessary & unknown variables 
dplyr::select(my_data, -c(iso2, iso3, Country_Region, Lat, Long_, Combined_Key, 
                          UID, FIPS, code3, Admin2))
whole_case = dplyr::select(my_data, -Province_State, -Population)

#store the index, population, number of deaths by states (~ 18th Mar 2022)
#Alabama
index_AL <- which(my_data$Province_State == "Alabama")
pop_AL <- as.numeric(sum(my_data$Population[index_AL]))
death_AL <- as.numeric(sum(whole_case$"3/18/22"[index_AL]))

#Alaska
index_AK <- which(my_data$Province_State == "Alaska")
pop_AK <- as.numeric(sum(my_data$Population[index_AK]))
death_AK <- as.numeric(sum(whole_case$"3/18/22"[index_AK]))

#Arizona
index_AZ <- which(my_data$Province_State == "Arizona")
pop_AZ <- as.numeric(sum(my_data$Population[index_AZ]))
death_AZ <- as.numeric(sum(whole_case$"3/18/22"[index_AZ]))

#Arkansas
index_AR <- which(my_data$Province_State == "Arkansas")
pop_AR <- as.numeric(sum(my_data$Population[index_AR]))
death_AR <- as.numeric(sum(whole_case$"3/18/22"[index_AR]))

#California
index_CA <- which(my_data$Province_State == "California")
pop_CA <- as.numeric(sum(my_data$Population[index_CA]))
death_CA <- as.numeric(sum(whole_case$"3/18/22"[index_CA]))

#Colorado
index_CO <- which(my_data$Province_State == "Colorado")
pop_CO <- as.numeric(sum(my_data$Population[index_CO]))
death_CO <- as.numeric(sum(whole_case$"3/18/22"[index_CO]))

#Connecticut
index_CT <- which(my_data$Province_State == "Connecticut")
pop_CT <- as.numeric(sum(my_data$Population[index_CT]))
death_CT <- as.numeric(sum(whole_case$"3/18/22"[index_CT]))

#Delaware
index_DE <- which(my_data$Province_State == "Delaware")
pop_DE <- as.numeric(sum(my_data$Population[index_DE]))
death_DE <- as.numeric(sum(whole_case$"3/18/22"[index_DE]))

#D.C.
index_DC <- which(my_data$Province_State == "District of Columbia")
pop_DC <- as.numeric(sum(my_data$Population[index_DC]))
death_DC <- as.numeric(sum(whole_case$"3/18/22"[index_DC]))

#Florida
index_FL <- which(my_data$Province_State == "Florida")
pop_FL <- as.numeric(sum(my_data$Population[index_FL]))
death_FL <- as.numeric(sum(whole_case$"3/18/22"[index_FL]))

#Georgia
index_GA <- which(my_data$Province_State == "Georgia")
pop_GA <- as.numeric(sum(my_data$Population[index_GA]))
death_GA <- as.numeric(sum(whole_case$"3/18/22"[index_GA]))

#Hawaii
index_HI <- which(my_data$Province_State == "Hawaii")
pop_HI <- as.numeric(sum(my_data$Population[index_HI]))
death_HI <- as.numeric(sum(whole_case$"3/18/22"[index_HI]))

#Idaho
index_ID <- which(my_data$Province_State == "Idaho")
pop_ID <- as.numeric(sum(my_data$Population[index_ID]))
death_ID <- as.numeric(sum(whole_case$"3/18/22"[index_ID]))

#Illinois
index_IL <- which(my_data$Province_State == "Illinois")
pop_IL <- as.numeric(sum(my_data$Population[index_IL]))
death_IL <- as.numeric(sum(whole_case$"3/18/22"[index_IL]))

#Indiana
index_IN <- which(my_data$Province_State == "Indiana")
pop_IN <- as.numeric(sum(my_data$Population[index_IN]))
death_IN <- as.numeric(sum(whole_case$"3/18/22"[index_IN]))

#Iowa
index_IA <- which(my_data$Province_State == "Iowa")
pop_IA <- as.numeric(sum(my_data$Population[index_IA]))
death_IA <- as.numeric(sum(whole_case$"3/18/22"[index_IA]))

#Kansas
index_KS <- which(my_data$Province_State == "Kansas")
pop_KS <- as.numeric(sum(my_data$Population[index_KS]))
death_KS <- as.numeric(sum(whole_case$"3/18/22"[index_KS]))

#Kentucky
index_KY <- which(my_data$Province_State == "Kentucky")
pop_KY <- as.numeric(sum(my_data$Population[index_KY]))
death_KY <- as.numeric(sum(whole_case$"3/18/22"[index_KY]))

#Louisiana
index_LA <- which(my_data$Province_State == "Louisiana")
pop_LA <- as.numeric(sum(my_data$Population[index_LA]))
death_LA <- as.numeric(sum(whole_case$"3/18/22"[index_LA]))

#Maine
index_ME <- which(my_data$Province_State == "Maine")
pop_ME <- as.numeric(sum(my_data$Population[index_ME]))
death_ME <- as.numeric(sum(whole_case$"3/18/22"[index_ME]))

#Maryland
index_MD <- which(my_data$Province_State == "Maryland")
pop_MD <- as.numeric(sum(my_data$Population[index_MD]))
death_MD <- as.numeric(sum(whole_case$"3/18/22"[index_MD]))

#Massachusetts
index_MA <- which(my_data$Province_State == "Massachusetts")
pop_MA <- as.numeric(sum(my_data$Population[index_MA]))
death_MA <- as.numeric(sum(whole_case$"3/18/22"[index_MA]))

#Michigan
index_MI <- which(my_data$Province_State == "Michigan")
pop_MI <- as.numeric(sum(my_data$Population[index_MI]))
death_MI <- as.numeric(sum(whole_case$"3/18/22"[index_MI]))

#Minnesota
index_MN <- which(my_data$Province_State == "Minnesota")
pop_MN <- as.numeric(sum(my_data$Population[index_MN]))
death_MN <- as.numeric(sum(whole_case$"3/18/22"[index_MN]))

#Mississippi
index_MS <- which(my_data$Province_State == "Mississippi")
pop_MS <- as.numeric(sum(my_data$Population[index_MS]))
death_MS <- as.numeric(sum(whole_case$"3/18/22"[index_MS]))

#Missouri
index_MO <- which(my_data$Province_State == "Missouri")
pop_MO <- as.numeric(sum(my_data$Population[index_MO]))
death_MO <- as.numeric(sum(whole_case$"3/18/22"[index_MO]))

#Montana
index_MT <- which(my_data$Province_State == "Montana")
pop_MT <- as.numeric(sum(my_data$Population[index_MT]))
death_MT <- as.numeric(sum(whole_case$"3/18/22"[index_MT]))

#Nebraska
index_NE <- which(my_data$Province_State == "Nebraska")
pop_NE <- as.numeric(sum(my_data$Population[index_NE]))
death_NE <- as.numeric(sum(whole_case$"3/18/22"[index_NE]))

#Nevada
index_NV <- which(my_data$Province_State == "Nevada")
pop_NV <- as.numeric(sum(my_data$Population[index_NV]))
death_NV <- as.numeric(sum(whole_case$"3/18/22"[index_NV]))

#New Hampshire
index_NH <- which(my_data$Province_State == "New Hampshire")
pop_NH <- as.numeric(sum(my_data$Population[index_NH]))
death_NH <- as.numeric(sum(whole_case$"3/18/22"[index_NH]))

#New Jersey
index_NJ <- which(my_data$Province_State == "New Jersey")
pop_NJ <- as.numeric(sum(my_data$Population[index_NJ]))
death_NJ <- as.numeric(sum(whole_case$"3/18/22"[index_NJ]))

#New Mexico
index_NM <- which(my_data$Province_State == "New Mexico")
pop_NM <- as.numeric(sum(my_data$Population[index_NM]))
death_NM <- as.numeric(sum(whole_case$"3/18/22"[index_NM]))

#New York
index_NY <- which(my_data$Province_State == "New Hampshire")
pop_NY <- as.numeric(sum(my_data$Population[index_NY]))
death_NY <- as.numeric(sum(whole_case$"3/18/22"[index_NY]))

#North Carolina
index_NC <- which(my_data$Province_State == "North Carolina")
pop_NC <- as.numeric(sum(my_data$Population[index_NH]))
death_NC <- as.numeric(sum(whole_case$"3/18/22"[index_NH]))

#North Dakota
index_ND <- which(my_data$Province_State == "North Dakota")
pop_ND <- as.numeric(sum(my_data$Population[index_ND]))
death_ND <- as.numeric(sum(whole_case$"3/18/22"[index_ND]))

#Ohio
index_OH <- which(my_data$Province_State == "Ohio")
pop_OH <- as.numeric(sum(my_data$Population[index_OH]))
death_OH <- as.numeric(sum(whole_case$"3/18/22"[index_OH]))

#Oklahoma
index_OK <- which(my_data$Province_State == "Oklahoma")
pop_OK <- as.numeric(sum(my_data$Population[index_OK]))
death_OK <- as.numeric(sum(whole_case$"3/18/22"[index_OK]))

#Oregon
index_OR <- which(my_data$Province_State == "Oregon")
pop_OR <- as.numeric(sum(my_data$Population[index_OR]))
death_OR <- as.numeric(sum(whole_case$"3/18/22"[index_OR]))

#Pennsylvania
index_PA <- which(my_data$Province_State == "Pennsylvania")
pop_PA <- as.numeric(sum(my_data$Population[index_PA]))
death_PA <- as.numeric(sum(whole_case$"3/18/22"[index_PA]))

#Oklahoma
index_OK <- which(my_data$Province_State == "Oklahoma")
pop_OK <- as.numeric(sum(my_data$Population[index_OK]))
death_OK <- as.numeric(sum(whole_case$"3/18/22"[index_OK]))

#Rhode Island
index_RI <- which(my_data$Province_State == "Rhode Island")
pop_RI <- as.numeric(sum(my_data$Population[index_RI]))
death_RI <- as.numeric(sum(whole_case$"3/18/22"[index_RI]))

#South Carolina
index_SC <- which(my_data$Province_State == "South Carolina")
pop_SC <- as.numeric(sum(my_data$Population[index_SC]))
death_SC <- as.numeric(sum(whole_case$"3/18/22"[index_SC]))

#South Dakota
index_SD <- which(my_data$Province_State == "South Dakota")
pop_SD <- as.numeric(sum(my_data$Population[index_SD]))
death_SD <- as.numeric(sum(whole_case$"3/18/22"[index_SD]))

#Tennessee
index_TN <- which(my_data$Province_State == "Tennessee")
pop_TN <- as.numeric(sum(my_data$Population[index_TN]))
death_TN <- as.numeric(sum(whole_case$"3/18/22"[index_TN]))

#Texas
index_TX <- which(my_data$Province_State == "Texas")
pop_TX <- as.numeric(sum(my_data$Population[index_TX]))
death_TX <- as.numeric(sum(whole_case$"3/18/22"[index_TX]))

#Utah
index_UT <- which(my_data$Province_State == "Utah")
pop_UT <- as.numeric(sum(my_data$Population[index_UT]))
death_UT <- as.numeric(sum(whole_case$"3/18/22"[index_UT]))

#Vermont
index_VT <- which(my_data$Province_State == "Vermont")
pop_VT <- as.numeric(sum(my_data$Population[index_VT]))
death_VT <- as.numeric(sum(whole_case$"3/18/22"[index_VT]))

#Virginia
index_VA <- which(my_data$Province_State == "Virginia")
pop_VA <- as.numeric(sum(my_data$Population[index_VA]))
death_VA <- as.numeric(sum(whole_case$"3/18/22"[index_VA]))

#Washington
index_WA <- which(my_data$Province_State == "Washington")
pop_WA <- as.numeric(sum(my_data$Population[index_WA]))
death_WA <- as.numeric(sum(whole_case$"3/18/22"[index_WA]))

#West Virginia
index_WV <- which(my_data$Province_State == "West Virginia")
pop_WV <- as.numeric(sum(my_data$Population[index_WV]))
death_WV <- as.numeric(sum(whole_case$"3/18/22"[index_WV]))

#Wisconsin
index_WI <- which(my_data$Province_State == "Wisconsin")
pop_WI <- as.numeric(sum(my_data$Population[index_WI]))
death_WI <- as.numeric(sum(whole_case$"3/18/22"[index_WI]))

#Wyoming
index_WY <- which(my_data$Province_State == "Wyoming")
pop_WY <- as.numeric(sum(my_data$Population[index_WY]))
death_WY <- as.numeric(sum(whole_case$"3/18/22"[index_WY]))

#create empty data frame to store the data
clean_data <- data.frame(matrix(ncol = 4, nrow = 51))

#name the column names
colnames(clean_data) <- c("state", "population", "death", "percentage")

#store states into state variable
clean_data$state <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", 
                      "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", 
                      "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", 
                      "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", 
                      "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", 
                      "VT", "VA", "WA", "WV", "WI", "WY")

#store population data into population variable
clean_data$population <- c(pop_AL, pop_AK, pop_AZ, pop_AR, pop_CA, pop_CO, 
                           pop_CT, pop_DE, pop_DC, pop_FL, pop_GA, pop_HI, 
                           pop_ID, pop_IL, pop_IN, pop_IA, pop_KS, pop_KY, 
                           pop_LA, pop_ME, pop_MD, pop_MA, pop_MI, pop_MN, 
                           pop_MS, pop_MO, pop_MT, pop_NE, pop_NV, pop_NH, 
                           pop_NJ, pop_NM, pop_NY, pop_NC, pop_ND, pop_OH, 
                           pop_OK, pop_OR, pop_PA, pop_RI, pop_SC, pop_SD, 
                           pop_TN, pop_TX, pop_UT, pop_VT, pop_VA, pop_WA, 
                           pop_WV, pop_WI, pop_WY)

#store death data into death variable
clean_data$death <- c(death_AL, death_AK, death_AZ, death_AR, death_CA, death_CO, death_CT, death_DE, death_DC, death_FL, death_GA, death_HI, death_ID, death_IL, death_IN, death_IA, death_KS, death_KY, death_LA, death_ME, death_MD, death_MA, death_MI, death_MN, death_MS, death_MO, death_MT, death_NE, death_NV, death_NH, death_NJ, death_NM, death_NY, death_NC, death_ND, death_OH, death_OK, death_OR, death_PA, death_RI, death_SC, death_SD, death_TN, death_TX, death_UT, death_VT, death_VA, death_WA, death_WV, death_WI, death_WY)

# divide number of death cases by the population to get mortality rate per 
# population, and store them into a new variable "percentage"
clean_data$percentage <- format(round((
  clean_data$death / clean_data$population) * 100, 2), nsmall = 2)

#mapping plot, loading usmap and ggplot2
library(usmap)
library(ggplot2)

#turn percentage variable into numeric
clean_data$percentage <- as.numeric(clean_data$percentage)

#store largest city population data set
cities_t <- usmap_transform(citypop)

#plot U.S. map by states using usmap, and add geom_point with data from cities_t (largest city population)
overall_plot <- plot_usmap(data = clean_data, values = "percentage",  
                           color = "white") + 
  scale_fill_continuous(low = "white", high = "purple", 
                        name = "Mortality Rate Estimate (%)", 
                        label = scales::comma, limits = c(0,0.45)) +
  theme(legend.position = "right") +
  geom_point(data = cities_t,
             aes(x = x, y = y, size = city_pop),
             color = "yellow", alpha = 0.5) +
  scale_size_continuous(name = "Largest City Population",
                        range = c(1, 16),
                        label = scales::comma) +
  labs(title = "States of the United States", 
       subtitle = "Mortality Rate from COVID-19 for Continental United States by states") +
  theme(legend.position = "right")

#plot U.S. map by states that voted for Trump in 2020 Presidential election
rep_plot <- plot_usmap(data = clean_data, values = "percentage",  
                       color = "white", 
                       include = c("TX", "OK", "KS", "NE", "SD", "ND", "WY", 
                                   "MT", "ID", "UT", "IA", "MO", "AR", "LA", 
                                   "MS", "TN", "AL", "FL", "SC", "NC", "KY", 
                                   "IN", "OH", "WV", "AK")) + 
  scale_fill_continuous(low = "white", high = "red", 
                        name = "Mortality Rate Estimate (%)", 
                        label = scales::comma, limits = c(0,0.50)) +
  labs(title = "U.S. Party Affiliation (Republican Party)", 
       subtitle = "Mortality Rate from COVID-19 for Continental United States by republican states") +
  theme(legend.position = "right")

#plot U.S. map by states that voted for Biden in 2020 Presidential election
dem_plot <- plot_usmap(data = clean_data, values = "percentage",  
                       color = "white", 
                       exclude = c("TX", "OK", "KS", "NE", "SD", "ND", "WY", 
                                   "MT", "ID", "UT", "IA", "MO", "AR", "LA", 
                                   "MS", "TN", "AL", "FL", "SC", "NC", "KY", 
                                   "IN", "OH", "WV", "AK")) + 
  scale_fill_continuous(low = "white", high = "blue", 
                        name = "Mortality Rate Estimate (%)", 
                        label = scales::comma, limits = c(0,0.50)) + 
  labs(title = "U.S. Party Affiliation (Democratic Party)", 
       subtitle = "Mortality Rate from COVID-19 for Continental United States by democratic states") +
  theme(legend.position = "right")

#show the plot
overall_plot
rep_plot
dem_plot



  #Make sure you have everything in the same working directory
  library(ggplot2)
  library(tidyverse)
  library(modelr)
  
  covidVaccineDose <- read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv")
  hospitalBeds <- read_csv("data.csv")
  demographics <- read_csv("demographics.csv")
  
  #Tidying for covidVaccineDose
  #Removing all information that won't be needed for modeling
  covidVaccineDose <- covidVaccineDose %>% select(-c(FIPS,Admin2,Lat,Long_,UID,iso2,iso3,code3, Combined_Key))
  
  
  #Filter where Province_State == NA as that is the overall vaccination rate for that country.
  #Then make the dataframe longer by giving every day a separate row
  covidVaccineDose <- covidVaccineDose %>% filter(is.na(Province_State)) %>% pivot_longer(4:ncol(covidVaccineDose), names_to = 'Day', values_to = 'Vaccination') 
  
  
  #Remove Province_State and drop all the NA Vaccinations or Vaccination is 0
  covidVaccineDose <- covidVaccineDose %>% select(-c(Province_State)) %>% filter(!is.na(Vaccination)) %>% filter(Vaccination != 0)

  #Group by Country_Region, add by one for each row . Also rename Day to Days since vaccination
  covidVaccineDose <- covidVaccineDose %>% group_by(Country_Region) %>% mutate(Day = row_number()) %>% rename("Days_Since_First_Vaccination" = Day)
  
  
  #Tidying for Hospital_Beds
  #Group by Country, Get the latest year
  hospitalBeds <- hospitalBeds %>% group_by(Country) %>% slice(which.max(Year))
  #Dropping year variable from hospitalBeds as that's unneeded
  hospitalBeds <- hospitalBeds %>% select(-c(Year))
  
  #Rename countries to be consistent between all tables
  #All name changes are based from covidVaccineDose country names
  hospitalBeds <- hospitalBeds %>% mutate(Country = replace(Country, Country == "Iran (Islamic Republic of)", "Iran")) 
  hospitalBeds <- hospitalBeds %>% mutate(Country = replace(Country, Country == "Republic of Korea", "Korea, South")) 
  hospitalBeds <- hospitalBeds %>% mutate(Country = replace(Country, Country == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom"))
  hospitalBeds <- hospitalBeds %>% mutate(Country = replace(Country, Country == "Venezuela (Bolivarian Republic of)", "Venezuela"))
  hospitalBeds <- hospitalBeds %>% mutate(Country = replace(Country, Country == "United States of America", "US"))
  
  
  #Tidying for demographics
  #Adding both male and female, pulled from homework 5 with some tidying up
  demographics <- demographics %>% pivot_wider(-'Series Name', names_from = `Series Code`, values_from = YR2015) %>% 
    mutate(`Population 80-Up`=(SP.POP.80UP.FE+SP.POP.80UP.MA), Mortality_Rate = (SP.DYN.AMRT.FE+SP.DYN.AMRT.MA), `Population`=(SP.POP.TOTL.FE.IN+SP.POP.TOTL.MA.IN)) %>%
    select(`Country Name`, SP.URB.TOTL, SP.DYN.LE00.IN, `Population 80-Up`, Mortality_Rate,`Population`)
  demographics <- demographics %>% rename("Urban_Pop" = SP.URB.TOTL)
  demographics <- demographics %>% rename("Life_Expectancy_At_Birth" = SP.DYN.LE00.IN )

  
  #Rename countries to be consistent between all tables
  demographics <- demographics %>% mutate(`Country Name` = replace(`Country Name`, `Country Name` == "Iran, Islamic Rep.", "Iran")) 
  demographics <- demographics %>% mutate(`Country Name` = replace(`Country Name`, `Country Name` == "Korea, Rep.", "Korea, South"))
  demographics <- demographics %>% mutate(`Country Name` = replace(`Country Name`, `Country Name` == "Venezuela, RB", "Venezuela"))
  demographics <- demographics %>% mutate(`Country Name` = replace(`Country Name`, `Country Name` == "United States", "US"))
  
  
  ##Joining all the tables together based on Country
  covidVaccineDose <- covidVaccineDose %>% left_join(hospitalBeds, covidVaccineDose, by = c("Country_Region" = "Country"))
  #This actually fills in some missing population figures
  #Got help from here https://stackoverflow.com/questions/42027390/r-how-to-fill-in-missing-value-with-another-dataset-effeciently
  covidVaccineDose <- covidVaccineDose %>% left_join(demographics, covidVaccineDose, by = c("Country_Region" = "Country Name")) %>% mutate(Population.x = ifelse(is.na(Population.x),Population.y, Population.x))
  
  #Remove the column and rename the original column 
  covidVaccineDose <- covidVaccineDose %>% select(-c(Population.y)) %>% rename(Population = c(2))
  
  #Now that we filled in missing population values, calculate vaccination rate and do some renaming for variables
  covidVaccineDose <- covidVaccineDose %>% mutate(Vaccination_Rate = Vaccination/Population)
  covidVaccineDose <- covidVaccineDose %>% rename("Hospital_Beds" = "Hospital beds (per 10 000 population)")
  covidVaccineDose <- covidVaccineDose %>% rename("Mortality_Rate" = "Mortality Rate")
  covidVaccineDose <- covidVaccineDose %>% rename("Population_80-Up" = "Population 80-Up")
  
  
  
  #Filter by Country_Region and filter where Days Since First Vaccination is == to their respecitve max year, return Days Since and Vacccination Rate 
  vacScatterPlotAll <- covidVaccineDose %>% group_by(Country_Region) %>% filter(Days_Since_First_Vaccination == which.max(Days_Since_First_Vaccination)) %>% select(Days_Since_First_Vaccination, Vaccination_Rate) %>% view()
  view(vacScatterPlotAll)
  ggplot(vacScatterPlotAll, aes(Days_Since_First_Vaccination, Vaccination_Rate)) + geom_point()
  
  #Same Scatter Plot but only countries that have vaccinating from 250 days or more
  vacScatterPlotTrimmed <- vacScatterPlot %>% filter(Days_Since_First_Vaccination > 300)
  view(vacScatterPlotTrimmed)
  ggplot(vacScatterPlotTrimmed, aes(Days_Since_First_Vaccination, Vaccination_Rate)) + geom_point()
  
  #Linear Modeling
  #Model 1
  model1 <- lm(Vaccination_Rate ~ Days_Since_First_Vaccination + Hospital_Beds + `Population_80-Up` + Urban_Pop + Mortality_Rate + Life_Expectancy_At_Birth + Population, data = covidVaccineDose)
  summary(model1)

  
  #Model 2 
  covidVaccineDose <- covidVaccineDose %>% mutate(Population_Proportion_80_Up = (`Population_80-Up` / (Population)))
  model2 <- lm(Vaccination_Rate ~ Population_Proportion_80_Up + Hospital_Beds + Days_Since_First_Vaccination , data = covidVaccineDose)
  summary(model2)


  
  #Model 3 
  covidVaccineDose <- covidVaccineDose %>% mutate(Bed_Proportion_80_Up = (Hospital_Beds/ (`Population_80-Up`)))
  model3 <- lm(Vaccination_Rate ~ Bed_Proportion_80_Up + Urban_Pop + Mortality_Rate , data = covidVaccineDose)
  summary(model3)

  
  
  #Model 4 
  covidVaccineDose <- covidVaccineDose %>% mutate(Hospital_BedsSq = Hospital_Beds^2)
  model4 <- lm(Vaccination_Rate ~ Days_Since_First_Vaccination + Hospital_BedsSq + Population + Life_Expectancy_At_Birth , data = covidVaccineDose)
  summary(model4)
  
  

  
  #Model 5 
  covidVaccineDose <- covidVaccineDose %>% mutate(Bed_Proportion_Population = (Hospital_Beds/ (Population)))
  model5 <- lm(Vaccination_Rate ~  Hospital_BedsSq + Bed_Proportion_80_Up + Urban_Pop + `Population_80-Up`, data = covidVaccineDose)
  summary(model5)
  
  #Model 6
  model6 <- lm(Vaccination_Rate ~  Days_Since_First_Vaccination + Hospital_BedsSq + Urban_Pop + `Population_80-Up`, data = covidVaccineDose)
  summary(model6)
  

  #Get all R2 Values
  model1R2 <- round(summary(model1)$adj.r.squared, digits = 3)
  model2R2 <- round(summary(model2)$adj.r.squared, digits = 3)
  model3R2 <- round(summary(model3)$adj.r.squared, digits = 3)
  model4R2 <- round(summary(model4)$adj.r.squared, digits = 3)
  model5R2 <- round(summary(model5)$adj.r.squared, digits = 3)
  model6R2 <- round(summary(model6)$adj.r.squared, digits = 3)
  
  
  R2Frame <- data.frame(
    R2Values = c(model1R2,model2R2,model3R2,model4R2,model5R2,model6R2),
    labels   = c("Model 1","Model 2","Model 3","Model 4","Model 5","Model 6")
  )
  view(R2Frame)
  
  ggplot(R2Frame,aes(x = labels, y = R2Values, label = R2Values )) + geom_bar(stat = "identity", color = "black", fill = "orange") + ggtitle("R2 Values (Higher means greater)")  + geom_text(size = 5, position = position_stack(vjust = 0.5), color = "black")
  

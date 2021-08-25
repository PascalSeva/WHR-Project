install.packages("readxl")
install.packages("sf")
install.packages("tmap")
install.packages("tidyverse")
install.packages("plotly")
install.packages("heatmaply")
install.packages("RColorBrewer")
install.packages("ggplot2")
library("ggplot2")
library("gplots")
library("readxl")
library("sf")
library("tmap")
library("tidyverse")
library("plotly")
library("heatmaply")
library("RColorBrewer")

#Load our dataset 
# (was a xl file, changes to a csv file for easier use)
happiness_report_2021<- read_csv("world-happiness-report-2021.csv")
covid_data <- read_csv("MortalityDataWHR2021C2.csv")
str(happiness_report_2021)
head(happiness_report_2021)
#view dataset
summary(happiness_report_2021)
#View the column names in the report
colnames(happiness_report_2021)
#View the dimensions
dim(happiness_report_2021)
#Removed some columns and save as new variable
hr2021 <- happiness_report_2021[ , -c(4, 5, 6, 13, 14, 15, 16, 17, 18, 19, 20)]
#View the columns in the refined dataset
#Country name, Ladder score, logged GDP per capita, social support, Health life expectancy, freedom to make life choices, generosity, preceptions of corruption, explained by: health life expectancy
#Rename the columns for easier use/reading
hr2021<- hr2021%>% rename(c("Country" = "Country name",
                            "Score" = "Ladder score",
                            "GDP" = "Logged GDP per capita",
                            "Support" = "Social support",
                            "Life Expectancy" = "Healthy life expectancy",
                            "Freedom" = "Freedom to make life choices",
                            "Corruption" = "Perceptions of corruption"))

colnames(hr2021)
head(hr2021)
#Add ranking to the list using the row number 
hr2021 <- hr2021 %>% mutate(Rank = row_number())
hr2021
#Ensure it was added to the table and move it to the first row in table
hr2021 <- hr2021[, c(10, 1, 2, 3, 4, 5, 6, 7, 8, 9)]
#Make the life expectancy name even shorter to view all columns on screen at once. 
hr2021<- hr2021 %>% rename(c("Health" = "Life Expectancy"))
hr2021 <- hr2021 %>% rename(c("Region" = "Regional indicator"))
colnames(hr2021) #it worked. can see all columns at once now

#Check for NA values in data frame
sum(is.na(hr2021)) #0 

#Plot the happiness score by frequency
plot(hr2021$Score, main = "2021 Happiness Score by Frequency", xlab= "Frequency", ylab = "Happiness Score")
head(hr2021)

#Plot happiness score by Region
boxplot(hr2021$Score ~ hr2021$Region, xlab= "Region", ylab="Happiness Score",  main ="2021 Happiness Score by Region")

#REvise COVID Data
colnames(covid_data)
covid_data<- covid_data%>% rename(c("Country" = "Country name",
                                    "Pop20" = "Population 2020",
                                    "Pop19" = "Population 2019",
                                    "COVID_Deaths" = "COVID-19 deaths per 100,000 population in 2020",
                                    "Age" = "Median age",
                                    "Exposure" = "Index of exposure to COVID-19  infections in other countries as of March 31",
                                    "Income" = "Gini coefficient of income",
                                    "All_Deaths_2019" = "All-cause death count, 2019",
                                    "All_Deaths_2020" = "All-cause death count, 2020", 
                                    "Excess_Deaths" = "Excess deaths in 2020 per 100,000 population, relative to 2017-2019 average"))
#Remove columns unnecessary for data review project
covid_data <- covid_data[ , -c(6, 8, 9, 10, 11, 13, 14)]
head(covid_data)

#Plot deaths by Region for 2019 and 2020 and COVID_Deaths by Country
boxplot(covid_data$All_Deaths_2019 ~ covid_data$Country , xlab= "Country", ylab="Deaths", main= "Total Deaths by Country in 2019")
boxplot(covid_data$All_Deaths_2020 ~ covid_data$Country , xlab= "Country", ylab="Deaths", main= "Total Deaths by Country in 2020")
boxplot(covid_data$COVID_Deaths ~ covid_data$Country , xlab= "Country", ylab="Deaths", main= "COVID-19 Related Deaths in 2020")

#Function to convert tibble to matrix
as_matrix <- function(x){
  if(!tibble::is_tibble(x) ) stop("Input must be a tibble.")
  y <- as.matrix.data.frame(x[, -1])
  rownames(y) <- x[[1]]
  y
}

#Matrix variable
mHr2021 <- as_matrix(hr2021)
mHr2021 <- mHr2021[1:10,]

#Remove non-numeric columns
mxHr2021 <- mHr2021[, !colnames(mHr2021) %in% c("Country", "Region")]

#Convert matrix to numeric matrix
mNumHr2021 <- matrix(as.numeric(mxHr2021),    
                     ncol = ncol(mxHr2021))

library("gplots")

heatmap.2(mNumHr2021, 
          cexRow = .9,
          cexCol = 1,
          col = rev(heat.colors(16)),
          main = "Top 10 Countries for Overall Happiness",
          xlab = "Factors",
          ylab = "Countries (Abbreviated)",
          labCol = c("Score", "GDP", "Support", "Health", "Freedom", "Generosity", "Corruption"),
          labRow = c("1. FIN", "2. DEN", "3. CHE", "4. ISL", "5. NLD", "6. NOR", "7. SWE", "8. LUX", "9. NZL", "10. AUT"),
          srtCol = 45
)
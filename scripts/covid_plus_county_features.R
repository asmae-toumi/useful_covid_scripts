library(tidyverse)
library(lubridate)
library(arrow)
library(readxl)
library(zoo)
library(Rcpp)
library(doParallel)

# list.of.packages <- c("ggplot2", "Rcpp", "grf", "caret", "mltools", "rpart", "minpack.lm", "doParallel", "rattle", "anytime")
# list.of.packages <- c(list.of.packages, "zoo","usmap","readxl","lubridate","tidyverse")

# URL of NYT Data
county_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# URL of COVID Tracking Data
track_data <- read_csv("https://covidtracking.com/data/download/all-states-history.csv")


# Pre-processing the data

track_data <- track_data[, !(names(track_data) %in% c("dataQualityGrade"))]
track_data$date <- ymd(track_data$date)

county_data$datetime <- as.Date(county_data$date)
county_data$date <- as.Date(county_data$date)

# CONVERT NYC fips from NA -> 99999
county_data[which(county_data$county=="New York City"),"fips"] <- 99999


# Find the earliest date and latest dates
start_date = min(county_data$datetime)
end_date = max(county_data$datetime)


# Add Columns of days from start

county_data$days_from_start <- as.numeric(county_data$datetime- start_date , units="days")

# Obtain list of fips
fips_list = sort(unique(county_data$fips))

# get active cases
for (fips in fips_list){
  
  fips.df <- county_data[which(county_data$fips==fips),]
  if (dim(fips.df)[1] == 0){
    print(paste("fips ",toString(fips)," has no entry ",sep=""))
    next
  }
  first.fips.date <- min(fips.df$days_from_start)
  last.fips.date <- max(fips.df$days_from_start)
  
  if(first.fips.date == last.fips.date){
    print(paste("fips ",toString(fips)," only has one entry ",sep=""))
    next
  }
  for (day in (first.fips.date+1):last.fips.date){
    print(day)
    county.day.slice <- fips.df[which(fips.df$days_from_start == day),]
    if (dim(county.day.slice)[1] == 0){
      # Missing days inbetween e.g. fips 31057 day 184 jumps to 189
      print(paste("imputing for day ",toString(day)," of fips ",toString(fips),sep=""))
      imputter <- fips.df[which(fips.df$days_from_start == day-1),]
      # Change the date
      imputter$days_from_start <- day
      imputter$datetime <- as.Date(imputter$datetime)+1
      imputter$date <- as.Date(imputter$date)+1
      # Change the deaths
      imputter$deaths<-0
      # Append the data
      fips.df<-rbind(fips.df,imputter)
      county_data <- rbind(county_data,imputter)
    }
  }
  
  if((first.fips.date+22) > last.fips.date){
    for(day in first.fips.date:last.fips.date){
    fips.df[which(fips.df$days_from_start == day),"active_cases"] <- fips.df[which(fips.df$days_from_start == day),"cases"]
    }
    next
  }else{
    
    for(day in first.fips.date:(first.fips.date+21)){
      #total.deaths<- sum(fips.df[which(fips.df$days_from_start<= day),"deaths"])
      fips.df[which(fips.df$days_from_start == day),"active_cases"] <- fips.df[which(fips.df$days_from_start == day),"cases"]
    }
    
    for (day in (first.fips.date+22):last.fips.date){
      fips.df[which(fips.df$days_from_start == day),"active_cases"] <- fips.df[which(fips.df$days_from_start == day),"cases"] - fips.df[which(fips.df$days_from_start == day-22),"cases"]
      
      }
    
    }
  county_data[which(county_data$fips==fips),"cases"] <- fips.df[,"active_cases"]
  
  print(fips)
}

# Take 7 day rolling average per county
foreach(fips = fips_list)%do%{
  county_slice = county_data[which(county_data$fips==fips), ]
  county_slice$rolled_cases = zoo::rollmean(county_slice$cases, 7, fill=NA, align="right")
  county_data[which(county_data$fips==fips), "rolled_cases"] <- county_slice$rolled_cases
}


# Obtain the daily new cases
present.fips.list <- sort(unique(county_data$fips))

county_data_backup <- county_data
for (fips in present.fips.list){
  
  fips.df <- county_data[which(county_data$fips==fips & !is.na(county_data$rolled_cases)),]
  if (dim(fips.df)[1] == 0){
    print(paste("fips ",toString(fips)," has no entry ",sep=""))
    next
  }
  first.fips.date <- min(fips.df$days_from_start)
  last.fips.date <- max(fips.df$days_from_start)
  #fips.df[which(fips.df$days_from_start == first.fips.date),"new_rolled_cases"] <- fips.df[which(fips.df$days_from_start == first.fips.date),"rolled_cases"]
  print(fips)
  if(first.fips.date == last.fips.date){
    print(paste("fips ",toString(fips)," only has one entry ",sep=""))
    next
  }
  for (day in (first.fips.date+1):last.fips.date){
    print(day)
    county.day.slice <- fips.df[which(fips.df$days_from_start == day),]
    if (dim(county.day.slice)[1] == 0){
      # Missing days inbetween e.g. fips 31057 day 184 jumps to 189
      print(paste("imputing for day ",toString(day)," of fips ",toString(fips),sep=""))
      imputter <- fips.df[which(fips.df$days_from_start == day-1),]
      # Change the date
      imputter$days_from_start <- day
      
      imputter$datetime <- as.Date(imputter$datetime)+1
      imputter$date <- as.Date(imputter$date)+1
      # Append the data
      fips.df<-rbind(fips.df,imputter)
      county_data <- rbind(county_data,imputter)
      
    }
   
  }
  
  
}

# Slice away first 6 days
county_data = county_data[complete.cases(county_data),]

# Process county features
county_features <- read_csv("data/county_features.csv")


# Drop all "M_..." prefix
county_features <- county_features[,which(!grepl("M_",names(county_features)))]


# Convert -999 to NA
county_features[county_features==-999] <-NA


# DROP ST, STATE, ST_ABBR, COUNTY, LOCATION since we already have fips
county_features <- county_features[, -which(names(county_features) %in% c("ST","STATE","ST_ABBR","COUNTY","LOCATION"))]
names(county_features)[names(county_features)=="FIPS"] <- "fips"
county_data_augmented <- merge(x=county_data, y=county_features, by="fips", all.x = TRUE)

# end_file = paste("data/augmented_us-counties_latest",".csv",sep="")
# 
# write.csv(county_data_augmented, end_file, row.names=FALSE)

# Load CUSP Data
CUSP = paste("data/COVID-19 US state policy database 1_7_2021",".xlsx",sep="")

# Pre-processing CUSP data
df <- read_excel(CUSP, sheet=1, skip=1, n_max=54)

names(df) <- gsub(" ","_",names(df))

library(usmap)

for(i in 1:length(names(df))){
  if(df[3,i]=="date"){
    df[,i][df[,i]=="0"]<-"50000"
    df[,i]<-as.Date(as.numeric(unlist(df[,i])), origin="1899-12-30")
    }
}

df <- df[-c(1,2,3),]

# DROP State
df <- df[, -which(names(df) %in% c("State"))]

#merge CUSP Data with county_data_augmented Data
county_data_augmented["FIPS_Code"]<- as.numeric(fips(county_data_augmented$state, 
                                                     county = c()))

data <- merge(x=county_data_augmented, y=df, by = "FIPS_Code", all.x = TRUE)

data <-data %>% rename(State_FIPS_Code=FIPS_Code)

data$datetime<-as.Date(data$datetime, "%Y-%m-%d")


for (i in (length(names(county_data_augmented))+2):length(names(data))) {
  if (inherits(data[,i], 'Date')){
    data[,i]<-data$datetime-data[,i]+1
    data[,i][data[,i]<0]<-0
    data[,i]<-as.numeric(data[,i])
  }
  if (inherits(data[,i], 'character')){
    data[,i]<-as.numeric(data[,i])
  }
}

# merge COVID Tracking Data with data

track_data <- track_data %>% rename(State_Abbreviation=state)
dataT <- merge(x=data, y=track_data, by = c("State_Abbreviation","date"), all.x = TRUE)

# DROP State_Abbreviation, state 
dataT <- dataT[, -which(names(dataT) %in% c("State_Abbreviation"))]

covid_plus_features <- dataT %>% filter(date >= "2020-07-28")

write_parquet(covid_plus_features, "covid_plus_features.parquet")


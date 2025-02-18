library(ggplot2)
library(patchwork)
library(tidyverse)
library(quantmod)
library(dplyr)
library(ggrepel) 
library(readxl)
library(bayesbio)

getwd()
setwd("~/Documents/R/BodyTempData")
body_temps <- read_excel("BodyTemperatures.xlsx")
flat_temps <- read_excel("FlatlineTemperatures.xlsx")



#reading in flatline temperatures from heartrate trials


#making a new column for average body temperatures in the field (average ABT - body temperature)
body_temps$Species <- factor(body_temps$Species, levels = c("NL", "ME", "LO" , "LS", "LL"))
body_temps$warming_tolerance <- NA
body_temps$warming_tolerance <- ifelse(body_temps$Species == "NL", (35.76200-body_temps$temp), body_temps$warming_tolerance)
body_temps$warming_tolerance <- ifelse(body_temps$Species == "LL", (39.16118-body_temps$temp), body_temps$warming_tolerance)
body_temps$warming_tolerance <- ifelse(body_temps$Species == "LS", (36.45583-body_temps$temp), body_temps$warming_tolerance)
body_temps$warming_tolerance <- ifelse(body_temps$Species == "LO", (38.33417-body_temps$temp), body_temps$warming_tolerance)
body_temps$warming_tolerance <- ifelse(body_temps$Species == "ME", (37.22800-body_temps$temp), body_temps$warming_tolerance)

#seeing how many days I've collected data for
unique(body_temps$DateCollected)

#to use stats test I need to attach thermal limits to the body_Temps file

#ENVIRONMENTAL TEMPERATURES
#reading all the data files in system and putting them into one list
setwd("/Users/chanceyan/Documents/R/BodyTempData/LoggerData")
fulldata <- list.files(pattern = ".csv") 
names <- vector()
Etemp = data.frame()

for(i in 1:length(fulldata)){
  Etemp <- c(Etemp, list(read.csv(fulldata[i])))
  names <- c(names, fulldata[i])
}

#Covert all time formats to correct formats
for(i in 1:length(fulldata)){
  DF <-strptime(Etemp[[i]]$time,format="%m/%d/%y %H:%M")
  Etemp[[i]]$time<-as.POSIXct(DF,format="%Y-%m-%d%H:%M")
}

#setting time zone to EST manually
hrs <- hours(5) 
{
Etemp[[1]]$time <- Etemp[[1]]$time - hrs 
Etemp[[2]]$time <- Etemp[[2]]$time - hrs 
Etemp[[3]]$time <- Etemp[[3]]$time - hrs 
Etemp[[4]]$time <- Etemp[[4]]$time - hrs 
Etemp[[5]]$time <- Etemp[[5]]$time - hrs 
Etemp[[6]]$time <- Etemp[[6]]$time - hrs 
Etemp[[7]]$time <- Etemp[[7]]$time - hrs 
Etemp[[8]]$time <- Etemp[[8]]$time - hrs 
Etemp[[9]]$time <- Etemp[[9]]$time - hrs 
Etemp[[10]]$time <- Etemp[[10]]$time - hrs 
Etemp[[11]]$time <- Etemp[[11]]$time - hrs 
Etemp[[12]]$time <- Etemp[[12]]$time - hrs 
Etemp[[13]]$time <- Etemp[[13]]$time - hrs 
Etemp[[14]]$time <- Etemp[[14]]$time - hrs 
Etemp[[15]]$time <- Etemp[[15]]$time - hrs 
Etemp[[16]]$time <- Etemp[[16]]$time - hrs 
Etemp[[17]]$time <- Etemp[[17]]$time - hrs 
}

#graphs all the temperature loggers fully
for(i in 1:length(Etemp)){
  plot <- ggplot(Etemp[[i]], aes(x = time, y = temp))+
    geom_point()+
    ggtitle(names[i])
  print(plot)
  #name = paste(i, "plot.png", sep="")
  #ggsave(name, plot = last_plot(),path = "/Users/chanceyan/Documents/R/BodyTempData")
}

#Need to create a new column in body_temps with time and date to match with environmental temperatures
#converting all time to correct format
#body_temps$time <-strptime(body_temps$time,format="%Y-%m-%d %H:%M")
#body_temps$time <- as.POSIXct(body_temps$time,format="%y-%m-%d%H:%M")

names
sites <- c("seaside", "seaside", "seaside", "seaside", "seaside", "seaside", "seaside", "seaside", "seaside",
           "loblolly", "loblolly", "loblolly", "loblolly", "loblolly", "loblolly", "loblolly", "loblolly")
Niches <- c("seaweed", "open", "crevice", "seaweed", "open", "crevice", 
           "vertical", "open", "crevice", "seaweed", "open", "crevice", "open",
           "crevice", "vertical", "open", "crevice")
Tides <- c("middle", "middle", "middle",
           "lower", "lower", "lower", 
           "upper", "upper", "upper",
           "middle", "middle", "middle",
           "lower", "lower",
           "upper", "upper", "upper")

#need to split up because it's too long if you don't
#matches body temperature to environmental temperature
#takes a long time 

r.value <- c()
logger.name <- c()
species.name <- c()
total.count <- c()
sig.track <- c()
p.value <- c()
#via t-test
over.under <- c()
avg <- c()

library(car)
#i = 1
for(i in 1:17){
    #GRAPING animal body temperatures against logger temperatures for their respective niche and tide height and site and time
    #sepparating data to comepare animals found in certain areas to their respective temperature logger
    subDF <- subset(body_temps, body_temps$Tide == Tides[i] & body_temps$NicheSpace == Niches[i] & body_temps$Site == sites[i])
    #remove all columns in the dataset
    subDF <- data.frame(subDF$time, subDF$temp, subDF$Species)
    colnames(subDF)[1] ="time"
    colnames(subDF)[2] ="temp"
    colnames(subDF)[3] ="species"
    #combine both environmental temperatures and body temperatures by time and date
    #then graph them by temperatures
    combined <- nearestTime(subDF, Etemp[[i]], "time", "time")
    colnames(combined)[4] = "EnvironmentTemperature"
    colnames(combined)[2] = "BodyTemperature"
    #title <- paste("Environmental Temperatures vs Body Temperatures for ", Tides[i], " Zone and ", Niches[i], " Niche Space at ", sites[i], sep = "")
    #print(ggplot(combined, aes(x = BodyTemperature, y = EnvironmentTemperature, color = species))+
    #       geom_point()+
    #        geom_smooth(method='lm', aes(x=BodyTemperature, y=EnvironmentTemperature, colour = species),formula=y~x, se=FALSE, span=.65, fullrange=TRUE)+
    #        ggtitle(title)+
    #        geom_abline(slope = 1))
    
    LO <- subset(combined, species == "LO")
    LL <- subset(combined, species == "LL")
    NL <- subset(combined, species == "NL")
    LS <- subset(combined, species == "LS")
    ME <- subset(combined, species == "ME")

#--------------------------------------------------------LO
    
    if(nrow(LO) > 10){
      lm.temp <- lm(LO$EnvironmentTemperature~LO$BodyTemperature)
      r.value <- c(r.value, summary(lm.temp)$r.squared)
      logger.name <- c(logger.name, names[i])
      species.name <- c(species.name, "LO")
      total.count <- c(total.count, length(LO$BodyTemperature))
      c.test <- cor.test(LO$EnvironmentTemperature, LO$BodyTemperature, method = "pearson")
      
      dif.list <- c()
      average <- c()
      
      for(j in 1:nrow(LO)){
        dif.list <- c(dif.list, (LO$BodyTemperature[j] - LO$EnvironmentTemperature[j]))
        average <- c(average, (LO$BodyTemperature[j] - LO$EnvironmentTemperature[j]))
      }
      
      ttest <- t.test(dif.list, mu = 0)
      over.under <- c(over.under, ttest$p.value)
      average <- mean(average)
      avg <- c(avg, average)
      
      if(c.test$p.value <= 0.05){
        sig.track <- c(sig.track, "yes")
        
        print(ggplot(LO, aes(LO$BodyTemperature)) + 
          geom_histogram(binwidth=1) +
          ggtitle("Hist of LO Body Temp"))
        
        print(qqPlot(LO$BodyTemperature))
        
        print(ggplot(LO, aes(LO$EnvironmentTemperature)) + 
                geom_histogram(binwidth=1) +
                ggtitle("Hist of LO ENV Temp"))
        
        print(qqPlot(LO$EnvironmentTemperature))
      }
      else{
        sig.track <- c(sig.track, "no")
      }
      p.value <- c(p.value, c.test$p.value)
    }
    else{
      r.value <- c(r.value, NA)
      logger.name <- c(logger.name, names[i])
      species.name <- c(species.name, "LO")
      total.count <- c(total.count, length(LO$BodyTemperature))
      sig.track <- c(sig.track, "no")
      p.value <- c(p.value, NA)
      over.under <- c(over.under, NA)
      
      
      for(j in 1:nrow(LO)){
        average <- c(average, (LO$BodyTemperature[j] - LO$EnvironmentTemperature[j]))
      }
      average <- mean(average)
      avg <- c(avg, average)
    }
    
#-------------------------------------------------------LL
    
    if(nrow(LL) > 10){
      lm.temp <- lm(LL$EnvironmentTemperature~LL$BodyTemperature)
      r.value <- c(r.value, summary(lm.temp)$r.squared)
      logger.name <- c(logger.name, names[i])
      species.name <- c(species.name, "LL")
      total.count <- c(total.count, length(LL$BodyTemperature))
      
      c.test <- cor.test(LL$EnvironmentTemperature, LL$BodyTemperature, method = "pearson")
      
      dif.list <- c()
      average <- c()
      
      for(j in 1:nrow(LL)){
        dif.list <- c(dif.list, (LL$BodyTemperature[j] - LL$EnvironmentTemperature[j]))
        average <- c(average, (LL$BodyTemperature[j] - LL$EnvironmentTemperature[j]))
      }
      
      ttest <- t.test(dif.list, mu = 0)
      over.under <- c(over.under, ttest$p.value)
      average <- mean(average)
      avg <- c(avg, average)
      
      if(c.test$p.value <= 0.05){
        sig.track <- c(sig.track, "yes")
        
        print(ggplot(LL, aes(LL$BodyTemperature)) + 
                geom_histogram(binwidth=1) +
                ggtitle("Hist of LL Body Temp"))
        
        print(qqPlot(LL$BodyTemperature))
        
        print(ggplot(LL, aes(LL$EnvironmentTemperature)) + 
                geom_histogram(binwidth=1) +
                ggtitle("Hist of LL ENV Temp"))
        
        print(qqPlot(LL$BodyTemperature))
      }
      else{
        sig.track <- c(sig.track, "no")
      }
      p.value <- c(p.value, c.test$p.value)
    }
    else{
      r.value <- c(r.value, NA)
      logger.name <- c(logger.name, names[i])
      species.name <- c(species.name, "LL")
      total.count <- c(total.count, length(LL$BodyTemperature))
      sig.track <- c(sig.track, "no")
      p.value <- c(p.value, NA)
      over.under <- c(over.under, NA)
      
      for(j in 1:nrow(LL)){
        average <- c(average, (LL$BodyTemperature[j] - LL$EnvironmentTemperature[j]))
      }
      average <- mean(average)
      avg <- c(avg, average)
    }

#-----------------------------------------------------LS
    
    if(nrow(LS) > 10){
      lm.temp <- lm(LS$EnvironmentTemperature~LS$BodyTemperature)
      r.value <- c(r.value, summary(lm.temp)$r.squared)
      logger.name <- c(logger.name, names[i])
      species.name <- c(species.name, "LS")
      total.count <- c(total.count, length(LS$BodyTemperature))
      
      c.test <- cor.test(LS$EnvironmentTemperature, LS$BodyTemperature, method = "pearson")
      
      dif.list <- c()
      average <- c()
      
      for(j in 1:nrow(LS)){
        dif.list <- c(dif.list, (LS$BodyTemperature[j] - LS$EnvironmentTemperature[j]))
        average <- c(average, (LS$BodyTemperature[j] - LS$EnvironmentTemperature[j]))
      }
      
      ttest <- t.test(dif.list, mu = 0)
      over.under <- c(over.under, ttest$p.value)
      average <- mean(average)
      avg <- c(avg, average)
      
      if(c.test$p.value <= 0.05){
        sig.track <- c(sig.track, "yes")
        
        print(ggplot(LS, aes(LS$BodyTemperature)) + 
                geom_histogram(binwidth=1) +
                ggtitle("Hist of LS Body Temp"))
        
        print(qqPlot(LS$BodyTemperature))
        
        print(ggplot(LS, aes(LS$EnvironmentTemperature)) + 
                geom_histogram(binwidth=1) +
                ggtitle("Hist of LS ENV Temp"))
        
        print(qqPlot(LS$EnvironmentTemperature))
      }
      else{
        sig.track <- c(sig.track, "no")
      }
      p.value <- c(p.value, c.test$p.value)
    }
    else{
      r.value <- c(r.value, NA)
      logger.name <- c(logger.name, names[i])
      species.name <- c(species.name, "LS")
      total.count <- c(total.count, length(LS$BodyTemperature))
      sig.track <- c(sig.track, "no")
      p.value <- c(p.value, NA)
      over.under <- c(over.under, NA)
      
      for(j in 1:nrow(LS)){
        average <- c(average, (LS$BodyTemperature[j] - LS$EnvironmentTemperature[j]))
      }
      average <- mean(average)
      avg <- c(avg, average)
    }

#--------------------------------------------------NL    

    if(nrow(NL) > 10){
      lm.temp <- lm(NL$EnvironmentTemperature~NL$BodyTemperature)
      r.value <- c(r.value, summary(lm.temp)$r.squared)
      logger.name <- c(logger.name, names[i])
      species.name <- c(species.name, "NL")
      total.count <- c(total.count, length(NL$BodyTemperature))
      
      c.test <- cor.test(NL$EnvironmentTemperature, NL$BodyTemperature, method = "pearson")
      
      dif.list <- c()
      average <- c()
      
      for(j in 1:nrow(NL)){
        dif.list <- c(dif.list, (NL$BodyTemperature[j] - NL$EnvironmentTemperature[j]))
        average <- c(average, (NL$BodyTemperature[j] - NL$EnvironmentTemperature[j]))
      }
      
      ttest <- t.test(dif.list, mu = 0)
      over.under <- c(over.under, ttest$p.value)
      average <- mean(average)
      avg <- c(avg, average)
      
      if(c.test$p.value <= 0.05){
        sig.track <- c(sig.track, "yes")
        
        print(ggplot(NL, aes(NL$BodyTemperature)) + 
                geom_histogram(binwidth=1) +
                ggtitle("Hist of NL Body Temp"))
        
        print(qqPlot(NL$BodyTemperature))
        
        print(ggplot(NL, aes(NL$EnvironmentTemperature)) + 
                geom_histogram(binwidth=1) +
                ggtitle("Hist of NL ENV Temp"))
        
        print(qqPlot(NL$EnvironmentTemperature))
      }
      else{
        sig.track <- c(sig.track, "no")
      }
      p.value <- c(p.value, c.test$p.value)
    }
    else{
      r.value <- c(r.value, NA)
      logger.name <- c(logger.name, names[i])
      species.name <- c(species.name, "NL")
      total.count <- c(total.count, length(NL$BodyTemperature))
      sig.track <- c(sig.track, "no")
      p.value <- c(p.value, NA)
      over.under <- c(over.under, NA)
      
      for(j in 1:nrow(NL)){
        average <- c(average, (NL$BodyTemperature[j] - NL$EnvironmentTemperature[j]))
      }
      average <- mean(average)
      avg <- c(avg, average)
    }
    
#--------------------------------------------ME
    
    if(nrow(ME) > 10){
      lm.temp <- lm(ME$EnvironmentTemperature~ME$BodyTemperature)
      r.value <- c(r.value, summary(lm.temp)$r.squared)
      logger.name <- c(logger.name, names[i])
      species.name <- c(species.name, "ME")
      total.count <- c(total.count, length(ME$BodyTemperature))
      
      c.test <- cor.test(ME$EnvironmentTemperature, ME$BodyTemperature, method = "pearson")
      
      dif.list <- c()
      average <- c()
      
      for(j in 1:nrow(ME)){
        dif.list <- c(dif.list, (ME$BodyTemperature[j] - ME$EnvironmentTemperature[j]))
        average <- c(average, (ME$BodyTemperature[j] - ME$EnvironmentTemperature[j]))
      }
      
      ttest <- t.test(dif.list, mu = 0)
      over.under <- c(over.under, ttest$p.value)
      average <- mean(average)
      avg <- c(avg, average)
      
      if(c.test$p.value <= 0.05){
        sig.track <- c(sig.track, "yes")
        
        print(ggplot(ME, aes(ME$BodyTemperature)) + 
                geom_histogram(binwidth=1) +
                ggtitle("Hist of ME Body Temp"))
        
        print(qqPlot(ME$BodyTemperature))
        
        print(ggplot(ME, aes(ME$EnvironmentTemperature)) + 
                geom_histogram(binwidth=1) +
                ggtitle("Hist of ME ENV Temp"))
        
        print(qqPlot(ME$EnvironmentTemperature))
      }
      else{
        sig.track <- c(sig.track, "no")
      }
      p.value <- c(p.value, c.test$p.value)
    }
    else{
      r.value <- c(r.value, NA)
      logger.name <- c(logger.name, names[i])
      species.name <- c(species.name, "ME")
      total.count <- c(total.count, length(ME$BodyTemperature))
      sig.track <- c(sig.track, "no")
      p.value <- c(p.value, NA)
      over.under <- c(over.under, NA)
      
      for(j in 1:nrow(ME)){
        average <- c(average, (ME$BodyTemperature[j] - ME$EnvironmentTemperature[j]))
      }
      average <- mean(average)
      avg <- c(avg, average)
    }
    
}
  #make graphs equal and both axis go to 50???
  #unsure of how to set graph scales

#Creating a heat map with P values of the loggers
heat_data <- data.frame(r.value, logger.name, species.name, total.count, sig.track, p.value, over.under, avg)
man_data <- heat_data
for(i in 1:nrow(man_data)){
  if(is.na(heat_data[i,7])){
    man_data[i, 8] <- NA
  }
}
ggplot(man_data, aes(species.name, logger.name, fill= p.value)) + 
  geom_tile() +
  theme_classic() +
  ggtitle("Heat map of P-values matching logger temperatures to species temperatures")

ggplot(man_data, aes(species.name, logger.name, fill= avg)) + 
  geom_tile() +
  theme_classic() +
  ggtitle("Heat map of Body Temperature Differences to Logger Temperatures")

#this bit of code chunk allows you to graph logger data during specific times mannually set time range below
{
  start_time <- as.POSIXct("2024-08-30 12:00:00")
  end_time <- as.POSIXct("2024-08-30 24:00:00")
  work_time <- as.POSIXct("2024-08-30 15:19:00")
  work_end_time <- as.POSIXct("2024-08-30 17:00:00")
  for(i in 1:17){
    print(ggplot(Etemp[[i]], aes(x = time, y = temp))+
      geom_point()+
      scale_x_datetime(limits = c(start_time, end_time))+
      ggtitle(names[i]))
      #+ geom_vline(xintercept = c(work_time, work_end_time), color="red", 
       #                             linetype="dashed", size=1.5))
    #name = paste(i, "plot.png", sep="")
    #ggsave(name, plot = last_plot(),path = "/Users/chanceyan/Documents/R/BodyTempData/Graphs/8.30.24")
  }
  
}

#-----------------------------------------------------------
#this section finds the hottest temperatures according to the different temperautre loggers and then graphs those temperautre
#loggers on those hottest days

#this was the first day we installed and implemented the loggers
start_time <- as.POSIXct("2024-07-11 00:00:00")
end_time <- as.POSIXct("2024-07-12 00:00:00")

#this code goes through every single logger and parses through every single temperature in the file
#if a temperature is above 47 then it records the date and time of that temperature 
dates <- NULL
logger <- NULL
for(i in 1:17){
  for(j in 1:nrow(Etemp[[i]])){
    if(Etemp[[i]]$temp[j] > 45){
      #print(Etemp[[i]]$time[j])
      #print(Etemp[[i]]$temp[j])
      date <- Etemp[[i]]$time[j]
      day <- as.Date(date)
      dates <- c(dates, toString(day))
      logger <- c(logger, names[i])
    }
  }
}

#this creates a new vector with all the unique dates from the hottest temperatures, so we can a vector
#containing all the days that alerted ANY logger above 47 degrees celcius. The logger vector holds logger names
#for those that reached those high temperatures
dates <- unique(dates)
logger <- unique(logger)

for(i in 1:length(dates)){
  dates[i] <- paste(dates[i], "00:00", sep = " ")
}

names

#Taking those dates with the hottest temperatures, we then graph every logger for every single one of those dates...
#it's a bit long
for(j in 1:17){
  for(i in 1:length(dates)){
    start_time <- as.POSIXct(dates[i])
    print(start_time)
    end_time <- as.POSIXct(start_time) + 24 * 60 * 60
    print(end_time)
    print(ggplot(Etemp[[j]], aes(x = time, y = temp)) 
          + geom_point()
          + scale_x_datetime(limits = c(start_time, end_time))
          + ylim(0,50)
          + ggtitle(names[j]))
  }
}

#-------------------------------------
#trying to mark when animals are below warming tolerance
body_temps$below_ABT <- "holder"
for(i in 1:nrow(body_temps)){
  if(body_temps[i, 16] < 0){
    body_temps[i, 17] = "yes"
  }
  else{
    body_temps[i, 17] = "no"
  }
}

below_set <- subset(body_temps, below_ABT == "yes")

below_set$time <- as.Date(below_set$time)

ggplot (below_set, aes(x = time)) +
  geom_bar(aes(fill=Species), stat="count")

#highlights time we sampled and checks if we sampled during the highest time during the day

dates <- unique(body_temps$DateCollected)

#setting time zone to EST
hrs <- hours(4) 
for(i in 1:length(dates)){
  dates[i] <- dates[i] + hrs
}

dates <- strftime(dates)

for(i in 1:17){
  for(j in 1:length(dates)){
    start_time <- as.POSIXct(dates[j])
    end_time <- as.POSIXct(paste(dates[j], "24:00:00"))
    work_time <- as.POSIXct(paste(dates[j], "13:00:00"))
    work_end_time <- as.POSIXct(paste(dates[j], "17:00:00"))
    
    print(ggplot(Etemp[[i]], aes(x = time, y = temp))+
            geom_point()+
            scale_x_datetime(limits = c(start_time, end_time))+
            ggtitle(names[i])+ geom_vline(xintercept = c(work_time, work_end_time), color="red", 
                                          linetype="dashed", size=1.5))
  }
}


library(rnoaa)

#STATS TESTING
#----------------------------------

#reset highest
highest_body_temps <- NULL

#ADJUSTING DATA TO HAVE APPROPRIATE BODY TEMPERATURES - only taking exposed refugia animals
#highest_body_temps <- rbind(highest_body_temps, subset(body_temps, NicheSpace == "seaweed exposed"))
#highest_body_temps <- rbind(highest_body_temps, subset(body_temps, NicheSpace == "vertical exposed"))
#highest_body_temps <- rbind(highest_body_temps, subset(body_temps, NicheSpace == "crevice exposed"))
#highest_body_temps <- rbind(highest_body_temps, subset(body_temps, NicheSpace == "open"))



#top 5% of values
#collect top 5% of each species then append all together
holdDF <- subset(body_temps, Species == "LO" & Site == "seaside")
holdDF <- subset(holdDF, temp > quantile(temp, prob = 1 - 5/100))
highest_body_temps <- rbind(highest_body_temps, holdDF)
holdDF <- subset(body_temps, Species == "LO" & Site == "loblolly")
holdDF <- subset(holdDF, temp > quantile(temp, prob = 1 - 5/100))
highest_body_temps <- rbind(highest_body_temps, holdDF)

holdDF <- subset(body_temps, Species == "LS" & Site == "seaside")
holdDF <- subset(holdDF, temp > quantile(temp, prob = 1 - 5/100))
highest_body_temps <- rbind(highest_body_temps, holdDF)
holdDF <- subset(body_temps, Species == "LS" & Site == "loblolly")
holdDF <- subset(holdDF, temp > quantile(temp, prob = 1 - 5/100))
highest_body_temps <- rbind(highest_body_temps, holdDF)

holdDF <- subset(body_temps, Species == "LL" & Site == "seaside")
holdDF <- subset(holdDF, temp > quantile(temp, prob = 1 - 5/100, na.rm = TRUE)) #MISSING SOME LL DATA I GUESS SMH
highest_body_temps <- rbind(highest_body_temps, holdDF)
holdDF <- subset(body_temps, Species == "LL" & Site == "loblolly")
holdDF <- subset(holdDF, temp > quantile(temp, prob = 1 - 5/100, na.rm = TRUE)) #MISSING SOME LL DATA I GUESS SMH
highest_body_temps <- rbind(highest_body_temps, holdDF)

holdDF <- subset(body_temps, Species == "ME" & Site == "seaside")
holdDF <- subset(holdDF, temp > quantile(temp, prob = 1 - 5/100))
highest_body_temps <- rbind(highest_body_temps, holdDF)
holdDF <- subset(body_temps, Species == "ME" & Site == "loblolly")
holdDF <- subset(holdDF, temp > quantile(temp, prob = 1 - 5/100))
highest_body_temps <- rbind(highest_body_temps, holdDF)

holdDF <- subset(body_temps, Species == "NL" & Site == "seaside")
holdDF <- subset(holdDF, temp > quantile(temp, prob = 1 - 5/100))
highest_body_temps <- rbind(highest_body_temps, holdDF)
holdDF <- subset(body_temps, Species == "NL" & Site == "lob")
holdDF <- subset(holdDF, temp > quantile(temp, prob = 1 - 5/100))
highest_body_temps <- rbind(highest_body_temps, holdDF)

#-----------------------------------------------------------------
#START BY MAKING GRAPHS OF THINGS YOU SHOULD PLOT AND MAKE STATS OF
#Perhaps start by diving all the animals by themselves so you compare apples to apples
#graphing each comparison to see if anything is interesting
#this set of comparisons focuses on differneces between species across sites
compare.group.1 <- subset(body_temps, Tide == "lower" & Species != "LS")

ggplot(compare.group.1, aes(x = Species, y = warming_tolerance))+
  geom_violin()+
  facet_wrap(~Site)+
  ggtitle("Lower Zone")

compare.group.2 <- subset(body_temps, Tide == "middle" & Species != "LS")

ggplot(compare.group.2, aes(x = Species, y = warming_tolerance))+
  geom_violin()+
  facet_wrap(~Site)+
  ggtitle("Middle Zone")

compare.group.3 <- subset(body_temps, Tide == "upper" & Species != "ME" & Species != "LO" & Species != "NL")

ggplot(compare.group.3, aes(x = Species, y = warming_tolerance))+
  geom_violin()+
  facet_wrap(~Site)+
  ggtitle("Upper Zone")

#---------------------------------------------------
#this goes within nichespace and species and compares across sites 
#NUCELLA LAPILLIS

compare.group.4 <- subset(body_temps, Tide == "lower" & Species == "NL")
#look for and remove groups with small amt of observations (idk what small is yet)
tapply(compare.group.4, compare.group.4$NicheSpace, FUN = count)
compare.group.4 <- subset(compare.group.4, NicheSpace == "seaweed" | NicheSpace == "water" | NicheSpace == "crevice" | NicheSpace == "vertical")

ggplot(compare.group.4, aes(x = NicheSpace, y = warming_tolerance))+
  geom_violin()+
  facet_wrap(~Site)+
  ggtitle("NL in the lower zone")

#--------------------------------------------------
#MYTILUS EDULIS
#look for and remove groups with small amt of observations (idk what small is yet)
compare.group.5 <- subset(body_temps, Tide == "lower" & Species == "ME" & Site == "seaside")
tapply(compare.group.5, compare.group.5$NicheSpace, FUN = count)
compare.group.5 <- subset(body_temps, Tide == "lower" & Species == "ME" & Site == "loblolly")
tapply(compare.group.5, compare.group.5$NicheSpace, FUN = count)

#after deciding what has enough observations, subset the dataset to only have those observations
compare.group.5 <- subset(body_temps, Tide == "lower" & Species == "ME")
compare.group.5 <- subset(compare.group.5, NicheSpace == "open" | NicheSpace == "seaweed" | NicheSpace == "water" | NicheSpace == "seaweed exposed")

ggplot(compare.group.5, aes(x = NicheSpace, y = warming_tolerance))+
  geom_violin()+
  facet_wrap(~Site)+
  ggtitle("ME in the lower zone")

#----------------------------------------------------
#Littorina saxatilis
#look for and remove groups with small amt of observations (idk what small is yet)
compare.group.6 <- subset(body_temps, Tide == "upper" & Species == "LS" & Site == "seaside")
tapply(compare.group.6, compare.group.6$NicheSpace, FUN = count)
compare.group.6 <- subset(body_temps, Tide == "upper" & Species == "LS" & Site == "loblolly")
tapply(compare.group.6, compare.group.6$NicheSpace, FUN = count)

#after deciding what has enough observations, subset the dataset to only have those observations
compare.group.6 <- subset(body_temps, Tide == "upper" & Species == "LS")
compare.group.6 <- subset(compare.group.6, NicheSpace == "vertical" | NicheSpace == "crevice")

ggplot(compare.group.6, aes(x = NicheSpace, y = warming_tolerance))+
  geom_violin()+
  facet_wrap(~Site)+
  ggtitle("LS in the upper zone")

#----------------------------------------------------------
#Littorina obtusata
compare.group.7 <- subset(body_temps, Tide == "lower" & Species == "LO" & Site == "seaside")
tapply(compare.group.7, compare.group.7$NicheSpace, FUN = count)
compare.group.7 <- subset(body_temps, Tide == "lower" & Species == "LO" & Site == "loblolly")
tapply(compare.group.7, compare.group.7$NicheSpace, FUN = count)

#after deciding what has enough observations, subset the dataset to only have those observations
compare.group.7 <- subset(body_temps, Tide == "lower" & Species == "LO")
compare.group.7 <- subset(compare.group.7, NicheSpace == "seaweed")

ggplot(compare.group.7, aes(x = NicheSpace, y = warming_tolerance))+
  geom_violin()+
  facet_wrap(~Site)+
  ggtitle("LO in the lower zone")

compare.group.8 <- subset(body_temps, Tide == "middle" & Species == "LO" & Site == "seaside")
tapply(compare.group.8, compare.group.8$NicheSpace, FUN = count)
compare.group.8 <- subset(body_temps, Tide == "middle" & Species == "LO" & Site == "loblolly")
tapply(compare.group.8, compare.group.8$NicheSpace, FUN = count)

#after deciding what has enough observations, subset the dataset to only have those observations
compare.group.8 <- subset(body_temps, Tide == "middle" & Species == "LO")
compare.group.8 <- subset(compare.group.8, NicheSpace == "seaweed")

ggplot(compare.group.8, aes(x = NicheSpace, y = warming_tolerance))+
  geom_violin()+
  facet_wrap(~Site)+
  ggtitle("LO in the middle zone")

#----------------------------------------------------------
#Littorina Littorea
#lower zone
compare.group.7 <- subset(body_temps, Tide == "lower" & Species == "LL" & Site == "seaside")
tapply(compare.group.7, compare.group.7$NicheSpace, FUN = count)
compare.group.7 <- subset(body_temps, Tide == "lower" & Species == "LL" & Site == "loblolly")
tapply(compare.group.7, compare.group.7$NicheSpace, FUN = count)

#after deciding what has enough observations, subset the dataset to only have those observations
compare.group.7 <- subset(body_temps, Tide == "lower" & Species == "LL")
compare.group.7 <- subset(compare.group.7, NicheSpace == "seaweed" | NicheSpace == "open" | NicheSpace =="seaweed exposed")

ggplot(compare.group.7, aes(x = NicheSpace, y = warming_tolerance))+
  geom_violin()+
  facet_wrap(~Site)+
  ggtitle("LL in the lower zone")

#middle zone
compare.group.7 <- subset(body_temps, Tide == "middle" & Species == "LL" & Site == "seaside")
tapply(compare.group.7, compare.group.7$NicheSpace, FUN = count)
compare.group.7 <- subset(body_temps, Tide == "middle" & Species == "LL" & Site == "loblolly")
tapply(compare.group.7, compare.group.7$NicheSpace, FUN = count)

#after deciding what has enough observations, subset the dataset to only have those observations
compare.group.7 <- subset(body_temps, Tide == "middle" & Species == "LL")
compare.group.7 <- subset(compare.group.7, NicheSpace == "seaweed" | NicheSpace == "water" | NicheSpace =="open")

ggplot(compare.group.7, aes(x = NicheSpace, y = warming_tolerance))+
  geom_violin()+
  facet_wrap(~Site)+
  ggtitle("LL in the middle zone")

#upper zone
compare.group.7 <- subset(body_temps, Tide == "upper" & Species == "LL" & Site == "seaside")
tapply(compare.group.7, compare.group.7$NicheSpace, FUN = count)
compare.group.7 <- subset(body_temps, Tide == "upper" & Species == "LL" & Site == "loblolly")
tapply(compare.group.7, compare.group.7$NicheSpace, FUN = count)

#after deciding what has enough observations, subset the dataset to only have those observations
compare.group.7 <- subset(body_temps, Tide == "upper" & Species == "LL")
compare.group.7 <- subset(compare.group.7, NicheSpace == "crevice" | NicheSpace == "water" | NicheSpace =="open" | NicheSpace == "vertical")

ggplot(compare.group.7, aes(x = NicheSpace, y = warming_tolerance))+
  geom_violin()+
  facet_wrap(~Site)+
  ggtitle("LL in the upper zone")


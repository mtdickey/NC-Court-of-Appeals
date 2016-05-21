### Modeling of NC Court of Appeals Data ####

setwd("C:/Users/Michael/Documents/Side Projects/NC Court of Appeals")

library(ggplot2)
library(magrittr)
library(dplyr)
library(googlesheets)

## Read in data using GoogleSheets package ####
# 2015 training data:
googlesheet <- gs_title("Copy of 2015 COA Opinions")
published <- googlesheet %>% gs_read(ws = "Published", range= "A1:Q339") # hard coded range - change if cases are added
unpublished <- googlesheet %>% gs_read(ws = "Unpublished")

#2016 test data:
testsheet <- gs_title("2016 COA Opinions")
testPublished <- testsheet %>% gs_read(ws = "published")
testUnpublished <- testsheet %>% gs_read(ws = "Unpublished")

#### Clean up and combine ####

published$Published <- "Yes"
unpublished$Published <- "No"
testPublished$Published <- "Yes"
testUnpublished$Published <- "No"

#rename columns so they match
names(published)[which(names(published) == "Concuring")[1]] <- "Concuring "
names(unpublished)[which(names(unpublished) == "Concuring")[1]] <- "Concuring "
names(testUnpublished)[which(names(testUnpublished) == "Concuring")[1]] <- "Concuring "
names(testPublished)[which(names(testPublished) == "Concuring")[1]] <- "Concuring "
names(published)[which(names(published) == "County ")] <- "County"
names(unpublished)[which(names(unpublished) == "Trail Court")] <- "Trial Court"
names(published)[which(names(published) == "Trial Court ")] <- "Trial Court"
names(testUnpublished)[c(1,15,16)] <- c("Days", "Length", "Oral Argument") 

cleanUp <- function(appeals){
  names(appeals)[3] <- "Type" #Change from unnamed column to Type
  
  # join the cases that are juvenile-criminal with the variations
  appeals$Type <- as.character(appeals$Type)
  appeals$Type[appeals$Type == "Juvenile - criminal" | appeals$Type == "Juvenile - Criminal"] <- "Juvenile-Criminal"
  appeals$Type[appeals$Type == "Juvenile"] <- "Juvenile - 3.1"
  # reset the levels
  appeals$Type <- as.factor(appeals$Type)
  
  
  # Count was recognized as a date, let's change that
  appeals$Count <- as.character(appeals$Count)
  appeals$Count[appeals$Count == "Mar-00"] <- "3-0"
  appeals$Count[appeals$Count == "1-Feb"] <- "2-1"
  appeals$Count <- as.factor(appeals$Count)  
  appeals[which(appeals$Count == ""),] # One of these is blank still?
  
  # Clean up levels of concurring/dissenting with parentheses and explanation
  appeals$Author <- as.character(appeals$Author)
  appeals$'Concuring ' <- as.character(appeals$'Concuring ')
  appeals$Concuring <- as.character(appeals$Concuring)
  appeals$Dissenting <- as.character(appeals$Dissenting)
  appeals$'Concuring '[grep("\\(", appeals$'Concuring ')] <-
    substr(appeals$'Concuring '[grep("\\(", appeals$'Concuring ')], 0, regexpr(" ?\\(", appeals$'Concuring '[grep("\\(", appeals$'Concuring ')])-1)
  appeals$Concuring[grep("\\(", appeals$Concuring)] <-
    substr(appeals$Concuring[grep("\\(", appeals$Concuring)], 0, regexpr(" ?\\(", appeals$Concuring[grep("\\(", appeals$Concuring)])-1)
  appeals$Dissenting[grep("\\(", appeals$Dissenting)] <-
    substr(appeals$Dissenting[grep("\\(", appeals$Dissenting)], 0, regexpr(" ?\\(", appeals$Dissenting[grep("\\(", appeals$Dissenting)])-1)
  
  #Combine spelling mistakes
  appeals$Author[which(appeals$Author == "Hunter" | appeals$Author == "Hunter,Jr" | appeals$Author == "Hunter, Jr")] <- "HunterJr."
  appeals$Concuring[which(appeals$Concuring == "Dillons")] <- "Dillon"
  appeals$Concuring[which(appeals$Concuring == "MCCullough")] <- "McCullough"
  appeals$Concuring[which(appeals$Concuring == "Hunter Jr." | appeals$Concuring == "Hunter, Jr" | appeals$Concuring == "Hunter Jr")] <- "HunterJr."
  appeals$'Concuring '[which(appeals$'Concuring ' == "Calavria")] <- "Calabria"
  appeals$'Concuring '[which(appeals$'Concuring ' == "Geeer")] <- "Geer"
  appeals$'Concuring '[which(appeals$'Concuring ' == "Hunter, Jr" | appeals$'Concuring ' == "Hunter Jr." | appeals$'Concuring ' == "Hunter Jr")] <- "HunterJr."
  appeals$Dissenting[which(appeals$Dissenting == "Hunter, Jr")] <- "HunterJr."
  
  #with oral argument too
  appeals$`Oral Argument`[which(appeals$`Oral Argument` == "Remand")] <- "Remanded"
  appeals$`Oral Argument`[which(appeals$`Oral Argument` == "yes")] <- "Yes"

    
  # Make a field for combo of judges
  # put in alphabetical order so that order doesn't make a new lvl
  for(i in 1:nrow(appeals)){
    appeals$Judges[i] <- paste0(sort(c(appeals$Author[i], appeals$'Concuring '[i], appeals$Concuring[i], appeals$Dissenting[i]),T)[1],
                             ", ", sort(c(appeals$Author[i], appeals$'Concuring '[i], appeals$Concuring[i], appeals$Dissenting[i]),T)[2],
                             ", ", sort(c(appeals$Author[i], appeals$'Concuring '[i], appeals$Concuring[i], appeals$Dissenting[i]),T)[3],
                             sort(c(appeals$Author[i], appeals$'Concuring '[i], appeals$Concuring[i], appeals$Dissenting[i]),T)[4])
}

  #Factorize
  appeals$'Concuring ' <- as.factor(appeals$'Concuring ')
  appeals$Concuring <- as.factor(appeals$Concuring)
  appeals$Dissenting <- as.factor(appeals$Dissenting)
  appeals$Author <- as.factor(appeals$Author)
  appeals$Judges <- as.factor(appeals$Judges)
  appeals$Published <- as.factor(appeals$Published)
  appeals$`Oral Argument` <- as.factor(appeals$`Oral Argument`)
    
  # Make date decided and date heard into dates
  appeals$'Date Decided' <- as.Date(as.character(appeals$'Date Decided'), "%m/%d/%Y")
  appeals$'Date Heard' <- as.Date(as.character(appeals$'Date Heard'), "%m/%d/%Y")
  
  # Target Variable: Days until Decision
  appeals$TimeTilDecision <- appeals$'Date Decided' - appeals$'Date Heard'
  appeals$TimeTilDecision <- as.numeric(appeals$TimeTilDecision)
  
  return(appeals)
}

published <- cleanUp(published)
unpublished <- cleanUp(unpublished)
testPublished <- cleanUp(testPublished)
testUnpublished <- cleanUp(testUnpublished)

appeals <- published %>% rbind(unpublished)
test <- testPublished %>% rbind(testUnpublished)

#### End Data cleaning ###

#### Visualizing ####

# Distribution of days until decision
ggplot(data = appeals, aes(appeals$TimeTilDecision) ) + 
  geom_histogram(aes(y=..density..),
                     binwidth = 20, colour = "black", fill = "white") +
  geom_density(alpha=.2,fill="#FF6666") + labs(x = "Days Until Decision", y="Density")

# Survival curve of Published vs. Unpublished
library(survival)
load("ggsurv.RData")
surv <- survfit(Surv(TimeTilDecision) ~ Published, data=appeals)
ggsurv(surv) + labs(x = "Days Until Decision", y = "Proportion Not Decided")
# looks like published take longer to decide on

# Survival curve of Types of cases
surv <- survfit(Surv(TimeTilDecision) ~ Type, data=appeals)
ggsurv(surv) + labs(x = "Days Since Date Heard", y = "Proportion Not Decided")
# looks like juvenile cases less time to decide on

# Survival curve of Types of cases
surv <- survfit(Surv(TimeTilDecision) ~ appeals$`Oral Argument`, data=appeals)
ggsurv(surv) + labs(x = "Days Since Date Heard", y = "Proportion Not Decided")
# looks like juvenile cases less time to decide on

## Now look at time until decision for county
#Some counties only have less than 5 cases... let's group those together
count(appeals, County) %>%
  arrange(desc(n)) %>%
  filter(n >=5) -> common

appeals$county1 <- as.character(appeals$County)
appeals$county1[!(appeals$County %in% common$County)] <- "Uncommon"  
appeals$county1 <- as.factor(appeals$county1) # Now we're down to 43 counties

counties <- data.frame(county = appeals$county1, TimeTilDecision = appeals$TimeTilDecision)
countyCount <- data.frame(county = names(summary(appeals$county1)), countyCount = summary(appeals$county1))
counties <- merge(counties, countyCount)
counties$countyAndNumber <- paste0(counties$county, " (", counties$countyCount, ")")
# A lot of the top and bottom ones have sample sizes less than 10
counties %>% 
  group_by(countyAndNumber) %>% 
  summarise(mean = mean(TimeTilDecision)) %>% 
  ggplot(aes(x = reorder(countyAndNumber, mean), y = mean)) +
  geom_point(size = 3, color = "dark blue") +
  theme_minimal() +
  coord_flip() +
  labs(x = "County (# of Cases)", y = "Days Until Decision") +
  ggtitle("Average Decision Time by County") +
  theme(axis.text.y = element_text(size=8))

countyMeans <- counties %>% 
  group_by(county) %>% 
  summarise(mean = mean(TimeTilDecision))
countyMeans <- countyMeans[order(countyMeans$mean),]

# Group lowest 8, top 4, and middle counties
appeals$countyGroup <- ifelse(appeals$County %in% countyMeans$county[1:8], "Low", 
                             ifelse(appeals$County %in% countyMeans$county[(nrow(countyMeans)-4):nrow(countyMeans)], "High",
                                    "Medium"))
test$countyGroup <- ifelse(test$County %in% countyMeans$county[1:8], "Low", 
                              ifelse(test$County %in% countyMeans$county[(nrow(countyMeans)-4):nrow(countyMeans)], "High",
                                     "Medium"))
appeals$countyGroup <- as.factor(appeals$countyGroup)
test$countyGroup <- as.factor(test$countyGroup)

surv <- survfit(Surv(TimeTilDecision) ~ countyGroup, data=appeals)
ggsurv(surv) + labs(x = "Days Since Date Heard", y = "Proportion Not Decided")
# looks like juvenile cases less time to decide on


# Now look at any Authors with longer times
authors <- data.frame(author = appeals$Author, TimeTilDecision = appeals$TimeTilDecision)
authors %>% 
  group_by(author) %>% 
  summarise(mean = mean(TimeTilDecision)) %>% 
  ggplot(aes(x = reorder(author, mean), y = mean)) +
  geom_point(size = 3, color = "dark blue") +
  theme_minimal() +
  coord_flip() +
  labs(x = "Author", y = "Days Until Decision") +
  ggtitle("Average Decision Time by Author") +
  theme(axis.text.y = element_text(size=8))
appeals$Geer <- ifelse(row.names(appeals) %in% grep("Geer", appeals$Judges), 1, 0)
appeals$Stroud <- ifelse(row.names(appeals) %in% grep("Stroud", appeals$Judges), 1, 0)
appeals$McCullough <- ifelse(row.names(appeals) %in% grep("McCullough", appeals$Judges), 1, 0)

# From Disposition classification: Judges
judgePcts <- appeals %>% group_by(Author) %>% 
  mutate(count = n(), JudgeName = paste0(Author, ' (', count, ")")) %>% 
  group_by(JudgeName,Disposition2, count) %>% 
  mutate(countIn = n(), percent= countIn/count) %>%
  summarise(percent = mean(percent)) %>% filter(count > 2) %>%
  arrange(percent)
ggplot(data = judgePcts[which(judgePcts$Disposition2 == "Affirmed"),], aes(x=percent, y = reorder(JudgeName, percent)))+
  geom_point(size=3, color = 'dark blue') + labs(y = "Judges (# of cases)", title = "Affirmation Rates by Judge",x="Affirmation Rate")
# Calabria seems to be somewhat of a high outlier (maybe we can make an indicator if she is involved in the case at all)
appeals$Calabria <- ifelse(row.names(appeals) %in% grep("Calabria", as.character(appeals$Judges)), 1, 0)
appeals$McGee <- ifelse(row.names(appeals) %in% grep("McGee", as.character(appeals$Judges)), 1, 0) #McGee a bit on the low side

### New Data from calendars - now included in google sheet (don't need to merge) ####
# calendars <- read.csv("calendars.csv", stringsAsFactors = F)
# calendars <- calendars[,-1]
# calendars$scheduled[which(calendars$scheduled == "('', 'WITHOUT')")] <- "No"
# calendars$scheduled[which(calendars$scheduled == "('ARE SCHEDULED', '')")] <- "Yes"
# calendars$case <- paste0(calendars$case, "-1") # adding -1 to the end of case number
# calendars$case <- as.factor(calendars$case)
# 
# appeals <- merge(appeals, calendars, by.x= "Case.Number", by.y="case", all.x = T)
# appealsTest <- merge(appealsTest, calendars, by.x= "Case.Number", by.y="case", all.x = T)
# 
# appeals$scheduled[which(is.na(appeals$scheduled))] <- "Fast Track"
# appeals$scheduled <- as.factor(appeals$scheduled)
# appealsTest$scheduled[which(is.na(appealsTest$scheduled))] <- "Fast Track"
# appealsTest$scheduled <- as.factor(appealsTest$scheduled)


# What does oral argument vs. fast track look like?
appeals$OralArgument <- appeals$`Oral Argument`
test$OralArgument <- test$`Oral Argument`
surv <- survfit(Surv(TimeTilDecision) ~ OralArgument, data=appeals)
ggsurv(surv) + labs(x = "Days Since Date Heard", y = "Proportion Not Decided")
# looks like juvenile cases less time to decide on

### Survival model of Time Until Decision ####
survReg <- survreg(Surv(TimeTilDecision) ~ OralArgument + Type + Geer + Calabria + Stroud, data = appeals, dist= "weibull")

## Predict the 2016 cases and evaluate accuracy
test$OralArgument <- as.character(test$OralArgument)
test$OralArgument[which(test$OralArgument == "Withdrawn")] <- "No" # get rid of invalid levels for oral
test$OralArgument[which(test$OralArgument == "#N/A")] <- "Fast Track" # get rid of invalid levels for oral
test$OralArgument <- as.factor(test$OralArgument)
test$Geer <- ifelse(row.names(test) %in% grep("Geer", test$Judges), 1, 0)
test$Calabria <- ifelse(row.names(test) %in% grep("Calabria", test$Judges), 1, 0)
test$Stroud <- ifelse(row.names(test) %in% grep("Stroud", test$Judges), 1, 0)
test$preds <- predict(survReg, newdata=test)
mean(abs((test$TimeTilDecision[which(!is.na(test$TimeTilDecision))] - 
          test$preds[which(!is.na(test$TimeTilDecision))])/
          test$TimeTilDecision[which(!is.na(test$TimeTilDecision))]))
test$Difference <- test$TimeTilDecision - test$preds

# Let's look at how far off we are usually

# Distribution of error
ggplot(data = test, aes(test$Difference) ) + 
  geom_histogram(aes(y=..density..),
                 binwidth = 20, colour = "black", fill = "white") +
  geom_density(alpha=.2,fill="#FF6666") + labs(x = "Difference Between Actual and Predicted Time", y="Density")

# save it to read into app
save(survReg, file = "surv.RData")
save(appeals, file = "appeals.RData")

# How can we predict a survival probability?
pct <- 1:98/100   # The 100th percentile of predicted survival is at +infinity ?
#Generate predictions for each percentile for a given input
ptime <- predict(survReg, newdata=list(Type = "Criminal", OralArgument = "Yes"), type='quantile', p=pct, se=TRUE)

# Visualization of survival time
library(reshape2)
plotData <- data.frame(cbind(surv = 1-pct, fit = ptime$fit, sdplus = ptime$fit + 2*ptime$se.fit,
                             sdminus = ptime$fit - 2*ptime$se.fit))
plotData <- melt(plotData, id = "surv")
ggplot(plotData, aes(x=value, y = surv, colour = variable, linetype = variable)) + geom_line(lwd=1.1) +
  labs(x = "Days Elapsed", y = "Probability Not Decided") +
  scale_colour_manual(values=c("black", "red", "red"), labels = c("Average", "2 SD Above", "2 SD Below")) + 
  scale_linetype_manual(values = c("solid","dashed","dashed"), labels = c("Average", "2 SD Above", "2 SD Below")) +
  geom_vline(xintercept = 100, lwd = 1, colour = "blue") +
  theme(legend.title=element_blank())

# The probability of a case not being decided by 100 days:
(plotData %>% mutate(diff = value - 100) %>%
  filter(variable == "fit" & diff > 0) %>%
  arrange(diff))$surv[1] # .45 or 45%

#### Classification model of Disposition ####

appeals$Disposition2 <- as.factor(appeals$`Disposition 2.0`)
appeals %>% group_by(Disposition2) %>% summarise(n = n()) %>% mutate(prop = n/sum(n)) 
# so we have 65% affirmed, 6% dismissed, 12.5% mixed, and 16% reversed

# let's check out how affirmation rate changes based on info that we will know
# first oral argument
oralPcts <- appeals %>% group_by(OralArgument) %>% 
            mutate(count = n(), ArgumentName = paste0(OralArgument, ' (', count, ")")) %>% 
            group_by(ArgumentName,Disposition2, count) %>% 
            mutate(countIn = n(), percent= countIn/count) %>%
            summarise(mean(percent)) %>% filter(count > 2)
ggplot(data = oralPcts[which(oralPcts$Disposition2 == "Affirmed"),],
       aes(x=`mean(percent)`, y = ArgumentName)) + geom_point(size=3, color = 'dark blue') + labs(title = "Affirmation Rates by Argument Status",x="Affirmation Rate")
# so the affirmation rate is about 10% higher for fast track and juv 3.1 cases than those simply with/without arguments

# Now County
countyPcts <- appeals %>% group_by(county1) %>% 
              mutate(count = n(), CountyName = paste0(county1, ' (', count, ")")) %>% 
              group_by(CountyName,Disposition2, count) %>% 
              mutate(countIn = n(), percent= countIn/count) %>%
              summarise(percent = mean(percent)) %>% filter(count > 2) %>%
              arrange(percent)
ggplot(data = countyPcts[which(countyPcts$Disposition2 == "Affirmed"),], aes(x=percent, y = reorder(CountyName, percent)))+
  geom_point(size=3, color = 'dark blue') + labs(y = "County (# of cases)", title = "Affirmation Rates by County",x="Affirmation Rate")
# There's some variation, but most of it is probably due to small sample size, not going to engineer a feature for this

# Type of Case
typePcts <- appeals %>% group_by(Type) %>% 
  mutate(count = n(), TypeName = paste0(Type, ' (', count, ")")) %>% 
  group_by(TypeName,Disposition2, count) %>% 
  mutate(countIn = n(), percent= countIn/count) %>%
  summarise(percent = mean(percent)) %>% filter(count > 2) %>%
  arrange(percent)
ggplot(data = typePcts[which(typePcts$Disposition2 == "Affirmed"),], aes(x=percent, y = reorder(TypeName, percent)))+
  geom_point(size=3, color = 'dark blue') + labs(y = "Type (# of cases)", title = "Affirmation Rates by Case Type",x="Affirmation Rate")
# Civil is affirmed far ~15-20% less often

# Build the model - starting with the classic random forest for now - not deemed to be accurate enough
# library(randomForest)
# forest <- randomForest(Disposition2 ~ McGee + Calabria + Type + OralArgument + countyGroup, data = appeals)

#Multinomial logistic
library(nnet)
appeals$disp2 <- relevel(appeals$Disposition2, ref = "Affirmed")
logistic <- multinom(disp2 ~ McGee + Calabria + Type + OralArgument + countyGroup, data = appeals)
save(logistic, file = "logistic.RData")

# Try feeding it a new observation and see the predicted probability for each class
test$McGee <- ifelse(row.names(test) %in% grep("McGee", as.character(test$Judges)), 1, 0)
test$Calabria <- ifelse(row.names(test) %in% grep("Calabria", as.character(test$Judges)), 1, 0)
obs <- data.frame(McGee = 0, Calabria = 0, Type = "Civil", OralArgument = "Yes", countyGroup = "Low", stringsAsFactors = T)
levels(obs$Type) <- levels(appeals$Type)
levels(obs$countyGroup) <- levels(appeals$countyGroup)
levels(obs$OralArgument) <- levels(appeals$OralArgument)
obs$Type <- as.factor(obs$Type)
prediction <- data.frame(predict(forest, obs, type = "vote"))
prediction <- melt(prediction)

prediction = prediction[order(prediction$value), ]
prediction$ymax = cumsum(prediction$value)
prediction$ymin = c(0, head(prediction$ymax, n=-1))


ggplot(prediction, aes(fill=variable, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect(colour="grey30") +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  labs(title="Probability of Dispositions", fill = "Disposition")

# To do here: Test accuracy (no longer aggressive on 3.1 when joined with Juvenile)

### Predicting likelihood of Dissent ####

#Make the target
appeals$Dissent <- ifelse(row.names(appeals) %in% grep("1", appeals$Count), 1, 0)
test$Dissent <- ifelse(row.names(test) %in% grep("1", test$Count), 1, 0)

## Exploring/ Visualizing ###
mean(appeals$Dissent) # less than 2% of cases in 2015 had a dissent
mean(test$Dissent) # in 2016 it has jumped to 5.7% of cases with a dissent... (still not so likely)
mean(test$Dissent[grep("Tyson",test$Judges)]) # Cases that involve Tyson have a dissent about 10.7% of the time

# Who are the top dissenters in 2016?
test<- test[,-4] # this duplicated column was an issue for some reason
temp <- test %>% group_by(Dissenting) %>% summarise(count = n()) %>% filter(!is.na(Dissenting)) %>% arrange(desc(count))

# Dillon has just as many dissents as Tyson.. let's calculate percentages just for fun
for(i in 1:nrow(temp)){
  temp$nCases[i] <- length(grep(temp$Dissenting[i], test$Judges))
}
temp$Percent <- temp$count/temp$nCases
(temp <- temp %>% arrange(desc(Percent)))

# Oooh... so apparently Tyson is only the 3rd most likely to dissent in 2016
temp %>% ggplot(aes(x=Percent, y= reorder(Dissenting,Percent))) + geom_point(color = "dark blue", size = 2) + 
  labs(x= "Dissent Percentage", y = "Judge")

## Visualize differences in type and Oral Argument
test %>% group_by(Type) %>% summarise(DissentPct = mean(Dissent)) %>% ggplot(aes(x=DissentPct, y = Type)) + geom_point(color = "dark blue", size = 1.5)
test %>% group_by(OralArgument) %>% summarise(DissentPct = mean(Dissent)) %>% ggplot(aes(x=DissentPct, y = OralArgument)) + geom_point(color = "dark blue", size = 1.5)

#### Model ###
#Judge features
test$Dillon <- ifelse(row.names(test) %in% grep("Dillon", test$Judges), 1, 0)
test$Hunter <- ifelse(row.names(test) %in% grep("Hunter", test$Judges), 1, 0)
test$Tyson <- ifelse(row.names(test) %in% grep("Tyson", test$Judges), 1, 0)

dissentModel <- glm(Dissent ~ OralArgument + Dillon + Hunter + Tyson , family = "binomial", data = test)
summary(dissentModel) # Hunter not significant (I guess because of low counts? )

plotData <- data.frame(Dissent = c("Yes", "No"), Probability = 
                         c(predict(dissentModel, 
                                   data.frame(OralArgument = "No", Dillon = 1, Hunter = 1, Tyson = 1), type= "response"),
                           1-predict(dissentModel, 
                                     data.frame(OralArgument = "No", Dillon = 1, Hunter = 1, Tyson = 1), type= "response")))

plotData$ymax = cumsum(plotData$Probability)
plotData$ymin = c(0, plotData$ymax[1])

ggplot(plotData, aes(fill=Dissent, ymin = c(0, plotData$Probability[1]), ymax= cumsum(plotData$Probability), xmax=2, xmin = 1)) +
  geom_rect(colour="grey30") +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  labs(title="Probability of Dissent", fill = "Dissent")

save(dissentModel, file = "dissent.RData")
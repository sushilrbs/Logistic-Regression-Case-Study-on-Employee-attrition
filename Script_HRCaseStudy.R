library(dplyr)
library(stringr)
library(GGally)
library(MASS)
library(car)
library(corrplot)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(tidyr)
library(ROCR)


## Set Working directory
setwd("E:/Personal/IIIT-B/PredictiveAnalysis/HR Analytics Case Study/PA-I_Case_Study_HR_Analytics")

### Load csv Data
employees<-read.csv("employee_survey_data.csv", stringsAsFactors = F)
generald<-read.csv("general_data.csv", stringsAsFactors = F)
intime<-read.csv("in_time.csv", stringsAsFactors = F)
managers<-read.csv("manager_survey_data.csv", stringsAsFactors = F)
outtime<-read.csv("out_time.csv", stringsAsFactors = F)

## View the Dataset structure
str(employees)    # 4410 obs of 4 variables 
str(generald) # 4410 obs of 24 variables
str(managers) # 4410 obs of 3 variables

str(intime) # 4410 obs of 262 variables
str(outtime) # 4410 obs of 262 variables

# Collate the data together in one single file
length(unique(tolower(employees$EmployeeID)))    # 4410, confirming EmployeeID is key 
length(unique(tolower(generald$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(managers$EmployeeID))) # 4410, confirming EmployeeID is key

# Check record difference on dataframe
setdiff(employees$EmployeeID,generald$EmployeeID) # Identical EmployeeID across these datasets
setdiff(employees$EmployeeID,managers$EmployeeID) # Identical EmployeeID across these datasets

employees_generald<- merge(employees,generald, by="EmployeeID", all = F)
Finaldatafile<- merge(employees_generald,managers, by="EmployeeID", all = F)

View(Finaldatafile) #master file

str(Finaldatafile)

#Data quality  Check

#### Missing  value on column as whole and remove from dataset, if it is
summary(Finaldatafile)

missing_values <- Finaldatafile %>%summarise_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')

missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_percentage),y=missing_percentage)) +
  geom_bar(stat = 'identity',fill='red') +
  coord_flip()

# There are no column value where the missing value on column is  > 15% 

str(Finaldatafile)

# Barcharts for categorical features with stacked employee attrition information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")


plot_grid(ggplot(Finaldatafile, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(), 
          ggplot(Finaldatafile, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Finaldatafile, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Finaldatafile, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

plot_grid(ggplot(Finaldatafile, aes(x=Gender,fill=Attrition))+ geom_bar(), 
          ggplot(Finaldatafile, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Finaldatafile, aes(x=Over18,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")  


plot_grid(ggplot(Finaldatafile, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(), 
          ggplot(Finaldatafile, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Finaldatafile, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Finaldatafile, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Finaldatafile, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Finaldatafile, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Finaldatafile, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")  

## All look Good

# Histogram and Boxplots for numeric variables for Checking Outlier and NA's
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

## Outliers Check
plot_grid(ggplot(Finaldatafile, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(Finaldatafile, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Finaldatafile, aes(DistanceFromHome))+ geom_histogram(binwidth = 20),
          ggplot(Finaldatafile, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Finaldatafile, aes(EmployeeCount))+ geom_histogram(),
          ggplot(Finaldatafile, aes(x="",y=EmployeeCount))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 


plot_grid(ggplot(Finaldatafile, aes(MonthlyIncome))+ geom_histogram(),
          ggplot(Finaldatafile, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Finaldatafile, aes(NumCompaniesWorked))+ geom_histogram(),
          ggplot(Finaldatafile, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

## No Outliers because NumCompaniesWorked<TotalWorkingYears

plot_grid(ggplot(Finaldatafile, aes(PercentSalaryHike))+ geom_histogram(),
          ggplot(Finaldatafile, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Finaldatafile, aes(StandardHours))+ geom_histogram(),
          ggplot(Finaldatafile, aes(x="",y=StandardHours))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Finaldatafile, aes(StockOptionLevel))+ geom_histogram(),
          ggplot(Finaldatafile, aes(x="",y=StockOptionLevel))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Finaldatafile, aes(TotalWorkingYears))+ geom_histogram(),
          ggplot(Finaldatafile, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
## No Outliers because YearsAtCompany=TotalWorkingYears

plot_grid(ggplot(Finaldatafile, aes(YearsAtCompany ))+ geom_histogram(),
          ggplot(Finaldatafile, aes(x="",y=YearsAtCompany ))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

## No Outliers because YearsAtCompany<=TotalWorkingYears


plot_grid(ggplot(Finaldatafile, aes(YearsSinceLastPromotion ))+ geom_histogram(),
          ggplot(Finaldatafile, aes(x="",y=YearsSinceLastPromotion ))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

## No Outliers because YearsSinceLastPromotion<YearsAtCompany

plot_grid(ggplot(Finaldatafile, aes(YearsWithCurrManager ))+ geom_histogram(),
          ggplot(Finaldatafile, aes(x="",y=YearsWithCurrManager ))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
## No Outliers because YearsWithCurrManager<YearsAtCompany

# Outlier treatment using BOX and Quantile
#Boxplot showed no outlier, Nevertheless confirming it also with percentiles
sapply(Finaldatafile[,c("Age","MonthlyIncome","TotalWorkingYears")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) #no outlier

#No outliers in numeric variables

#######################################################################

############### Excluding Working hours from Final sheet  ########################
## Remove NA's column
intime<-intime[, colSums(is.na(intime)) != nrow(intime)]
outtime<-outtime[, colSums(is.na(outtime)) != nrow(outtime)]

## Column name Correction

intime<-intime%>%gather(InDate,In_time,2:250)
colnames(intime)<-c("EmployeeID","InDate","In_time")
intime$InDate <- str_replace_all(intime$InDate, "X", "")
#intime<-intime[-which(is.na(intime$In_time)==TRUE),]

intime<-intime %>% drop_na()

#intime$In_time_1<-format(as.POSIXct(intime$In_time, format="%Y-%m-%d %H:%M"),"%H")

#intime<-intime[-3]


outtime<-outtime%>%gather(OutDate,Out_time,2:250)
colnames(outtime)<-c("EmployeeID","OutDate","Out_time")
outtime$OutDate <- str_replace_all(outtime$OutDate, "X", "")
##outtime<-outtime[-which(is.na(outtime$Out_time)==TRUE),]
outtime<-outtime %>% drop_na()

#outtime$Out_time_1<-format(as.POSIXct(outtime$Out_time, format="%Y-%m-%d %H:%M"),"%H")

#outtime<-outtime[-3]

outtime$OutDate<-as.Date(outtime$OutDate,"%Y.%m.%d")
intime$InDate<-as.Date(intime$InDate,"%Y.%m.%d")


# Collate the data together in one single file
length(unique(tolower(intime$EmployeeID)))    # 4410, confirming EmployeeID is key 
length(unique(tolower(outtime$EmployeeID))) # 4410, confirming EmployeeID is key

# Check record difference on dataframe
setdiff(c(intime$EmployeeID,intime$InDate),c(outtime$EmployeeID,outtime$OutDate)) # Identical EmployeeID across these datasets

## Merge InTime and OutFime Data frame
intime<-intime[order(intime$EmployeeID),]
outtime<-outtime[order(outtime$EmployeeID),]

## Merge column
intime<-cbind(intime,outtime$Out_time)
colnames(intime)[which(names(intime) == "outtime$Out_time")] <- "Out_time"

## Calculate working hours
intime$WorkingHours<-difftime(intime$Out_time, intime$In_time, units="hours")
intime$WorkingHours<-round(intime$WorkingHours)
finalworkinghours<-intime[,c(1,2,5)]
finalworkinghours<-finalworkinghours%>%group_by(EmployeeID)%>%summarise(finalTotalworkinghours=sum(WorkingHours))
### Final data frame for working hours which extracted from in and out csv

##intime%>%group_by(In_time_1)%>%summarise(n())
######################################################################

############## NA's handling

sapply(Finaldatafile, function(x) sum(is.na(x))) 

# Missing value

# shows 19 NAs -  NumCompaniesWorked  
# shows 9 NAs  -  TotalWorkingYears
# shows 25 NAs -  EnvironmentSatisfaction 
# shows 20 NAs  - JobSatisfaction
# shows 38 NAs  - WorkLifeBalance

## Total percentage of NA's
sum(is.na(Finaldatafile))/prod(dim(Finaldatafile)) ##0.0008679334(Do you think, we should remove NA's)

## checking Indivisually 

####################   WorkLifeBalance
summary((Finaldatafile$WorkLifeBalance))

View(subset(Finaldatafile, is.na(WorkLifeBalance))) 
# 38 NAs in WorkLifeBalance and with Attrition Yes and No, so we should not remove these NA's from data set

sum(is.na(Finaldatafile$WorkLifeBalance))/length(Finaldatafile$WorkLifeBalance)
# 0.00861678 i.e 0.8%, so best to handle NA instead of removing from dataset 
# Lets Handle this NA by replacing from median value of WorkLifeBalance

## Replacing NA with Median
#m<-median(Finaldatafile$WorkLifeBalance,na.rm = TRUE)
#Finaldatafile$WorkLifeBalance[which(is.na(Finaldatafile$WorkLifeBalance))] <- m

########### EnvironmentSatisfaction ###

summary((Finaldatafile$EnvironmentSatisfaction))
View(subset(Finaldatafile, is.na(EnvironmentSatisfaction)))
## These NA's are with Attrition Yes and No so can not remove from data set
sum(is.na(Finaldatafile$EnvironmentSatisfaction))/length(Finaldatafile$EnvironmentSatisfaction)
## Missing value % is .5% to better to handled this instead of removing
## Replacing NA with Median
##m<-median(Finaldatafile$EnvironmentSatisfaction,na.rm = TRUE)
##Finaldatafile$EnvironmentSatisfaction[which(is.na(Finaldatafile$EnvironmentSatisfaction))] <- m

#############JobSatisfaction ######(Do we need handle NA)

summary((Finaldatafile$JobSatisfaction))
View(subset(Finaldatafile, is.na(JobSatisfaction)))
## These NA's are with Attrition Yes and No so can not remove from data set
sum(is.na(Finaldatafile$JobSatisfaction))/length(Finaldatafile$JobSatisfaction)
## Missing value % is .4% to better to handled this instead of removing

## Replacing NA with Median
#m<-median(Finaldatafile$JobSatisfaction,na.rm = TRUE)

#Finaldatafile$JobSatisfaction[which(is.na(Finaldatafile$JobSatisfaction))] <- m


#############NumCompaniesWorked ###### 

summary((Finaldatafile$NumCompaniesWorked))

View(subset(Finaldatafile, is.na(NumCompaniesWorked)))
## These NA's are with Attrition Yes and No so can not remove from data set
sum(is.na(Finaldatafile$NumCompaniesWorked))/length(Finaldatafile$NumCompaniesWorked)
## Missing value % is .4% to better to handled this instead of removing

## Replacing NA with Median

## Replacing NA with Median
m<-median(Finaldatafile$NumCompaniesWorked,na.rm = TRUE)
Finaldatafile$NumCompaniesWorked[which(is.na(Finaldatafile$NumCompaniesWorked))] <- m
## Replacing 0 with Median 

# NumCompaniesWorked varibale can not be 0 so it should be replaced by Median which is 2. 
Finaldatafile$NumCompaniesWorked[which(Finaldatafile$NumCompaniesWorked==0)] <- m

##Think if Totalnumberofworkingyear=0 and companiesworked=2(normalised with median) , it could be correct becuase employee would not have completed 1 years on 2 companies.

#############TotalWorkingYears ###### (Do we need to remove NA's)
## 
summary((Finaldatafile$TotalWorkingYears))

View(subset(Finaldatafile, is.na(TotalWorkingYears)))
## These NA's are with Attrition Yes and No so can not remove from data set
sum(is.na(Finaldatafile$TotalWorkingYears))/length(Finaldatafile$TotalWorkingYears)
## Missing value % is .2% to better to handled this instead of removing

## Replacing NA with Median

## Replacing NA with Median
m<-median(Finaldatafile$TotalWorkingYears,na.rm = TRUE)
Finaldatafile$TotalWorkingYears[which(is.na(Finaldatafile$TotalWorkingYears))] <- m


# Correlation between numeric variables

ggpairs(Finaldatafile[, c("Age", "MonthlyIncome", "TotalWorkingYears")])

#As expected, Age and TotalWorkingYears are highly correlated (corr 0.68)
### Data Preparation

# De-Duplication
# not needed

## Data Validation
Finaldatafile[which(Finaldatafile$YearsWithCurrManager>Finaldatafile$TotalWorkingYears),]
## 0
Finaldatafile[which(Finaldatafile$YearsAtCompany>Finaldatafile$TotalWorkingYears),]
## It might possible, employee was on leave

Finaldatafile[which(Finaldatafile$TotalWorkingYears==0),]
## 178.. It may have possiblity, employee has worked less than a year in company

# Bringing the variables in the correct format

str(Finaldatafile)
####### EnvironmentSatisfaction #####
str(Finaldatafile$EnvironmentSatisfaction)
summary(factor(Finaldatafile$EnvironmentSatisfaction))

Finaldatafile$EnvironmentSatisfaction<-ifelse(Finaldatafile$EnvironmentSatisfaction==1, "Low", 
                          ifelse(Finaldatafile$EnvironmentSatisfaction==2, "Medium", 
                          ifelse(Finaldatafile$EnvironmentSatisfaction==3, "High", 
                          ifelse(Finaldatafile$EnvironmentSatisfaction==4, "Very High", 
                          "Missing"))))

## Missing handled(if it has not handled above)
Finaldatafile$EnvironmentSatisfaction[which(is.na(Finaldatafile$EnvironmentSatisfaction))] <- "Missing"

####### JobSatisfaction #####
str(Finaldatafile$JobSatisfaction)
summary(factor(Finaldatafile$JobSatisfaction))

Finaldatafile$JobSatisfaction<-ifelse(Finaldatafile$JobSatisfaction==1, "Low", 
                                        ifelse(Finaldatafile$JobSatisfaction==2, "Medium", 
                                        ifelse(Finaldatafile$JobSatisfaction==3, "High", 
                                        ifelse(Finaldatafile$JobSatisfaction==4, "Very High", 
                                        "Missing"))))

## Missing handled(if it has not handled above)
Finaldatafile$JobSatisfaction[which(is.na(Finaldatafile$JobSatisfaction))] <- "Missing"


####### WorkLifeBalance #####
str(Finaldatafile$WorkLifeBalance)
summary(factor(Finaldatafile$WorkLifeBalance))

Finaldatafile$WorkLifeBalance<-ifelse(Finaldatafile$WorkLifeBalance==1, "Bad", 
                               ifelse(Finaldatafile$WorkLifeBalance==2, "Good", 
                               ifelse(Finaldatafile$WorkLifeBalance==3, "Better", 
                               ifelse(Finaldatafile$WorkLifeBalance==4, "Best", 
                               "Missing"))))
## Missing handled(if it has not handled above)
Finaldatafile$WorkLifeBalance[which(is.na(Finaldatafile$WorkLifeBalance))] <- "Missing"


####### Education #####
str(Finaldatafile$Education)
summary(factor(Finaldatafile$Education))

Finaldatafile$Education<-ifelse(Finaldatafile$Education==1, "Below College", 
                               ifelse(Finaldatafile$Education==2, "College", 
                               ifelse(Finaldatafile$Education==3, "Bachelor", 
                               ifelse(Finaldatafile$Education==4, "Master", 
                               "Doctor"))))

####### JobLevel #####
str(Finaldatafile$JobLevel)
summary(factor(Finaldatafile$JobLevel))

Finaldatafile$JobLevel<-ifelse(Finaldatafile$JobLevel==1, "Low", 
                         ifelse(Finaldatafile$JobLevel==2, "Medium", 
                         ifelse(Finaldatafile$JobLevel==3, "High", 
                         ifelse(Finaldatafile$JobLevel==4, "Very High", 
                         "Execuitive"))))

####### JobInvolvement #####
str(Finaldatafile$JobInvolvement)
summary(factor(Finaldatafile$JobInvolvement))

Finaldatafile$JobInvolvement<-ifelse(Finaldatafile$JobInvolvement==1, "Low", 
                        ifelse(Finaldatafile$JobInvolvement==2, "Medium", 
                        ifelse(Finaldatafile$JobInvolvement==3, "High", 
                        ifelse(Finaldatafile$JobInvolvement==4, "Very High", 
                        "Missing"))))


####### PerformanceRating #####
str(Finaldatafile$PerformanceRating)
summary(factor(Finaldatafile$PerformanceRating))

Finaldatafile$PerformanceRating<-ifelse(Finaldatafile$PerformanceRating==1, "Low", 
                              ifelse(Finaldatafile$PerformanceRating==2, "Good", 
                              ifelse(Finaldatafile$PerformanceRating==3, "Excellent", 
                              ifelse(Finaldatafile$PerformanceRating==4, "Outstanding", 
                              "Missing"))))


#Let us see the structure of variable "Over18".
str(Finaldatafile$Over18)
summary(factor(Finaldatafile$Over18))
#Y=1 (Assign numerical value and convert to numeric)

Finaldatafile$Over18<-ifelse(Finaldatafile$Over18=="Y",1,0)
Finaldatafile$Over18<-as.numeric(Finaldatafile$Over18)

############ "Attrition".
str(Finaldatafile$Attrition)
summary(factor(Finaldatafile$Attrition))
#Yes=1 and No=0 (Assign numerical value and convert to numeric)

Finaldatafile$Attrition<-ifelse(Finaldatafile$Attrition=="Yes",1,0)
Finaldatafile$Attrition<-as.numeric(Finaldatafile$Attrition)

str(Finaldatafile)

##############  There are 3 constant variables in the dataset(EmployeeCount,Over18,StandardHours), that need to be removed ###

Finaldatafile<-Finaldatafile[,-c(12,19,21)]

################################################################
# Feature standardisation (Normalise Numerical varibale)
Finaldatafile1<-Finaldatafile
##Finaldatafile<-Finaldatafile1


Finaldatafile$MonthlyIncome<- scale(Finaldatafile$MonthlyIncome) # scale used: mean 65029

# Checking attrition rate of prospect employee

att <- sum(Finaldatafile$Attrition)/nrow(Finaldatafile)
att # 16.12% Attrition rate. 

####### Handle categorical variables having more than 2 levels ##### 
# creating a dataframe of categorical features
hr_chr<- Finaldatafile[,c(2,3,4,7,8,10,11,12,13,14,15,25,26)]


# converting categorical attributes to factor
hr_fact<- data.frame(sapply(hr_chr, function(x) factor(x)))
str(hr_fact)


# creating dummy variables for factor attributes
dummies<- data.frame(sapply(hr_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =hr_fact))[,-1]))


# Final dataset
HR_final<- cbind(Finaldatafile[,c(1,5,6,9,16,17,18,19,20,21,22,23,24)],dummies) 
View(HR_final) #4410 obs. of  57 variables

##########################Merging Employee Total working hours on HR_final data frame #######################################

HR_final<- merge(HR_final,finalworkinghours, by="EmployeeID", all = F)

HR_final$finalTotalworkinghours<-as.numeric(HR_final$finalTotalworkinghours)

summary(HR_final$finalTotalworkinghours) ## Scaling is not required since it's distribution has not much difference

#### Removing employeeID from final data frame

HR_final<-HR_final[-1]

str(HR_final)


#4410 obs. of  57 variables
########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(HR_final$Attrition, SplitRatio = 0.7)

train = HR_final[indices,]

test = HR_final[!(indices),]


########################################################################
# Logistic Regression: 

str(HR_final)

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2152.5....56 coeff..nullDev 2728.0...resDev 2038.5

# Stepwise selection

model_2<- stepAIC(model_1, direction="both")

summary(model_2)

#AIC 2124.2....nullDev 2728.0...resDev 2056.2

# Removing multicollinearity through VIF check
vif(model_2)

a<-data.frame(vif(model_2))

#Excluding YearsAtCompany
model_3<- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + WorkLifeBalance.xMissing + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.xCollege + EducationField.xOther + 
                JobLevel.xMedium + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + JobInvolvement.xLow + JobInvolvement.xVery.High +
                finalTotalworkinghours, family = "binomial", data = train)

summary(model_3) 
#AIC 2126.3....nullDev 2728.0...resDev 2060.3

vif(model_3) 
# Cannot exclude any more variable based on vif as most of them have high significance 

# Excluding MaritalStatus.xMarried due to lower significance (High P-value)
model_4<- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + WorkLifeBalance.xMissing + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.xCollege + EducationField.xOther + 
                JobLevel.xMedium + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + JobInvolvement.xLow + JobInvolvement.xVery.High +
                finalTotalworkinghours, family = "binomial", data = train)

summary(model_4)
#AIC 2126....nullDev 2728.0...resDev 2062

#Excluding JobInvolvement.xVery.High   due to lower significance (High P-value)
model_5<- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + WorkLifeBalance.xMissing + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.xCollege + EducationField.xOther + 
                JobLevel.xMedium + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + JobInvolvement.xLow +
                finalTotalworkinghours, family = "binomial", data = train)

summary(model_5) 

#AIC 2126.1....nullDev 2728.0...resDev 2064.1

#Excluding JobInvolvement.xLow  due to lower significance
model_6<- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + WorkLifeBalance.xMissing + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.xCollege + EducationField.xOther + 
                JobLevel.xMedium + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle +
                finalTotalworkinghours, family = "binomial", data = train) 

summary(model_6)

#AIC 2126.2....nullDev 2728.0...resDev 2066.2

#Excluding EducationField.xOther  due to lower significance
model_7<- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + WorkLifeBalance.xMissing + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.xCollege + 
                JobLevel.xMedium + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle +
                finalTotalworkinghours, family = "binomial", data = train)    

summary(model_7) 

#AIC 2126.7....nullDev 2728...resDev 2068.7

#Excluding DistanceFromHome due to lower significance with respect to other variables
model_8<- glm(formula = Attrition ~ Age + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood + WorkLifeBalance.xMissing + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.xCollege + 
                JobLevel.xMedium + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle +
                finalTotalworkinghours, family = "binomial", data = train)     

summary(model_8) 

#AIC 2127.3....nullDev 2728.0...resDev 2071.3

#Excluding WorkLifeBalance.xMissing due to lower significance with respect to other variables
model_9<- glm(formula = Attrition ~ Age + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xGood +  
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.xCollege + 
                JobLevel.xMedium + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle +
                finalTotalworkinghours, family = "binomial", data = train) 

summary(model_9) 

#AIC 2129.4....nullDev 2728.0...resDev 2075.4

#Excluding NumCompaniesWorked due to lower significance with respect to other variables
model_10<- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood +  
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 Education.xCollege + 
                 JobLevel.xMedium + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle +
                 finalTotalworkinghours, family = "binomial", data = train)  


summary(model_10) 

#AIC 2165.4.....nullDev 2728.0...resDev 2113.4

#Excluding Education.xCollege due to lower significance with respect to other variables
model_11<- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood +  
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobLevel.xMedium + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle +
                 finalTotalworkinghours, family = "binomial", data = train) 


summary(model_11) 

#AIC 2166.7....nullDev 2728.0...resDev 2116.7

#Excluding EnvironmentSatisfaction.xVery.High due to lower significance with respect to other variables
model_12<- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood +  
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobLevel.xMedium + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle +
                 finalTotalworkinghours, family = "binomial", data = train)     

summary(model_12) 

#AIC 2169......nullDev 2728.0...resDev 2121

#Excluding JobRole.xLaboratory.Technician due to lower significance with respect to other variables
model_13<- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood +  
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobLevel.xMedium + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle +
                 finalTotalworkinghours, family = "binomial", data = train) 

summary(model_13) 

#AIC 2172.6....nullDev 2728.0...resDev 2126.6

#Excluding JobRole.xResearch.Scientist due to lower significance with respect to other variables
model_14<- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood +  
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobLevel.xMedium + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 MaritalStatus.xSingle +
                 finalTotalworkinghours, family = "binomial", data = train)      

summary(model_14) 

#AIC 2174.5....nullDev 2728.0...resDev  2130.5

#Excluding JobLevel.xMedium due to lower significance with respect to other variables
model_15<- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood +  
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 MaritalStatus.xSingle +
                 finalTotalworkinghours, family = "binomial", data = train)     

summary(model_15) 

#AIC 2177.8....nullDev 2728.0...resDev  2135.8

#Excluding MonthlyIncome due to lower significance with respect to other variables
model_16<- glm(formula = Attrition ~ Age + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood +  
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 MaritalStatus.xSingle +
                 finalTotalworkinghours, family = "binomial", data = train)      

summary(model_16) 

#AIC 2180....nullDev 2728.0...resDev  2140

#Excluding WorkLifeBalance.xBest   due to lower significance with respect to other variables
model_17<- glm(formula = Attrition ~ Age + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood +  
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 MaritalStatus.xSingle +
                 finalTotalworkinghours, family = "binomial", data = train)     

summary(model_17) 

#AIC 2186....nullDev 2728...resDev  2148

#Excluding WorkLifeBalance.xGood  due to lower significance with respect to other variables
model_18<- glm(formula = Attrition ~ Age + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood +  
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 MaritalStatus.xSingle +
                 finalTotalworkinghours, family = "binomial", data = train)   

summary(model_18) 

#AIC 2186....nullDev 2728...resDev  2148

#Excluding JobRole.xResearch.Director  due to lower significance with respect to other variables
model_19<- glm(formula = Attrition ~ Age + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBetter +  
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 MaritalStatus.xSingle +
                 finalTotalworkinghours, family = "binomial", data = train)      

summary(model_19) 

#AIC 2189.7....nullDev  2728...resDev  2153.7

#Excluding  JobRole.xSales.Executive due to lower significance with respect to other variables
model_20<- glm(formula = Attrition ~ Age + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBetter +  
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobRole.xResearch.Director + 
                 MaritalStatus.xSingle +
                 finalTotalworkinghours, family = "binomial", data = train)  

summary(model_20) 

#AIC 2195.7....nullDev 2728.0...resDev  2161.7

#Excluding  JobRole.xResearch.Director due to lower significance with respect to other variables
model_21<- glm(formula = Attrition ~ Age + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xBetter +  
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 MaritalStatus.xSingle +
                 finalTotalworkinghours, family = "binomial", data = train)  

summary(model_21) 

#AIC 2200.3....nullDev 2728.0...resDev  2168.3

#Now All 15 variables are significant So this model is final model

final_model<- model_21

summary(final_model) 

################Factor Analysis ################
## Lets find the correlation between attrition and final variables

corrplot(cor(HR_final[,c(2,1,8,9,11,12,13,57)]))

str(HR_final)
#######################################################################


### Model Evaluation

### Test Data ####

### Test Data ####

#predicted probabilities of attrition 1 for test data

test_pred = predict(final_model, type = "response",newdata = test[-2])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_attr <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attr <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attr,test_pred_attr)

##                  test_pred_attr
##test_actual_attr   No  Yes
##       No         1084   26
##       Yes        160   53

test_conf <- confusionMatrix(test_pred_attr, test_actual_attr, positive = "Yes")
test_conf

#Sensitivity : 0.50704         
#Specificity : 0.90541

#######################################################################
test_pred_attr <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

table(test_actual_attr,test_pred_attr)

##                  test_pred_attr
##test_actual_attr   No   Yes
##       No         1054   56
##       Yes        143    70

test_conf <- confusionMatrix(test_pred_attr, test_actual_attr, positive = "Yes")
test_conf

##Sensitivity : 0.32864         
##Specificity : 0.94955 

#######################################################################
test_pred_attr <- factor(ifelse(test_pred >= 0.30, "Yes", "No"))

table(test_actual_attr,test_pred_attr)

##                  test_pred_attr
##test_actual_attr   No    Yes
##       No         1005   105
##       Yes        105    108

test_conf <- confusionMatrix(test_pred_attr, test_actual_attr, positive = "Yes")
test_conf

#Sensitivity : 0.50704         
#Specificity : 0.90541 

#######################################################################

#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attr <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attr, test_actual_attr, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

##0.1616162

# Let's choose a cutoff value of 0.1616162 for final model

test_cutoff_attr <- factor(ifelse(test_pred >=0.1616162, "Yes", "No"))

table(test_actual_attr,test_cutoff_attr)

conf_final <- confusionMatrix(test_cutoff_attr, test_actual_attr, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc   ##0.7226002

sens  ##0.7230047

spec  ## 0.7225225

View(test)

##################################################################################################
### KS -statistic #############################

test_cutoff_attr <- ifelse(test_cutoff_attr=="Yes",1,0)
test_actual_attr <- ifelse(test_actual_attr=="Yes",1,0)



#on testing  data
pred_object_test<- prediction(test_cutoff_attr, test_actual_attr)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)  ##0.4455272

############################Lets create Gain & Lift Chart on test data

test2<-test[order(test$prob,decreasing = TRUE),]

length(which(test2$Attrition[1:133]==1)) ##72

length(which(test2$Attrition[134:266]==1)) ## 50


lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_attr, test_pred, groups = 10)

## Please Refer xls for Left & Gain metrics




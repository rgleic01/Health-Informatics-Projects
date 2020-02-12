set.seed(1389)

install.packages("tidyverse")
install.packages("e1071")
install.packages("MASS")
install.packages("car")
install.packages("mvoutlier")
install.packages("mice")

setwd("/Users/robgleich/Documents/HI@GW/r_files")

urlfile="https://raw.githubusercontent.com/gwcbi/ResearchAnalyticsLabs/master/ProblemSets/PS2/Session5PimaDiabetes.csv"
dat <- read.csv(url(urlfile))
head(dat) # take a peek at the first few rows

# 1. Find & remove variables with more than 40% missing values
sapply(dat, function(x) 100 * sum(is.na(x)) / length(dat$Age)) #display % NA for each column
dat$Insulin <- NULL #remove 'Insulin' column

# 2. Impute missing values using mice 
library(mice)
cc = md.pattern(dat) #get pattern of missingness

ind_dat = dat[,-c(8)] #drop response variable "diabetes"
imp1=mice(ind_dat);  #creates 5 (or specified) data sets, can represent uncertainty
imp1$method #find out methods used for each variable
dat2=complete(imp1,action=1) #extracts first of the 5 imputed datasets
sapply(dat2, function(x) sum(is.na(x))) #validate

# 3. Find Skewness & Kurtosis
library(e1071)
#find metrics for data
metrics = cbind(apply(dat2,2,skewness),apply(dat2,2,kurtosis))
#coerce object to dataframe and rename columns
metrics_df <- as.data.frame(metrics)
names(metrics_df)[1] <- 'skew'
names(metrics_df)[2] <- 'kurtosis'
#display df
metrics_df



# 4. Transform variables with excess skewness
library(MASS)
library(car)

#function to set floor of 1
anchor1 = function(y){
  y1 = y - min(y) + 1
  y1
}

#boxcox transform the anchored values for Pedigree and Age
BC1 = boxcox(anchor1(dat2$Pedigree)~1, lambda=seq(-10,10,1/10));
BC2 = boxcox(anchor1(dat2$Age)~1, lambda=seq(-10,10,1/10));

lambda_ped=BC1$x[(1:length(BC1$y)) [BC1$y==max(BC1$y)]]
lambda_age=BC2$x[(1:length(BC2$y)) [BC2$y==max(BC2$y)]]

#transform values based on optimal lambda
ped_tran = bcPower(anchor1(dat2$Pedigree), lambda_ped)
age_tran = bcPower(anchor1(dat2$Age), lambda_age)
#replace skewed values with transformed values
dat_tran = cbind(dat2$numPregnant,dat2$Glucose,dat2$DiastolicBP,dat2$TriceptSkinFold,dat2$BMI,ped_tran,age_tran)
metrics_tran = cbind(apply(dat_tran,2,skewness),apply(dat_tran,2,kurtosis))

#coerce to df and rename columns and rows
metrics_tran_df <- as.data.frame(metrics_tran)
names(metrics_tran_df)[1] <- 'skew'
names(metrics_tran_df)[2] <- 'kurtosis'
library(data.table)
(setattr(metrics_tran_df, "row.names", c("numPregnant","Glucose","DiastolicBP","TriceptSkinFold","BMI","Skewness","Kurtosis")))


# 5. Assess multivariate outliers
library(mvoutlier)

#transform to df and rename columns
dat_tran_df <- as.data.frame(dat_tran)
names(dat_tran_df)[1] <- 'numPregnant'
names(dat_tran_df)[2] <- 'Glucose'
names(dat_tran_df)[3] <- 'DiastolicBP'
names(dat_tran_df)[4] <- 'SkinFold'
names(dat_tran_df)[5] <- 'BMI'
names(dat_tran_df)[6] <- 'Pedigree'
names(dat_tran_df)[7] <- 'Age'

bb=uni.plot(dat_tran_df,alpha=0.025)
sum(bb$outliers) #find total number of outliers

(1:dim(dat_tran)[1])[bb$outliers] # indices of outliers
bb$md[bb$outliers] # mahalonoblis distances of outliers

which(bb$md==max(bb$md)) # index of biggest outlier
max(bb$md) # mahalonoblis distance of biggest outlier







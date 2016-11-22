# import relevant packages and headers
source("/Users/sam/All-Program/App/IIT-Projects/Insurance-Defaulters/Data_Cleaner.R")
library(caret)
library(dummy)
#################   Data File Location    ##################
# Load the Data
datam = read.csv2("/Users/sam/All-Program/App-DataSet/IIT-Projects/Insurance-Defaulters/LoanStats3a.csv", header=TRUE, sep=",", skip=1)


varm = read.csv2("/Users/sam/All-Program/App-DataSet/IIT-Projects/Insurance-Defaulters/Variables-Sheet1.csv", header=TRUE, sep=",")


#clean_data = read.csv2("/Users/sam/All-Program/App-DataSet/IIT-Projects/Insurance-Defaulters/LoanStats3a.csv", header=TRUE, sep=",", skip=1)


#################   Data File Location    ##################


# Remove all the rows with response as "current"
unique(datam$loan_status)
dat <- subset(datam, loan_status == c("Charged Off","Fully Paid"))


# Check How many are null or NA
summary(dat)


# Data on Status:
feature_set1<-subset(varm, Status=="1")
feature_set1$LoanStatNew
feature_set0<-subset(varm, Status=="0")
feature_set0$LoanStatNew
feature_set2<-subset(varm, Status=="2")
feature_set2$LoanStatNew


# Get the data for the given feature set.
intrsct<-intersect(feature_set1$LoanStatNew, colnames(dat))
# Column names that are present in varm but not a part of datam column_names
setdiff(feature_set1$LoanStatNew,intrsct)


# Finding the subset of datam with features decided as initial analysis
dat_1<-subset(dat, select = intrsct)
head(dat_1)




############  Clean columns 15-30   ##########
# Remove Columns:
col_names <- c("id")
x_clean <- clean(dat_1)
x_clean <- drop_columns(x_clean,col_names)


dim(datam)
dim(dat)
dim(x_clean)






# Select sample random data from the Dataframe
sample_x_clean <- x_clean[sample(nrow(x_clean), 100), ]






#########  creating Dummy Data  ##########
sample_x_clean <- var_cleaner(sample_x_clean)


grade <- predict(dummyVars(~grade, data = sample_x_clean), newdata = sample_x_clean)
application_type <- predict(dummyVars(~application_type, data = sample_x_clean), newdata = sample_x_clean)
home_ownership <- predict(dummyVars(~home_ownership, data = sample_x_clean), newdata = sample_x_clean)
pymnt_plan <- predict(dummyVars(~pymnt_plan, data = sample_x_clean), newdata = sample_x_clean)
grade <- predict(dummyVars(~grade, data = sample_x_clean), newdata = sample_x_clean)






######### Check by fiiting a simple model  #############
loan_status1 <- predict(dummyVars(~loan_status, data = sample_x_clean), newdata = sample_x_clean)
# Fit a simple linear Model
fit0<-lm(sample_x_clean$loan_status.new~sample_x_clean$annual_inc,data=sample_x_clean)
# Fit a simple logit model
glm.out = glm(sample_x_clean$loan_status.new~sample_x_clean$annual_inc, family=binomial(logit), data=sample_x_clean)
glm.out = glm(sample_x_clean$loan_status.new~sample_x_clean$dti, family=binomial(logit), data=sample_x_clean)


summary(glm.out)



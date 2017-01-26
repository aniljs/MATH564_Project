# import relevant packages and headers
source("Data_Cleaner.R")
library(caret)
library(dummy)
library(feather)

#################   Data File Location    ##################
# Load the Data
datam = read.csv2("LoanStats3a.csv", header=TRUE, sep=",", skip=1)
write.csv(datam, file = "step0.csv", row.names=FALSE, na="")
#write_feather(datam, "step0.feather")

varm = read.csv2("Variables-Sheet1.csv", header=TRUE, sep=",")


#clean_data = read.csv2("/Users/sam/All-Program/App-DataSet/IIT-Projects/Insurance-Defaulters/LoanStats3a.csv", header=TRUE, sep=",", skip=1)


#################   Data File Location    ##################


# Remove all the rows with response as "current"
unique(datam$loan_status)
dat <- subset(datam, loan_status == 'Charged Off' | loan_status == 'Fully Paid')

#write_feather(dat, "step1.feather")

write.csv(dat, file = "step1.csv",row.names=FALSE, na="")

#dim(subset(datam, loan_status == 'Charged Off' | loan_status == 'Fully Paid'))


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
write.csv(dat_1, file = "step2.csv",row.names=FALSE, na="")
#write_feather(dat_1, "step2.feather")


############  Clean columns 15-30   ##########
# Remove Columns:
col_names <- c("id","application_type")
x_clean <- clean(dat_1)
x_clean <- drop_columns(x_clean,col_names)
dim(datam)
dim(dat)
dim(x_clean)
write.csv(x_clean, file = "step3.csv", row.names=FALSE, na="")
#write_feather(x_clean, "step3.feather")




# Select sample random data from the Dataframe
sample_x_clean <- x_clean[sample(nrow(x_clean), 1000), ]
sample_x_clean <- var_cleaner(sample_x_clean)
sample_x_clean <- subset(sample_x_clean, verification_status == c("Source Verified","Verified"))
y <- sample_x_clean$loan_status.new
x <- sample_x_clean[ , ! colnames(sample_x_clean) %in% c("loan_status", "loan_status.new") ]






############  Linear Regression  ########
fit <- lm(y~., data=x)
forwardAIC <- stepAIC(fit, direction="forward")
backwardAIC <- stepAIC(fit, direction="backward")
bothAIC <- stepAIC(fit, direction="both")
forwardAIC $anova # display results
backwardAIC $anova # display results
bothAIC $anova # display results

fit <- lm(y~annual_inc+delinq_2yrs+delinq_2yrs+dti+earliest_cr_line+emp_length+grade+home_ownership+inq_last_6mths+installment+int_rate+last_pymnt_amnt+loan_amnt+mths_since_last_delinq+open_acc+pub_rec+recoveries+revol_bal+revol_util+term+total_acc+total_pymnt+total_pymnt_inv+total_rec_int+total_rec_late_fee+total_rec_prncp+verification_status+zip_code, data=x) 




fit <- lm(x$int_rate~annual_inc, data=x)


fit <- lm(x$int_rate~annual_inc+delinq_2yrs+delinq_2yrs+dti+earliest_cr_line+emp_length+grade+home_ownership+inq_last_6mths+installment+last_pymnt_amnt+loan_amnt+mths_since_last_delinq+open_acc+pub_rec+recoveries+revol_bal+revol_util+term+total_acc+total_pymnt+total_pymnt_inv+total_rec_int+total_rec_late_fee+total_rec_prncp+verification_status, data=x) 






# ######### Check by fiiting a simple model  #############
#loan_status1 <- predict(dummyVars(~loan_status, data = sample_x_clean), newdata = sample_x_clean)
glm.out = glm(y~., family=binomial(logit), data=x)
glm.out <- glm(y~annual_inc+delinq_2yrs+dti+earliest_cr_line+emp_length+grade+home_ownership+inq_last_6mths+installment+int_rate+last_pymnt_amnt+loan_amnt+mths_since_last_delinq+open_acc+pub_rec+recoveries+revol_bal+revol_util+term+total_acc+total_pymnt+total_pymnt_inv+total_rec_int+total_rec_late_fee+total_rec_prncp+verification_status, family=binomial(logit), data=x) 


glm.out <- glm(y~int_rate, family=binomial(logit), data=x) 




cor(y,x$home_ownership)
tbl <- table(y, x$home_ownership)
chisq.test(tbl)






######### Analysis  ##########


# Drop : anual_inc
# Keep : delinq_2yrs, dti, grade(subgrade), int_rate
# Maybe: earliest_cr_line, home_ownership
# Doesn't make sense: emp_length






##########  ##########  ##########  ##########  ##########  ##########  ##########  ##########
##########  ##########  ##########  ##########  ##########  ##########  ##########  ##########
##########  ##########  ##########  ##########  ##########  ##########  ##########  ####################  ##########  ##########  ##########  ##########  ##########  ##########  ##########










# glm.out = glm(sample_x_clean$loan_status.new~sample_x_clean$dti, family=binomial(logit), data=sample_x_clean)
# 
# summary(glm.out)








#fit <- lm(y~acc_now_delinq+annual_inc+chargeoff_within_12_mths+delinq_2yrs+delinq_2yrs+delinq_amnt+dti+earliest_cr_line+emp_length+grade+home_ownership+inq_last_6mths+installment+int_rate+last_pymnt_amnt+loan_amnt+mths_since_last_delinq+open_acc+out_prncp+out_prncp_inv+pub_rec+pub_rec_bankruptcies+pymnt_plan+recoveries+revol_bal+revol_util+tax_liens+term+total_acc+total_pymnt+total_pymnt_inv+total_rec_int+total_rec_late_fee+total_rec_prncp+verification_status+zip_code, data=sample_x_clean)  




#glm.out <- glm(y~annual_inc+delinq_2yrs+delinq_2yrs+dti+earliest_cr_line+emp_length+grade+home_ownership+inq_last_6mths+installment+int_rate+last_pymnt_amnt+loan_amnt+mths_since_last_delinq+open_acc+pub_rec+recoveries+revol_bal+revol_util+term+total_acc+total_pymnt+total_pymnt_inv+total_rec_int+total_rec_late_fee+total_rec_prncp+verification_status, family=binomial(logit), data=x) 






# lm(formula = lnWeight ~ lnLength, data = alligator)   
#             
# "acc_now_delinq"           "annual_inc"               "application_type"        
# [4] "chargeoff_within_12_mths" "delinq_2yrs"              "delinq_amnt"             
# [7] "dti"                      "earliest_cr_line"         "emp_length"              
# [10] "grade"                    "home_ownership"           "inq_last_6mths"          
# [13] "installment"              "int_rate"                 "last_pymnt_amnt"         
# [16] "loan_amnt"                "loan_status"              "mths_since_last_delinq"  
# [19] "open_acc"                 "out_prncp"                "out_prncp_inv"           
# [22] "pub_rec"                  "pub_rec_bankruptcies"     "pymnt_plan"              
# [25] "recoveries"               "revol_bal"                "revol_util"              
# [28] "tax_liens"                "term"                     "total_acc"               
# [31] "total_pymnt"              "total_pymnt_inv"          "total_rec_int"           
# [34] "total_rec_late_fee"       "total_rec_prncp"          "verification_status"     
# [37] "zip_code"                 "loan_status.new"         




######### Create Dummy Variables  ###########


# grade <- predict(dummyVars(~grade, data = sample_x_clean), newdata = sample_x_clean)
# application_type <- predict(dummyVars(~application_type, data = sample_x_clean), newdata = sample_x_clean)
# home_ownership <- predict(dummyVars(~home_ownership, data = sample_x_clean), newdata = sample_x_clean)
# pymnt_plan <- predict(dummyVars(~pymnt_plan, data = sample_x_clean), newdata = sample_x_clean)
# grade <- predict(dummyVars(~grade, data = sample_x_clean), newdata = sample_x_clean)








# levels(x_clean$grade)
# levels(x_clean$application_type)
# levels(x_clean$home_ownership)
# levels(x_clean$mths_since_last_delinq)      # To check
# levels(x_clean$out_prncp)                   # Not Categorical
# levels(x_clean$out_prncp_inv)               # Not Categorical 
# levels(x_clean$pub_rec)                     # To Check
# levels(x_clean$pub_rec_bankruptcies)        # To Check
# levels(x_clean$pymnt_plan)
# levels(x_clean$tax_liens)                   # To Check
# levels(x_clean$term)
# levels(x_clean$total_rec_late_fee)          # Not Categorical
# levels(x_clean$verification_status)
# levels(x_clean$zip_code)
# levels(x_clean$loan_status)












######### Rough #########
# library(ggplot2)
# mysvm      <- svm(Species ~ ., iris)
# Predicted  <- predict(mysvm, iris)
# 
# mydf = cbind(iris, Predicted)
# qplot(sample_x_clean$annual_inc, sample_x_clean$earliest_cr_line, colour = Species, shape = Predicted, 
#       data = iris)
# 
# attach(sample_x_clean); plot(sample_x_clean$annual_inc, sample_x_clean$earliest_cr_line, col=c("red","blue")[sample_x_clean$loan_status]); detach(sample_x_clean)
# 
# 
# 
# n = c(1, 2, 3,4,5) 
# s = c("a", "a", "b", "c", "c") 
# b = c(TRUE, FALSE, TRUE) 
# df = data.frame(n, s) 
# predict(dummyVars(~s, data = df), newdata = df)
# 
# aaa <- dummy(iris$Species)














########### Analysis  ##########


# Look data for the given columns
#sel_col<-datam[c("loan_amnt","funded_amnt","installment", "annual_inc")]
# Top 5 elements:
#head(sel_col)


#colnames(datam)
#head(datam[15:30])
#unique(datam$verification_status)
#unique(datam$pymnt_plan)
#URL and description not needed.
#unique(datam$purpose)
#unique(datam$title)





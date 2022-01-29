#set working directory
setwd("~/Desktop")

# loading the data
library(inspectdf)
library(foreign)
library(tidyverse)
library(ggplot2)
library(GGally)
library(dplyr)
data <- read.spss("data/W1 Merged Data/Wave.1_Data/Merge/Wave1_20170906.sav", to.data.frame=TRUE)
attach(data)
data %>% inspect_types() 
data = as.data.frame(data)


DATA <- read.spss("data/W1 Merged Data/Wave.1_Data/Merge/Wave1_20170906.sav", to.data.frame=TRUE)
var_names <- attr(DATA, 'variable.labels')

###########################################################################
###########################################################################
##############################################Preprocessing
###########################################################################


#Select democracy features not overlapping
demos <- c("country", "level3", "se002", "se003a", "se004a","se005","se005a", "se006","se008a","se009","se012a","se017" )
demo_data <- data[,demos]

##Merge
questions_data <- data[,grep("q001", colnames(data)):grep("w_all", colnames(data))]
data <- cbind(demo_data,questions_data)



###### define target variable #######
# Make data uniform
data$q101 <- as.character(data$q101)
data$q101[which(data$q101=="10 Complete democracy")] <- "10"
data$q101 <- as.numeric(data$q101)
data$q100 <- as.character(data$q100)
data$q100[which(data$q100=="1 Complete dictatorship")] <- "1"
data$q100 <- as.numeric(data$q100)
# Create discrepancy column
data['discrepancy'] <- data['q101'] - data['q100']
data['discrepancy'] <- round(data['discrepancy'])
discrepancy_plot = data %>% group_by(as.factor(discrepancy)) %>% count() %>% rename(dis=names(.)[1], counts = names(.)[2]) 
discrepancy_plot

dropcol <- c("q100","q101")
colnames(data)
data <- data[ , !names(data) %in% dropcol] 


data = data[!is.na(data$discrepancy),]



# Deciding what to do with categorical variables with over 15 categories
x=list()
too_many = c()
for (i in 1:ncol(data)) {
  k = count(unique(data[i]))
  if (k>15) {
    too_many = append(too_many, colnames(data)[i])
    print(i)
  }
} 
too_many
colnames(data) 


# Remove meaningless columns that are not interpretable
drops <- c("w_all","idnumber","yrsurvey", "se003a",  "se005a", "q062", "q097_1","ir001" , "ir002", "ir002a" ,"ir002b", "ir003", "ir004",  "ir005", "ir006", "ir007_1", "ir007_2", "ir007_3", "ir009", "ir010a", "ir010b", "ir010c", "ir010d", "ir010e" ,"se008a","q100","q101","q099","q102","q103")
#w_all - hard to intepret
#idnumber - this is just idnumber
#yrsurvey - year the survey was conducted, does not help our prediction
#se003a - exact age, we already have age in brackets 
#se005a - exact year of education, we already have education level in brackets 
#q062 - Different political parties of different countries, which are hard to interpret, we have no domain knowledge of  
#q097_1 - Features people think are crucial in democracy, (question not in multiple choice), too many different features 
#se008a - family members, however there are no members and many variables that are over 10.
#ir's are interviewer data
# q99,q102,q103 are preceding subsequent question to the q100and q101, which is used to compute our Y variable. These variables are too correlated to our target variable and would damage our prediction model
data = data[ , !(names(data) %in% drops)]


# NA Check and drop columns with 40% or more NAs
x = sapply(data, function(x) round(sum(is.na(x))/nrow(data),2))
data[is.na(data['q023']),'q023'] <- 'Never'
dropcol <- names(x[x>=.4])
data <- data[ , !names(data) %in% dropcol] 

summary(data)

high_na=c()
for (i in 1:ncol(data)) {
  if (sum(is.na(data[,i])) >= 1000) {
    high_na = append(high_na,names(data[i]))
    print(sum(is.na(data[i])))
  }
}
high_na
length(high_na)


###########################################################################
###########################################################################
##############################################Explore NA data

#NA proportion
summary(data)
mean(is.na(data))

#NA corrleation
x<- as.data.frame(abs(is.na(data)))
na<- x[which(apply(x,2,sum)>0)]

na

library(corrplot)

CorMatirx <- cor(na)
##### Correlation Matrix#####
data1 = subset(na, select = -c(discrepancy) )
corr_simple <- function(data=data1){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  data_cor <- data %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(data_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #sort by highest correlation
  corr <- head(corr[order(-abs(corr$Freq)),] , n = 50)
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}
corr_simple(data1)
###### NA correlation matrix comment #####
# By investigating the variables that were identified to have high correlations in the correlation matrix, we found that many of the NA values are coming from certain questions 
# that were not asked to some countries (for instance, q063 and q104 were both not asked in China).

#######################
###########Exporting columns with a lot of NA values to manually analyze the data 
high_na_var=c()
for (i in 1:ncol(data)) {
  if (sum(is.na(data[,i])) >= 500) {
    high_na_var = append(high_na_var,names(data[i]))
  } 
}
high_na_var

high_na_num=c()
for (i in 1:ncol(data)) {
  if (sum(is.na(data[,i])) >= 500) {
    print(sum(is.na(data[i])))
    high_na_num = append(high_na_num,sum(is.na(data[i])))
  } 
}

high_na_data <- cbind(high_na_var,high_na_num)
write.csv(high_na_data, file = "high_na_data.csv")
# As we found out from analysis of the correlation matrix has revealed, many of the NA values are coming from certain questions that were not asked to some countries. 
# 13 questions out of 32 questions (se009,q003,q029,q030,q107,q112,q113,q114,q118,q134,q137,q141,q145) that has more than 500 NA values were not coming from above-mentioned "not asked questions."
# For those questions, we will treat such responses as "No Answer."

### fillna
# Questions not asked to some countries 
not_asked = c("q003","q013","q015","q018","q028",'q029','q030',"q063",'q072','q077','q080','q104','q107','q109',"q110","q111","q112","q113","q114","q115","q118","q120","q121","q122","q125","q134","q137","q141","q144","q145","q147")

countries =c("Japan","Hong Kong","Korea",   "Mainland China","Mongolia","Philippines","Taiwan","Thailand")
# Adding new factors
for (i in 1:ncol(data)) {
  if (names(data[i]) %in% not_asked) {
    levels(data[,names(data[i])]) = c(levels(data[,names(data[i])]), "Not Asked", "No Answer") 
  }
} 
# some countries have questions not asked
vec = c()
for (q in not_asked) {
  for (cc in countries){
    vec = c(cc,q)
    if (sum(is.na(data[data['country']==cc, q])) == sum(data[,'country'] == cc)) {
      data[data['country']==cc,q] = 'Not Asked'
    } 
  }
} 
# Rest of the NA's are not answered == No Answer
high_na=c()
vec=c()
for (i in 1:ncol(data)) {
  if (sum(is.na(data[,i])) > 0) { 
    high_na = append(high_na,names(data[i]))
    print(colnames(data[i]))
    vec = append(vec, colnames(data[i]))
  } 
}
# Add no answer as a factor
for (i in 1:ncol(data)) {
  if (names(data[i]) %in% vec) {
    levels(data[,names(data[i])]) = c(levels(data[,names(data[i])]), "No Answer") 
  }
} 
# fill na's with no answer 
for (j in 1:ncol(data)) {
  if (sum(is.na(data[,j]))!=0) {
    data[is.na(data[,j]),j] = 'No Answer'
  }
}


summary(data)
# All Na's are filled


nrow(data)
ncol(data)

###Now! we are goin to analyze factors With this 6875 observations and 101 features!!!!!!!!

###########################################################################
###########################################################################
###########################################Exploratory Factor Analysis
#install.packages("psych")
#install.packages("GPArotation")
library(psych)


summary(data)
dropcol <- c("discrepancy")
data1 <- data[ , !names(data) %in% dropcol]

colnames(data1) 
for (i in 1:ncol(data1)) {
  if (is.factor(data1[,i])) {
    data1[,i] = as.numeric(data1[,i])
  }
} 
summary(data1)
data_cor <- cor(data1)
data_cor

####Determining the number of factors to extract
#install.packages("nFactors")
library(nFactors)
ev <- eigen(cor(data1)) # get eigenvalues
ap <- parallel(subject=nrow(data1),var=ncol(data1), rep=100, cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


my_fa <- fa(r = data1, nfactors = 17)
rownames(my_fa$loadings)
write.csv(my_fa$loadings, file = "my_fa.csv")

##Merge with Important variables and extracted latent features
n <- c('q007','q008','q009','q010','q006','q098','q128','q005','q027','q028','q105','q106','q121','q123','q127')
important <- data[ , names(data) %in% n]
colnames(important)
#scaling discrepancy because range is to large
discrep <- scale(data$discrepancy)

new_data<- cbind(important, my_fa$scores, discrep)
summary(new_data)
colnames(new_data)

###########################################################################
###########################################################################
##############################################Predictive Modeling
## 75% of the sample size
smp_size <- floor(0.75 * nrow(new_data))
smp_size
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(new_data)), size = smp_size)

train_data <- new_data[train_ind, ]
test_data <- new_data[-train_ind, ]


nrow(train_data)
nrow(test_data)


#####defined functions
### Returns the indices for which |x[i]| > tr
support<- function(x, tr = 10e-6) {
  m<- rep(0, length(x))
  for (i in 1:length(x)) if( abs(x[i])> tr ) m[i]<- i
  m <- m[m>0]
  m
}

###########################
#### linear Regression ####
##########################

model.lm <-glm(discrep ~ ., data=train_data)
summary(model.lm)
pred.lm <- predict(model.lm, newdata=test_data)

###########################
#### Random Forest ####
##########################

library(randomForest)
model.rf <- randomForest(discrep ~ ., data=train_data, importance=TRUE) 
pred.rf <- predict(model.rf, newdata=test_data)


##################
#### LASSO ####
##################
library(glmnet)
My <- train_data$discrep  
# the features need to be a matrix ([,-1] removes the first column which is the intercept)
Mx <- model.matrix(discrep ~., data = train_data)[,-1] 
Mx_test <- model.matrix(  ~., data = test_data)[,-1] 
lassoCV <- cv.glmnet(Mx,My) 
lasso_min_lambda <- lassoCV$lambda.min   
### Using Lasso with the lambdas we found
lasso <- glmnet(Mx, My, lamda= lasso_min_lambda)  
summary(lasso)
### Show the coefficients of selected variables
coef(lasso)
summary(lassoCV)

pred.pl <- predict(lasso, newdata=Mx_test)

#####################
### POST LASSO ######
### Random Forest ###
### Linear Forest ###
####################
### Getting post lasso data set by getting the selected variables
features.min <- support(lasso$beta[,which.min(lassoCV$cvm)]) 
data.min <- data.frame(Mx[,features.min],My)
new.data.min <- data.frame(Mx_test[,features.min],test_data$discrep)
features.min
data.min
postlasso <- randomForest(My~., data=data.min, important=TRUE)
summary(postlasso)
pred.pl <- predict(postlasso, newdata=data.min)

summary(data.min)

plostlasso_lm <- lm(My ~ ., data=data.min)
pred.pll <- predict(plostlasso_lm, data=new.data.min)




###########################
#### Evaluation ####
##########################

library(Metrics)

###########################
#### Null (mean) Model   ####
pred.null <-mean(train_data$discrep)


#RMSE Plotting

r5<-rmse(test_data$discrep, pred=pred.pl)
r4<-rmse(test_data$discrep, pred.pll)
r1<-rmse(test_data$discrep, pred=pred.rf)
r2<-rmse(test_data$discrep, pred.lm)
r3<-rmse(test_data$discrep, pred.null)
Rmsex<-c('Random_Forest','Multivariate Linear','Null','Post-Lasso Linear','Post-Lasso Random Forest')
Rmsey<-c(r1,r2,r3,r4,r5)
data_rmse<-as.data.frame(Rmsey)
rownames(data_rmse)<-Rmsex 

#plotting R2 for all compared models
Rmsemodels<-ggplot(data=data_rmse, aes(x=Rmsex, y=Rmsey)) +
  geom_bar(stat = 'identity', aes(fill = Rmsey)) +
  scale_fill_gradient(low="lightblue",high="darkblue")+
  labs(title='Model Performance',x='Model',y='OOS - RMSE',fill='RMSE')+
  theme(axis.text.x = element_text(angle=45, hjust=1))
Rmsemodels



####variable importance analysis
#calculating %IncMSE (importance measure) with importance() function.
variable_analysis <- importance(model.rf, type=1)
write.csv(variable_analysis, file = "variable_analysis.csv")




####################################################################
##Script: OR 568 Final Project
##Programmers: 
##Jem Anderson,Michael Ruhl,Snigdha Cheekoty,Wanru Li, Yang Liu 
####################################################################
#installing the packages
install.packages("rsq")
install.packages("tidyverse")
install.packages("caret")
install.packages("ggplot2")
install.packages("magrittr")
install.packages("earth")
install.packages("pls")
install.packages("gridExtra")
install.packages("partykit")
install.packages("rpart.plot")
install.packages("rpart")
install.packages("corrplot")
install.packages("Metrics")
install.packages("plyr")
install.packages("dplyr")
install.packages("scales")
install.packages("glmnet")
install.packages("elasticnet")
install.packages("dummies")
install.packages("stats")
install.packages("e1071")
install.packages("randomForest")

#libraries
library(rsq)
library(tidyverse)
library(caret)
library(ggplot2)
library(magrittr)
library(earth)
library(pls)
library(gridExtra)
library(partykit)
library(rpart.plot)
library(rpart)
library(corrplot)
library(Metrics)
library(plyr)
library(dplyr)
library(scales)
library(glmnet)
library(elasticnet)
library(dummies)
library(stats)
library(e1071)
library(randomForest)



#####################################################################################
#### skewness of data, cleaning data,correlation matrix, Linear regression model#########
####################################################################################

###Read data

data = read.csv("~/Downloads/AmesImp-3.csv", stringsAsFactors = F)
data
dim(data)
str(data)


###Data Preprocessing
colnames(data)
summary(data$SalePrice)
median(data$SalePrice)
##Removing nearZeroVaraince 
nearVars=nearZeroVar(data)
amesdata=data[-nearVars,]
##Checking missing value
missmap(data[,1:82],main ="Missing values",col = c("blue", "orange"),y.cex = 0.5, x.cex = 0.5,y.labels = NULL,y.at = NULL)
sapply(data[,1:82], function(x) sum(is.na(x)))
misvals = sort(apply(amesdata, 2, function(x) sum(is.na(x))),decreasing = T)
misvals[misvals>0]
#According to the missing value chart, there are a lot of missing values. PoolQC, MiscFeature, Alley and Fence have 90% of the data as NA.
##Dealing with missing value
#For those categorical features,missing values will be imputed as "None".
number_na_col=list()
for(x in c(8, 59, 60, 62, 65, 66, 74, 75, 76)){
  amesdata[is.na(amesdata[,x]),x]="None"
}
#For numerical variable(Lot Frontage),changing blank to 0.
for(x in c(5)){
  amesdata[is.na(amesdata[,x]),x]="0"
}
number_na_col
colSums(is.na(amesdata))
#For some futures which have few missing values, just remove them.
dim(amesdata)
number_na_row=list()
for(i in 1:2909){
  number_na_row[i]=sum(is.na(amesdata[i,]))
}
remove_row=which(number_na_row > 0)
amesdata=amesdata[-remove_row,]
colSums(is.na(amesdata))
#As we can see, the data is clean now.There are 2662 observations and 82 variables. 

###Data Analysis
##Devide into train and test dataset
smp_size = floor(0.75 * nrow(amesdata))
set.seed(2017)
train_ind = sample(seq_len(nrow(amesdata)), size = smp_size)
train = amesdata[train_ind, ]
test = amesdata[-train_ind, ]
#Plot histogram
#Get data frame of SalePrice and use log form for plotting
ggplot(train, aes(x=SalePrice)) + geom_histogram(col = 'white') + theme_light() +scale_x_continuous(labels = comma)
summary(train$SalePrice)
#Normalize distribution---Using log to make the distribution of the target variable normalization.
ggplot(train, aes(x=log(SalePrice+1))) + geom_histogram(col = 'white') + theme_light()
#Make Factor Variables
train = as.data.frame(lapply(train, function(x) if(class(x)=="character") {as.factor(x)} else {x}))
test = as.data.frame(lapply(test, function(x) if(class(x)=="character") {as.factor(x)} else {x}))
##For numeric variables.
feature_classes = sapply(names(amesdata),function(x){class(amesdata[[x]])})
numeric_feats = names(feature_classes[feature_classes != "character"])
numeric_df = amesdata[numeric_feats]
##Correlations
correlations = cor(numeric_df[,-2])
row_indic = apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations = correlations[row_indic ,row_indic ]
#Correlation metrix
corrplot(correlations, method="square")
corrplot(correlations, method="number")

###Models
##Linear regression model
#For all numeric dataset
LinearRM1 = lm(SalePrice~., data=numeric_df) 
summary(LinearRM1)
plot(LinearRM1)

#Predict test data
predict1 = predict(LinearRM1,test)
test_predict1 = cbind(test,round(predict1))
test_predict1
RMSE1 = RMSE(predict1, test$SalePrice)
RMSE1
R21 = rsq::rsq(LinearRM1)
R21
MAE1 = MAE(predict1, test$SalePrice)
MAE1

#For several variables have strong corrrelations with predicting house price.
#I choose this variables because 
#Sub settting the high correlation variable with predictor variable.
predict_var = train[c("Overall.Qual","Total.Bsmt.SF","X1st.Flr.SF", "Gr.Liv.Area", "Garage.Area","Garage.Cars","SalePrice")]
plot(predict_var)
LinearRM2 = lm(SalePrice~Overall.Qual+Gr.Liv.Area+Garage.Area+Garage.Cars+Total.Bsmt.SF+X1st.Flr.SF, data = train,method ="cv")
summary(LinearRM2)
plot(LinearRM2)

#Predict test data
predict2 = predict(LinearRM2,test,type = "response")
test_predict = cbind(test,round(predict2))
test_predict
RMSE2 = RMSE(predict2, test$SalePrice)
RMSE2
R22 = rsq::rsq(LinearRM2)
R22
MAE2 = MAE(predict2, test$SalePrice)
MAE2

#### ~~~~~~~~~~~~~~~~~~~~ ####
#### Partial Least Square ####
#### ~~~~~~~~~~~~~~~~~~~~ ####
##read the data
AmesImp <- read.csv("~/Downloads/AmesImp-3.csv", stringsAsFactors = default.stringsAsFactors(), header = TRUE)


# install.packages("gridExtra")
library(pls)
library(caret)
library(gridExtra)


set.seed(100) # Set seed for consistent calculations

pls_ames <- AmesImp # Place source data into a secondary table for work

dim(pls_ames) 
# [1] 2930   82


######################################
####~~~~~~~~~~Remove NZV~~~~~~~~~~####
######################################

pls_nearzero <- nearZeroVar(pls_ames) # Identify NZV columns
pls_ames <- pls_ames[,-pls_nearzero] # Remove NZV columns

dim(pls_ames) 
# [1] 2930   61

######################################
####~~~~Remove Columns with NA~~~~####
######################################

n_na_col=list() # Loop to count missing data in columns
for (i in 1:ncol(pls_ames)){
  n_na_col[i]=sum(is.na(pls_ames[,i]))
}
n_na_col  # Read which columns they are


remove_col=which(n_na_col > 100) # 100 was a threshold used to screen out some columns but not all

pls_ames=pls_ames[-remove_col] # Remove columns with more than 100 NAs present

dim(pls_ames)
# [1] 2930   53


######################################
####~~~~~Remove Rows with NA~~~~~~####
######################################

n_na_row=list() # Loop to go through all rows and see which contain NA cells
for (i in 1:nrow(pls_ames)){
  n_na_row[i]=sum(is.na(pls_ames[i,])) 
}
remove_row=which(n_na_row > 0) # Identify rows for removal

remove_row

pls_ames=pls_ames[-remove_row,] # Remove those rows

dim(pls_ames)
# [1] 2823   53


######################################
####~~Create Training/Test Data~~~####
######################################


# split predictors/result into 2 dataframes
# Y tables = price; X tables = everything else 

pls_y <- pls_ames[,53] 
pls_x <- pls_ames[,-53] 


View(sapply(pls_x, class)) # Make sure the columns are factor, not character


smpl_size <- floor(0.8 * nrow(pls_ames)) # Sample 80% of data
smpl <- sample(seq_len(nrow(pls_ames)), size = smpl_size)


pls_x_train <- data.frame(pls_x[smpl,]) # Create 80-20 Split in training/test
pls_x_test <- data.frame(pls_x[-smpl,])

pls_y_train <- unlist(pls_ames[smpl,53]) # Create 80-20 Split in training/test
pls_y_test <- unlist(pls_ames[-smpl,53])



######################################
####~~~~~Partial Least Square~~~~~####
######################################


options(scipen=999) # Remove scientific notation formatting


plsfit = plsr(pls_y_train ~ ., data = pls_x_train, scale = FALSE)

plspred = predict(plsfit, pls_x_test, ncomp = 2) # With 2 components

plsvalue = data.frame(obs = pls_y_test, pred = plspred[,,1])

test1 <- defaultSummary(plsvalue)
test1

#          RMSE      Rsquared            MAE 
#  73835.587493      0.136887   52835.241478 



plsfit2 = plsr(pls_y_train ~ ., data = pls_x_train, scale = FALSE) # Tuning model

plspred2 = predict(plsfit2, pls_x_test, ncomp = 5) # With 5 components

plsvalue2 = data.frame(obs = pls_y_test, pred = plspred2[,,1])

test2 <- defaultSummary(plsvalue2)
test2

#          RMSE      Rsquared             MAE 
# 39534.5116661     0.7643176   27145.3057457   



plsfit3 = plsr(pls_y_train ~ ., data = pls_x_train, scale = FALSE) # Tuning Model

plspred3 = predict(plsfit3, pls_x_test, ncomp = 10) # With 10 components

plsvalue3 = data.frame(obs = pls_y_test, pred = plspred3[,,1])

test3 <- defaultSummary(plsvalue3)
test3

#          RMSE      Rsquared             MAE 
# 35612.9381690     0.8076289   22827.2995377 



plsfit4 = plsr(pls_y_train ~ ., data = pls_x_train, scale = FALSE) # Tuning Model

plspred4 = predict(plsfit4, pls_x_test, ncomp = 16) # With 16 components

plsvalue4 = data.frame(obs = pls_y_test, pred = plspred4[,,1])

test4 <- defaultSummary(plsvalue4)
test4

#          RMSE      Rsquared           MAE 
# 35594.5945144     0.8054186 22950.1274463 



######################################
####~~~~~~~~~Plot PLS Line~~~~~~~~####
######################################

ctrl <- trainControl(method = "cv", number = 10) # Create cross validation control


pls_plot <- train(x = pls_x_train, y = pls_y_train, method = "pls", tuneGrid = expand.grid(ncomp = 1:18), trControl = ctrl)


FPplsResamples <- pls_plot$results
FPplsResamples$Model <- "PLS"

xyplot(RMSE ~ ncomp, # Plot PLS component results on a chart
       data = FPplsResamples,
       aspect = 1.2,
       main="PLS",
       xlab = "# Components",
       ylab = "RMSE (Cross-Validation)",       
       col = c("blue ","red"),
       groups = Model,
       type = c("o", "g"))


######################################
####~~~~Make Table of Results~~~~~####
######################################


plsall <- data.frame(rbind(test1, test2, test3, test4)) # Create dataframe of all results
plsall$'Num Components' <- c('2','5','10', '16')
plsall <- plsall[,c(4,1,2,3)]

View(plsall)

dev.off() # clear R plots
par(mar=c(0,0,0,0)) # reset margins
grid.table(plsall) # Make graphic of chart


plsimp <- data.frame(varImp(plsfit4)) # Identify variable importance
View(plsimp)
# Most important variables: Overall.Qual, Bedroom.AbvG, Fireplace 





#####################################
# Multivariate Adaptive Regression Splines (MARS) Model  
#####################################
#
# Read data file; donít bring the character strings in as factors:
AmesImp <- read.csv("~/Downloads/AmesImp-3.csv", stringsAsFactors=FALSE)

View(AmesImp)
# We imported the data file with strings not treated as factors to make it
# easy to address the NA values and missing data in categorical predictors.

# Install necessary packages
# install.packages(ìearthî)
# install.packages(ìcaretî)
# Address NA values ñ missing data and ìoriginal NAî values
# Create a copy of the input data for us to update
AmesImpTrim <- AmesImp
# First, find out which columns have NA values in them
#
NA_col <- list()
for (i in 1:82){
  NA_col[i] <- sum(is.na(AmesImpTrim[,i]))
}
NA_col
#
# Columns 8, 32, 33, 34, 35, 37, 59, 60, 62, 65, 66, 74, 75, and 76 all 
# have significant numbers of missing values, but they are character 
# strings that we can just change to ìNoneî
#
NA_col=list()
for(x in c(8, 32, 33, 34, 35, 37, 59, 60, 62, 65, 66, 74, 75, 76)){
  AmesImpTrim [is.na(AmesImpTrim[,x]),x]="None"
}
NA_col
head(AmesImpTrim)
# Column 5 (Lot.Frontage) has 490 missing numerical values we cannot 
# confidently impute; remove the column rather than sacrifice so many rows
# of data or risk skewing modeling results by imputing data on an
# arbitrary basis
#
AmesImpTrim <- subset (AmesImpTrim, select=-Lot.Frontage) 
#
# Since we imported the data without treating the character columns as 
# factors, convert them to factors now
character_vars <- lapply(AmesImpTrim, class) == "character"
AmesImpTrim[, character_vars] <- lapply(AmesImpTrim[, character_vars], as.factor)
#
# Now weíll see how many NA values are left (and in which columns)
#
NA_Trim_col <- list()
for (i in 1:81){
  NA_Trim_col[i]=sum(is.na(AmesImpTrim[,i]))
}
NA_Trim_col
#
# Column 27 (Mas.Vnr.Type) still has missing data, but weíll just remove the 
# affected rows to get rid of the problems those values present in modeling. 
# We also will remove the row with Order = 445, to address missing value of
# BsmtFin.Type.2 that cannot be imputed
#
dim(AmesImpTrim)
NA_row <- list()
for (i in 1:2930){
  NA_row[i] <- sum(is.na(AmesImpTrim[i,]))
}
NA_rowY <- which(NA_row > 0)
NA_rowY
AmesImpTrim <- AmesImpTrim[-NA_rowY,]
AmesImpTrim <- AmesImpTrim[AmesImpTrim$Order!=445,]

# Double check to see that the NAs have been removed from the rows:
NA_row <- list()
for (i in 1:2905){
  NA_row[i] <- sum(is.na(AmesImpTrim[i,]))
}
NA_rowY <- which(NA_row > 0)
NA_rowY

# Divide data into training and test sets
Trim70 <- floor(0.7*nrow(AmesImpTrim))
set.seed(2017)
trIndxTrim <- sample(seq_len(nrow(AmesImpTrim)), size = Trim70)
TrnTrim <- AmesImpTrim[trIndxTrim, ]
TestTrim <- AmesImpTrim[-trIndxTrim, ]
TrnTrimY <- TrnTrim$SalePrice
TrnTrimPred <- subset(TrnTrim, select = -SalePrice)
TestTrimPred <- subset(TestTrim, select = -SalePrice)


# Generate MARS model
marsGrid <- expand.grid(.degree=1:2, .nprune=2:80)
#
# Run without preprocessing
marsTuned <- train(TrnTrimPred, TrnTrimY, method="earth", tuneGrid=marsGrid, 
                   trControl=trainControl(method="cv"))
#
# Now wait awhile as the laptop crunches all those numbers...
# Run again with center/scale preprocessing 
marsTunedCS <- train(TrnTrimPred, TrnTrimY, method="earth",  
                     preProc=c("center", "scale"), tuneGrid=marsGrid,  
                     trControl=trainControl(method="cv"))

# Compare predicted Sale Price from running model on test set predictors 
# with actual Sale Price for test set records
mars_yHat <- predict(marsTuned, newdata = TestTrimPred)
mars_Perf <- postResample(pred=mars_yHat, obs=TestTrim$SalePrice)
mars_Perf

# Generate results data to support briefing and paper
marsTuned
varImp(marsTuned)
marsTunedCS
varImp(marsTunedCS)


#####################################
#  Tuned Single Tree Model  
#####################################


# Divide data into training and test sets
s70 <- floor(0.7*nrow(AmesImp))
set.seed(2017)
trIndx <- sample(seq_len(nrow(AmesImp)), size = s70)
TreeTrn <- AmesImp[trIndx, ]
TreeTest <- AmesImp[-trIndx, ]
TreeTrnY <- TreeTrn$SalePrice
TreeTrnPred <- subset(TreeTrn, select = -SalePrice)

# Install necessary packages
# install.packages(ìcaretî)
# install.packages("partykit")
# install.packages(ìrpart.plotî)


# Generate tuned tree model
# Tune for maxdepth
TreeTune <- train(TreeTrnPred, TreeTrnY, method = "rpart2", tuneLength =20,  
                  trControl = trainControl(method = "cv"))
TreeTune
# Tune for complexity parameter
TreeTune1 <- train(TreeTrnPred, TreeTrnY, method = "rpart", tuneLength = 50, trControl = trainControl(method = "cv"))
TreeTune1
# Run with both tuning parameters
TreeDF <- data.frame(x=TreeTrnPred, y=TreeTrnY)
rTreeTuned = rpart(y ~ ., data=TreeDF, method="anova", 
                   control=rpart.control(cp=0.0008581731, maxdepth = 11))

# Compare predicted Sale Price from running model on test set predictors 
# with actual Sale Price for test set records
TreeTestPred <- subset(TreeTest, select = -SalePrice)
rTTune_yHat <- predict(rTreeTuned, newdata = data.frame(x=TreeTestPred))
rTTunePerf <- postResample(pred=rTTune_yHat, obs=TreeTest$SalePrice)
rTTunePerf

# Generate results data to support briefing and paper
plot(rTreeTuned)
rTreeTuned
summary(rTreeTuned)


#####################################
#  Random Forest Model Code in R
#####################################

# Read data file:
AmesImp <- read.csv("~/Downloads/AmesImp-3.csv", stringsAsFactors=FALSE)
# We imported the data file with strings not treated as factors to make it
# easy to address the NA values and missing data in categorical predictors.
#
# Install necessary packages
# install.packages(ìrandomForestî)
# install.packages(ìcaretî)
# Address NA values ñ missing data and ìoriginal NAî values
# Create a copy of the input data for us to update
AmesImpTrim <- AmesImp
# First, find out which columns have NA values in them
#
NA_col <- list()
for (i in 1:82){
  NA_col[i] <- sum(is.na(AmesImpTrim[,i]))
}
NA_col
#
# Columns 8, 32, 33, 34, 35, 37, 59, 60, 62, 65, 66, 74, 75, and 76 all 
# have significant numbers of missing values, but they are character 
# strings that we can just change to ìNoneî
#
NA_col=list()
for(x in c(8, 32, 33, 34, 35, 37, 59, 60, 62, 65, 66, 74, 75, 76)){
  AmesImpTrim [is.na(AmesImpTrim[,x]),x]="None"
}
NA_col
head(AmesImpTrim)
# Column 5 (Lot.Frontage) has 490 missing numerical values we cannot 
# confidently impute; remove the column rather than sacrifice so many rows
# of data or risk skewing modeling results by imputing data on an
# arbitrary basis
#
AmesImpTrim <- subset (AmesImpTrim, select=-Lot.Frontage) 
#
# Since we imported the data without treating the character columns as 
# factors, convert them to factors now
character_vars <- lapply(AmesImpTrim, class) == "character"
AmesImpTrim[, character_vars] <- lapply(AmesImpTrim[, character_vars], as.factor)
#
# Now weíll see how many NA values are left (and in which columns)
#
NA_Trim_col <- list()
for (i in 1:81){
  NA_Trim_col[i]=sum(is.na(AmesImpTrim[,i]))
}
NA_Trim_col
#
# Column 27 still has missing data, but weíll just remove the affected rows
# to get rid of the problems those values present in modeling.  We also will 
# remove the row with Order = 445, to address missing value of BsmtFin.Type.2
# that cannot be imputed
#
dim(AmesImpTrim)
NA_row <- list()
for (i in 1:2930){
  NA_row[i] <- sum(is.na(AmesImpTrim[i,]))
}
NA_rowY <- which(NA_row > 0)
NA_rowY
AmesImpTrim <- AmesImpTrim[-NA_rowY,]
AmesImpTrim <- AmesImpTrim[AmesImpTrim$Order!=445,]

# Double check to see that the NAs have been removed from the rows:
NA_row <- list()
for (i in 1:2905){
  NA_row[i] <- sum(is.na(AmesImpTrim[i,]))
}
NA_rowY <- which(NA_row > 0)
NA_rowY

# Divide data into training and test sets
Trim70 <- floor(0.7*nrow(AmesImpTrim))
set.seed(2017)
trIndxTrim <- sample(seq_len(nrow(AmesImpTrim)), size = Trim70)
TrnTrim <- AmesImpTrim[trIndxTrim, ]
TestTrim <- AmesImpTrim[-trIndxTrim, ]
TrnTrimY <- TrnTrim$SalePrice
TrnTrimPred <- subset(TrnTrim, select = -SalePrice)
TestTrimPred <- subset(TestTrim, select = -SalePrice)


# Generate random forest model
set.seed(2017)
RF_Ames <- randomForest(TrnTrimPred, TrnTrimY, importance=TRUE, ntree=1500)

# Now wait awhile as the laptop crunches all those numbers...

# Compare predicted Sale Price from running model on test set predictors 
# with actual Sale Price for test set records
RF_yHat <- predict(RF_Ames, newdata = TestTrimPred)
RF_Perf <- postResample(pred=RF_yHat, obs=TestTrim$SalePrice)
RF_Perf

# Generate results data to support briefing and paper
importance(RF_Ames)
varImpPlot(RF_Ames)





#####################################
##Model: Support Vector Machine    ##
#####################################


###Read the data####################################################

ames_data=read.csv("~/Downloads/AmesImp-3.csv", na.strings=c("", "NA"), header=TRUE, sep=",")

###Define variables##################################################
CategoricalVar=c("MS.SubClass","MS.Zoning", "Street", "Lot.Shape", "Land.Contour", 
                 "Utilities", "Lot.Config", "Land.Slope", "Neighborhood", "Condition.1",
                 "Condition.2", "Bldg.Type", "House.Style","Overall.Qual","Overall.Cond",
                 "Roof.Style","Roof.Matl","Exterior.1st","Exterior.2nd","Mas.Vnr.Type",
                 "Exter.Qual","Exter.Cond","Foundation","Bsmt.Qual","Bsmt.Cond",
                 "Bsmt.Exposure","BsmtFin.Type.1", "BsmtFin.Type.2","Heating","Heating.QC", 
                 "Central.Air","Electrical", "Kitchen.Qual", "Functional","Paved.Drive",
                 "Sale.Type","Sale.Condition") #37 in total, after remove missing columns.

ContinuousVar=c("Lot.Area","Mas.Vnr.Area","BsmtFin.SF.1","BsmtFin.SF.2","Bsmt.Unf.SF",
                "Total.Bsmt.SF","X1st.Flr.SF","X2nd.Flr.SF","Low.Qual.Fin.SF","Gr.Liv.Area",
                "Garage.Area","Wood.Deck.SF","Open.Porch.SF","Enclosed.Porch","X3Ssn.Porch",
                "Screen.Porch","Pool.Area","Misc.Val","SalePrice") #19 in total
DiscreteVar=c("Year.Built","Year.Remod.Add","Bsmt.Full.Bath","Bsmt.Half.Bath","Full.Bath",
              "Half.Bath","Bedroom.AbvGr","Kitchen.AbvGr","TotRms.AbvGrd","Fireplaces",
              "Garage.Yr.Blt","Garage.Cars","Mo.Sold","Yr.Sold") #14 in total

###Deal with missing data##############################################
number_na_col=list()
for (i in 1:82){
  number_na_col[i]=sum(is.na(ames_data[,i]))
}
number_na_col  
##from the result above, the following columns contains more than 150 missing values (col 5, 60, 62, 65, 66), 
##some of them contain more than 1400 missing values (col 8, 59, 74, 75, 76). 
##So removing these variables would be approperiate: col 5, 8, 59, 60, 62, 65, 66, 74, 75, 76
##PID is col2

ames_data=ames_data[,-c(2, 5, 8, 59, 60, 62, 65, 66, 74, 75, 76)]

number_na_col=list()
for (i in 1:72){
  number_na_col[i]=sum(is.na(ames_data[,i]))
}
number_na_col
##Now, we have 72 variables in total. From the result of the total number of NA in each column, 
##we get col 25, 26, 30, 31, 32, 33, 35, 57, 58, 59, have less than 100 NAs.
##It would not be approperiate if we remove these columns, so we could remove the observations for the above variables. 
dim(ames_data)
number_na_row=list()
for (i in 1:2930){
  number_na_row[i]=sum(is.na(ames_data[i,]))
}
remove_row=which(number_na_row > 0)
ames_data=ames_data[-remove_row,]
##Test if the current data contains any missing values
number_na_col=numeric()
for (i in 1:71){
  number_na_col[i]=sum(is.na(ames_data[,i]))
}
sum(number_na_col) #sum is zero means no missing values in current data set. 


###Change categorical variable into factor#############################################################

ames_data$MS.SubClass=as.factor(ames_data$MS.SubClass) #treat categorical var as a factor
ames_data$Mo.Sold=as.factor(ames_data$Mo.Sold)

names=c("MS.SubClass","MS.Zoning", "Street", "Lot.Shape", "Land.Contour", 
        "Utilities", "Lot.Config", "Land.Slope", "Neighborhood", "Condition.1",
        "Condition.2", "Bldg.Type", "House.Style","Overall.Qual","Overall.Cond",
        "Roof.Style","Roof.Matl", "Exterior.1st","Exterior.2nd","Mas.Vnr.Type",
        "Exter.Qual","Exter.Cond","Foundation","Bsmt.Qual","Bsmt.Cond","Bsmt.Exposure",
        "BsmtFin.Type.1", "BsmtFin.Type.2","Heating","Heating.QC", "Central.Air",
        "Electrical", "Kitchen.Qual", "Functional","Paved.Drive","Sale.Type",
        "Sale.Condition")
ames_data[,names]=lapply(ames_data[,names] , factor)

###Full Model: Support Vector Machine72Variables########################################################

###Data Partition######################
samplesize=floor(0.75*nrow(ames_data)) # 75% of the sample size

set.seed(2017)
train_ames=sample(seq_len(nrow(ames_data)), size=samplesize)

ames_training1=ames_data[train_ames, ]
ames_testing1=ames_data[-train_ames, ]

ames_x_training1=ames_training1[,-72]
ames_saleprice_training1=ames_training1$SalePrice

dim(ames_testing1)
dim(ames_training1)
# perform a grid search
tuneResult=tune(svm, SalePrice~., data=ames_training1, 
                ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
print(tuneResult)
# Draw the tuning graph
plot(tuneResult) ###From the plot, we can see the darker the performance of svm is better, so we choose epsilon<0.18, cost<28
tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, ames_testing1) 
tunedModelRMSE <- rmse(ames_testing1$SalePrice, tunedModelY)  
R2 <- 1 - (sum((ames_testing1$SalePrice-tunedModelY )^2)/sum((ames_testing1$SalePrice-mean(ames_testing1$SalePrice))^2))
tunedModelMAE=MAE(ames_testing1$SalePrice, tunedModelY)

###The second model: SVM near zero variance################
nearZeroVar(ames_data)
##original col number: 7 10 11 13 16 24 33 37 38 41 47 54 57 65 66 69 70 71 72 73 77
ames_data_near=ames_data[,-c(nearZeroVar(ames_data))]

samplesize=floor(0.75*nrow(ames_data_near)) # 75% of the sample size

set.seed(2017)
train_ames_near=sample(seq_len(nrow(ames_data_near)), size=samplesize)

ames_training_near=ames_data[train_ames_near, ]
ames_testing_near=ames_data[-train_ames_near, ]

ames_x_training_near=ames_training_near[,-52]  #for ames_data, -72, for ames_data2, -42
ames_saleprice_training_near=ames_training_near$SalePrice

tuneResult_near=tune(svm, SalePrice~., data=ames_training_near, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
print(tuneResult_near)
# Draw the tuning graph
plot(tuneResult_near) 
tunedModel_near = tuneResult_near$best.model
tunedModelY_near = predict(tunedModel_near, ames_testing_near) 
tunedModelRMSE_near = rmse(ames_testing_near$SalePrice, tunedModelY_near)
tunedModelRSquared_near= 1 - (sum((ames_testing_near$SalePrice-tunedModelY_near )^2)/sum((ames_testing_near$SalePrice-mean(ames_testing_near$SalePrice))^2))
tunedModelMAE_near=MAE(ames_testing_near$SalePrice, tunedModelY_near)

###The third model: Re-run the svm based on the backward variable selection ##################################################

###Linear Regression Model
LinearReg1=lm(SalePrice~., data=ames_data) #Full Model
#Y=predict(LinearReg1, ames_testing1)
#rmse(ames_testing1$SalePrice, Y)
###Use likelihood ratio test to find the significance of each variable

##List of categorical variables that are not significant

LinearReg2_MS_SubClass=lm(SalePrice~.-Utilities , data=ames_data) #p=0.5602
#LinearReg2_MS_SubClass=lm(SalePrice~.-MS.SubClass , data=ames_data)#p=0.2404
#LinearReg2_MS_SubClass=lm(SalePrice~.-Land.Contour , data=ames_data)#p=0.05657
#LinearReg2_MS_SubClass=lm(SalePrice~.-Bldg.Type , data=ames_data)#p=0.1001
#LinearReg2_MS_SubClass=lm(SalePrice~.-House.Style , data=ames_data)#p=0.2914
#LinearReg2_MS_SubClass=lm(SalePrice~.-Roof.Style , data=ames_data)#p=0.07034
#LinearReg2_MS_SubClass=lm(SalePrice~.-Exterior.2nd , data=ames_data)#p=0.2814
#LinearReg2_MS_SubClass=lm(SalePrice~.-Exter.Qual , data=ames_data)#p=0.3254
#LinearReg2_MS_SubClass=lm(SalePrice~.-Exter.Cond , data=ames_data)#p=0.4174
#LinearReg2_MS_SubClass=lm(SalePrice~.-Foundation , data=ames_data)#p=0.06482
#LinearReg2_MS_SubClass=lm(SalePrice~.-Bsmt.Cond , data=ames_data)#p=0.9848
#LinearReg2_MS_SubClass=lm(SalePrice~.-Heating , data=ames_data)#0.1308
#LinearReg2_MS_SubClass=lm(SalePrice~.-Heating.QC , data=ames_data)#0.07306
#LinearReg2_MS_SubClass=lm(SalePrice~.-Central.Air , data=ames_data)#p=0.5109
#LinearReg2_MS_SubClass=lm(SalePrice~.-Electrical , data=ames_data)#p=0.4961
#LinearReg2_MS_SubClass=lm(SalePrice~.-Paved.Drive , data=ames_data)#p=0.5066
#LinearReg2_MS_SubClass=lm(SalePrice~.-Sale.Type , data=ames_data)#p=0.2623

anova(LinearReg2_MS_SubClass,LinearReg1,test="LRT")

##List of continuous variables that are not significant

LinearReg2_MS_SubClass=lm(SalePrice~.-Garage.Area , data=ames_data)#p=0.1223
#LinearReg2_MS_SubClass=lm(SalePrice~.-Wood.Deck.SF , data=ames_data)#p=0.1184
#LinearReg2_MS_SubClass=lm(SalePrice~.-Open.Porch.SF , data=ames_data)#p=0.665
#LinearReg2_MS_SubClass=lm(SalePrice~.-Enclosed.Porch , data=ames_data)#0.3766
#LinearReg2_MS_SubClass=lm(SalePrice~.-X3Ssn.Porch , data=ames_data)#0.3585

anova(LinearReg2_MS_SubClass,LinearReg1,test="LRT")

##List of discrete variables that are not significant

LinearReg2_MS_SubClass=lm(SalePrice~.-Year.Remod.Add , data=ames_data)#p=0.08623
#LinearReg2_MS_SubClass=lm(SalePrice~.-Bsmt.Full.Bath , data=ames_data) #p=0.1277
#LinearReg2_MS_SubClass=lm(SalePrice~.-Bsmt.Half.Bath , data=ames_data)#p=0.07265
#LinearReg2_MS_SubClass=lm(SalePrice~.-Bedroom.AbvGr , data=ames_data)#p=0.05943
#LinearReg2_MS_SubClass=lm(SalePrice~.-TotRms.AbvGrd , data=ames_data)#p=0.1073
#LinearReg2_MS_SubClass=lm(SalePrice~.-Mo.Sold , data=ames_data)#p=0.1139
#LinearReg2_MS_SubClass=lm(SalePrice~.-Yr.Sold , data=ames_data)#p=0.4681

anova(LinearReg2_MS_SubClass,LinearReg1,test="LRT")
summary(LinearReg1)

###Get the column number given a column name
#which(colnames(ames_data)=="Utilities" )
which(names(ames_data)%in%c("MS.SubClass","Utilities","Land.Contour","Bldg.Type","House.Style",
                            "Roof.Style","Exterior.2nd","Exter.Qual","Exter.Cond","Foundation",
                            "Bsmt.Cond","Heating","Heating.QC","Central.Air","Electrical",
                            "Paved.Drive","Sale.Type","Garage.Area","Wood.Deck.SF","Open.Porch.SF",
                            "Enclosed.Porch","X3Ssn.Porch","Year.Remod.Add","Bsmt.Full.Bath","Bsmt.Half.Bath",
                            "Bedroom.AbvGr","TotRms.AbvGrd","Mo.Sold","Yr.Sold"))
###Remove the columns based on the likelihood ratio test result
ames_data2=ames_data[,-c(2, 7, 8, 14, 15, 19, 20, 23, 26, 27, 28, 30, 38, 39, 40, 41,
                         46, 47, 50, 53, 58, 59, 60, 61, 62, 63, 67, 68, 69)]
dim(ames_data2)
colnames(ames_data2)[c(4,7,10,14,22,23,28,32,34,38,39,40)]

###Create dummy variables
#ames_data2_dummy<- dummy.data.frame(ames_data2, sep = " ")
#names(ames_data2_dummy)

###Splitting data into training dataset and testing dataset 
samplesize=floor(0.75*nrow(ames_data2)) # 75% of the sample size

set.seed(2017)
train_ames=sample(seq_len(nrow(ames_data2)), size=samplesize)

ames_training3=ames_data2[train_ames, ]
ames_testing3=ames_data2[-train_ames, ]

ames_x_training3=ames_training3[,-42]  #for ames_data, -72, for ames_data2, -42
ames_saleprice_training3=ames_training3$SalePrice
#########################SVM based on backward variable selection (ames_data2)###
tuneResult=tune(svm, SalePrice~., data=ames_training3, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
print(tuneResult)
# Draw the tuning graph
plot(tuneResult) 
tunedModel3 = tuneResult$best.model
tunedModelY3 = predict(tunedModel3, ames_testing3) 
tunedModelRMSE3 = rmse(ames_testing3$SalePrice, tunedModelY3)
tunedModelRSquared3= 1 - (sum((ames_testing3$SalePrice-tunedModelY3 )^2)/sum((ames_testing3$SalePrice-mean(ames_testing3$SalePrice))^2))
tunedModelMAE3=MAE(ames_testing3$SalePrice, tunedModelY3)

###The final model: SVM BASED ON AMES_DATA2_NEAR (Use nearzerovar)###############################################
dim(ames_data2)
ames_data2_near=ames_data2[,-c(nearZeroVar(ames_data2))]
dim(ames_data2_near)
samplesize=floor(0.75*nrow(ames_data2_near)) # 75% of the sample size
set.seed(2017)
train_ames2_near=sample(seq_len(nrow(ames_data2_near)), size=samplesize)

ames_training2_near=ames_data2_near[train_ames2_near, ]
ames_testing2_near=ames_data2_near[-train_ames2_near, ]

ames_x_training2_near=ames_training2_near[,-30]  #for ames_data, -72, for ames_data2, -42
ames_saleprice_training2_near=ames_training2_near$SalePrice

tuneResult2_near=tune(svm, SalePrice~., data=ames_training2_near, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
print(tuneResult2_near)
# Draw the tuning graph
plot(tuneResult2_near) 
tunedModel2_near = tuneResult2_near$best.model
tunedModelY2_near = predict(tunedModel2_near, ames_testing2_near) 
tunedModelRMSE2_near = rmse(ames_testing3$SalePrice, tunedModelY2_near)
tunedModelRSquared2_near= 1 - (sum((ames_testing2_near$SalePrice-tunedModelY2_near)^2)/sum((ames_testing2_near$SalePrice-mean(ames_testing2_near$SalePrice))^2))
tunedModelMAE2_near=MAE(ames_testing3$SalePrice, tunedModelY2_near)


#################################################################
### KNN Classisfication
#################################################################
##read the data
amesData <- read.csv("~/Downloads/AmesImp-3.csv")

## checking the distribution of sales price in different neighborhoods
amesData %>%
  ggplot(aes(x=Neighborhood, y= SalePrice, color= Land.Slope)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


str(amesData)#structure of the data

###DATA SLICING
intrain <- createDataPartition(y = amesData$Land.Slope, p= 0.7, list = FALSE) #creates a matrix called "intrain" with training set values
training <- amesData[intrain,] #creates training data-frame
testing <- amesData[-intrain,] #creates test data-frame

dim(training) # checking the dimensions of the training dataframe
dim(testing) #checking the dimensions of the test dataframe

###PREPROCESSING and TRAINING
anyNA(amesData) # checking for missing values in the dataframe
summary(amesData) # gives a summarized view of each ATTRIBUTE 
typeof(training$Land.Slope) 
training[["Land.Slope"]] = factor(training[["Land.Slope"]]) 

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)

##fitting the KNN model
knn_fit <- train(Land.Slope ~SalePrice, data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit
plot(knn_fit)

### TEST SET PREDICTION
test_pred <- predict(knn_fit, newdata = testing)
test_pred

barchart(test_pred)

### checking the accuracy of the predictions---here,by confusion matrix
confusionMatrix(test_pred, testing$Land.Slope)








### Rei Esaka
### Kaggle Assignment
### October 14, 2021

### Referenced Dr. Jorge Colazo's code (Trinity University)


### Data Cleaning time
  
    # setting the seed to ensure that I get the same result when testing models
      set.seed(123) # just used the same value as the one in Colazo's code
      
    # load raw data
      library(readxl)
      
      # the warning messages I'm getting from reading the data already tell me there's plenty of NA values
      raw_data <- read_xlsx('HousePricesCompleteData.xlsx')
      
    # Let's see if there are any duplicates
      library(dplyr)
      count(raw_data[duplicated(raw_data) == TRUE, ]) #great, no duplicates
  
    # glimpse into the dataset shows that we have plenty of NA values to deal with
      glimpse(raw_data)
    

### Dealing with NA values
  
     # To see which columns contain NA values
       sapply(raw_data[,1:81], function(x) sum(is.na(x)))

  
      # MSZoning 
      raw_data$MSZoning[raw_data$MSZoning == "NA"] <- names(which.max(table(raw_data$MSZoning))) 
      raw_data$MSZoning <- as.factor(raw_data$MSZoning)
      
      # LotFrontage
      raw_data$LotFrontage[raw_data$LotFrontage == "NA"] <- 0
      raw_data$LotFrontage <- as.numeric(raw_data$LotFrontage)
      
      # For Alley
        raw_data$Alley[raw_data$Alley == "NA"] <- "None"
        raw_data$Alley <- as.factor(raw_data$Alley)
       
      # Utilities
        library(dplyr)
         raw_data <- raw_data %>%
           dplyr::select(-Utilities) 
          
      # Exterior1st
        raw_data$Exterior1st[raw_data$Exterior1st == "NA"] <- names(which.max(table(raw_data$Exterior1st)))

      # Exterior2nd
        summary(raw_data$Exterior2nd)
        raw_data$Exterior2nd[raw_data$Exterior2nd == "NA"] <- names(which.max(table(raw_data$Exterior2nd)))

      # For MasVnrArea
        raw_data$MasVnrArea[raw_data$MasVnrArea == "NA"] <- 0
        raw_data$MasVnrArea <- as.numeric(raw_data$MasVnrArea)
        
      # For BsmtQual
        raw_data$BsmtQual[raw_data$BsmtQual == "NA"] <- "None"
        raw_data$BsmtQual <- as.factor(raw_data$BsmtQual)
        
      # BsmtCond
        raw_data$BsmtCond[raw_data$BsmtCond == "NA"] <- "None"
        raw_data$BsmtCond <- as.factor(raw_data$BsmtCond)
        
      # BsmtExposure
        raw_data$BsmtExposure[raw_data$BsmtExposure == "NA"] <- "None"
        raw_data$BsmtExposure <- as.factor(raw_data$BsmtExposure)
        
      # BsmtFinType1 
        raw_data$BsmtFinType1[raw_data$BsmtFinType1 == "NA"] <- "None"
        raw_data$BsmtFinType1 <- as.factor(raw_data$BsmtFinType1)
        
      # BsmtFinSF1
        raw_data$BsmtFinSF1[is.na(raw_data$BsmtFinSF1)] <- 0
      
      # Same thing for BsmtFinType2 and BsmtFinSF2
        raw_data$BsmtFinType2[raw_data$BsmtFinType2 == "NA"] <- "None"
        raw_data$BsmtFinType2 <- as.factor(raw_data$BsmtFinType2)
        
        raw_data$BsmtFinSF2[is.na(raw_data$BsmtFinSF2)] <- 0
        
      # BsmtUnfSF 
        raw_data$BsmtUnfSF[is.na(raw_data$BsmtUnfSF)] <- 0
        
      # TotalBsmtSF
        raw_data$TotalBsmtSF[is.na(raw_data$TotalBsmtSF)] <- 0
      
      # Electrical
        raw_data$Electrical[is.na(raw_data$Electrical)] <- names(which.max(table(raw_data$Electrical)))
        
      # BsmtFullBath
        raw_data$BsmtFullBath[is.na(raw_data$BsmtFullBath)] <- 0
        
      # BsmtHalfBath
        raw_data$BsmtHalfBath[is.na(raw_data$BsmtHalfBath)] <- 0
      
      # KitchenQual
        raw_data$KitchenQual[raw_data$KitchenQual == "NA"] <- names(which.max(table(raw_data$KitchenQual)))

      # Functional
        raw_data$Functional[raw_data$Functional == "NA"] <- "Typ"
       
      # FireplaceQu
        raw_data$FireplaceQu[raw_data$FireplaceQu == "NA"] <- "None"
        raw_data$FireplaceQu <- as.factor(raw_data$FireplaceQu)
      
      # GarageType
        raw_data$GarageType[raw_data$GarageType == "NA"] <- "None"
        raw_data$GarageType <- as.factor(raw_data$GarageType)
        
      # GarageYrBuilt
        raw_data$GarageYrBlt[raw_data$GarageYrBlt == "NA"] <- 1978 #mean
        raw_data$GarageYrBlt[raw_data$GarageYrBlt == "2207"] <- 2007
        raw_data$GarageYrBlt <- as.numeric(raw_data$GarageYrBlt)
                            
      # GarageFinish
        raw_data$GarageFinish[raw_data$GarageFinish == "NA"] <- "None"
        raw_data$GarageFinish <- as.factor(raw_data$GarageFinish)
        
      # GarageCars
        raw_data$GarageCars[is.na(raw_data$GarageCars)] <- 0

      # GarageArea
        raw_data$GarageArea[is.na(raw_data$GarageArea)] <- 0
 
      # GarageQual
        raw_data$GarageQual[raw_data$GarageQual == "NA"] <- "None"
        raw_data$GarageQual <- as.factor(raw_data$GarageQual)
        
      # GarageCond
        raw_data$GarageCond[raw_data$GarageCond == "NA"] <- "None"
        raw_data$GarageCond <- as.factor(raw_data$GarageCond)
        
      # PoolQC
        raw_data$PoolQC[raw_data$PoolQC == "NA"] <- "None"
        raw_data$PoolQC <- as.factor(raw_data$PoolQC)
        
      # Fence
        raw_data$Fence[raw_data$Fence == "NA"] <- "None"
        raw_data$Fence <- as.factor(raw_data$Fence)
        
      # MiscFeature 
        raw_data$MiscFeature[raw_data$MiscFeature=="NA"] <- "None"
        raw_data$MiscFeature <- as.factor(raw_data$MiscFeature)
      
      # SaleType
        raw_data$SaleType[raw_data$SaleType=="NA"] <- names(which.max(table(raw_data$SaleType)))
      
    # checking that there are no more NA values. Decreased the column range by 1 since I took out Utilities
      sapply(raw_data[,1:80], function(x) sum(is.na(x)))
        
    
      
### Creating ordinal variables
    raw_data$ExterQual <- recode(raw_data$ExterQual,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
    raw_data$ExterCond <- recode(raw_data$ExterCond,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
    raw_data$BsmtQual <- recode(raw_data$BsmtQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
    raw_data$BsmtCond <- recode(raw_data$BsmtCond,"None"=2,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
    raw_data$BsmtExposure <- recode(raw_data$BsmtExposure,"None"=0,"No"=1,"Mn"=2,"Av"=3,"Gd"=5)
    raw_data$BsmtFinType1 <- recode(raw_data$BsmtFinType1,"None"=0,"Unf"=0,"LwQ"=1,"Rec"=2,"BLQ"=3,"ALQ"=4,"GLQ"=5)
    raw_data$BsmtFinType2 <- recode(raw_data$BsmtFinType2,"None"=0,"Unf"=0,"LwQ"=1,"Rec"=2,"BLQ"=3,"ALQ"=4,"GLQ"=5)
    raw_data$HeatingQC <- recode(raw_data$HeatingQC,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
    raw_data$FireplaceQu <- recode(raw_data$FireplaceQu,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
    raw_data$GarageFinish <- recode(raw_data$GarageFinish,"None"=0,"Unf"=1,"RFn"=2,"Fin"=3)
    raw_data$GarageQual <- recode(raw_data$GarageQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
    raw_data$GarageCond <- recode(raw_data$GarageCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
    raw_data$PoolQC <- recode(raw_data$PoolQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
    raw_data$Fence <- recode(raw_data$Fence,"None"=1,"MnWw"=1,"GdWo"=2,"MnPrv"=3,"GdPrv"=4)   


# character to factor    
   raw_data$MSSubClass <- as.factor(raw_data$MSSubClass)
   raw_data$MoSold <- as.factor(raw_data$MoSold) #treat dates as categorical
   raw_data$YrSold <- as.factor(raw_data$YrSold) #treat dates as categorical
   raw_data$Street <- as.factor(raw_data$Street) 
   raw_data$LotShape <- as.factor(raw_data$LotShape) 
   raw_data$LandContour <- as.factor(raw_data$LandContour)
   raw_data$LotConfig <- as.factor(raw_data$LotConfig)
   raw_data$LandSlope <- as.factor(raw_data$LandSlope)
   raw_data$Neighborhood <- as.factor(raw_data$Neighborhood)
   raw_data$LotConfig <- as.factor(raw_data$LotConfig)
   raw_data$Condition1 <- as.factor(raw_data$Condition1)
   raw_data$Condition2 <- as.factor(raw_data$Condition2)
   raw_data$BldgType <- as.factor(raw_data$BldgType)
   raw_data$HouseStyle <- as.factor(raw_data$HouseStyle)
   raw_data$RoofStyle <- as.factor(raw_data$RoofStyle)
   raw_data$RoofMatl <- as.factor(raw_data$RoofMatl)
   raw_data$Exterior1st <- as.factor(raw_data$Exterior1st)
   raw_data$Exterior2nd <- as.factor(raw_data$Exterior2nd)
   raw_data$MasVnrType <- as.factor(raw_data$MasVnrType)
   raw_data$Foundation <- as.factor(raw_data$Foundation)
   raw_data$Heating <- as.factor(raw_data$Heating)
   raw_data$CentralAir <- as.factor(raw_data$CentralAir)
   raw_data$Electrical <- as.factor(raw_data$Electrical)
   raw_data$KitchenQual <- as.factor(raw_data$KitchenQual)
   raw_data$Functional <- as.factor(raw_data$Functional)
   raw_data$PavedDrive <- as.factor(raw_data$PavedDrive)
   raw_data$SaleType <- as.factor(raw_data$SaleType)
   raw_data$SaleCondition <- as.factor(raw_data$SaleCondition)
   
### Transformation
    
    library(ggplot2) #for plots such as histograms and regressions
    
    # We can see that the SalePrice is  skewed to the right, so the mean is greater than the median. 
    # This makes sense considering that some house prices can be much higher than others
    ggplot(raw_data, aes(SalePrice)) + geom_density()
    
    # We now have a normal distribution 
    ggplot(raw_data, aes(SalePrice)) + geom_density() +
      scale_x_log10(breaks = c(0, 25, 50, 75)) + labs(title = "SalePrice Distribution")
    
    # Apply transformation to data
    raw_data$SalePrice <- log(raw_data$SalePrice) # I need to remember to exp when applying my models to cancel out the log transf.
    
    
    
### Dealing with Outliers 
  # plotting numeric variables against SalePrice
    
      # LotFrontage
          # Looks like there's a few outliers
          plot(raw_data$SalePrice, raw_data$LotFrontage)
          # replace outliers with the mean
          raw_data$LotFrontage[raw_data$LotFrontage>200] <- mean(raw_data$LotFrontage) 
          # plot looks better
          plot(raw_data$SalePrice, raw_data$LotFrontage)
          
      # LotArea
          # Looks like there's a few outliers
          plot(raw_data$SalePrice, raw_data$LotArea)
          # replace outliers with the mean
          raw_data$LotArea[raw_data$LotArea>75000] <- mean(raw_data$LotArea)
          # plot looks better
          plot(raw_data$SalePrice, raw_data$LotArea)
          
      # MasVnrArea: in my opinion, there are no clear outliers in this case
          
      # BsmtFinSF1
          # Looks like there's a few outliers
          plot(raw_data$SalePrice, raw_data$BsmtFinSF1)
          # replace outliers with the mean
          raw_data$BsmtFinSF1[raw_data$BsmtFinSF1>3000] <- mean(raw_data$BsmtFinSF1)
          # plot looks better
          plot(raw_data$SalePrice, raw_data$BsmtFinSF1)          
          
      # BsmtFinSF2: I don't think there are any clear outliers in this case
          
      # BsmtUnfSF: No clear outliers here
  
      # TotalBsmtSF
          # Looks like there's a few outliers
          plot(raw_data$SalePrice, raw_data$TotalBsmtSF)
          # replace outliers with the mean
          raw_data$TotalBsmtSF[raw_data$TotalBsmtSF>4000] <- mean(raw_data$TotalBsmtSF)
          # plot looks better
          plot(raw_data$SalePrice, raw_data$TotalBsmtSF)
          
      # 1stFlrSF
          # to see that 1stFlrSF has a column index of 43
            colnames(raw_data) 
          # renaming the column name since it starts with a number
            colnames(raw_data)[43] <- "FirstFlrSF" 
          # Looks like there's a few outliers
            plot(raw_data$SalePrice, raw_data$FirstFlrSF)
          # replace outliers with the mean
            raw_data$FirstFlrSF[raw_data$FirstFlrSF>4000] <- mean(raw_data$FirstFlrSF)
          # plot looks better
            plot(raw_data$SalePrice, raw_data$FirstFlrSF)  
              
      # 2ndFlrSF no clear outliers
            # to see that 2ndFlrSF has a column index of 44
            colnames(raw_data) 
            # renaming the column name since it starts with a number
            colnames(raw_data)[44] <- "SecondFlrSF"  
          
      # LowQualFinSF no clear outliers
          
      # GrLivArea
         # Looks like there's a few outliers
           plot(raw_data$SalePrice, raw_data$GrLivArea)
         # replace outliers with the mean
           raw_data$GrLivArea[raw_data$GrLivArea>4500] <- mean(raw_data$GrLivArea)
         # plot looks better
           plot(raw_data$SalePrice, raw_data$GrLivArea)     
            
      # GarageArea no clear outliers
           
      # WoodDeckSF no clear outliers
          
      # OpenPorchSF no clear outliers
              
      # EnclosedPorch
           # Looks like there's a few outliers
           plot(raw_data$SalePrice, raw_data$EnclosedPorch)
           # replace outliers with the mean
           raw_data$EnclosedPorch[raw_data$EnclosedPorch>500] <- mean(raw_data$EnclosedPorch)
           # plot looks better
           plot(raw_data$SalePrice, raw_data$EnclosedPorch) 
          
      # 3SsnPorch no clear outliers
          # to see that 3SsnPorch has a column index of 70
          colnames(raw_data) 
          # renaming the column name since it starts with a number
          colnames(raw_data)[69] <- "ThreeSsnPorch" 
          
      # ScreenPorch no clear outliers
          
      # PoolArea no clear outliers
          
      # MiscVal no clear outliers
          
      
     

# Replacing values with similar ones while looking at the data description. To take care of the error: factor has new levels
  raw_data$MSSubClass[raw_data$MSSubClass == "150"] <- "160"  # 1.5 story pub is similar to 2 story pub       

        
### Modeling
         
  
      
   # splitting the data
     train <- raw_data[(!is.na(raw_data$SalePrice)),] # includes SalePrice
     test <- raw_data[is.na(raw_data$SalePrice),] # does NOT include SalePrice
     test$SalePrice <- 0  
     
      
### "Plain vanilla" OLS regression with all or the most features you can use 
     
  # When running the OLS_regression function below, I was initially getting an error that said "contrasts can be applied only to factors with 2 or more levels"
  # I included the str function below to see if any factors had less than 2 levels, and there were
  # With the following variables, I went back to fix my cleaning        
  # Alley (fixed)
  # GarageType (fixed)
  # Miscfeature (fixed)      
  
    str(raw_data)
          
    
     OLS_regression <- lm(SalePrice~.-Id, data = train) #excluding unnecessary id column
     summary(OLS_regression)
     
     fit_OLS <- as.numeric(exp(predict(OLS_regression, newdata=test)))  #exp to cancel out log transformation
     
     predict_OLS <- data.frame(ID = test$Id, SalePrice = fit_OLS) 
    
     write.csv(predict_OLS, file = "esakaRei_OLSregression_submission.csv", row.names = FALSE)
     
    # Best Kaggle score of 0.13608

      
      
### OLS Regression performing
      # these methods try to find the best set of explanatory variables to explain the model
      library(MASS) # for the stepAIC
      # https://www.rdocumentation.org/packages/MASS/versions/7.3-54/topics/stepAIC 

      
### Forward selection 
  # starts with no predictors in the model. iteratively adds the most contributive predictors, and
  # stops when the improvement is no longer statiscally significant
    simpleOLS <- lm(SalePrice~.-Id, data = train)  

    forward <- stepAIC(simpleOLS, direction="forward", na.action = na.remove) # start AIC = -6441.99
          
    fit_forwardOLS <- as.numeric(exp(predict(forward, newdata = test))) # cancel out log transformation
    
    predict_forwardOLS <- data.frame(ID = test$Id, SalePrice = fit_forwardOLS)
     
    write.csv(predict_forwardOLS, file = "esakaRei_forwardOLS_submission.csv", row.names = FALSE)
    # best kaggle score: 0.13608
    

### Backward selection
    backward <- stepAIC(simpleOLS, direction="backward", na.action = na.remove) #Step AIC=-6582.84
    
    fit_backwardOLS <- as.numeric(exp(predict(backward, newdata = test)))
    
    predict_backwardOLS <- data.frame(ID = test$Id, SalePrice = fit_backwardOLS)
    
    write.csv(predict_backwardOLS, file = "esakaRei_backwardOLS_submission.csv", row.names = FALSE)       
    # best Kaggle score: 0.13059
      
### Hybrid feature selection
    hybrid <- stepAIC(simpleOLS, direction="both", na.action = na.remove) #-6582.8
    
    fit_hybridOLS <- as.numeric(exp(predict(hybrid, newdata = test)))
    
    predict_hybridOLS <- data.frame(ID = test$Id, SalePrice = fit_hybridOLS)
    
    write.csv(predict_hybridOLS, file = "esakaRei_hybridOLS_submission.csv", row.names = FALSE)
    
    # best kaggle score: 0.13059
    
         
   
    
### Full Ridge regression, Full LASSO regression, Elastic net regression optimizing the mix parameter
  
  # splitting the data   
    
    # test
      x_test <- subset(test, select = -c(SalePrice, Id)) #x_test will not have saleprice nor id
      x_test <- data.matrix(x_test) #got an error saying needs to be in a matrix
    
    # for finding best lambda
      x_train<-subset(train, select = -c(SalePrice, Id))
      x_train <- data.matrix(x_train)
      y_train <- train$SalePrice
    
    # best lambda calculation. Colazo's code wasn't working out for me
    # but I found a simplified version that works
    # https://www.rdocumentation.org/packages/glmnet/versions/4.1-2/topics/cv.glmnet
      cv.out <- cv.glmnet(x_train, y_train)
      best.lambda <- cv.out$lambda.min
      best.lambda # ok good. got a value between 0 and 1. (0.0002302081)
    
  
### alpha 0 ridge, alpha 1 LASSO, anything in between is elastic net
      
    # Ridge (alpha = 0)
      ridge <- glmnet(x_train, y_train, alpha = 0, standardize = TRUE)
      
      fit_ridge <- as.numeric(exp(predict(ridge, newx = x_test, s = best.lambda))) #exp to cancel out log transformation
     
      predict_ridge <- data.frame(ID = test$Id, SalePrice = fit_ridge)
      
      write.csv(predict_ridge, file = "esakaRei_ridge_submission.csv", row.names = FALSE)
      # best kaggle Score of 0.12742
      
      # LASSO (alpha = 1)
      lasso <- glmnet(x_train, y_train, alpha = 1, standardize = TRUE)
      
      fit_lasso <- as.numeric(exp(predict(lasso, newx = x_test, s = best.lambda))) #exp to cancel out log transformation
      
      predict_lasso <- data.frame(ID = test$Id, SalePrice = fit_lasso)
      
      write.csv(predict_lasso, file = "esakaRei_lasso_submission.csv", row.names = FALSE)    
      # best kaggle score 0.12609
      
      # Elastic net ###try different alpha values
      elasticnet <- glmnet(x_train, y_train, alpha = 0.75, standardize = TRUE)
      
      fit_elasticnet <- as.numeric(exp(predict(elasticnet, newx = x_test, s = best.lambda))) #exp to cancel out log transformation
      
      predict_elasticnet <- data.frame(ID = test$Id, SalePrice = fit_elasticnet)
      
      write.csv(predict_elasticnet, file = "esakaRei_elasticnet_submission.csv", row.names = FALSE) 
      # best kaggle score: 0.12623 
      # where alpha = 0.75 
    
      
      
      
### SVM regression with different kernels ### in order for this to work, subset train set without categorical variables
    # Linear
    # Polynomial
    # Radial
      
      
      #load required library
      library(e1071) # for svm function
      
      #### For SVM
         # not sure if there's a more efficient way of subsetting, so I did it the hard way
         # I looked at the dropdown of raw_data in global environment and looked for all factor variables
  
      # was xsvm, had saleprice in after id
       subset_train_svm <- subset(train,select = -c(Id, SalePrice, MSSubClass, MSZoning, Street, Alley, LotShape, LandContour, LotConfig, LandSlope, Neighborhood,
                           Condition1, Condition2, BldgType, HouseStyle, RoofStyle, RoofMatl,Exterior1st, Exterior2nd, MasVnrType, Foundation, Heating, CentralAir, 
                           Electrical, GarageType, MiscFeature, MoSold, YrSold, SaleType, SaleCondition, KitchenQual, Functional, PavedDrive))
     # was x_testsvm had saleprice in 
       subset_test_svm <- subset(test,select = -c(Id, SalePrice, MSSubClass, MSZoning, Street, Alley, LotShape, LandContour, LotConfig, LandSlope, Neighborhood,
                                 Condition1, Condition2, BldgType, HouseStyle, RoofStyle, RoofMatl,Exterior1st, Exterior2nd, MasVnrType, Foundation, Heating, CentralAir,
                                 Electrical, GarageType, MiscFeature, MoSold, YrSold, SaleType, SaleCondition, KitchenQual, Functional, PavedDrive))
     
       
### SVM linear regression
       # make sure that the subset train svm dataset is all numeric
       svm_linear <- svm(subset_train_svm, y_train, type="eps-regression", kernel="linear")

       fit_linearsvm <-as.numeric(exp(predict(svm_linear,newdata = subset_test_svm)))
       
       predict_linearsvm <- data.frame(ID = test$Id, SalePrice = fit_linearsvm)
       
       write.csv(predict_linearsvm, file = "esakaRei_svmlinear_submission.csv", row.names = FALSE)
       # best kaggle score of 0.13588
       
       
### SVM polynomial
       svm_poly <- svm(subset_train_svm, y_train, type="eps-regression", kernel="polynomial")
       
       fit_polysvm <-as.numeric(exp(predict(svm_poly,newdata = subset_test_svm)))
       
       predict_polysvm <- data.frame(ID = test$Id, SalePrice = fit_polysvm)
       
       write.csv(predict_polysvm, file = "esakaRei_svmpoly_submission.csv", row.names = FALSE)
       # best kaggle score of 0.29966
       
       
### SVM radial
       svm_radial<- svm(subset_train_svm, y_train, type="eps-regression", kernel="radial")
       
       fit_radsvm <-as.numeric(exp(predict(svm_radial,newdata = subset_test_svm)))
       
       predict_radsvm <- data.frame(ID = test$Id, SalePrice = fit_radsvm)
       
       write.csv(predict_radsvm, file = "esakaRei_svmrad_submission.csv", row.names = FALSE)
       # best kaggle score of 0.15581
       
 
      
      
### Regression trees with random forests
### Same with bagging
### Same with boosting
 
       
### Random Forests     
       library(randomForest)
       
       rf<-randomForest(SalePrice ~. -Id, train, na.action = na.exclude)
       
       fit_rf <- as.numeric(exp(predict(rf, newdata=test)))
       
       predict_rf <- data.frame(ID = test$Id, SalePrice = fit_rf)
       
       write.csv(predict_rf, file = "esakaRei_rf_submission.csv", row.names = FALSE)
       # best kaggle score: 0.14236
       
### Bagging
      randomforest_bagging <- randomForest(SalePrice~.-Id, train, importance = TRUE, ntrees= 100)

      fit_bagging <- as.numeric(exp(predict(randomforest_bagging,newdata=test)))
      
      predict_bagging <- data.frame(ID = test$Id, SalePrice = fit_bagging)
      
      write.csv(predict_bagging, file = "esakaRei_bagging_submission.csv", row.names = FALSE)
      # best Kaggle score: 0.14225
      
### Boosting
    library(gbm)
      
    boosting <-gbm(SalePrice~.-Id,data=train,distribution="gaussian")
      
    fit_boosting <-exp(predict.gbm(boosting,newdata=test,n.trees=100))
      
    predict_boosting <- data.frame(ID = test$Id, SalePrice = fit_boosting)
      
    write.csv(predict_boosting, file = "esakaRei_boosting_submission.csv", row.names = FALSE)
    # best Kaggle score of 0.14362
      
  
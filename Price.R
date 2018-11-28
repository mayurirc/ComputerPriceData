
> ComputerPricesData=read.csv("ComputerPricesData.csv")
> #exploring the data
> str(ComputerPricesData) 
> summary(ComputerPricesData) 
> head(ComputerPricesData) 
>  dim(ComputerPricesData)
>  nrow(ComputerPricesData) 
>  ncol(ComputerPricesData)
>  names(ComputerPricesData)
>  colnames(ComputerPricesData)          
>  #Missing Data Treatment
>  colSums(is.na(ComputerPricesData))  #no missing value
> library(dummies)               #Performing one-hot encoding on nominal features 
>  ComputerPricesDataCleaned=dummy.data.frame(ComputerPricesData, sep='_') 
>  names(ComputerPricesDataCleaned)=make.names(names(ComputerPricesDataCleaned)) 
>  head(ComputerPricesDataCleaned) 
>  saveRDS(ComputerPricesDataCleaned, 'ComputerPricesDataCleaned.rds') 
>  ConcreteDataFromRDSFile=readRDS('ComputerPricesDataCleaned.rds') 
>  head(ConcreteDataFromRDSFile)
> library(MASS)
>  head(ComputerPricesDataCleaned) 
>  str(ComputerPricesDataCleaned)
>  cor(ComputerPricesDataCleaned) 
>  plot (x= ComputerPricesData$ram ,y=ComputerPricesDataCleaned$price)
#Dividing the data into training and testing for Machine Learning 
> sampleIndex=sample(1:nrow(ComputerPricesDataCleaned), size=0.7 * nrow(ComputerPricesDataCleaned) , replace=FALSE) 
>  ComputerPricesDataCleanedTraining=ComputerPricesDataCleaned[sampleIndex, ] 
>  ComputerPricesDataCleanedTesting=ComputerPricesDataCleaned[-sampleIndex, ] 
#Creating a model to predict price  
>  ComputerPricesDataCleanedRegModel =lm(price ~ ram + hd+speed, data= ComputerPricesDataCleanedTraining ) 
> summary(ComputerPricesDataCleanedRegModel) 
>  ComputerPricesDataCleanedRegModel =lm(price ~ ., data= ComputerPricesDataCleanedTraining ) 
> summary(ComputerPricesDataCleanedRegModel) 
> ComputerPricesDataCleanedRegModel =lm(price ~ .-cd_yes    -multi_yes -premium_yes, data= ComputerPricesDataCleanedTraining ) 
>  summary(ComputerPricesDataCleanedRegModel)             #R-squared:  0.7775 
#Evaluating the model’s performance on Testing Data 
> ComputerPricesDataCleanedTesting$predictedprice=predict(ComputerPricesDataCleanedRegModel, newdata=ComputerPricesDataCleanedTesting) 
#Calculating the accuracy of model
> ComputerPricesDataCleanedTesting$APE= 100 * (abs( ComputerPricesDataCleanedTesting$predictedprice -ComputerPricesDataCleanedTesting$price)/ ComputerPricesDataCleanedTesting$price)
> MAPE = mean(ComputerPricesDataCleanedTesting$APE)
>  MAPE       # 9.542483
>  Accuracy= 100- MAPE 
>  Accuracy            # 90.45752


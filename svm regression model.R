library(e1071) #loading the e1071 package for svm regression
library(psych) #loading the psych package for summary statistics and visualization
describe(Pphydat) #summary statistics of the data loaded
Pphydataper <- Pphydat[,c("long","lat","permeability","elevation")] #calling out the required  for permeability modelling and prediction
Pphydatapor <- Pphydat[,c("long","lat","porosity","elevation")]#declaring the required varaiables for porosity modelling and prediction
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))} # defining the normalize function for standardization transformation of the varaiables
Pphydataper$longnorm<-normalize(Pphydataper$long) #standardizing longitude variable in pphydataper dataset
Pphydataper$latnorm<-normalize(Pphydataper$lat) #standardizing latitude variable in pphydataper dataset
Pphydataper$elevationnorm<-normalize(Pphydataper$elevation) #standardizing elevation variable in pphydataper dataset
Pphydataper$permeabilitynorm<-normalize(Pphydataper$permeability)#standardizing permeability variable in pphydataper dataset
Pphydataper =Pphydataper[,c("longnorm","latnorm","elevationnorm","permeabilitynorm")] # attributing the standardize variables to Pphydataper
describe(Pphydataper) #summary statistics of Pphydataper scaling variables onto [0,1]
require(caTools)
 set.seed(101) # setting the seed for random selection of recurring data points
sample = sample.split(Pphydataper$permeabilitynorm,SplitRatio= .8) # splitting the datset to 80% and 20%
trainper = subset(Pphydataper,sample == TRUE) #declaring the 80% of the data for trainining data (permeability modelling)
testper  = subset(Pphydataper,sample == FALSE) #declaring 20% of the data to testing data (permeability modelling)
Pphydatapor$longnormm <-normalize(Pphydatapor$long) #standardizing longitude variable in pphydatapor dataset
Pphydatapor$latnormm <-normalize(Pphydatapor$lat) #standardizing latitude variable in pphydatapor dataset
Pphydatapor$elevationnormm <-normalize(Pphydatapor$elevation) #standardizing elevation variable in pphydatapor dataset
Pphydatapor$porositynormm <-normalize(Pphydatapor$porosity)#standardizing permeability variable in pphydatapor dataset
Pphydatapor <- Pphydatapor[,c("longnormm","latnormm","elevationnormm","porositynormm")] # declaring the standardize variables to Pphydatapor
describe(Pphydatapor)#summary statistics of Pphydatapor scaling variables onto [0,1]
require(caTools)
set.seed(101) # setting the seed for random selection of recurring data points
samplee = sample.split(Pphydatapor$porositynormm,SplitRatio= .8) # splitting the datset to 80% and 20%
trainpor = subset(Pphydatapor,samplee == TRUE) # splitting 80% of the data for trainining data (porosity modelling)
testpor  = subset(Pphydatapor,samplee == FALSE) # splitting 20% of the data to testing data (porosity modelling)
fitpolyperm<-svm(permeabilitynorm~.,data=trainper,type ="epsregression",kernel="polynomial") #training the svm polynomial regression model,using standardized permeability as response value
fitpolyporo<-svm(porositynormm~.,data=trainpor,type ="eps-regression",kernel="polynomial") #training the svm polynomial regresiion model,using standardized porosity as response value
fitradialperm<-svm(permeabilitynorm~.,data=trainper,type ="eps-regression",kernel="radial") #training the svm radial regresiion model,using standardized permeability as response value
fitradialporo=svm(porositynormm~.,data=trainpor,type="epsregression",kernel="radial")#training the svm radialmodel,using standardized porosity as response value
fitsigmoidperm=svm(permeabilitynorm~.,data=trainper,type="epsregression",kernel="sigmoid")#training the svm sigmoid regresiion model,using standardized permeability as response value
fitsigmoidporo<- svm(porositynormm~.,data=trainpor,type ="eps-regression",kernel="sigmoid") #training the svm sigmoid regresiion model,using standardized porosity as response value
predictperm<- predict(fitpolyperm,testper) #predicting permeability using trained svm polynomial model
predictpermrad <-predict(fitradialperm,testper)#predicting permeability using trained svm radial model
predictpermsigmoid <-predict(fitsigmoidperm,testper)#predicting permeability using trained svm sigmoid model
predictporo<- predict(fitpolyporo,testpor) #predicting porosity using trained svm polynomial model
predictpororad <-predict(fitradialporo,testpor)#predicting porosity using trained svm radial model
predictporosigmoid <-predict(fitsigmoidporo,testpor)#predicting porosity using trained svm sigmoid model
plot(testper$permeabilitynorm,predictperm)#plotting the observed test data against predicted permeability data from polynomial regression model
plot(testper$permeabilitynorm,predictpermrad)#plotting the observed test data against predicted permeability data from radial regression model
plot(testper$permeabilitynorm,predictpermsigmoid)#plotting the observed test data against predicted permeability data from sigmoid regression model
plot(testpor$porositynormm,predictporo)#plotting the observed test data against predicted permeability data from polynomial regression model
plot(testpor$porositynormm,predictpororad)#plotting the observed test data against predicted permeability data from radial regression model
plot(testpor$porositynormm,predictporosigmoid)#plotting the observed test data against predicted sigmoid data from sigmoid regression model
cor(testper$permeabilitynorm,predictpermrad)#plotting the observed test data against predicted permeability data from radial regression model
cor(testper$permeabilitynorm,predictperm)#plotting the observed test data against predicted permeability data from polynomial regression model
cor(testper$permeabilitynorm,predictpermsigmoid)#plotting the observed test data against predicted permeability data from sigmoid regression model
cor(testpor$porositynormm,predictporo)#plotting the observed test data against predicted porosity data from polynomial regression model
cor(testpor$porositynormm,predictpororad)#plotting the observed test data against predicted porosity data from radial regression model
cor(testpor$porositynormm,predictporosigmoid)#plotting the observed test data against predicted porosity data from sigmoid regression model
library(Metrics)
RMSE(testper$permeabilitynorm, predictpermrad)#obtaining the rootmeansquarederror of the radialkernel model.RMSE(testper$permeabilitynorm, predictpermsigmoid)#obtaining the rootmeansquarederror of the sigmoidkernel model
RMSE(testper$permeabilitynorm, predictperm)#obtaining the rootmeansquarederror of the polynomialkernel model
RMSE(testpor$porositynormm,predictporo)#obtaining the rootmeansquarederror of the polynomialkernel model
RMSE(testpor$porositynormm,predictpororad)#obtaining the rootmeansquarederror of the radiallkerenel model
RMSE(testpor$porositynormm,predictporosigmoid)#obtaining the rootmeansquarederror of the sigmoidkernel model
Pphydataperm <- Pphydat[,c("long","lat","permeability","elevation")]
Pphydataporo <- Pphydat[,c("long","lat","porosity","elevation")]
set.seed(1000) # setting the seed for random selection of recurring data points
206*0.8 
train <- sample(1:206,165,replace= FALSE) # splitting the datset to 80% and 20%
traindataperm <- Pphydataperm[train,] #declaring the 80% of the data for trainining data (permeability modelling)
testdataperm<- Pphydataperm[-train,] #declaring 20% of the data to testing data (permeability modelling)
traindataporo <- Pphydataporo[train,]#declaring the 80% of the data for trainining data (porosity modelling)
testdataporo<- Pphydataporo[-train,] #declaring 20% of the data to testing data (porosity modelling)
fitpolyperm<- svm(permeability~.,data=traindataperm,type ="eps-regression",kernel="polynomial")#training the svm polynomial regression model,using untransformed permeability data as response value
fitpolyporo<- svm(porosity~.,data=traindataporo,type ="eps-regression",kernel="polynomial")#training the svm polynomial regression model,using untransformed porosity data as response value
fitradialperm<- svm(permeability~.,data=traindataperm,type ="eps-regression",kernel="radial") #training the svm radial regression model,using untransformed permeability data as response value
fitradialporo<- svm(porosity~.,data=traindataporo,type ="eps-regression",kernel="radial")#training the svm radial regression model,using untransformed porosity data as response value
fitsigmoidperm<- svm(permeability~.,data=traindataperm,type ="eps-regression",kernel="sigmoid") #training the svm sigmoid regression model,using untransformed permeability data as response value
fitsigmoidporo<- svm(porosity~.,data=traindataporo,type ="eps-regression",kernel="sigmoid")#training the svm sigmoid regression model,using untransformed porosity data as response value
predictperm<- predict(fitpolyperm,testdataperm) #predicting permeability using trained svm polynomial model
predictpermrad <-predict(fitradialperm,testdataperm)#predicting permeability using trained svm radial model
predictpermsigmoid <-predict(fitsigmoidperm,testdataperm)#predicting permeability using trained svm sigmoid model
predictporo<- predict(fitpolyporo,testdataporo) #predicting pororsity using trained svm polynomial model
predictpororad <-predict(fitradialporo,testdataporo)#predicting pororsity using trained svm radial model
predictporosigmoid <-predict(fitsigmoidporo,testdataporo)#predicting pororsity using trained svm sigmoid model
plot(testdataperm$permeability,predictperm)#plotting the observed test data against predicted permeability data from polynomial regression model
plot(testdataperm$permeability,predictpermrad)#plotting the observed test data against predicted permeability data from radial regression model
plot(testdataperm$permeability,predictpermsigmoid)#plotting the observed test data against predicted permeability data from sigmoid regression model
plot(testdataporo$porosity,predictpororad)#plotting the observed test data against predicted porosity data from sradial regression model
plot(testdataporo$porosity,predictporosigmoid)#plotting the observed test data against predicted porosity data from sigmoid regression model
cor(testdataperm$permeability,predictperm)^2#correlating the observed test data against predicted permeability data from polynomial regression model
cor(testdataperm$permeability,predictpermrad)^2 #correlating the observed test data against predicted permeability data from radial regression model
cor(testdataperm$permeability,predictpermsigmoid)^2 #correlating the observed test data against predicted permeability data from polynomial regression model
cor(testdataporo$porosity,predictporo)^2#correlating the observed test data against predicted porosity data from polynomial regression model
cor(testdataporo$porosity,predictpororad)^2#correlating the observed test data against predicted porosity data from radial regression model
cor(testdataporo$porosity,predictporosigmoid)^2 #correlatingthe observed test data against predicted porosity data from radial regression model
Petrodata <- (log10(Pphydat)) #Log transformation of the data
Petrodata
nrow(Petrodata)
Petrodataperm <- Petrodata[,c("long","lat","permeability","elevation")]
Petrodataporo <- Petrodata[,c("long","lat","elevation","porosity")]
require(caTools)
set.seed(1000)
sample = sample.split(Petrodataperm$permeability,SplitRatio= .8)
traindataperm  = subset(Petrodataperm,sample == TRUE) 
testdataperm = subset(Petrodataperm,sample == FALSE)
set.seed(1000)
samplee = sample.split(Petrodataporo$porosity,SplitRatio= .8)
traindataporo = subset(Petrodataporo,sample == TRUE) 
testdataporo = subset(Petrodataporo,sample == FALSE)
fitpolyperm<- svm(permeability~.,data=traindataperm,type ="eps-regression",kernel="polynomial") #training the svm polynomial regression model,using logarithmic transformatio permeability data as response value
fitpolyporo<- svm(porosity~.,data=traindataporo,type ="eps-regression",kernel="polynomial")#training the svm polynomial regression model,using logarithmic transformatio porosity data as response value
fitradialperm<- svm(permeability~.,data=traindataperm,type ="eps-regression",kernel="radial")#training the svm radial regression model,using logarithmic transformatiopermeability data as response value
fitradialporo<- svm(porosity~.,data=traindataporo,type ="eps-regression",kernel="radial")#training the svm radial regression model,using logarithmic transformation porosity data as response value
fitsigmoidperm<- svm(permeability~.,data=traindataperm,type ="eps-regression",kernel="sigmoid")#training the svm sigmoid regression model,using logarithmic transformatio permeability data as response value
fitsigmoidporo<- svm(porosity~.,data=traindataporo,type ="eps-regression",kernel="sigmoid")#training the svm sigmoid regression model,using logarithmic transformatio porosity data as response value
predictperm<- predict(fitpolyperm,testdataperm) #using polynomial kernel
predictpermrad <-predict(fitradialperm,testdataperm)#using radial kernel
predictpermsigmoid <-predict(fitsigmoidperm,testdataperm)#using sigmoid kernel
predictporo<- predict(fitpolyporo,testdataporo) #using polynomial kernel
predictpororad <-predict(fitradialporo,testdataporo)#using radial kernel
predictporosigmoid <-predict(fitsigmoidporo,testdataporo)#using sigmoid kernel
cor(testdataperm$permeability,predictperm)^2
cor(testdataperm$permeability,predictpermrad)^2 #only prediction of permebility using radial kernel looks reasonable
cor(testdataperm$permeability,predictpermsigmoid)^2
cor(testdataporo$porosity,predictporo)^2
cor(testdataporo$porosity,predictpororad)^2
cor(testdataporo$porosity,predictporosigmoid)^2 
library(Metrics)
RMSE(testdataperm$permeability, predictpermrad)#obtaining the rootmeansquarederror of the radialkernel model 
RMSE(testdataperm$permeability, predictpermsigmoid)#obtaining the rootmeansquarederror of the sigmoidkernel model 
RMSE(testdataperm$permeability, predictperm)#obtaining the rootmeansquarederror of the polynomialkernel model 
RMSE(testdataporo$porosity,predictporo)#obtaining the rootmeansquarederror of the polynomialkernel model 
RMSE(testdataporo$porosity,predictpororad)#obtaining the rootmeansquarederror of the radialkerenel model
RMSE(testdataporo$porosity,predictporosigmoid)#obtaining the rootmeansquarederror of the sigmoidkerenel model

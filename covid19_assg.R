install.packages("quantmod")
install.packages("tseries")
install.packages("forecast")
install.packages("timeSeries")
library(quantmod)
library(tseries)
library(forecast)
library(timeSeries)
library(xts)
library(class)

#DATA COLLECTION OF STATES AND COUNTRIES FOR OUR GROUP

#Data Collection of Massachusetts in United States (1) : JIHWAN JEONG
Massachusetts<-read.csv("C:/covid19/covid19_massachussets(Jihwan).csv",header=TRUE)
View(Massachusetts)
str(Massachusetts)

#Data Collection of Pennsylvania in United States (2) : JIHWAN JEONG
Pennsylvania<-read.csv("C:/covid19/covid19_pennsylvania(Jihwan).csv",header=TRUE)
View(Pennsylvania)
str(Pennsylvania)

#Data Collection of Illinois in United States (3) : JENNY KIM
Illinois<-read.csv("C:/covid19/covid19_illinois(Jenny).csv",header=TRUE)
View(Illinois)
str(Illinois)

#Data Collection of Florida in United States (4) : JENNY KIM
Florida<-read.csv("C:/covid19/covid19_florida(Jenny).csv",header=TRUE)
View(Florida)
str(Florida)

#Data Collection of Louisiana in United States (5) : VO VAN SON
Louisiana<-read.csv("C:/covid19/covid19_louisiana(Dayson).csv",header=TRUE)
View(Louisiana)
str(Louisiana)

#Data Collection of Texas in United States (6) : VO VAN SON
Texas<-read.csv("C:/covid19/covid19_texas(Dayson).csv",header=TRUE)
View(Texas)
str(Texas)

#Data Collection of Azerbaijan (7) : Tasfique Enam
Azerbaijan<-read.csv("C:/covid19/covid19_azerbaijan(Taz).csv",header=TRUE)
View(Azerbaijan)
str(Azerbaijan)

#Data Collection of Ukraine (8) : Tasfique Enam
Ukraine<-read.csv("C:/covid19/covid19_ukraine(Taz).csv",header=TRUE)
View(Ukraine)
str(Ukraine)

#Data Collection of Oman (9) : Adilet Kulakhmet
Oman<-read.csv("C:/covid19/covid19_oman(Adilet).csv",header=TRUE)
View(Oman)
str(Oman)

#Data Collection of Reunion (10) : Adilet Kulakhmet
Reunion<-read.csv("C:/covid19/covid19_reunion(Adilet).csv",header=TRUE)
View(Reunion)
str(Reunion)

#Data Collection of Guadeloupe (11) : Zhang Yutong
Guadeloupe<-read.csv("C:/covid19/covid19_guadeloupe(Yutong).csv",header=TRUE)
View(Guadeloupe)
str(Guadeloupe)

#Data Collection of Kazakhstan (12) : Zhang Yutong
Kazakhstan<-read.csv("C:/covid19/covid19_kazakhstan(Yutong).csv",header=TRUE)
View(Kazakhstan)
str(Kazakhstan)





#DATA CLEANING

#Data Cleaning for Massachusetts (1)
Massachusetts$Date<-as.Date(Massachusetts$Date,format="%m/%d/%Y")
Massachusetts$Date

#Data Cleaning for Pennsylvania (2)
Pennsylvania$Date<-as.Date(Pennsylvania$Date,format="%m/%d/%Y")
Pennsylvania$Date

#Data Cleaning for Illinois (3)
Illinois$Date<-as.Date(Illinois$Date,format="%m/%d/%Y")
Illinois$Date

#Data Cleaning for Florida (4)
Florida$Date<-as.Date(Florida$Date,format="%m/%d/%Y")
Florida$Date

#Data Cleaning for Louisiana (5)
Louisiana$Date<-as.Date(Louisiana$Date,format="%m/%d/%Y")
Louisiana$Date

#Data Cleaning for Texas (6)
Texas$Date<-as.Date(Texas$Date,format="%m/%d/%Y")
Texas$Date

#Data Cleaning for Azerbaijan (7)
Azerbaijan$Date<-as.Date(Azerbaijan$Date,format="%m/%d/%Y")
Azerbaijan$Date

#Data Cleaning for Ukraine (8)
Ukraine$Date<-as.Date(Ukraine$Date,format="%m/%d/%Y")
Ukraine$Date

#Data Cleaning for Oman (9)
Oman$Date<-as.Date(Oman$Date,format="%m/%d/%Y")
Oman$Date

#Data Cleaning for Reunion (10)
Reunion$Date<-as.Date(Reunion$Date,format="%m/%d/%Y")
Reunion$Date

#Data Cleaning for Guadeloupe (11)
Guadeloupe$Date<-as.Date(Guadeloupe$Date,format="%m/%d/%Y")
Guadeloupe$Date

#Data Cleaning for Kazakhstan (12)
Kazakhstan$Date<-as.Date(Kazakhstan$Date,format="%m/%d/%Y")
Kazakhstan$Date






#DATA ANALYSIS

#Data Analysis (1) : The number of days in which the new cases, deaths exceed 1000 in Massachusetts
countries_dataFrame<-data.frame(NumOfCasesOver100=c(nrow(Massachusetts[Massachusetts$New.Cases > 100,]), 
                                                     nrow(Pennsylvania[Pennsylvania$New.Cases > 100,]),
                                                     nrow(Illinois[Illinois$New.Cases > 100,]),
                                                     nrow(Florida[Florida$New.Cases > 100,]),
                                                     nrow(Louisiana[Louisiana$New.Cases > 100,]),
                                                     nrow(Texas[Texas$New.Cases > 100,]),
                                                     nrow(Azerbaijan[Azerbaijan$New.Cases > 100,]),
                                                     nrow(Ukraine[Ukraine$New.Cases > 100,]),
                                                     nrow(Oman[Oman$New.Cases > 100,]),
                                                     nrow(Reunion[Reunion$New.Cases > 100,]),
                                                     nrow(Guadeloupe[Guadeloupe$New.Cases > 100,]),
                                                     nrow(Kazakhstan[Kazakhstan$New.Cases > 100,])),
                                    NumOfDeathsOver100=c(nrow(Massachusetts[Massachusetts$New.deaths > 100,]),
                                                         nrow(Pennsylvania[Pennsylvania$New.deaths > 100,]),
                                                         nrow(Illinois[Illinois$New.deaths > 100,]),
                                                         nrow(Florida[Florida$New.deaths > 100,]),
                                                         nrow(Louisiana[Louisiana$New.deaths > 100,]),
                                                         nrow(Texas[Texas$New.deaths > 100,]),
                                                         nrow(Azerbaijan[Azerbaijan$New.deaths > 100,]),
                                                         nrow(Ukraine[Ukraine$New.deaths > 100,]),
                                                         nrow(Oman[Oman$New.deaths > 100,]),
                                                         nrow(Reunion[Reunion$New.deaths > 100,]),
                                                         nrow(Guadeloupe[Guadeloupe$New.deaths > 100,]),
                                                         nrow(Kazakhstan[Kazakhstan$New.deaths > 100,])),
                                    RateOfRecovery=c(paste(round(((tail(Massachusetts$Total.Recovery,n=1)/(tail(Massachusetts$Total.Cases,n=1))*100)),digit=2),"%",sep=""),
                                                     paste(round(((tail(Pennsylvania$Total.Recovery,n=1)/(tail(Pennsylvania$Total.Cases,n=1))*100)),digit=2),"%",sep=""),
                                                     paste(round(((tail(Illinois$Total.Recovery,n=1)/(tail(Illinois$Total.Cases,n=1))*100)),digit=2),"%",sep=""),
                                                     paste(round(((tail(Florida$Total.Recovery,n=1)/(tail(Florida$Total.Cases,n=1))*100)),digit=2),"%",sep=""),
                                                     paste(round(((tail(Louisiana$Total.Recovery,n=1)/(tail(Louisiana$Total.Cases,n=1))*100)),digit=2),"%",sep=""),
                                                     paste(round(((tail(Texas$Total.Recovery,n=1)/(tail(Texas$Total.Cases,n=1))*100)),digit=2),"%",sep=""),
                                                     paste(round(((tail(Azerbaijan$Total.Recovery,n=1)/(tail(Azerbaijan$Total.Cases,n=1))*100)),digit=2),"%",sep=""),
                                                     paste(round(((tail(Ukraine$Total.Recovery,n=1)/(tail(Ukraine$Total.Cases,n=1))*100)),digit=2),"%",sep=""),
                                                     paste(round(((tail(Oman$Total.Recovery,n=1)/(tail(Oman$Total.Cases,n=1))*100)),digit=2),"%",sep=""),
                                                     paste(round(((tail(Reunion$Total.Recovery,n=1)/(tail(Reunion$Total.Cases,n=1))*100)),digit=2),"%",sep=""),
                                                     paste(round(((tail(Guadeloupe$Total.Recovery,n=1)/(tail(Guadeloupe$Total.Cases,n=1))*100)),digit=2),"%",sep=""),
                                                     paste(round(((tail(Kazakhstan$Total.Recovery,n=1)/(tail(Kazakhstan$Total.Cases,n=1))*100)),digit=2),"%",sep="")),
                                    row.names=c("Massachusetts", "Pennsylvania", "Illinois", "Florida", "Louisiana", "Texas", "Azerbaijan", "Ukraine", "Oman", "Reunion", "Guadeloupe", "Kazakhstan"))
countries_dataFrame





#DATA VISUALIZATION

#Data Visualization for Massachusetts (1)
plot(Massachusetts$Date,xlab="Date",Massachusetts$New.Cases,ylab="Daily New Cases",type="l",col="blue",main="Daily new COVID-19 cases in Massachusetts")
plot(Massachusetts$Date,xlab="Date",Massachusetts$New.deaths,ylab="Daily New Deaths",type="l",col="red",main="Daily new COVID-19 deaths in Massachusetts")
plot(Massachusetts$Date,xlab="Date",Massachusetts$New.Recovery,ylab="Daily New Recovery",type="l",col="blue",main="Daily new COVID-19 recovery in Massachusetts")
plot(Massachusetts$Date,xlab="Date",Massachusetts$Total.Cases,ylab="Total cases",type="l",col="blue",main="Total COVID-19 cases in Massachusetts")
plot(Massachusetts$Date,xlab="Date",Massachusetts$Total.Deaths,ylab="Total deaths",type="l",col="red",main="Total COVID-19 deaths in Massachusetts")
plot(Massachusetts$Date,xlab="Date",Massachusetts$Total.Recovery,ylab="total Recovery",type="l",col="blue",main="Total COVID-19 recovery in Massachusetts")
plot(Massachusetts$Date,xlab="Date",Massachusetts$Total.Active.Cases,ylab="Total active Cases",type="l",col="red",main="Total active COVID-19 cases in Massachusetts")

#Data Visualization for Pennsylvania (2)
plot(Pennsylvania$Date,xlab="Date",Pennsylvania$New.Cases,ylab="Daily New Cases",type="l",col="blue",main="Daily new COVID-19 cases in Pennsylvania")
plot(Pennsylvania$Date,xlab="Date",Pennsylvania$New.deaths,ylab="Daily New Deaths",type="l",col="red",main="Daily new COVID-19 deaths in Pennsylvania")
plot(Pennsylvania$Date,xlab="Date",Pennsylvania$New.Recovery,ylab="Daily New Recovery",type="l",col="blue",main="Daily new COVID-19 recovery in Pennsylvania")
plot(Pennsylvania$Date,xlab="Date",Pennsylvania$Total.Cases,ylab="Total cases",type="l",col="blue",main="Total COVID-19 cases in Pennsylvania")
plot(Pennsylvania$Date,xlab="Date",Pennsylvania$Total.Deaths,ylab="Total deaths",type="l",col="red",main="Total COVID-19 deaths in Pennsylvania")
plot(Pennsylvania$Date,xlab="Date",Pennsylvania$Total.Recovery,ylab="total Recovery",type="l",col="blue",main="Total COVID-19 recovery in Pennsylvania")
plot(Pennsylvania$Date,xlab="Date",Pennsylvania$Total.Active.Cases,ylab="Total active Cases",type="l",col="red",main="Total active COVID-19 cases in Pennsylvania")

#Data Visualization for Illinois (3)
plot(Illinois$Date,xlab="Date",Illinois$New.Cases,ylab="Daily New Cases",type="l",col="blue",main="Daily new COVID-19 cases in Illinois")
plot(Illinois$Date,xlab="Date",Illinois$New.deaths,ylab="Daily New Deaths",type="l",col="red",main="Daily new COVID-19 deaths in Illinois")
plot(Illinois$Date,xlab="Date",Illinois$New.Recovery,ylab="Daily New Recovery",type="l",col="blue",main="Daily new COVID-19 recovery in Illinois")
plot(Illinois$Date,xlab="Date",Illinois$Total.Cases,ylab="Total cases",type="l",col="blue",main="Total COVID-19 cases in Illinois")
plot(Illinois$Date,xlab="Date",Illinois$Total.Deaths,ylab="Total deaths",type="l",col="red",main="Total COVID-19 deaths in Illinois")
plot(Illinois$Date,xlab="Date",Illinois$Total.Recovery,ylab="total Recovery",type="l",col="blue",main="Total COVID-19 recovery in Illinois")
plot(Illinois$Date,xlab="Date",Illinois$Total.Active.Cases,ylab="Total active Cases",type="l",col="red",main="Total active COVID-19 cases in Illinois")

#Data Visualization for Florida (4)
plot(Florida$Date,xlab="Date",Florida$New.Cases,ylab="Daily New Cases",type="l",col="blue",main="Daily new COVID-19 cases in Florida")
plot(Florida$Date,xlab="Date",Florida$New.deaths,ylab="Daily New Deaths",type="l",col="red",main="Daily new COVID-19 deaths in Florida")
plot(Florida$Date,xlab="Date",Florida$New.Recovery,ylab="Daily New Recovery",type="l",col="blue",main="Daily new COVID-19 recovery in Florida")
plot(Florida$Date,xlab="Date",Florida$Total.Cases,ylab="Total cases",type="l",col="blue",main="Total COVID-19 cases in Florida")
plot(Florida$Date,xlab="Date",Florida$Total.Deaths,ylab="Total deaths",type="l",col="red",main="Total COVID-19 deaths in Florida")
plot(Florida$Date,xlab="Date",Florida$Total.Recovery,ylab="total Recovery",type="l",col="blue",main="Total COVID-19 recovery in Florida")
plot(Florida$Date,xlab="Date",Florida$Total.Active.Cases,ylab="Total active Cases",type="l",col="red",main="Total active COVID-19 cases in Florida")

#Data Visualization for Louisiana (5)
plot(Louisiana$Date,xlab="Date",Louisiana$New.Cases,ylab="Daily New Cases",type="l",col="blue",main="Daily new COVID-19 cases in Louisiana")
plot(Louisiana$Date,xlab="Date",Louisiana$New.deaths,ylab="Daily New Deaths",type="l",col="red",main="Daily new COVID-19 deaths in Louisiana")
plot(Louisiana$Date,xlab="Date",Louisiana$New.Recovery,ylab="Daily New Recovery",type="l",col="blue",main="Daily new COVID-19 recovery in Louisiana")
plot(Louisiana$Date,xlab="Date",Louisiana$Total.Cases,ylab="Total cases",type="l",col="blue",main="Total COVID-19 cases in Louisiana")
plot(Louisiana$Date,xlab="Date",Louisiana$Total.Deaths,ylab="Total deaths",type="l",col="red",main="Total COVID-19 deaths in Louisiana")
plot(Louisiana$Date,xlab="Date",Louisiana$Total.Recovery,ylab="total Recovery",type="l",col="blue",main="Total COVID-19 recovery in Louisiana")
plot(Louisiana$Date,xlab="Date",Louisiana$Total.Active.Cases,ylab="Total active Cases",type="l",col="red",main="Total active COVID-19 cases in Louisiana")

#Data Visualization for Texas (6)
plot(Texas$Date,xlab="Date",Texas$New.Cases,ylab="Daily New Cases",type="l",col="blue",main="Daily new COVID-19 cases in Texas")
plot(Texas$Date,xlab="Date",Texas$New.deaths,ylab="Daily New Deaths",type="l",col="red",main="Daily new COVID-19 deaths in Texas")
plot(Texas$Date,xlab="Date",Texas$New.Recovery,ylab="Daily New Recovery",type="l",col="blue",main="Daily new COVID-19 recovery in Texas")
plot(Texas$Date,xlab="Date",Texas$Total.Cases,ylab="Total cases",type="l",col="blue",main="Total COVID-19 cases in Texas")
plot(Texas$Date,xlab="Date",Texas$Total.Deaths,ylab="Total deaths",type="l",col="red",main="Total COVID-19 deaths in Texas")
plot(Texas$Date,xlab="Date",Texas$Total.Recovery,ylab="total Recovery",type="l",col="blue",main="Total COVID-19 recovery in Texas")
plot(Texas$Date,xlab="Date",Texas$Total.Active.Cases,ylab="Total active Cases",type="l",col="red",main="Total active COVID-19 cases in Texas")

#Data Visualization for Azerbaijan (7)
plot(Azerbaijan$Date,xlab="Date",Azerbaijan$New.Cases,ylab="Daily New Cases",type="l",col="blue",main="Daily new COVID-19 cases in Azerbaijan")
plot(Azerbaijan$Date,xlab="Date",Azerbaijan$New.Deaths,ylab="Daily New Deaths",type="l",col="red",main="Daily new COVID-19 deaths in Azerbaijan")
plot(Azerbaijan$Date,xlab="Date",Azerbaijan$New.Recovery,ylab="Daily New Recovery",type="l",col="blue",main="Daily new COVID-19 recovery in Azerbaijan")
plot(Azerbaijan$Date,xlab="Date",Azerbaijan$Total.Cases,ylab="Total cases",type="l",col="blue",main="Total COVID-19 cases in Azerbaijan")
plot(Azerbaijan$Date,xlab="Date",Azerbaijan$Total.Deaths,ylab="Total deaths",type="l",col="red",main="Total COVID-19 deaths in Azerbaijan")
plot(Azerbaijan$Date,xlab="Date",Azerbaijan$Total.Recovery,ylab="total Recovery",type="l",col="blue",main="Total COVID-19 recovery in Azerbaijan")
plot(Azerbaijan$Date,xlab="Date",Azerbaijan$Total.Active.Cases,ylab="Total active Cases",type="l",col="red",main="Total active COVID-19 cases in Azerbaijan")

#Data Visualization for Ukraine (8)
plot(Ukraine$Date,xlab="Date",Ukraine$New.Cases,ylab="Daily New Cases",type="l",col="blue",main="Daily new COVID-19 cases in Ukraine")
plot(Ukraine$Date,xlab="Date",Ukraine$New.Deaths,ylab="Daily New Deaths",type="l",col="red",main="Daily new COVID-19 deaths in Ukraine")
plot(Ukraine$Date,xlab="Date",Ukraine$New.Recovery,ylab="Daily New Recovery",type="l",col="blue",main="Daily new COVID-19 recovery in Ukraine")
plot(Ukraine$Date,xlab="Date",Ukraine$Total.Cases,ylab="Total cases",type="l",col="blue",main="Total COVID-19 cases in Ukraine")
plot(Ukraine$Date,xlab="Date",Ukraine$Total.Deaths,ylab="Total deaths",type="l",col="red",main="Total COVID-19 deaths in Ukraine")
plot(Ukraine$Date,xlab="Date",Ukraine$Total.Recovery,ylab="total Recovery",type="l",col="blue",main="Total COVID-19 recovery in Ukraine")
plot(Ukraine$Date,xlab="Date",Ukraine$Total.Active.Cases,ylab="Total active Cases",type="l",col="red",main="Total active COVID-19 cases in Ukraine")

#Data Visualization for Oman (9)
plot(Oman$Date,xlab="Date",Oman$New.Cases,ylab="Daily New Cases",type="l",col="blue",main="Daily new COVID-19 cases in Oman")
plot(Oman$Date,xlab="Date",Oman$New.Deaths,ylab="Daily New Deaths",type="l",col="red",main="Daily new COVID-19 deaths in Oman")
plot(Oman$Date,xlab="Date",Oman$New.Recovery,ylab="Daily New Recovery",type="l",col="blue",main="Daily new COVID-19 recovery in Oman")
plot(Oman$Date,xlab="Date",Oman$Total.Cases,ylab="Total cases",type="l",col="blue",main="Total COVID-19 cases in Oman")
plot(Oman$Date,xlab="Date",Oman$Total.Deaths,ylab="Total deaths",type="l",col="red",main="Total COVID-19 deaths in Oman")
plot(Oman$Date,xlab="Date",Oman$Total.Recovery,ylab="total Recovery",type="l",col="blue",main="Total COVID-19 recovery in Oman")
plot(Oman$Date,xlab="Date",Oman$Total.Active.Cases,ylab="Total active Cases",type="l",col="red",main="Total active COVID-19 cases in Oman")

#Data Visualization for Reunion (10)
plot(Reunion$Date,xlab="Date",Reunion$New.Cases,ylab="Daily New Cases",type="l",col="blue",main="Daily new COVID-19 cases in Reunion")
plot(Reunion$Date,xlab="Date",Reunion$New.Deaths,ylab="Daily New Deaths",type="l",col="red",main="Daily new COVID-19 deaths in Reunion")
plot(Reunion$Date,xlab="Date",Reunion$New.Recovery,ylab="Daily New Recovery",type="l",col="blue",main="Daily new COVID-19 recovery in Reunion")
plot(Reunion$Date,xlab="Date",Reunion$Total.Cases,ylab="Total cases",type="l",col="blue",main="Total COVID-19 cases in Reunion")
plot(Reunion$Date,xlab="Date",Reunion$Total.Deaths,ylab="Total deaths",type="l",col="red",main="Total COVID-19 deaths in Reunion")
plot(Reunion$Date,xlab="Date",Reunion$Total.Recovery,ylab="total Recovery",type="l",col="blue",main="Total COVID-19 recovery in Reunion")
plot(Reunion$Date,xlab="Date",Reunion$Total.Active.Cases,ylab="Total active Cases",type="l",col="red",main="Total active COVID-19 cases in Reunion")

#Data Visualization for Guadeloupe (11)
plot(Guadeloupe$Date,xlab="Date",Guadeloupe$New.Cases,ylab="Daily New Cases",type="l",col="blue",main="Daily new COVID-19 cases in Guadeloupe")
plot(Guadeloupe$Date,xlab="Date",Guadeloupe$New.deaths,ylab="Daily New Deaths",type="l",col="red",main="Daily new COVID-19 deaths in Guadeloupe")
plot(Guadeloupe$Date,xlab="Date",Guadeloupe$New.Recovery,ylab="Daily New Recovery",type="l",col="blue",main="Daily new COVID-19 recovery in Guadeloupe")
plot(Guadeloupe$Date,xlab="Date",Guadeloupe$Total.Cases,ylab="Total cases",type="l",col="blue",main="Total COVID-19 cases in Guadeloupe")
plot(Guadeloupe$Date,xlab="Date",Guadeloupe$Total.Deaths,ylab="Total deaths",type="l",col="red",main="Total COVID-19 deaths in Guadeloupe")
plot(Guadeloupe$Date,xlab="Date",Guadeloupe$Total.Recovery,ylab="total Recovery",type="l",col="blue",main="Total COVID-19 recovery in Guadeloupe")
plot(Guadeloupe$Date,xlab="Date",Guadeloupe$Total.Active.Cases,ylab="Total active Cases",type="l",col="red",main="Total active COVID-19 cases in Guadeloupe")

#Data Visualization for Kazakhstan (12)
plot(Kazakhstan$Date,xlab="Date",Kazakhstan$New.Cases,ylab="Daily New Cases",type="l",col="blue",main="Daily new COVID-19 cases in Kazakhstan")
plot(Kazakhstan$Date,xlab="Date",Kazakhstan$New.deaths,ylab="Daily New Deaths",type="l",col="red",main="Daily new COVID-19 deaths in Kazakhstan")
plot(Kazakhstan$Date,xlab="Date",Kazakhstan$New.Recovery,ylab="Daily New Recovery",type="l",col="blue",main="Daily new COVID-19 recovery in Kazakhstan")
plot(Kazakhstan$Date,xlab="Date",Kazakhstan$Total.Cases,ylab="Total cases",type="l",col="blue",main="Total COVID-19 cases in Kazakhstan")
plot(Kazakhstan$Date,xlab="Date",Kazakhstan$Total.Deaths,ylab="Total deaths",type="l",col="red",main="Total COVID-19 deaths in Kazakhstan")
plot(Kazakhstan$Date,xlab="Date",Kazakhstan$Total.Recovery,ylab="total Recovery",type="l",col="blue",main="Total COVID-19 recovery in Kazakhstan")
plot(Kazakhstan$Date,xlab="Date",Kazakhstan$Total.Active.Cases,ylab="Total active Cases",type="l",col="red",main="Total active COVID-19 cases in Kazakhstan")





#DATA PREDICTION PART

#Data prediction and forecasting in Massachusetts (1)
library(class)
Massachusetts$New.Cases<-as.character(Massachusetts$New.Cases)
Massachusetts$New.Cases<-as.numeric(Massachusetts$New.Cases)
Massachusetts<-Massachusetts[complete.cases(Massachusetts),]

Massachusetts$New.Cases<-factor(ifelse(Massachusetts$New.Cases>=500, "Highly-infectious", "Relieved"))
View(Massachusetts)
trainingSet<-Massachusetts[1:70, 3:5]
testingSet<-Massachusetts[71:97, 3:5]
trainingOutcomes<-Massachusetts[1:70, 2]
testingOutcomes<-Massachusetts[71:97, 2]
predictions<-knn(train = trainingSet, test = testingSet, cl = trainingOutcomes, k = 8)
View(predictions)
str(predictions)
table(testingOutcomes)

#Linear Regression in Massachusetts
trainingSet<-Massachusetts[1:69,]
testingSet<-Massachusetts[70:97,]
View(trainingSet)
linearRegression = lm(Total.Cases ~ Total.Active.Cases + Total.Deaths + Total.Recovery, trainingSet)
summary(linearRegression)
coef(summary(linearRegression))
predict(linearRegression)
totalCases = data.frame(Total.Active.Cases=0, Total.Deaths= 3000, Total.Recovery=70000)
predict(linearRegression, totalCases)



#Data prediction and forecasting in Pennsylvania (2)
Pennsylvania$New.Cases<-as.character(Pennsylvania$New.Cases)
Pennsylvania$New.Cases<-as.numeric(Pennsylvania$New.Cases)
Pennsylvania<-Pennsylvania[complete.cases(Pennsylvania),]

Pennsylvania$New.Cases<-factor(ifelse(Pennsylvania$New.Cases>=500, "Highly-infectious", "Relieved"))
View(Pennsylvania)
trainingSet<-Pennsylvania[1:70, 3:5]
testingSet<-Pennsylvania[71:97, 3:5]
trainingOutcomes<-Pennsylvania[1:70, 2]
testingOutcomes<-Pennsylvania[71:97, 2]
predictions<-knn(train = trainingSet, test = testingSet, cl = trainingOutcomes, k = 8)
View(predictions)
str(predictions)
table(testingOutcomes)

#Linear Regression in Pennsylvania
trainingSet<-Pennsylvania[1:69,]
testingSet<-Pennsylvania[70:97,]
View(trainingSet)
linearRegression = lm(Total.Cases ~ Total.Active.Cases + Total.Deaths + Total.Recovery, trainingSet)
summary(linearRegression)
coef(summary(linearRegression))
predict(linearRegression)
totalCases = data.frame(Total.Active.Cases=0, Total.Deaths= 3000, Total.Recovery=70000)
predict(linearRegression, totalCases)



#Data prediction and forecasting in Illinois (3)
Illinois$New.Cases<-as.character(Illinois$New.Cases)
Illinois$New.Cases<-as.numeric(Illinois$New.Cases)
Illinois<-Illinois[complete.cases(Illinois),]

Illinois$New.Cases<-factor(ifelse(Illinois$New.Cases>=500, "Highly-infectious", "Relieved"))
View(Illinois)
trainingSet<-Illinois[1:70, 3:5]
testingSet<-Illinois[71:97, 3:5]
trainingOutcomes<-Illinois[1:70, 2]
testingOutcomes<-Illinois[71:97, 2]
predictions<-knn(train = trainingSet, test = testingSet, cl = trainingOutcomes, k = 8)
View(predictions)
str(predictions)
table(testingOutcomes)

#Linear Regression in Pennsylvania
trainingSet<-Illinois[1:69,]
testingSet<-Illinois[70:97,]
View(trainingSet)
linearRegression = lm(Total.Cases ~ Total.Active.Cases + Total.Deaths + Total.Recovery, trainingSet)
summary(linearRegression)
coef(summary(linearRegression))
predict(linearRegression)
totalCases = data.frame(Total.Active.Cases=0, Total.Deaths= 3000, Total.Recovery=70000)
predict(linearRegression, totalCases)



#Data prediction and forecasting in Florida (4)
Florida$New.Cases<-as.character(Florida$New.Cases)
Florida$New.Cases<-as.numeric(Florida$New.Cases)
Florida<-Florida[complete.cases(Florida),]

Florida$New.Cases<-factor(ifelse(Florida$New.Cases>=500, "Highly-infectious", "Relieved"))
View(Florida)
trainingSet<-Florida[1:70, 3:5]
testingSet<-Florida[71:97, 3:5]
trainingOutcomes<-Florida[1:70, 2]
testingOutcomes<-Florida[71:97, 2]
predictions<-knn(train = trainingSet, test = testingSet, cl = trainingOutcomes, k = 8)
View(predictions)
str(predictions)
table(testingOutcomes)

#Linear Regression in Florida
trainingSet<-Florida[1:69,]
testingSet<-Florida[70:97,]
View(trainingSet)
linearRegression = lm(Total.Cases ~ Total.Active.Cases + Total.Deaths + Total.Recovery, trainingSet)
summary(linearRegression)
coef(summary(linearRegression))
predict(linearRegression)
totalCases = data.frame(Total.Active.Cases=0, Total.Deaths= 3000, Total.Recovery=70000)
predict(linearRegression, totalCases)



#Data prediction and forecasting in Louisiana (5)
Louisiana$New.Cases<-as.character(Louisiana$New.Cases)
Louisiana$New.Cases<-as.numeric(Louisiana$New.Cases)
Louisiana<-Louisiana[complete.cases(Louisiana),]

Louisiana$New.Cases<-factor(ifelse(Louisiana$New.Cases>=500, "Highly-infectious", "Relieved"))
View(Louisiana)
trainingSet<-Louisiana[1:70, 3:5]
testingSet<-Louisiana[71:97, 3:5]
trainingOutcomes<-Louisiana[1:70, 2]
testingOutcomes<-Louisiana[71:97, 2]
predictions<-knn(train = trainingSet, test = testingSet, cl = trainingOutcomes, k = 8)
View(predictions)
str(predictions)
table(testingOutcomes)

#Linear Regression in Louisiana
trainingSet<-Louisiana[1:69,]
testingSet<-Louisiana[70:97,]
View(trainingSet)
linearRegression = lm(Total.Cases ~ Total.Active.Cases + Total.Deaths + Total.Recovery, trainingSet)
summary(linearRegression)
coef(summary(linearRegression))
predict(linearRegression)
totalCases = data.frame(Total.Active.Cases=0, Total.Deaths= 3000, Total.Recovery=70000)
predict(linearRegression, totalCases)



#Data prediction and forecasting in Texas (6)
Texas$New.Cases<-as.character(Texas$New.Cases)
Texas$New.Cases<-as.numeric(Texas$New.Cases)
Texas<-Texas[complete.cases(Texas),]

Texas$New.Cases<-factor(ifelse(Texas$New.Cases>=500, "Highly-infectious", "Relieved"))
View(Texas)
trainingSet<-Texas[1:70, 3:5]
testingSet<-Texas[71:97, 3:5]
trainingOutcomes<-Texas[1:70, 2]
testingOutcomes<-Texas[71:97, 2]
predictions<-knn(train = trainingSet, test = testingSet, cl = trainingOutcomes, k = 8)
View(predictions)
str(predictions)
table(testingOutcomes)

#Linear Regression in Texas
trainingSet<-Texas[1:69,]
testingSet<-Texas[70:97,]
View(trainingSet)
linearRegression = lm(Total.Cases ~ Total.Active.Cases + Total.Deaths + Total.Recovery, trainingSet)
summary(linearRegression)
coef(summary(linearRegression))
predict(linearRegression)
totalCases = data.frame(Total.Active.Cases=0, Total.Deaths= 3000, Total.Recovery=70000)
predict(linearRegression, totalCases)



#Data prediction and forecasting in Azerbaijan (7)
Azerbaijan$New.Cases<-as.character(Azerbaijan$New.Cases)
Azerbaijan$New.Cases<-as.numeric(Azerbaijan$New.Cases)
Azerbaijan<-Azerbaijan[complete.cases(Azerbaijan),]

Azerbaijan$New.Cases<-factor(ifelse(Azerbaijan$New.Cases>=500, "Highly-infectious", "Relieved"))
View(Azerbaijan)
trainingSet<-Azerbaijan[1:70, 3:5]
testingSet<-Azerbaijan[71:97, 3:5]
trainingOutcomes<-Azerbaijan[1:70, 2]
testingOutcomes<-Azerbaijan[71:97, 2]
predictions<-knn(train = trainingSet, test = testingSet, cl = trainingOutcomes, k = 8)
View(predictions)
str(predictions)
table(testingOutcomes)

#Linear Regression in Azerbaijan
trainingSet<-Azerbaijan[1:69,]
testingSet<-Azerbaijan[70:97,]
View(trainingSet)
linearRegression = lm(Total.Cases ~ Total.Active.Cases + Total.Deaths + Total.Recovery, trainingSet)
summary(linearRegression)
coef(summary(linearRegression))
predict(linearRegression)
totalCases = data.frame(Total.Active.Cases=0, Total.Deaths= 3000, Total.Recovery=70000)
predict(linearRegression, totalCases)



#Data prediction and forecasting in Ukraine (8)
Ukraine$New.Cases<-as.character(Ukraine$New.Cases)
Ukraine$New.Cases<-as.numeric(Ukraine$New.Cases)
Ukraine<-Ukraine[complete.cases(Ukraine),]

Ukraine$New.Cases<-factor(ifelse(Ukraine$New.Cases>=500, "Highly-infectious", "Relieved"))
View(Ukraine)
trainingSet<-Ukraine[1:70, 3:5]
testingSet<-Ukraine[71:97, 3:5]
trainingOutcomes<-Ukraine[1:70, 2]
testingOutcomes<-Ukraine[71:97, 2]
predictions<-knn(train = trainingSet, test = testingSet, cl = trainingOutcomes, k = 8)
View(predictions)
str(predictions)
table(testingOutcomes)

#Linear Regression in Ukraine
trainingSet<-Ukraine[1:69,]
testingSet<-Ukraine[70:97,]
View(trainingSet)
linearRegression = lm(Total.Cases ~ Total.Active.Cases + Total.Deaths + Total.Recovery, trainingSet)
summary(linearRegression)
coef(summary(linearRegression))
predict(linearRegression)
totalCases = data.frame(Total.Active.Cases=0, Total.Deaths= 3000, Total.Recovery=70000)
predict(linearRegression, totalCases)



#Data prediction and forecasting in Oman (9)
Oman$New.Cases<-as.character(Oman$New.Cases)
Oman$New.Cases<-as.numeric(Oman$New.Cases)
Oman<-Oman[complete.cases(Oman),]

Oman$New.Cases<-factor(ifelse(Oman$New.Cases>=500, "Highly-infectious", "Relieved"))
View(Oman)
trainingSet<-Oman[1:70, 3:5]
testingSet<-Oman[71:97, 3:5]
trainingOutcomes<-Oman[1:70, 2]
testingOutcomes<-Oman[71:97, 2]
predictions<-knn(train = trainingSet, test = testingSet, cl = trainingOutcomes, k = 8)
View(predictions)
str(predictions)
table(testingOutcomes)

#Linear Regression in Oman
trainingSet<-Oman[1:69,]
testingSet<-Oman[70:97,]
View(trainingSet)
linearRegression = lm(Total.Cases ~ Total.Active.Cases + Total.Deaths + Total.Recovery, trainingSet)
summary(linearRegression)
coef(summary(linearRegression))
predict(linearRegression)
totalCases = data.frame(Total.Active.Cases=0, Total.Deaths= 3000, Total.Recovery=70000)
predict(linearRegression, totalCases)



#Data prediction and forecasting in Reunion (10) 
Reunion$New.Cases<-as.character(Reunion$New.Cases)
Reunion$New.Cases<-as.numeric(Reunion$New.Cases)
Reunion<-Reunion[complete.cases(Reunion),]

Reunion$New.Cases<-factor(ifelse(Reunion$New.Cases>=500, "Highly-infectious", "Relieved"))
View(Reunion)
trainingSet<-Reunion[1:70, 3:5]
testingSet<-Reunion[71:97, 3:5]
trainingOutcomes<-Reunion[1:70, 2]
testingOutcomes<-Reunion[71:97, 2]
predictions<-knn(train = trainingSet, test = testingSet, cl = trainingOutcomes, k = 8)
View(predictions)
str(predictions)
table(testingOutcomes)

#Linear Regression in Reunion
trainingSet<-Reunion[1:69,]
testingSet<-Reunion[70:97,]
View(trainingSet)
linearRegression = lm(Total.Cases ~ Total.Active.Cases + Total.Deaths + Total.Recovery, trainingSet)
summary(linearRegression)
coef(summary(linearRegression))
predict(linearRegression)
totalCases = data.frame(Total.Active.Cases=0, Total.Deaths= 3000, Total.Recovery=70000)
predict(linearRegression, totalCases)



#Data prediction and forecasting in Guadeloupe (11)
Guadeloupe$New.Cases<-as.character(Guadeloupe$New.Cases)
Guadeloupe$New.Cases<-as.numeric(Guadeloupe$New.Cases)
Guadeloupe<-Guadeloupe[complete.cases(Guadeloupe),]

Guadeloupe$New.Cases<-factor(ifelse(Guadeloupe$New.Cases>=500, "Highly-infectious", "Relieved"))
View(Guadeloupe)
trainingSet<-Guadeloupe[1:70, 3:5]
testingSet<-Guadeloupe[71:97, 3:5]
trainingOutcomes<-Guadeloupe[1:70, 2]
testingOutcomes<-Guadeloupe[71:97, 2]
predictions<-knn(train = trainingSet, test = testingSet, cl = trainingOutcomes, k = 8)
View(predictions)
str(predictions)
table(testingOutcomes)

#Linear Regression in Guadeloupe
trainingSet<-Guadeloupe[1:69,]
testingSet<-Guadeloupe[70:97,]
View(trainingSet)
linearRegression = lm(Total.Cases ~ Total.Active.Cases + Total.Deaths + Total.Recovery, trainingSet)
summary(linearRegression)
coef(summary(linearRegression))
predict(linearRegression)
totalCases = data.frame(Total.Active.Cases=0, Total.Deaths= 3000, Total.Recovery=70000)
predict(linearRegression, totalCases)



#Data prediction and forecasting in Kazakhstan (12)
Kazakhstan$New.Cases<-as.character(Kazakhstan$New.Cases)
Kazakhstan$New.Cases<-as.numeric(Kazakhstan$New.Cases)
Kazakhstan<-Kazakhstan[complete.cases(Kazakhstan),]

Kazakhstan$New.Cases<-factor(ifelse(Kazakhstan$New.Cases>=500, "Highly-infectious", "Relieved"))
View(Kazakhstan)
trainingSet<-Kazakhstan[1:70, 3:5]
testingSet<-Kazakhstan[71:97, 3:5]
trainingOutcomes<-Kazakhstan[1:70, 2]
testingOutcomes<-Kazakhstan[71:97, 2]
predictions<-knn(train = trainingSet, test = testingSet, cl = trainingOutcomes, k = 8)
View(predictions)
str(predictions)
table(testingOutcomes)

#Linear Regression in Kazakhstan
trainingSet<-Kazakhstan[1:69,]
testingSet<-Kazakhstan[70:97,]
View(trainingSet)
linearRegression = lm(Total.Cases ~ Total.Active.Cases + Total.Deaths + Total.Recovery, trainingSet)
summary(linearRegression)
coef(summary(linearRegression))
predict(linearRegression)
totalCases = data.frame(Total.Active.Cases=0, Total.Deaths= 3000, Total.Recovery=70000)
predict(linearRegression, totalCases)
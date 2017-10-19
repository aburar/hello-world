
library("AzureML")
library(ggplot2)
ws = workspace()
housingData = download.datasets(ws,"HDBResalePricesMultiFeature.csv")
housingData = housingData[,-c(10,11)]
colnames(housingData)
head(housingData)

boxplot(housingData$ResalePrice)

qplot(data=housingData,FlatType,ResalePrice,geom="boxplot")

myplot=ggplot(data=housingData,aes(log(SQMS),log(ResalePrice),Floor))+geom_point(aes(color=Floor,size=20))
myplot = myplot+ facet_wrap( ~ Area, ncol=4)

print(myplot)

t(t(prop.table(table(housingData$Area))*100))

housingData$Floor = as.factor(housingData$Floor)
housingData$FlatType= as.factor(housingData$FlatType)
housingData$BuildingAge <- as.numeric(format(Sys.time(), "%Y")) - housingData$LeaseCommenceDate
housingData$ApprovalDate <- as.Date(housingData$ApprovalDate, '%d/%m/%Y')
housingData$TimeElapasedSinceTransaction <- as.numeric(format(Sys.time(), "%Y")) - as.numeric(format(housingData$ApprovalDate,"%Y"))
qplot(data=housingData[housingData$FlatType!="Terrace",],BuildingAge,ResalePrice)

head(housingData)
housingDataAMK = housingData[housingData$Area=="Ang Mo Kio" & housingData$FlatType!="Terrace",]
sapply(housingDataAMK,class)
levels(housingDataAMK$Floor)
housingDataAMK$FloorN = as.numeric(housingDataAMK$Floor)
colnames(housingDataAMK)
housingDataAMKFlatType <- model.matrix(~housingDataAMK$FlatType-1, data=housingDataAMK)
colnames(housingDataAMKFlatType)=c("Improved","ModelA","NewGen","Simpl","Stand","Terrace")
housingDataAMKMod = cbind(housingDataAMK,housingDataAMKFlatType)
boxplot(housingDataAMKMod$ResalePrice)

ncol(housingDataAMKMod)
nrow(housingDataAMKMod)
set.seed(1234)
trainInd=
sample(1:nrow(housingDataAMKMod),0.9*nrow(housingDataAMKMod),replace=FALSE)
housingDataAMKTrain = housingDataAMKMod[trainInd,]
housingDataAMKTest = housingDataAMKMod[-trainInd,]
head(housingDataAMKTrain[,c(5,9:12)])
qplot(housingDataAMKMod$ResalePrice)+stat_function(fun = dnorm)

resPrMod=lm(ResalePrice ~ .,data=housingDataAMKTrain[,c(5,9:12)])

summary(resPrMod)
plot(resPrMod)

housingActPred =
data.frame(actual=housingDataAMKTrain$ResalePrice)
housingActPred$prediction =
predict(resPrMod, newdata=housingDataAMKTest[,c(5,9:12)])

#plot(housingActPred$actual, col="blue", type="l")
#lines(housingActPred$prediction, col="red", type="l")
qplot(data=housingActPred,prediction,prediction-actual)

head(housingActPred)
housingActPred$Diff=housingActPred$actual-housingActPred$prediction

housingActPred$Diff=housingActPred$Diff*housingActPred$Diff

qplot(log(housingActPred$Diff))

qplot(data=housingDataAMK,as.factor(SQMS),ResalePrice,geom="boxplot")

myplot=ggplot(data=housingDataAMKTest,aes(log(SQMS),log(ResalePrice),Floor))+geom_point(aes(color=Floor,size=20))
print(myplot)

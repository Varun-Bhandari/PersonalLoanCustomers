setwd("C:/Users/Ajay/Desktop/RWorkingDirectory/")
library(readxl)
miniproject3.dataset <- read_xlsx("Bank Personal Loan Dataset2.xlsx")
is.na(miniproject3.dataset)
sum(miniproject3.dataset$`Personal Loan`)
sum(miniproject3.dataset$`Personal Loan`)/nrow(miniproject3.dataset)
##install.packages("randomForest")
library(randomForest)
#RF <- randomForest(as.factor(Personal Loan) ~ ., data = miniproject3.dataset[,-1], ntree=501, mtry = 3, nodesize = 10,importance=TRUE)
#RFmodel=randomForest(as.factor(miniproject3.dataset$`Personal Loan`)~.,data=miniproject3.dataset[,-1], ntree=501, mtry=3, nodesize=10, importance=TRUE)
?randomForest
?rm.na
?na.omit
miniproject3.dataset2=na.omit(miniproject3.dataset)
#RFmodel=randomForest(as.factor(miniproject3.dataset2$`Personal Loan`)~.,data=miniproject3.dataset[,-1], ntree=501, mtry=3, nodesize=10, importance=TRUE)
summary(miniproject3.dataset2$`Age (in years)`)
#miniproject3.dataset2$`Age (in years)`=as.numeric(miniproject3.dataset2$`Age (in years)`)
summary(miniproject3.dataset2)
miniproject3.dataset2$ID=as.factor((miniproject3.dataset2$ID))
#miniproject3.dataset2$Zone=(miniproject3.dataset2$'ZIP Code'%/%100)*100
miniproject3.dataset2$`ZIP Code`=as.factor((miniproject3.dataset2$`ZIP Code`))
#miniproject3.dataset2$`Personal Loan`=as.factor((miniproject3.dataset2$`Personal Loan`))
miniproject3.dataset2$`Securities Account`=as.factor((miniproject3.dataset2$`Securities Account`))
miniproject3.dataset2$`CD Account`=as.factor((miniproject3.dataset2$`CD Account`))
miniproject3.dataset2$Online=as.factor((miniproject3.dataset2$Online))
miniproject3.dataset2$CreditCard=as.factor((miniproject3.dataset2$CreditCard))
colnames(miniproject3.dataset2)=make.names(colnames(miniproject3.dataset2))

pairs(miniproject3.dataset2[,c(4,10)])
relmodel1=aov(miniproject3.dataset2$Personal.Loan~., data=miniproject3.dataset2[,-c(1,5)])
summary(relmodel1)
?pairs


RFmodel=randomForest(as.factor(miniproject3.dataset2$Personal.Loan)~.,data=miniproject3.dataset2[,c(-1,-5)], 
                     ntree=501, mtry=3, nodesize=100, importance=TRUE)
summary(miniproject3.dataset2)
print(RFmodel)

plot(RFmodel, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest Thera Bank")

RFmodel$err.rate

impVar <- round(randomForest::importance(RFmodel), 2)
impVar[order(impVar[,3], decreasing=TRUE),]

?tuneRF

tRF <- tuneRF(x = miniproject3.dataset2[,-c(1,5)], 
              y=as.factor(miniproject3.dataset2$Personal.Loan),
              mtryStart = 3, 
              ntreeTry=201, 
              stepFactor = 1.5, 
              improve = 0.0001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 100, 
              importance=TRUE
)

View(miniproject3.dataset2)
## Scoring syntax
miniproject3.dataset2$predict.class <- predict(RFmodel, miniproject3.dataset2, type="class")
miniproject3.dataset2$predict.score <- predict(RFmodel, miniproject3.dataset2, type="prob")
head(miniproject3.dataset2)
class(miniproject3.dataset2$predict.score)


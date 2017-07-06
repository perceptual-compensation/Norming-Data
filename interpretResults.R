library(nnet)
library(data.table)
library(vcd)
library(irr)

results <- read.csv("~/GitHub/Norming-Data/results.csv", header = FALSE, sep = ",")
colnames(results) <- c("ID", "IP", "SentenceType", "ItemNumber", "ElementNumber", "SentType", "SentGroup", "Question", "Answer", "Correct", "Time")

#Remove unwanted columns and rows, transform subject and item number columns
results$Subject <- as.factor(as.integer(results$IP))
results$ItemNumber <- as.factor(results$ItemNumber)
results <- results[results$Answer != "" & results$SentType != "practice",]
results <- unique(results)
results <- results[,c("IP", "Subject", "ItemNumber", "SentType", "Answer")]

#Organize sentence categories and answers into neat, matching factors
results$Answer <- droplevels(results$Answer)
results$ItemNumber <- droplevels(results$ItemNumber)
results$Expected <- as.factor(c("neg"="Negative", 
                                "neu"="Neutral", 
                                "pos"="Positive")[substr(results$SentType,1,3)])

#Create these columns to "cast" each answer into its own respective column.
results$NegAns <- ifelse(results$Answer == "Negative", 1 ,0)
results$NeutAns <- ifelse(results$Answer == "Neutral", 1 ,0)
results$PosAns <- ifelse(results$Answer == "Positive", 1 ,0)
results$OrdinalAns <- results$PosAns - results$NegAns
results$OrdinalExp <- ifelse(results$Expected=="Negative",-1,ifelse(results$Expected=="Positive",1,0))

#Aggregate the data separately by items and subjects
Item.tbl <- data.table(results)[,.(NegAns=mean(NegAns),
                                   NeutAns=mean(NeutAns),
                                   PosAns=mean(PosAns),
                                   Count=.N),
                                by=.(ItemNumber,Expected)][order(ItemNumber)]
Subj.tbl <- data.table(results)[,.(NegAns=mean(NegAns),
                                   NeutAns=mean(NeutAns),
                                   PosAns=mean(PosAns),
                                   Count=.N),
                                by=Subject][order(Subject)]
Neutral.tbl <- subset(Item.tbl, NeutAns > PosAns + NegAns)

#Plot the by-item results as a stacked barplot.
plotOrder <- order(-1000*(Item.tbl$Expected=="Negative")+1000*(Item.tbl$Expected=="Positive")-Item.tbl$NegAns+Item.tbl$PosAns)
barplot(t(as.matrix(Item.tbl[plotOrder,-c(1,2,6)])))

#This prints a contingency table, and graphs it as a mosaic plot.
contingency <- table(results$Expected, results$Answer, dnn=c("Expected", "Observed"))
contingency
mosaic(contingency, shade=TRUE, legend=TRUE)
assocstats(contingency)
fleissKappaTable <- table(results$ItemNumber, results$Subject)
m <- matrix(mapply(function(x,y){ifelse(fleissKappaTable[x,y],as.character(results$Answer)[results$ItemNumber==levels(results$ItemNumber)[x]&results$Subject==levels(results$Subject)[y]],NA)},
                   row(fleissKappaTable),
                   col(fleissKappaTable)),
            nrow=length(levels(results$ItemNumber)))
rownames(m) <- levels(results$ItemNumber)
colnames(m) <- levels(results$Subject)
kappam.fleiss(m)

#Separate logistic models for positive and negative answers, with subject and item factors
logModel.neg <- glm(NegAns~ItemNumber+Subject, data=results, family=binomial(link="probit"))
logModel.pos <- glm(PosAns~ItemNumber+Subject, data=results, family=binomial(link="probit"))
n <- coef(summary(logModel.neg))[substr(rownames(coef(summary(logModel.neg))),1,1) %in% c("I","("),1]
p <- coef(summary(logModel.pos))[substr(rownames(coef(summary(logModel.pos))),1,1) %in% c("I","("),1]
n <- n + c(0, rep(n[1], length(n)-1))
p <- p + c(0, rep(p[1], length(p)-1))
plot(n, p, col=c("red","black")[1+(Item.tbl$Expected!="Neutral")])
abline(h=0,v=0)

#Just the item factor estimates, for graphing purposes
est.neg <- coef(summary(glm(NegAns~ItemNumber, data=results, family=binomial(link="probit"))))[,1]
est.pos <- coef(summary(glm(PosAns~ItemNumber, data=results, family=binomial(link="probit"))))[,1]
est.neg <- est.neg + c(0, rep(est.neg[1], length(est.neg)-1))
est.pos <- est.pos + c(0, rep(est.pos[1], length(est.pos)-1))
Item.tbl$Estimate.neg <- est.neg
Item.tbl$Estimate.pos <- est.pos
plot(est.neg, est.pos, col=c("red","black")[1+(Item.tbl$Expected!="Neutral")])
abline(h=0,v=0)

#Ordinal representation
model.ord <- lm(OrdinalAns~ItemNumber+Subject,data=results)
o <- coef(summary(model.ord))[substr(rownames(coef(summary(model.ord))),1,1) %in% c("I","("),1]
o <- o + c(0, rep(o[1], length(o)-1))
Item.tbl$Estimate.lm <- o
Neut28.log <- Item.tbl[order(Item.tbl$Estimate.neg*Item.tbl$Estimate.pos,decreasing = T)[1:28],]
Neut28.ord <- Item.tbl[order(abs(Item.tbl$Estimate.lm),decreasing = F)[1:28],]
plot(o,col=c(Neutral="black",Positive="red",Negative="blue")[as.character(Item.tbl$Expected)],pch=19)
points(Neut28.log$ItemNumber,Neut28.log$Estimate.lm,pch="O")
abline(h=c(min(Neut28.ord$Estimate.lm),max(Neut28.ord$Estimate.lm)),lty=3)

#Multinomial logistic model with subject and item factors
multinom(Answer~Expected+Subject, data=results)

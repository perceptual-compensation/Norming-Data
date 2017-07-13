library(data.table) #for summary tables
library(vcd) #for stats on contingency table
library(ordinal) #for cumulative link mixed model
library(polycor) #for ordinal correlation

results <- read.csv("https://raw.githubusercontent.com/perceptual-compensation/Norming-Data/master/results.csv", header = FALSE, sep = ",")
colnames(results) <- c("ID", "IP", "SentenceType", "ItemNumber", "ElementNumber", "SentType", "SentGroup", "Question", "Answer", "Correct", "Time")

#Remove unwanted columns and rows, transform subject and item number columns
results$Subject <- as.factor(as.integer(results$IP))
results$ItemNumber <- as.factor(results$ItemNumber)
results <- results[results$Answer != "" & results$SentType != "practice",]
results <- unique(results)
results <- results[,c("IP", "Subject", "ItemNumber", "SentType", "Answer")]

#Organize sentence categories and answers into neat, matching factors
results$Answer <- factor(results$Answer, levels=c("Negative","Neutral","Positive"), ordered = TRUE)
results$ItemNumber <- droplevels(results$ItemNumber)
results$Expected <- factor(c("neg"="Negative", 
                             "neu"="Neutral", 
                             "pos"="Positive")[substr(results$SentType,1,3)], 
                           levels=c("Negative","Neutral","Positive"), 
                           ordered = TRUE)

#Create these columns to "cast" each answer into its own respective column.
results$NegAns <- ifelse(results$Answer == "Negative", 1 ,0)
results$NeutAns <- ifelse(results$Answer == "Neutral", 1 ,0)
results$PosAns <- ifelse(results$Answer == "Positive", 1 ,0)
results$NumericAns <- results$PosAns - results$NegAns
results$NumericExp <- ifelse(results$Expected=="Negative",-1,ifelse(results$Expected=="Positive",1,0))

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

#This prints a contingency table, and graphs it as a mosaic plot.  Plus, does some stats.
contingency <- table(results$Expected, results$Answer, dnn=c("Expected", "Observed"))
contingency
mosaic(contingency, shade=TRUE, legend=TRUE)
assocstats(contingency)

#As above, but with just the item factor estimates.  Seems to come out more sensibly.
model.neg <- glm(NegAns~ItemNumber, data=results, family=binomial(link="probit"))
model.pos <- glm(PosAns~ItemNumber, data=results, family=binomial(link="probit"))
est.neg <- coef(summary(model.neg))[,1]
est.pos <- coef(summary(model.pos))[,1]
Item.tbl$Est.neg <- est.neg + c(0, rep(est.neg[1], length(est.neg)-1))
Item.tbl$Est.pos <- est.pos + c(0, rep(est.pos[1], length(est.pos)-1))
plot(Item.tbl$Est.neg, Item.tbl$Est.pos, col=c("blue","black","red")[Item.tbl$Expected], pch=19)
abline(h=0,v=0)

#Numeric representation, plus graph of the kinda-positions of the 28 most neutral sentences
model.num <- lm(NumericAns~ItemNumber+Subject,data=results)
o <- coef(summary(model.num))[substr(rownames(coef(summary(model.num))),1,1) %in% c("I","("),1]
Item.tbl$Est.lm <- o + c(0, rep(o[1], length(o)-1))
logOrder <- mapply(function(x,y)max(x,y),Item.tbl$Est.neg,Item.tbl$Est.pos)
Neut28.log <- Item.tbl[order(logOrder,decreasing = F)[1:28],]
Neut28.num <- Item.tbl[order(abs(Item.tbl$Est.lm),decreasing = F)[1:28],]
plot(Item.tbl$Est.lm, col=c("blue","black","red")[Item.tbl$Expected], pch=19)
points(Neut28.log$ItemNumber,Neut28.log$Est.lm,pch="O")
abline(h=c(min(Neut28.num$Est.lm),max(Neut28.num$Est.lm)),lty=3)

#Ordinal representation
model.ord <- clmm2(Answer~ItemNumber,random=Subject,data=results,link="probit",threshold="flexible",Hess=TRUE)
o <- c(0, coef(summary(model.ord))[-c(1,2),1])
Item.tbl$Est.ord <- o
thresholds <- coef(summary(model.ord))[c(1,2),1]
Neut28.ord <- Item.tbl[order(abs(Item.tbl$Est.ord-mean(thresholds)),decreasing = F)[1:28],]
Item.tbl$OrdAns <- factor(ifelse(Item.tbl$Est.ord < thresholds[1], "Negative", 
                                 ifelse(Item.tbl$Est.ord > thresholds[2], "Positive", "Neutral")),
                          levels=c("Negative","Neutral","Positive"), 
                          ordered = TRUE)
results$Est.ord <- Item.tbl$Est.ord[match(results$ItemNumber,Item.tbl$ItemNumber)]
results$OrdAns <- Item.tbl$OrdAns[match(results$ItemNumber,Item.tbl$ItemNumber)]
plot(Item.tbl$Est.ord, col=c("blue","black","red")[Item.tbl$Expected],pch=19)
points(Neut28.ord$ItemNumber,Neut28.ord$Est.ord,pch="O")
abline(h=thresholds)

#Measures of inter-rater agreement, via polychoric correlation
polyserial(results$Est.ord, results$Answer, ML = TRUE)
polychor(results$OrdAns, results$Answer, ML = TRUE)
pc <- sapply(levels(results$Subject), 
             function(x)polychor(results$OrdAns[results$Subject==x], 
                                 results$Answer[results$Subject==x], 
                                 ML=TRUE))
ps <- sapply(levels(results$Subject), 
             function(x)polyserial(results$Est.ord[results$Subject==x], 
                                   results$Answer[results$Subject==x], 
                                   ML=FALSE))

#Compare separate logistic models with the ordinal model
plot(Item.tbl$Est.neg, Item.tbl$Est.pos, col=c("blue","black","red")[Item.tbl$OrdAns], pch=19)
abline(h=0,v=0)


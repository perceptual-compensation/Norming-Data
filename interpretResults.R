results <- read.csv('norming/results.csv', header = FALSE, sep = ",")
colnames(results) <- c("ID", "IP", "SentenceType", "ItemNumber", "ElementNumber", "SentType", "SentGroup", "Question", "Answer", "Correct", "Time")

results <- subset(results, select = -c(ID, ElementNumber, Correct))
results <- results[results$Answer !="" & results$SentType != "practice",]
results$Neutral <- ifelse(substr(results$SentType,1,4) == "neut", 1, 0)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

resultTbl <- aggregate(Answer~IP+ItemNumber+Neutral,results,Mode)

library(tidyverse)
library(zoo)

df <- read.csv("CleaningProcess.csv", header = FALSE)

df <- separate(df, col = V1, into = c("left","right"), sep = "\\:", fill = "left")

df[df == ""] <- NA


#Stores lines where dialogue is on the same line as character 
DialgoueSameLine <- df[complete.cases(df),]



RestOfDialogue <- df[!complete.cases(df),]


RestOfDialogue$left <-  na.locf(RestOfDialogue$left) 

RestOfDialogue <-  RestOfDialogue %>% na.omit()

cleanedDf <- rbind(RestOfDialogue, DialgoueSameLine)
write.csv(cleanedDf, "HarryPotterAddition.csv", row.names = FALSE)
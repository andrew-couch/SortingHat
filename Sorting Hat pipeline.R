library(tidyverse)
library(tidytext)
library(sentimentr)

bow <- read.csv("bowlist.csv", header = TRUE,stringsAsFactors = FALSE)
bigram <- read.csv("bigramlist.csv", header = TRUE,stringsAsFactors = FALSE)
trigram <- read.csv("trigramlist.csv", header = TRUE,stringsAsFactors = FALSE)


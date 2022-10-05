#Carton & Rosette 2011 Analysis
#Part 1.2
#set the Working Directory
setwd("~/bu311")

#Read in file
#USE UPDATED FILE - V2
d <- read.csv("https://raw.githubusercontent.com/Huntooon/BU311/main/CartonRosette2011v2.csv")

str(d) #looks at internal structure
head(d) #prints first 6 rows
dim(d) #dimensions of dataset - rows and columns
colnames(d) #prints the names of columns

#summary stats
mean(d$Incompetence, na.rm=TRUE) #remove missing values
mean(d$Athleticism) #Average of Athleticism column
mean(d$Competence) #Average of Competence column

sd(d$Incompetence) #Standard Deviation of Incompetence

summary(d) #Summary of the dataset

dcor <- as.data.frame(cor(d)) #Create a new correlation matrix

colnames(d) #Prints the Column Names
#Below is used to set order for the columns
col_order <- c("Performance",
               "QB_race",
               "Peer_assessment_of_QBs_school",
               "Rushing_yards",
               "Words_published_per_QB_per_interval",
               "Source_of_phrase",
               "Incompetence",
               "Athleticism",
               "Competence",
               "Lack_of_athleticism",
               "Evaluative_phrases_positive",
               "Evaluative_phrases_negative"
               )
d2 <- d[,col_order] #put the columns in the order specified above


d2cor <- as.data.frame(cor(d2)) #creates a new correlation matrix in specified order
write.csv(d2cor,file = "CartonRosetteCorrelations.csv") #writes a new csv that includes correlation data

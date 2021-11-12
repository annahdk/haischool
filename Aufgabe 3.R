setwd("C:/Users/Romina/Documents/GitHub/haischool")

edu <- read.csv2("StudentsPerformance.csv")

attach(edu)


unique(parental.level.of.education)

parent = c("some high school", "high school", "some college", "associate's degree", 
           "bachelor's degree", "master's degree" )

parents = factor(parental.level.of.education, levels = parent, ordered = TRUE)


test.preparation.course

table(parents, test.preparation.course)

fisher.test(test.preparation.course, parents)


table(parentsedu, test.preparation.course)

table(parents, test.preparation.course)

parentsedu = c()
parentsedu[parents > "some college"] = "college degree"
parentsedu[parents <= "some college"] = "no college degree"

parentsedu

chisq.test(parentsedu, test.preparation.course)



prepN   <- sum(prep$parental.level.of.education == "some high school")
noprepN <- sum(noprep$parental.level.of.education == "some high school")
sumN <- sum(edu$parental.level.of.education == "some high school")

prepHs <- sum(prep$parental.level.of.education == "high school") + sum(prep$parental.level.of.education == "some college") 
noprepHs <- sum(noprep$parental.level.of.education == "high school") + sum(noprep$parental.level.of.education == "some college")
sumHs <- sum(edu$parental.level.of.education == "high school") + sum(edu$parental.level.of.education == "some college")

prepC <- sum(prep$parental.level.of.education == "associate's degree") + sum(prep$parental.level.of.education == "bachelor's degree") + sum(prep$parental.level.of.education == "master's degree")
noprepC <- sum(noprep$parental.level.of.education == "associate's degree") + sum(noprep$parental.level.of.education == "bachelor's degree") + sum(noprep$parental.level.of.education == "master's degree")
sumC <- sum(edu$parental.level.of.education == "associate's degree") + sum(edu$parental.level.of.education == "bachelor's degree") + sum(edu$parental.level.of.education == "master's degree")

matrix(c(prepN,noprepN,sumN,prepHs,noprepHs,sumHs,prepC,noprepC,sumC), ncol=3, byrow = T)

test <- matrix(c(prepN,noprepN,prepHs,noprepHs,prepC,noprepC), ncol=2, byrow = T)

row.names(test) <- c("kein Abschluss","High School Abschluss","College Abschluss")
colnames(test) <- c("Teilnahme am Vorkurs","Keine Teilnahme am Vorkurs")

mosaicplot(test, col=c("black","white"))

chisq.test(as.numeric(factor(edu$test.preparation.course)), as.numeric(factor(edu$parental.level.of.education)))
chisq.test(test)

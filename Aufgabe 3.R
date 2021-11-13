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


#### Alternativ nur Bachelor und Master als Gruppe

parent <- c("some high school", "high school", "some college", "associate's degree", 
           "bachelor's degree", "master's degree" )

parents <- factor(edu$parental.level.of.education, levels = parent, ordered = TRUE)

table(parents, edu$test.preparation.course)
mosaicplot(table(parents, edu$test.preparation.course), color = c("red", "blue"),
           main = "", xlab = "Schulabschluss Eltern", ylab = "Vorkurs")

fisher.test(edu$test.preparation.course, parents)
chisq.test(parents, edu$test.preparation.course)

parents2 <- edu$parental.level.of.education
parents2[parents2 %in% c("bachelor's degree", "master's degree")] <- "higher"
parent2 <- c("some high school", "high school", "some college", "associate's degree", 
            "higher")
parents2 <- factor(parents2, levels = parent2, ordered = TRUE)

table(parents2, edu$test.preparation.course)
mosaicplot(table(parents2, edu$test.preparation.course), color = c("red", "blue"),
           main = "", xlab = "Schulabschluss Eltern", ylab = "Vorkurs")

# deutsche Beschriftung 
pdf("mosaik.pdf", width = 13)
par(cex = 0.66, cex.lab = 1.5)
vorkurs_deutsch <- as.factor(edu$test.preparation.course) 
levels(vorkurs_deutsch) <- c("ja", "nein")
mosaicplot(table(parents2, vorkurs_deutsch), color = c("blue", "grey"),
          main = "", xlab = "Schulabschluss Eltern", ylab = "Vorkurs", cex.axis = 1)
dev.off()


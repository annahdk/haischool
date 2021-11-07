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

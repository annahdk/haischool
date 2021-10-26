#setwd("~/Documents/Studium/Statistik/Kurse/Fallstudien I/Projekt 2")
edu <- read.csv2("StudentsPerformance.csv")
str(edu)
summary(edu)
attach(edu)
unique(parental.level.of.education) # hoechster Abschluss der Eltern, some 
# high school/college bedeutet nicht abgeschlossen; koennen wir nach Rfolge 
# ordern, some high school < high school < some college < associate's degree < 
# bachelor's degree < master's degree 
unique(race.ethnicity) # fuenf versch. ethnische Gruppen 
any(is.na(edu)) # keine fehlenden Daten, schon bereinigt 

# fuer aufg 1 das skalenniveau beruecksichtigen. lunch und testPreperationCourse
# kann man factor und dann as.numeric um tests darauf rechnen zu koennen 

# aufgabe 2: u.scheiden sich die Leistungen in den Bereichen Mathe/Lesen/Schreiben 
# bei Absolventen, deren Essen subventioniert wird?

# annahme: 100 ist beste Note, 0 ist schlechteste Note 

length(lunch)
unique(lunch)
subabs <- edu[lunch == "free/reduced",]
length(subabs$race.ethnicity)/length(lunch) # nur 51/150, also ca. 1/3 der probanden 
# bekommt das Essen subventioniert 

attach(subabs)
length(math.score)
mean(math.score)
mean(reading.score)
mean(writing.score)

# anova fuer vergleich aller drei stichproben in der lage 
scores  <- list(math.score, reading.score, writing.score)
shapiro.test(unlist(scores))
bartlett.test(scores)
scores_frame <- data.frame(subject = rep(c("math", "reading", "writing"), 
                               c(length(math.score), length(reading.score),
                                 length(writing.score))), scores = unlist(scores))
anova(lm(scores ~ subject, scores_frame))
# p-Wert: 0.4733, also kann kein Lageuntersch. nachgewiesen werden (also folgende
# Bemerkungen eigentlich ueberfluessig)

## wenn lageunterschied nachgewiesen werden kann, paarweise t-Tests
## 
##t.test(math.score, reading.score)
##t.test(writing.score, reading.score)
##t.test(math.score, writing.score)


# Visualisierungen

# caption fuer latex: Boxplots zum Lage- und Streuungsvergleich der Noten
# von Absolventen, deren Essen subventioniert wird 

#pdf(".pdf")
boxplot(scores, names = c("Mathe", "Lesen", "Schreiben"), ylab = "Leistungen")
#dev.off()


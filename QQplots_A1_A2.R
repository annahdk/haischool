# Benoetigt: edu Datensatz
#            prepared und unprepared Datensaetze

# edu <- read.csv2("StudentsPerformance.csv")
# edu_test <- edu[,c("test.preparation.course",
#                    "math.score",
#                    "reading.score",
#                    "writing.score")]
# prepared <- split(edu_test, test.preparation.course)[[1]]
# unprepared <- split(edu_test, test.preparation.course)[[2]]


## Aufgabe 1

#pdf("QQplots_Vorkurs.pdf")
op <- par(mfrow = c(1,3), cex.main = 1.5)
opq <- par(mar = c(5,6.5,4,2) + 0.1)

qqplot(unprepared$writing.score,prepared$writing.score, main = "Schreiben", xlab = "", ylab = 
         "", ylim = c(0,100), cex.lab = 1.5)
mtext("Teilnahme am Vorbereitungskurs", line = 3, side = 2)
abline(a = 0, b = 1, col = "red")
par(opq)
qqplot(unprepared$reading.score,prepared$reading.score, main = "Lesen",cex.lab = 1.5, xlab = "", ylab = "", ylim = c(0,100))
mtext("Keine Teilnahme am Vorbereitungskurs", line = 3, side = 1)
abline(a = 0, b = 1, col = "red")
qqplot(unprepared$math.score, prepared$math.score ,main = "Mathe", xlab = "", ylab = "", ylim = c(0,100))
abline(a = 0, b = 1, col = "red")
#dev.off()



## Aufgabe 2

 #Benoetigt:
 #lunch_split <- split(edu, lunch)
 #names(lunch_split)[1] <- c("free_reduced")

 
 
 #pdf("QQplots_Lunch.pdf")
 op <- par(mfrow = c(1,3), cex.main = 1.5)
 opq <- par(mar = c(5,6.5,4,2) + 0.1)
 
 qqplot(lunch_split$free_reduced$writing.score, lunch_split$standard$writing.score, main = "Schreiben", xlab = "", ylab = 
          "", ylim = c(0,100), cex.lab = 1.5)
 mtext("Nicht subventioniertes Mittagessen", line = 3, side = 2)
 abline(a = 0, b = 1, col = "red")
 par(opq)
 qqplot(lunch_split$free_reduced$reading.score,lunch_split$standard$reading.score, main = "Lesen",cex.lab = 1.5, xlab = "", ylab = "", ylim = c(0,100))
 mtext("Subventioniertes Mittagessen", line = 3, side = 1)
 abline(a = 0, b = 1, col = "red")
 qqplot(lunch_split$free_reduced$math.score, lunch_split$standard$math.score ,main = "Mathe", xlab = "", ylab = "", ylim = c(0,100))
 abline(a = 0, b = 1, col = "red")
#dev.off()
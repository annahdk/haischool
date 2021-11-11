## Vortest-Funktion von Romina, fuer Aufgabe 2 verallgemeinert: 

# man muss hier das interessierende Fach (man kann auch alle scores zusammen-
# schmeissen) und die Gruppen, fuer deren Vergleich wir uns interessieren angeben,
# wobei die Gruppen jeweils subsets des ganzen Datensatzes nach den entspr.
# Gruppen sind

vortests.ttest.faecher <- function(score, gruppe1, gruppe2){
  
  if(score == "math"){
    gruppe1 <- gruppe1$math.score
    gruppe2 <- gruppe2$math.score
    all <- c(gruppe1, gruppe2)
  }else if(score == "reading"){
    gruppe1 <- gruppe1$reading.score
    gruppe2 <- gruppe2$reading.score
    all <- c(gruppe1, gruppe2)
  }else if(score == "writing"){
    gruppe1 <- gruppe1$writing.score
    gruppe2 <- gruppe2$writing.score
    all <- c(gruppe1, gruppe2)
  }
  else return("falsche score-Eingabe")
  
  # Shapiro-Test auf Normalverteilung, die Voraussetzung ist fuer Bartlett-Test:
  norm_gruppe1 <- shapiro.test(gruppe1)$p.value
  norm_gruppe2 <- shapiro.test(gruppe2)$p.value

  # Falls nicht erfuellt:
  if(norm_gruppe1 < 0.05 | norm_gruppe2 < 0.05){
    cat("Folgende Stichprobe ist nicht normalverteilt: \n")
    if(norm_gruppe1 < 0.05){
      cat("Gruppe 1, P-Wert:", norm_gruppe1)
    }
    if(norm_gruppe2 < 0.05){
      cat("Gruppe 2, P-Wert:", norm_gruppe2)
    }
  }
  
  # Falls doch erfuellt:
  else{
    varianzgleichheit <- bartlett.test(list(gruppe1, gruppe2))$p.value
    
    cat("Shapiro Wilk Test auf Normalverteilung: \n \n")
    cat("Gruppe 1", norm_gruppe1, "\n")
    cat("Gruppe 2", norm_gruppe2, "\n \n")
    
    cat("Bartlett Test auf Varianzgleichheit in beiden Stichproben: ", varianzgleichheit)
  }
}




## QQ-Plots fuer Normalverteilungsueberprüfung: 

qqmatrix <- function(gruppe1, gruppe2, ylab1, ylab2){
  op <- par(mfrow = c(2,3), cex.main = 1.5)
  opq <- par(mar = c(5,6.5,4,2) + 0.1)
  attach(gruppe1)
  qqnorm(writing.score, main = "Schreiben", xlab = "", ylab = 
           "Empirische Quantile", ylim = c(0,100))
  qqline(writing.score)
  title(ylab = ylab1, line = 5, cex.lab = 1.5, font.lab = 2)
  par(opq)
  qqnorm(reading.score, main = "Lesen", xlab = "", ylab = "", ylim = c(0,100))
  qqline(reading.score)
  qqnorm(math.score, main = "Mathe", xlab = "", ylab = "", ylim = c(0,100))
  qqline(math.score)
  detach(gruppe1)
  opq <- par(mar = c(5,6.5,4,2) + 0.1)
  attach(gruppe2)
  qqnorm(writing.score, xlab = "", ylab = 
           "Empirische Quantile", main = "", mar = c(5,6,4,2) + 0.1, ylim = c(0,100))
  qqline(writing.score)
  title(ylab = ylab2, line = 5, cex.lab = 1.5, font.lab = 2)
  par(opq)
  qqnorm(reading.score, xlab = "Theoretische Quantile", ylab = "", main = "",
         ylim = c(0,100))
  qqline(reading.score)
  qqnorm(math.score, xlab = "", ylab = "", main = "", ylim = c(0,100))
  qqline(math.score)
  detach(gruppe2)
  par(op)
}  

## Boxplots zur Visualisierung der Lageunterschiede 
boxpl <- function(gruppe1, gruppe2, xlab){
  op2 <- par(mfrow = c(1,3), cex.lab = 1.5, cex.main = 1.5)
  boxplot(list(gruppe1$writing.score, gruppe2$writing.score), names = c("ja", "nein"), 
          main = "Schreiben", ylim = c(0,100), ylab = "Leistung")
  boxplot(list(gruppe1$reading.score, gruppe2$reading.score), names = c("ja", "nein"), 
          xlab = xlab, main = "Lesen", ylim = c(0,100)) # range?
  boxplot(list(gruppe1$math.score, gruppe2$math.score), names = c("ja", "nein"), 
          main = "Mathe", ylim = c(0,100))
  par(op2)
}

# lunch_split <- split(edu, lunch)
# names(lunch_split)[1] <- c("free_reduced")

#pdf("qq_lunch.pdf", width = 13)
qqmatrix(lunch_split$standard, lunch_split$free_reduced, "nicht subventioniert",
         "subventioniert")
#dev.off()

#pdf("boxplots_lunch.pdf", width = 13)
boxpl(lunch_split$standard, lunch_split$free_reduced, "Subvention")
#dev.off()

#edu_test <- edu[,c("test.preparation.course", 
#                   "math.score",
#                   "reading.score",
#                   "writing.score")]

prepared <- split(edu_test, test.preparation.course)[[1]]
unprepared <- split(edu_test, test.preparation.course)[[2]]

#pdf("qq_prep.pdf", width = 13)
qqmatrix(prepared, unprepared, "kein Vorkurs", "Vorkurs")
#dev.off()


#pdf("boxplots_prep.pdf", width = 13)
boxpl(prepared, unprepared, "Vorkurs")
#dev.off()


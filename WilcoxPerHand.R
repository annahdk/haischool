### Wilcoxon Rangsummentest

edu <- read.csv("StudentsPerformance.csv", header = TRUE, sep = ";")


edu_test <- edu[,c("test.preparation.course", 
                   "math.score",
                   "reading.score",
                   "writing.score")]

prepared <- split(edu_test, edu$test.preparation.course)[[1]]
unprepared <- split(edu_test, edu$test.preparation.course)[[2]]

free <- split(edu_test, edu$lunch)[[1]]
standard <- split(edu_test, edu$lunch)[[2]]

sum(as.numeric(table(c(unique(prepared$math.score), unique(unprepared$math.score)))) == 2)
# [1] 22
## 22 Auspraegungen mit Bindungen zwischen x und y
sum(as.numeric(table(c(unique(prepared$reading.score), unique(unprepared$reading.score)))) == 2)
# [1] 26
## 26 Auspraegungen mit Bindungen zwischen x und y
sum(as.numeric(table(c(unique(prepared$writing.score), unique(unprepared$writing.score)))) == 2)
# [1] 25
## 25 Auspraegungen mit Bindungen zwischen x und y


sum(as.numeric(table(c(unique(free$math.score), unique(standard$math.score)))) == 2)
# [1] 25
## 25 Auspraegungen mit Bindungen zwischen x und y
sum(as.numeric(table(c(unique(free$reading.score), unique(standard$reading.score)))) == 2)
# [1] 23
## 23 Auspraegungen mit Bindungen zwischen x und y
sum(as.numeric(table(c(unique(free$writing.score), unique(standard$writing.score)))) == 2)
# [1] 23
## 23 Auspraegungen mit Bindungen zwischen x und y


wilcoxPerHand <- function(x, y){
  # Erstelle kombinierte Stichprobe
  comb <- c(x, y)
  # Benenne die Elemente nach dem Vektor, aus dem sie kommen
  names(comb) <- c(rep("x", length(x)), rep("y", length(y)))
  # Bilde die Raenge ueber die sortierte, kombinierte Stichprobe
  comb <- sort(comb)
  comb <- rank(comb, ties.method = "average")
  # Teststatistik berechnen
  T_X <- sum(comb[names(comb) == "x"])
  
  # Bestimme die Stichprobengroessen von x und y
  n <- length(x)
  m <- length(y)
  # Berechne den Erwartungswert der Teststatistik
  E_T <- (n * (n + m + 1)) / 2
  
  b <- 0
  for(i in 1:length(names(table(comb)))){
    b <- b + (as.numeric(table(comb)[i])^3 - as.numeric(table(comb))[i])
  }
  
  V_T <- ((n * m) / 12) * (n + m + 1 - (1 / ((n + m) * (n + m - 1) * b)))
  
  return(list("Teststatistik" = Z <- (T_X - E_T) / sqrt(V_T),
              "Quantil" = qnorm(0.975),
              "p-Wert" = 2*min(1 - pnorm(Z), pnorm(Z))))
}

wilcoxPerHand(prepared$math.score, unprepared$math.score)
wilcox.test(prepared$math.score, unprepared$math.score) 

wilcoxPerHand(prepared$reading.score, unprepared$reading.score)
wilcox.test(prepared$reading.score, unprepared$reading.score)

wilcoxPerHand(prepared$writing.score, unprepared$writing.score)
wilcox.test(prepared$writing.score, unprepared$writing.score)



wilcoxPerHand(free$math.score, standard$math.score)
wilcox.test(free$math.score, standard$math.score) 

wilcoxPerHand(free$reading.score, standard$reading.score)
wilcox.test(free$reading.score, standard$reading.score)

wilcoxPerHand(free$writing.score, standard$writing.score)
wilcox.test(free$writing.score, standard$writing.score)




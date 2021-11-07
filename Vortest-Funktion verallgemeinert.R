## Vortest-Funktion von Romina, fuer Aufgabe 2 abgeaendert: 

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
      cat("Auf den Test vorbereitet, P-Wert:", norm_gruppe1)
    }else{
      cat("Nicht auf den Test vorbereitet, P-Wert:", norm_gruppe2)
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

setwd("C:/Users/Romina/Documents/GitHub/haischool")

edu <- read.csv2("StudentsPerformance.csv")

attach(edu)

str(edu)

unique(parental.level.of.education)

unique(test.preparation.course) # nominal binär none und completed

math.score
reading.score
writing.score


edu_test <- edu[,c("test.preparation.course", 
                   "math.score",
                   "reading.score",
                   "writing.score")]

prepared <- split(edu_test, test.preparation.course)[[1]]
unprepared <- split(edu_test, test.preparation.course)[[2]]


# Vortests auf Normalverteilung

# h0: die scores bei den vorkurs teilnehmern und nicht vorkursteilnehmern sind normalverteilt
# h1: sie sind nicht normalverteilt

shapiro.test(prepared$math.score)$p.value #0.8126
shapiro.test(unprepared$math.score)$p.value #0.06949


shapiro.test(prepared$reading.score)$p.value #0.3646
shapiro.test(unprepared$reading.score)$p.value# 0.6515


shapiro.test(prepared$writing.score)$p.value #0.3036
shapiro.test(unprepared$writing.score)$p.value# 0.3569


### Alle sind normalverteilt aber math score unprepared kritisch

#Vortest (Bartlett auf Varianzgleichheit)



vortests <- function(score){
  
  if(score == "math"){
    prep <- prepared$math.score
    unprep <- unprepared$math.score
    all = edu_test$math.score
  }else if(score == "reading"){
    prep <- prepared$reading.score
    unprep <- unprepared$reading.score
    all = edu_test$reading.score
  }else if(score == "writing"){
    prep <- prepared$writing.score
    unprep <- unprepared$writing.score
    all = edu_test$writing.score
  }
 else return("falsche Eingabe")
  
  norm_prep <- shapiro.test(prep)$p.value
  norm_unprep <- shapiro.test(unprep)$p.value
  
  # Voraussetzung fuer Bartlett test ist Normalverteilung
  # Falls nicht erfuellt:
  if(norm_prep < 0.05 | norm_unprep < 0.05){
      cat("Folgende Stichprobe ist nicht normalverteilt: \n")
      if(norm_prep < 0.05){
        cat("Auf den Test vorbereitet, P-Wert:", norm_prep)
      }else{
        cat("Nicht auf den Test vorbereitet, P-Wert:", norm_unprep)
      }
        
      return()
  }# Falls doch erfuellt
  else{
    
      varianzgleichheit <- bartlett.test(all ~ edu_test$test.preparation.course)$p.value
      
    
      cat("Shapiro Wilk Test auf Normalverteilung: \n \n")
      cat("Vorbereitet: ", norm_prep, "\n")
      cat("Unvorbereitet: ", norm_unprep, "\n \n")
      
      cat("Bartlett Test auf Varianzgleichheit in beiden Stichproben: ", varianzgleichheit)
      
  }
}


vortests("math")
vortests("writing")
vortests("reading")


## Alles ueber 0.05

# Idee: zweistichproben T test mit Nullhypothese prepared > unprepared


# h0: prepared <= unprepared
# h1: prepared > unprepared

alpha = 0.05/4

alpha
# 0.0125

t.test(prepared$math.score, unprepared$math.score, alternative = "greater")$p.value
# 0.0004890269

t.test(prepared$writing.score, unprepared$writing.score, alternative = "greater")$p.value
#4.997516e-05

t.test(prepared$reading.score, unprepared$reading.score, alternative = "greater")$p.value
#0.001518616

prepared$all.scores <- apply(prepared[,2:4], 1, sum)

unprepared$all.scores <- apply(unprepared[,2:4], 1, sum)

t.test(prepared$all.scores, unprepared$all.scores, alternative = "greater")$p.value
#0.0002148819

# Nullhypothesen ueberall abgelehnt
# Vorkurs lohnt sich doch



# in welchem fach ist der Effekt am groessten?


#Eingabe: Vektor der score
#Ausgabe: score aufgeteilt in A-F

grading <- function(x){
  
  res <- c()
  j <- 1
  
  for (i in x) {
    
    if(i %in% 0:58) res[j] <- "F"
    if(i %in% 59:69) res[j] <- "D"
    if(i %in% 70:79) res[j] <- "C"
    if(i %in% 80:89) res[j] <- "B"
    if(i %in% 90:100) res[j] <- "A"
    
    j = j + 1
    
  }  
  
  return(res)
}

edu_test_letter <- apply(edu_test[,2:4], 2, grading )

edu_test_letter <- data.frame(edu_test_letter)

edu_test_letter$test.preparation <- edu_test$test.preparation.course

plot(table(edu_test_letter$math.score, edu_test_letter$test.preparation))



#bartlett
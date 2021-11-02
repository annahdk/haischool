#setwd("~/Documents/Studium/Statistik/Kurse/Fallstudien I/Projekt 2")
edu <- read.csv2("StudentsPerformance.csv")
attach(edu)

## weitere Notizen: 
unique(parental.level.of.education) # hoechster Abschluss der Eltern: some 
# high school/college bedeutet nicht abgeschlossen; koennen wir nach Rfolge 
# ordern, some high school < high school < some college < associate's degree < 
# bachelor's degree < master's degree 
unique(race.ethnicity) # fuenf versch. ethnische Gruppen 
any(is.na(edu)) # keine fehlenden Daten, schon bereinigt 

# fuer aufg 1 das skalenniveau beruecksichtigen. lunch und testPreperationCourse
# kann man factor und dann as.numeric um tests darauf rechnen zu koennen 

## aufgabe 2: u.scheiden sich die Leistungen in den Bereichen Mathe/Lesen/Schreiben 
## zwischen Schuelern, deren Essen subventioniert wird und Schuelern, die keine
## Unterstuetzung bekommen? 

# annahme: 100 ist beste Note, 0 ist schlechteste Note 

length(lunch) # 150 probanden
unique(lunch) # subventioniert vs. nicht subventioniert (2 Auspraegungen)

# Datensatz nach den beiden Gruppen aufteilen:
lunch_split <- split(edu, lunch) 
names(lunch_split)[1] <- c("free_reduced")
attach(lunch_split)

length(free_reduced$race.ethnicity)/length(edu$lunch) # nur 51/150, also ca. 
# 1/3 der probanden bekommt das Essen subventioniert 

# selbst erstmal Ueberblick ueber Mittelwerte in den einzelnen Faechern je nach 
# lunch-Gruppe verschaffen: 
attach(free_reduced)
mean(math.score) # 56
mean(reading.score) # 60
mean(writing.score) # 59
detach(free_reduced)

attach(standard)
mean(math.score) # 68 
mean(reading.score) # 70
mean(writing.score) # 69
detach(standard)

# die nicht-subventionierten Schueler haben im Schnitt 10 Score-Punkte
# mehr in jedem Fach 

## Vortests auf Normalverteilung und gleiche Varianz fuer T-Test

# denn Annahme bei t-Test: Das untersuchte Merkmal ist in den Grundgesamtheiten 
# der beiden Gruppen normalverteilt mit gleichen Varianzen

# Rominas verallgemeinerte Vortest-Funktion:

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
    cat("p-Wert Gruppe 1", norm_gruppe1, "\n")
    cat("p-Wert Gruppe 2", norm_gruppe2, "\n \n")
    
    cat("Bartlett Test auf Varianzgleichheit in beiden Stichproben: \n p-Wert", 
        varianzgleichheit)
  }
}

vortests.ttest.faecher("math", standard, free_reduced)
vortests.ttest.faecher("writing", standard, free_reduced)
vortests.ttest.faecher("reading", standard, free_reduced)

# die Annahmen koennen beibehalten werden

## T-Tests zum Lagevergleich der Noten von subventionierten und nicht
## subventionierten Schuelern 

# weitere Annahme: die Noten der subventionierten haengen nicht von den Noten der 
# nicht-subventionierten Schueler ab 

t.test(free_reduced$math.score, standard$math.score)
t.test(free_reduced$reading.score, standard$reading.score)
t.test(free_reduced$writing.score, standard$writing.score)

# alle p-Werte unter 0.05 -> Lageunterschied signifikant 
# keine Adjustierung des p-Wertes, da nicht die selben Daten mehrmals benutzt (?)

# man kann auch Unterschied generell, also faecherunspezifisch testen: 

attach(free_reduced)
total_free_reduced <- c(math.score, reading.score, writing.score)
detach(free_reduced)

attach(standard)
total_standard <- c(math.score, reading.score, writing.score)
detach(standard)

shapiro.test(total_free_reduced)
shapiro.test(total_standard)
bartlett.test(list(total_free_reduced, total_standard)) # hier koennen keine
# gleichen Varianzen mehr angenommen werden (vielleicht Ausreisser?)

# t.test(total_free_reduced, total_standard) 
# signifikanter unterschied (hier muesste man jetzt aber adjustieren, wenn man
# wirklich beide Tests macht)


# Visualisierungen

# caption fuer latex: Boxplots zum Lage- und Streuungsvergleich der Noten
# von Absolventen, deren Essen subventioniert wird 

#pdf(".pdf")
#boxplot(scores, names = c("Mathe", "Lesen", "Schreiben"), ylab = "Leistungen")
#dev.off()


#setwd("~/Documents/Studium/Statistik/Kurse/Fallstudien I/Projekt 2")
edu <- read.csv2("StudentsPerformance.csv")
attach(edu)


## Datensatz-Notizen: 
unique(parental.level.of.education) # hoechster Abschluss der Eltern: some 
# high school/college bedeutet nicht abgeschlossen; koennten wir nach Rfolge 
# ordern, some high school < high school < some college < associate's degree < 
# bachelor's degree < master's degree 
unique(race.ethnicity) # fuenf versch. ethnische Gruppen 
any(is.na(edu)) # keine fehlenden Daten, schon bereinigt 

# fuer aufg 1 das skalenniveau beruecksichtigen. lunch und testPreperationCourse
# kann man als factor umwandeln und dann as.numeric um tests darauf rechnen zu koennen 


## Aufgabe 2: u.scheiden sich die Leistungen in den Bereichen Mathe/Lesen/Schreiben 
## zwischen Schuelern, deren Essen subventioniert wird und Schuelern, die keine
## Unterstuetzung bekommen? 

# annahme: 100 ist beste Note, 0 ist schlechteste Note 

length(lunch) # 150 probanden
unique(lunch) # subventioniert vs. nicht subventioniert (2 Auspraegungen)

# Datensatz nach den beiden Gruppen aufteilen:
lunch_split <- split(edu, lunch) 
names(lunch_split)[1] <- c("free_reduced") # so einfacher zu coden
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

# --> die nicht-subventionierten Schueler haben im Schnitt 10 Score-Punkte
# mehr in jedem Fach 

## Vortests auf Normalverteilung und gleiche Varianz fuer T-Test
# (denn Annahme bei t-Test: Das untersuchte Merkmal ist in den Grundgesamtheiten 
# der beiden Gruppen normalverteilt mit gleichen Varianzen)

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

# -> p-Werte ueber 0.05 bei lesen und schreiben, die Annahmen koennen hier
# beibehalten werden, bei Mathe ist p-Wert des Bartlett-Tests bei 0.023,
# hier muss Alternative zum T-Test her

## Visualisierung der Normalverteilungsueberpruefung

# Histogramme mit eingezeichneter Normalverteilungskurve (kann sein dass hier 
# noch ein Fehler drin steckt)

hist.faecher <- function(x){
  hist(x, xlim = c(0,150), freq = FALSE)
  curve(dnorm(x, mean(x), sd(x)), add = TRUE)
}

op <- par(mfrow = c(2,3))
attach(standard)
hist.faecher(writing.score)
hist.faecher(reading.score)
hist.faecher(math.score)
detach(standard)
attach(free_reduced)
hist.faecher(writing.score)
hist.faecher(reading.score)
hist.faecher(math.score)
detach(free_reduced)

# QQ-Plots

attach(standard)
qqnorm(writing.score)
qqline(writing.score)
qqnorm(reading.score)
qqline(reading.score)
qqnorm(math.score)
qqline(nath.score)
detach(standard)
attach(free_reduced)
qqnorm(writing.score)
qqline(writing.score)
qqnorm(reading.score)
qqline(reading.score)
qqnorm(math.score)
qqline(math.score)
detach(free_reduced)
par(op)



## T-Tests zum Lagevergleich der Noten von subventionierten und nicht
## subventionierten Schuelern (fuer Schreiben und Lesen)

# weitere Annahme: die Noten der subventionierten haengen nicht von den Noten der 
# nicht-subventionierten Schueler ab 

t.test(free_reduced$reading.score, standard$reading.score)
t.test(free_reduced$writing.score, standard$writing.score)

# beide p-Werte unter 0.05 -> Lageunterschied signifikant 
# keine Adjustierung des p-Wertes, da nicht die selben Daten mehrmals benutzt (?)

## Lagevergleich fuer nicht normalverteilte Stichproben für das Fach Mathe 




## man koennte auch Unterschied generell, also faecherunspezifisch testen: 

attach(free_reduced)
total_free_reduced <- c(math.score, reading.score, writing.score)
detach(free_reduced)

attach(standard)
total_standard <- c(math.score, reading.score, writing.score)
detach(standard)

shapiro.test(total_free_reduced) 
shapiro.test(total_standard)
# Normalverteilung --> check 
bartlett.test(list(total_free_reduced, total_standard)) 
# hier koennen aber keine gleichen Varianzen angenommen werden (wohl wegen Mathe)
# anderer Test auf Lagevergleich:


## Visualisierungen

# caption fuer latex: Boxplots zum Lage- und Streuungsvergleich Schulleistungen 
# von Schuelern mit und ohne Subvention des Mittagessens

#pdf("boxplots_lunch.pdf")

# Vergleich aller Faecher zusammen:
# boxplot(list(total_free_reduced, total_standard), names = c("ja", "nein"), 
        xlab = "Subvention", ylab = "Leistungen", main = "Vergleich von Schülern
        mit und ohne staatliche Unterstützung") 

# Vergleich nach Faechern:
op <- par(mfrow = c(1,3), oma = c(0,0,2,0))
boxplot(list(free_reduced$reading.score, standard$reading.score), names = c("ja", "nein"), 
        xlab = "Subvention", ylab = "Leistungen", main = "Lesen")
boxplot(list(free_reduced$writing.score, standard$writing.score), names = c("ja", "nein"), 
        xlab = "Subvention", ylab = "Leistungen", main = "Schreiben")
boxplot(list(free_reduced$math.score, standard$math.score), names = c("ja", "nein"), 
        xlab = "Subvention", ylab = "Leistungen", main = "Mathe")
title("Vergleich von Schülern
        mit und ohne staatliche Unterstützung", outer = TRUE)
par(op)
#dev.off()




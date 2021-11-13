

## Boxplots mit quantilen wie wir sie definiert haben


# benoetige:
#library(ggplot2)
# edu <- read.csv2("StudentsPerformance.csv")
# edu_test <- edu[,c("test.preparation.course",
#                    "math.score",
#                    "reading.score",
#                    "writing.score")]
# prepared <- split(edu_test, test.preparation.course)[[1]]
# unprepared <- split(edu_test, test.preparation.course)[[2]]



#unprep dann prep
mini = c(apply(unprepared[,2:4],2, min),apply(prepared[,2:4],2, min))
 
quants = c(apply(unprepared[,2:4],2, quantile, 0.25, type = 2),
           apply(prepared[,2:4],2, quantile, 0.25, type = 2))

med = c(apply(unprepared[,2:4], 2, median),apply(prepared[,2:4], 2, median))

quants75 = c(apply(unprepared[,2:4],2, quantile, 0.75, type = 2),
             apply(prepared[,2:4],2, quantile, 0.75, type = 2))

maxi =  c(apply(unprepared[,2:4],2, max),apply(prepared[,2:4],2, max))

df <- data.frame(
  fach = c(rep(c("Mathematik", "Lesen", "Schreiben"), 2)),
  x = rep(c("nein"," ja"), each = 3),
  y0 = mini,
  y25 = quants,
  y50 = med,
  y75 = quants75,
  y100 = maxi
)

ggplot(data = df)+
  geom_boxplot(aes(x = x, ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
               stat = "identity")+
  xlab("Teilnahme am Vorbereitungskurs")+
  ylab("Testergebnisse")+
  facet_grid(~fach)
  
  
  
  

red = lunch_split$free_reduced[,c("math.score","reading.score","writing.score")]

nor = lunch_split$standard[,c("math.score","reading.score","writing.score")]
  
mini_lunch = c(apply(red,2, min),apply(nor,2, min))

quants_lunch = c(apply(red,2, quantile, 0.25, type = 2),
           apply(nor,2, quantile, 0.25, type = 2))

med_lunch = c(apply(red, 2, median),apply(nor, 2, median))

quants75_lunch = c(apply(red,2, quantile, 0.75, type = 2),
             apply(nor,2, quantile, 0.75, type = 2))

maxi_lunch =  c(apply(red,2, max),apply(nor,2, max))

df2 <- data.frame(
  fach = c(rep(c("Mathematik", "Lesen", "Schreiben"), 2)),
  x = rep(c("nein"," ja"), each = 3),
  y0 = mini_lunch,
  y25 = quants_lunch,
  y50 = med_lunch,
  y75 = quants75_lunch,
  y100 = maxi_lunch
)

ggplot(data = df2)+
  geom_boxplot(aes(x = x, ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
               stat = "identity")+
  xlab("Subventioniertes Mittagessen")+
  ylab("Testergebnisse")+
  facet_grid(~fach)

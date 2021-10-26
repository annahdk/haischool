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

# Idee: T test mit Nullhypothese prepared > unprepared


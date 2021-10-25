############################################### Slide 1	
############################################### Slide 2	
############################################### Slide 3	
############################################### Slide 4	
############################################### Slide 5	
############################################### Slide 6	
############################################### Slide 7	
############################################### Slide 8	
############################################### Slide 9	
library(EdSurvey)	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", 	
                            package="NAEPprimer"))	
	
downloadTIMSS(years = c(2019), root = "C:/")	
	
TIMSS19<- readTIMSS(paste0(edsurveyHome, "TIMSS/2019"), 	
                    countries = c("usa"), gradeLvl = "4")	
	
############################################### Slide 10	
library(EdSurvey)	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", 	
                            package="NAEPprimer"))	
edsurveyHome <- "C:/"	
	
TIMSS19<- readTIMSS(paste0(edsurveyHome, "TIMSS/2019"), 	
                    countries = c("usa"), gradeLvl = "4")	
	
showCutPoints(sdf)	
showCutPoints(TIMSS19)	
############################################### Slide 11	
ach <- achievementLevels("composite", data = sdf, 	
                         returnCumulative = TRUE)	
ach	
############################################### Slide 12	
ach$discrete
ach$cumulative
achievementLevels("composite", data = sdf, 
                  returnCumulative = TRUE, 
                  cutpoints = c(250,300,350))
############################################### Slide 13	
achievementLevels("mmat", data = TIMSS19, returnCumulative = TRUE)	
############################################### Slide 14	
ach1 <- achievementLevels(c("composite", "dsex"), data = sdf)	
ach1$discrete	
############################################### Slide 15	
ach2 <- achievementLevels(c("composite", "dsex"), 	
                          aggregateBy = "dsex", data = sdf)	
ach2$discrete	
############################################### Slide 16	
ach3 <- achievementLevels(c("composite", "dsex"), 	
                          aggregateBy = "composite", data = sdf)	
ach3$discrete	
############################################### Slide 17	
dsex_iep <- achievementLevels(c("composite", "dsex", "iep"),	
                              aggregateBy = c("dsex", "iep"), 	
                              data = sdf)	
searchSDF("dsex",data = sdf, levels = TRUE)	
searchSDF("iep",data = sdf, levels = TRUE)	
############################################### Slide 18	
dsex_iep$discrete	
############################################### Slide 19	
############################################### Slide 20
help(package = "EdSurvey")
searchSDF("text", sdf)
levelsSDF("myvar", sdf)
############################################### Slide 21
exerciseAL <- achievementLevels(c("composite", "b018201"), aggregateBy = c("b018201"),
                                data = sdf, returnCumulative = TRUE)
exerciseAL$cumulative	
############################################### Slide 22
############################################### Slide 23
############################################### Slide 24
############################################### Slide 25
############################################### Slide 26
############################################### Slide 27
mathGap <- gap(variable = "composite", data = sdf,	
               groupA = dsex == "Male", groupB = dsex == "Female")	
############################################### Slide 28	
mathGap$results
############################################### Slide 29	
mathGap$percentage
############################################### Slide 30	
mathGap2 <- gap(variable = "composite", data = sdf,
               groupA = dsex %in% "Male", groupB = dsex %in% "Female",
               achievementLevel = c("Basic", "Proficient", "Advanced"))
mathGap2$results
############################################### Slide 31
mathGap3 <- gap(variable = "composite", data = sdf,
               groupA = dsex %in% "Male", groupB = dsex %in% "Female",
               percentiles = c(25, 50, 75))
mathGap3$results
############################################### Slide 32
############################################### Slide 33
downloadTIMSS(year=c(2019, 2015, 2011), root = "C:/")
TIMSS11<- readTIMSS("C:/TIMSS/2011", 
                    countries = c("usa"), gradeLvl = "4")
TIMSS15<- readTIMSS("C:/TIMSS/2015", 
                    countries = c("usa"), gradeLvl = "4")
TIMSS19<- readTIMSS("C:/TIMSS/2019", 
                    countries = c("usa"), gradeLvl = "4")
############################################### Slide 34
trend <- edsurvey.data.frame.list(list(TIMSS19, TIMSS15, TIMSS11), 
                  labels=c("TIMSS 2019", "TIMSS 2015", "TIMSS 2011"))
############################################### Slide 35
#check the consistency of the gender variable
searchSDF("itsex", trend, level=TRUE)
TIMSS11_recode <- recode.sdf(TIMSS11,
                      recode = list(itsex = list(from = "GIRL",
                                                 to = "FEMALE"),
                                    itsex = list(from = "BOY",
                                                 to = "MALE")))
trend2 <- edsurvey.data.frame.list(list(TIMSS19, TIMSS15, TIMSS11_recode), 
                      labels=c("TIMSS 2019", "TIMSS 2015", "TIMSS 2011"))
############################################### Slide 36
mathGap4 <- gap(variable = 'mmat', data = trend2) 
mathGap4$results
############################################### Slide 37
mathGap5 <- gap(variable = 'mmat', data = trend, 
                referenceDataIndex = 2) 
mathGap5$results
############################################### Slide 38
trendGap <- gap(variable = "mmat", 
                data = trend2,
                groupA = itsex %in% "MALE", 
                groupB = itsex %in% "FEMALE")
trendGap$results
############################################### Slide 39
############################################### Slide 40
# for reference
help(package = "EdSurvey")
searchSDF("text", sdf)
levelsSDF("myvar", sdf)
############################################### Slide 41
TIMSS19USA<- readTIMSS(path = "C:/TIMSS/2019/", countries = c("usa"), gradeLvl = "4")
TIMSS19FIN<- readTIMSS(path = "C:/TIMSS/2019/", countries = c("fin"), gradeLvl = "4")
TIMSS19HKG<- readTIMSS(path = "C:/TIMSS/2019/", countries = c("hkg"), gradeLvl = "4")
trend3 <- edsurvey.data.frame.list(list(TIMSS19USA, TIMSS19FIN, TIMSS19HKG))
############################################### Slide 42
trendGap <- gap(variable = "mmat", 
                data = trend3,
                groupA = itsex %in% "MALE", 
                groupB = itsex %in% "FEMALE")
trendGap$results
############################################### Slide 43
############################################### Slide 44
############################################### Slide 45
vignette("introduction", package="EdSurvey")
# There are additional functions that we couldn't cover!
gap() #gap analysis
cor.sdf() # Bivariate correlations using "Pearson", "Spearman", "polychoric", or "polyserial" methods
edsurveyTable2pdf() # creating production ready summary tables
cbind(), rbind(), append(), merge() # useful functions in processing data
help(package = "EdSurvey")
############################################### Slide 46
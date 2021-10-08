############################################### Slide 1	
############################################### Slide 2	
############################################### Slide 3	
############################################### Slide 4	
############################################### Slide 5	
############################################### Slide 6	
############################################### Slide 7	
############################################### Slide 8	
############################################### Slide 9	
############################################### Slide 10	
############################################### Slide 11	
############################################### Slide 12	
############################################### Slide 13	
############################################### Slide 14	
############################################### Slide 15	
# this line is not executed	
x <- 12	
x	
j <- 12	
J	
############################################### Slide 16	
?mean	
############################################### Slide 17	
colors <- c("red", "green", "blue")	
colors	
numbers <- c(1, 2, 3)	
numbers	
############################################### Slide 18	
mean(x = numbers)	
mean(numbers)	
############################################### Slide 19	
############################################### Slide 20	
# to install the package	
install.packages("EdSurvey")	
# to load the package	
library(EdSurvey)	
############################################### Slide 21	
vignette("introduction", package="EdSurvey")	
help(package = "EdSurvey")	
############################################### Slide 22	
############################################### Slide 23	
############################################### Slide 24	
############################################### Slide 25	
############################################### Slide 26	
############################################### Slide 27	
############################################### Slide 28	
############################################### Slide 29	
############################################### Slide 30	
############################################### Slide 31	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))	
############################################### Slide 32	
math17 <- readNAEP("//path_to_directory/Data/M48NT2AT.dat")	
############################################### Slide 33	
############################################### Slide 34	
?downloadTIMSS() #view documentation	

#This will save the 2015 and 2019 TIMSS data	
#To the '~' folder (user's 'home' folder)	
downloadTIMSS(root = "~", years = c(2015, 2019))	
############################################### Slide 35	
############################################### Slide 36	
#note the previous 'downloadTIMSS' function created 'TIMSS/2019' subfolder in the root folder.	
usaGr4 <- readTIMSS("~/TIMSS/2019", 	
                    countries = "usa", 	
                    gradeLvl = 4)	
gr8esdfl <- readTIMSS("~/TIMSS/2019", 	
                      countries = c("usa", "sgp", "nor"), 	
                      gradeLvl = 8)	
############################################### Slide 37	
############################################### Slide 38	
############################################### Slide 39	
############################################### Slide 40	
print(sdf)	
############################################### Slide 41	
dim(sdf)	
############################################### Slide 42	
colnames(sdf)	
############################################### Slide 43	
searchSDF("education", sdf)	
searchSDF("b003501", sdf, levels = TRUE)	
searchSDF("", sdf)	
############################################### Slide 44	
levelsSDF("b018201", sdf)	
############################################### Slide 45	
showCodebook(sdf)	
book <- showCodebook(sdf)	
View(showCodebook(sdf))	
############################################### Slide 46	
showPlausibleValues(sdf)	
showPlausibleValues(sdf, verbose = TRUE)	
############################################### Slide 47	
showWeights(sdf)	
showWeights(sdf, verbose = TRUE)	
############################################### Slide 48	
############################################### Slide 49	
############################################### Slide 50	
############################################### Slide 51	
############################################### Slide 52	
############################################### Slide 53	
gddat <- getData(sdf, varnames = c('dsex', 'sdracem', 'b018201', 'b017451',	
                                   'composite', 'geometry', 'origwt'),	
                 addAttributes = TRUE, omittedLevels = FALSE)	
############################################### Slide 54	
# Note: head returns the first 6 rows of a data frame	
head(gddat)	
############################################### Slide 55	
gddat <- getData(sdf, varnames = c('dsex', 'sdracem', 'b018201', 'b017451',	
                                   'composite', 'geometry', 'origwt'),	
                 addAttributes = TRUE, omittedLevels = FALSE)	
############################################### Slide 56	
subsetSDF <- subset(gddat, dsex %in% c("Male"))	
dim(sdf)	
dim(subsetSDF)	
############################################### Slide 57	
############################################### Slide 58	
library(dplyr)	
# get data 	
gddat <- getData(sdf, varnames = c('dsex', 'sdracem', 'b018201', 'b017451',	
                                   'composite', 'geometry', 'origwt'),	
                 addAttributes = FALSE, omittedLevels = FALSE)	
# subset with dplyr 	
subsetSDF <- gddat %>% filter(sdracem != "White")	
head(subsetSDF[,1:10], 6)	
############################################### Slide 59	
showWeights(subsetSDF)	
names(attributes(subsetSDF))	
############################################### Slide 60	
subsetSDF <- gddat %>% 	
  filter(sdracem != "White") %>%  	
  rebindAttributes(sdf) # adding rebindAttributes	
showWeights(subsetSDF)	
names(attributes(subsetSDF))	
############################################### Slide 61	
############################################### Slide 62	
sdf2 <- recode.sdf(sdf, recode =	
                     list(b017451 = 	
                            list(from = c("Never or hardly ever", "Once every few weeks"),	
                                 to = c("Infrequently"))	
                     )	
)	
searchSDF("b017451", sdf2, levels = TRUE)	
############################################### Slide 63	
sdf2 <- rename.sdf(sdf2, oldnames = "b017451",	
                   newnames = "studytalkfrequency")	
searchSDF("studytalkfrequency", sdf2, levels = TRUE)	
############################################### Slide 64	
############################################### Slide 65	
############################################### Slide 66	
############################################### Slide 67	
############################################### Slide 68	
# R code for loading the EdSurvey package and reading in NAEP data	
library(EdSurvey)	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))	
# R code for accessing specified NAEP data 	
gddat <- getData(sdf,	
                 c('dsex', 'sdracem', 'pared', 'b017451',	
                   'composite', 'geometry', 'origwt'),	
                 omittedLevels = FALSE)	
############################################### Slide 69	
# R code for loading ggplot2	
install.packages("ggplot2") # only necessary once, or if updating	
# R code for loading ggplot2	
library(ggplot2)	
############################################### Slide 70	
############################################### Slide 71	
# R code generating Figure 1	
bar1 <- ggplot(data=gddat, aes(x=b017451)) +	
  geom_bar() +	
  coord_flip() +	
  labs(title = "Figure 1") 	
bar1	
############################################### Slide 72	
# R code for generating Figure 2	
bar2 <- ggplot(data=gddat, aes(x=b017451, fill=dsex)) +	
  geom_bar(position='dodge') +	
  coord_flip() +	
  labs(title = "Figure 2")	
bar2	
############################################### Slide 73	
# R code for generating Figure 3	
hist1 <- ggplot(gddat, aes(x=mrpcm1)) +	
  geom_histogram(bins=30) + # geom type changed	
  labs(title = "Figure 3")	
hist1	
############################################### Slide 74	
hist2 <- ggplot(gddat, aes(x=mrpcm1, fill = dsex)) +	
  geom_histogram(position="identity", alpha = 0.6, bins=30) + # alpha adjust color transparency	
  labs(title = "Figure 4") + 	
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) # scale fill can change default colors	
hist2	
############################################### Slide 75	
ggplot(gddat, aes(x=mrpcm1)) +	
  geom_histogram(position="identity", alpha = 0.6, bins=30) + # alpha adjust color transparency	
  geom_vline(aes(xintercept = mean(mrpcm1)),col='red',size=1) + # add mean line	
  theme_minimal() + 	
  labs(y = "count") + 	
  facet_wrap(~ sdracem, ncol = 3 ) # great graph faceted by group 	
############################################### Slide 76	
# R code for generating Figure 4	
box1 <- ggplot(gddat, aes(x=sdracem, y=mrpcm1)) + 	
  geom_boxplot() +	
  stat_summary(fun=mean, geom="point", shape=23, size=4) + # transorm data in function	
  coord_flip() +	
  labs(title = "Figure 4") 	
box1	
############################################### Slide 77	
# R code for generating Figure 4	
box1 <- ggplot(gddat, aes(x=sdracem, y=mrpcm1), fill = dsex) + 	
  geom_boxplot(aes(fill = dsex)) +	
  stat_summary(fun=mean, geom="point", shape=23, size=4) + # transorm data in function	
  coord_flip() +	
  labs(title = "Figure 4") 	
box1	
############################################### Slide 78	
############################################### Slide 79	
ggplot(gddat, aes(x=mrpcm1, fill = dsex)) +	
  geom_histogram(position="identity", alpha = 0.6, bins=30) + # alpha adjust color transparency	
  geom_vline(aes(xintercept = mean(mrpcm1)),col='red',size=1) + # add mean line	
  theme_minimal() + 	
  labs(y = "count") + 	
  facet_wrap(~ sdracem, ncol = 3) # great graph faceted by group 	
############################################### Slide 80	
############################################### Slide 81	
# R code for generating Figure 8	
contourPlot(x = gddat$mrpcm1, y = gddat$mrps31, xlab = "mrpcm1", 	
            ylab = "mrps31", main="Figure 8")	
############################################### Slide 82	
# R code for estimating the linear regression model	
summary(lm1 <- lm.sdf(composite ~ pared, sdf))	
############################################### Slide 83	
# R code for generating Figure 9	
contourPlot(x=lm1$fitted.values, y=lm1$residuals[,1], 	
            xlab="fitted values", ylab="residuals", main="Figure 9")	

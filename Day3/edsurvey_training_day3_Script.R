############################################### Slide 1	
############################################### Slide 2	
############################################### Slide 3	
############################################### Slide 4	
# to load the package	
library(EdSurvey)	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat",	
                            package = "NAEPprimer"))	
############################################### Slide 5	
############################################### Slide 6	
############################################### Slide 7	
summary2(sdf, "composite")	
############################################### Slide 8	
summary2(sdf, "composite", weightVar = NULL)	
############################################### Slide 9	
summary2(sdf, "b017451")	
############################################### Slide 10	
summary2(sdf, "b017451", omittedLevels = TRUE)	
############################################### Slide 11	
############################################### Slide 12	
es1 <- edsurveyTable(composite ~ dsex + b017451, data = sdf)	
library(knitr)	
library(kableExtra)	
kable(es1$data, format="html") %>%	
  kable_styling(font_size = 16) %>%	
  scroll_box(width="100%", height = "30%")	
############################################### Slide 13	
es2 <- edsurveyTable(composite ~ dsex + b017451, data = sdf, pctAggregationLevel = 0)	
library(knitr)	
library(kableExtra)	
kable(es2$data, format="html") %>%	
  kable_styling(font_size = 16) %>%	
  scroll_box(width="100%", height = "75%")	
############################################### Slide 14	
############################################### Slide 15	
edexercise <- edsurveyTable(composite ~ iep + b013801,	
                            weightVar = 'origwt', data = sdf)	
edexercise	
############################################### Slide 16	
############################################### Slide 17	
############################################### Slide 18	
############################################### Slide 19	
library(EdSurvey)	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))	
lm1 <- lm.sdf(composite ~ b017451,	
              weightVar = 'origwt', data = sdf)	
summary(lm1)	
############################################### Slide 20	
lm2 <- lm.sdf(composite ~ dsex + b017451, 	
              weightVar = 'origwt', data = sdf)	
############################################### Slide 21	
summary(lm2)	
############################################### Slide 22	
summary(lm2, src = TRUE)	
############################################### Slide 23	
lm3 <- lm.sdf(composite ~ dsex + b017451, 	
              weightVar = 'origwt',	
              relevels = list(dsex = "Female"), data = sdf)	
############################################### Slide 24	
summary(lm3)	
############################################### Slide 25	
############################################### Slide 26	
lmexercise2 <- lm.sdf(composite ~ b017101 + b018201,	
                      weightVar = 'origwt', data = sdf)	
summary(lmexercise2)	
############################################### Slide 27	
############################################### Slide 28	
m1 <- mml.sdf(algebra ~ dsex + b013801, sdf, weightVar='origwt')	
summary(m1)	
############################################### Slide 29	
############################################### Slide 30	
# 25th, 50th and 75th percentiles	
per <- percentile("composite", percentiles = c(25,50,75), data = sdf)	
per	
############################################### Slide 31	
# note df/se at 0 and 100. We would not report these.	
per <- percentile("composite", percentiles = c(0:100), data = sdf)	
library(knitr)	
library(kableExtra)	
kable(per, format="html") %>%	
  kable_styling(font_size = 16) %>%	
  scroll_box(width="100%", height = "60%")	
############################################### Slide 32	
############################################### Slide 33	
sexes <- levels( sdf$dsex )	
sexes #  lapply to pass each sex to subset() using an anon function 	
sex_list <- lapply( sexes , function(each_sex) {	
                                               subset(sdf, subset=dsex==each_sex)})	
# create an edsurvey df list, labelling each subset	
esdflist <- edsurvey.data.frame.list(sex_list, labels= sexes)	
              	
percentile("algebra", percentiles = c(5,95), weightVar = "origwt", esdflist)	
############################################### Slide 34	
############################################### Slide 35	
invisible(lapply(c("EdSurvey", "ggplot2", "RColorBrewer"), library, character.only = TRUE))	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))	
# make a regular data frame, with some columns we may be interested in	
allvars <- colnames(sdf)	
somevars <- allvars[c(1:10, grep(sdf$stratumVar , allvars),grep(sdf$psuVar , allvars), grep("rpcm", allvars), grep("wt", allvars))]	
df <- getData(data=sdf, varnames=somevars)	
# for demo purposes, add a random continuous IV that varies by sex	
rand_vals <- list(df$mrpcm1 * pmax(.01, rnorm(1:nrow(df),mean=7,sd=2.5)), df$mrpcm1 * pmax(.01, rnorm(1:nrow(df),mean=7,sd=3)))	
df$stu_rand[df$dsex==1] <- df$mrpcm1[df$dsex==1]/7 + rand_vals[[1]][df$dsex==1]	
df$stu_rand[df$dsex==2] <- df$mrpcm1[df$dsex==2]/8 + rand_vals[[2]][df$dsex==2]	
# ggplot will plot quantile regression lines. see ggplot2::geom_quantile	
ggplot(df, aes(y=mrpcm1, x=stu_rand, color=dsex)) + geom_point(alpha=.1) + scale_color_hue(direction=-1) +	
  geom_quantile(aes(y=mrpcm1, x=stu_rand, color=dsex ), quantiles= c(.01, .25, .75, .99), size=1, inherit.aes=FALSE) +theme_bw()  	
############################################### Slide 36	
############################################### Slide 37	
############################################### Slide 38	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))	
# conduct quantile regression at a given tau value (by default, tau is set to be 0.5) 	
rq1 <- rq.sdf(composite ~ dsex + b017451, data=sdf, tau = .75)	
summary(rq1)	
############################################### Slide 39	
# convert the data frame back to an edsurvey data frame using rebindAttributes	
new_sdf <- rebindAttributes(df, sdf)	
# typically, one is interested in multiple quantiles, which can be achieved using a for loop.	
taus <- c(.01, .25, .50, .75, .99)	
# set color palette (blue for boys, red for girls), go slightly darker for clarity 	
shades <- c(RColorBrewer::brewer.pal(length(taus)+2, "Blues")[1:length(taus)+2], 	
            RColorBrewer::brewer.pal(length(taus)+2, "Reds")[1:length(taus)+2])	
############################################### Slide 40	
plot(df$stu_rand, df$mrpcm1, col= alpha(c("blue","pink")[df$dsex], .33), cex=.5)	
for( tau_n in 1:length(taus)) {                   # tau_n = 1,2,3... to number of tau values which is given by length(taus)	
  for (sex_n in 1:length(levels(df$dsex))) {      # sex_n = 1 for males (dsex[1]) , 2 for females (dsex[2])	
    	
    rq <- rq.sdf(composite ~ stu_rand,  data=new_sdf[new_sdf$dsex==sex_n,], tau=taus[tau_n])	
    abline(rq$coef, lwd=2.5, col=shades[((sex_n-1)*5) + tau_n])	
    	
    if (tau_n==1 & sex_n==1) {  # add text to margins and stack output 	
      mtext(line=sex_n, adj=0, text=paste(levels(df$dsex)[sex_n]," "), col= "black", cex=.8)	
      mtext(line=sex_n, adj=tau_n*.21-.1, text=bquote(tau ~ "=" ~ .(taus[tau_n])), col= shades[((sex_n-1)*5) + tau_n])	
      models <- data.frame("sex"=levels(df$dsex)[sex_n], "tau"=taus[tau_n], "term"=rownames(rq$coefmat), rq$coefmat)    	
    } else {	
      mtext( line=sex_n, adj=tau_n*.21-.1, text=bquote(tau ~ "=" ~ .(taus[tau_n])), col= shades[((sex_n-1)*5) + tau_n])	
      models <- rbind(models, data.frame("sex"=levels(df$dsex)[sex_n], "tau"= taus[tau_n], "term"=rownames(rq$coefmat), rq$coefmat)) }	
  }	
}	
############################################### Slide 41	
############################################### Slide 42	
models	
############################################### Slide 43	
############################################### Slide 44	
qfit5 <- rq.sdf(composite ~ dsex + sdracem , tau=.05, data = sdf)	
qfit95 <- rq.sdf(composite ~ dsex + sdracem , tau=.95, data = sdf)	
summary(qfit5)	
############################################### Slide 45	
#install.packages("rbenchmark")	
rbenchmark::benchmark(replications = 3, columns = c("test", "replications", "elapsed", "relative", "user.self", "sys.self"),	
 "Barrodale & Roberts" ={v1<-rq.sdf(composite ~ dsex , tau=.95, data = sdf, method="br")},	
 "Frisch-Newton"       ={v2<-rq.sdf(composite ~ dsex , tau=.95, data = sdf, method="fn")}	
)	
v2$coefmat	
v1$coefmat	
############################################### Slide 46	
logit1 <- logit.sdf(I(b013801 %in% ">100") ~ dsex,	
                    weightVar = 'origwt', data = sdf)	
############################################### Slide 47	
summary(logit1)	
############################################### Slide 48	
oddsRatio(logit1)	
############################################### Slide 49	
############################################### Slide 50	
logitexercise1 <- logit.sdf(I(lep %in% "Yes") ~ b018201,	
                          weightVar = 'origwt', data = sdf)	
summary(logitexercise1)	
############################################### Slide 51	
#Read in multiple USA G4 students' data	
TIMSS11<- readTIMSS("C:/TIMSS/2011", 	
                    countries = c("usa"), gradeLvl = "4")	
TIMSS15<- readTIMSS("C:/TIMSS/2015", 	
                    countries = c("usa"), gradeLvl = "4")	
TIMSS19<- readTIMSS("C:/TIMSS/2019", 	
                    countries = c("usa"), gradeLvl = "4")	
############################################### Slide 52	
##Read in 2019 TIMSS G4 student data from multiple education systems. 	
## Pick the education systems that you like. Below are examples	
TIMSS19USA<- readTIMSS(path = "C:/TIMSS/2019/", 	
                       countries = c("usa"), gradeLvl = "4")	
TIMSS19FIN<- readTIMSS(path = "C:/TIMSS/2019/", 	
                       countries = c("fin"), gradeLvl = "4")	
TIMSS19HKG<- readTIMSS(path = "C:/TIMSS/2019/", 	
                       countries = c("hkg"), gradeLvl = "4")	

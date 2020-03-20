#R Studio API Code
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Libraries
library(tidyverse)
library(GGally)
library(psych)
library(lavaan)

full_data_imp <- load("../data/training_data_imp.RData")
imp_dat<- mice::complete(training_data_imp,1)
imp_biodata <- imp_dat %>% 
  select(UNIQUE_ID, High_Performer, Protected_Group, Biodata_01:Biodata_20)


summary(imp_biodata[4:23])
#Response options in each of the items
#Q1: 1-6
#Q2: 1-8
#Q3: 1-7
#Q4: 1-8
#Q5: 1-5
#Q6: 1-7
#Q7: 1-6
#Q8: 1-8
#Q9: 1-7
#Q10: 1-7
#Q11: 1-8
#Q12: 1-6
#Q13: 1-6
#Q14: 1-6
#Q15: 1-6
#Q16: 1-6
#Q17: 1-7
#Q18: 1-7
#Q19: 1-7
#Q20: 1-6

#correlations, density plots, and scatter plots. Slow
ggpairs(imp_biodata[4:23])

#just correlations
round(cor(imp_biodata[4:23], use="na.or.complete"), 2)


#Histograms to visualize patterns in answering. 
par(mfrow=c(3,4))
sapply(imp_biodata[4:23], hist)
par(mfrow=c(1,1))

##Factor analysis of the imputed dataset 

#find the number of factors suggested
fa.parallel(imp_biodata[4:23], fm="minres", fa="fa") #suggests 5 factors

#do the actual factor analysis 
imp_fa_5factors <-fa(imp_biodata[4:23], nfactors=5, rotate="oblimin", fm="minres")
print(imp_fa_5factors$loadings, cutoff=.3)
fa.diagram(imp_fa_5factors)

imp_fa_4factors <-fa(imp_biodata[4:23], nfactors=4, rotate="oblimin", fm="minres")
print(imp_fa_4factors$loadings, cutoff=.3)
fa.diagram(imp_fa_4factors)

imp_fa_6factors <-fa(imp_biodata[4:23], nfactors=6, rotate="oblimin", fm="minres")
print(imp_fa_6factors$loadings, cutoff=.3)
fa.diagram(imp_fa_6factors)

# EFA on the 20 biodata items. Parallel analysis suggested 5 factors, but if I use 5 factors, 2 of my factors correlate .80. So I dropped it to a four factor model. In that model none of the factors correlate more than .3, but then 4 of the items are dropped for being insignificant, and one item has cross loadings.
#The four dropped items are 3, 4, 11, and 19. Number 3 and 19 have variance, but have very low correlations with the remaining items. Items number 11 and 20 have almost no variance. Almost all of the participants responded to one of the response options. 
#6 items load onto factor 1, 6 items load on to factor 2, 2 items load onto factor 3, and 2 items load onto factor 4. 
#Given that only 2 items load on to each factor 3 and 4, it seems that there are only 2 strong factors in the biodata, and the remaining data may not nest into factors. 
#Talking with khue, we speculated that this might indicate the difference between items that are orginal vs those that are not ordinal. 

#doing Factor Analysis on the 12 items that nested into the 2 factors to confirm 
#Try FA on only the items that load heavily onto two factors 
fa_test_data <-imp_biodata %>%
  select(Biodata_01, Biodata_02, Biodata_04, Biodata_05, Biodata_06, Biodata_07, Biodata_08, Biodata_10, Biodata_12, Biodata_14, Biodata_15, Biodata_17)

fa.parallel(fa_test_data, fm="minres", fa="fa")

fa_test_2f <-fa(fa_test_data, nfactors=2, rotate="oblimin", fm="minres")
print(fa_test_2f$loadings, cutoff=.3)
fa.diagram(fa_test_2f)

#CFA for 4 factor and 2 factor model below 

##Confirmatory Factor Analysis for the 4 factor model 

mod1 <-'MR1 =~ Biodata_01 + Biodata_06 + Biodata_07 + Biodata_12 + Biodata_14 + Biodata_15
MR2 =~ Biodata_02 + Biodata_04 + Biodata_05 +Biodata_08 + Biodata_10 + Biodata_17
MR3 =~ Biodata_09 + Biodata_18
MR4 =~ Biodata_13 + Biodata_16'
fit <- cfa(mod1, data=imp_biodata)
summary(fit, fit.measures=TRUE)

#Confirmatory factor analysis for the 2 factor model using only the 12 items that nested into the factors above (looks like it fits better than 4)
mod2 <-'MR1 =~ Biodata_01 + Biodata_06 + Biodata_07 + Biodata_12 + Biodata_14 + Biodata_15
MR2 =~ Biodata_02 + Biodata_04 + Biodata_05 +Biodata_08 + Biodata_10 + Biodata_17'
fit2 <- cfa(mod2, data=imp_biodata)
summary(fit2, fit.measures=TRUE)

##Creating a dataset with factor composites. I included factor 3 and 4, although this is a reminder that they each only have two items loading on to them, and as such are not strong factors. Khue and I are leaning towards the usefulness of the first 2 factors as opposed to all 4. 
#item 4 and 17 were negatively loaded, so they've been recoded below. 

biodata_factors <- imp_biodata %>%
  mutate(Biodata_04_reverse =car::recode(Biodata_04, "1=8; 2=7; 3=6; 4=5; 5=4; 6=3; 7=2; 8=1"), Biodata_17_reverse=car::recode(Biodata_17, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"), factor1_composite=c(Biodata_01+ Biodata_06+ Biodata_07+ Biodata_12+ Biodata_14+ Biodata_15), factor2_composite=c(Biodata_02 + Biodata_04_reverse + Biodata_05 +Biodata_08 + Biodata_10 + Biodata_17_reverse), factor3_composite=(c(Biodata_09 + Biodata_18)), factor4_composite = c(Biodata_13 + Biodata_16))

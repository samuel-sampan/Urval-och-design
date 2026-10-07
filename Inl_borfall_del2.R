# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("tibble")
#install.packages("mice")

# ---------------------------------------# 
library("tidyverse")
library("readxl")
library("tibble")
library("mice")
library("ggplot2")
library("dplyr")
# --------------------------------------- # 
#Hämta data
childdata_big26 <- read.csv("//stuur01.it.liu.se/students/chrni417/Desktop/Bortfall_ del 2/childdata 2026.csv")

#-------------------------------------------------------------------------------

# 2st set. Med och utan NA
D1_plays <- childdata_big26[c("BPI2014_2014","COGNP2014_2014","RECOG2014_2014","CRACE_XRND","CSEX_XRND","CYRB_XRND" )]

#-------------------------------------------------------------------------------

## NON NA
Valid_D1_players <- D1_plays[
  
    !is.na(D1_plays$BPI2014_2014) &
    !is.na(D1_plays$COGNP2014_2014) &
    !is.na(D1_plays$RECOG2014_2014) &
    !is.na(D1_plays$CRACE_XRND) &
    !is.na(D1_plays$CSEX_XRND) &
    !is.na(D1_plays$CYRB_XRND),
  ]

## NA MAXING
NA_Valid_D1_players <- D1_plays 

#-------------------------------------------------------------------------------

#The big rename

## NON NA 
colnames(Valid_D1_players ) <- c( "BPI2014_2014"= "Behavioral_problem",
                                  "Kognitiv_stimulans",
                                  "Ordförståelse",
                                  "Race",
                                  "Kön",
                                  "Födelseår")

## NA MAXERS
colnames(NA_Valid_D1_players) <- c( "BPI2014_2014"= "Behavioral_problem",
                                  "Kognitiv_stimulans",
                                  "Ordförståelse",
                                  "Race",
                                  "Kön",
                                  "Födelseår")

#-------------------------------------------------------------------------------

#The big analysis of the NON NA MAXERS

##kognitiv_stimulans
 ggplot(Valid_D1_players) +
   geom_point(aes(x = Kognitiv_stimulans ,y = Behavioral_problem)) + 
   labs( title = "Samband mellan Kognitiv_stimulans och Behavioral_problem",x = "Kognitiv_stimulans", y = "Behavioral_problem") +
   theme_classic()
 
##ordförsåelse
 ggplot(Valid_D1_players) +
   geom_point(aes(x = Ordförståelse ,y = Behavioral_problem)) + 
   labs( title = "Samband mellan Ordförståelse och Behavioral_problem",
         x = "Ordförståelse", 
         y = "Behavioral_problem") +
   theme_classic()
 
##race
 ggplot(Valid_D1_players) +
   geom_boxplot(aes(x = factor(Race) ,y = Behavioral_problem)) + 
   labs( title = "Samband mellan Race och Behavioral_problem",x = "Race", y = "Behavioral_problem") +
   theme_classic()
 
 
##Kön
 ggplot(Valid_D1_players) +
   geom_boxplot(aes(x = factor(Kön) ,y = Behavioral_problem)) + 
   labs( title = "Samband mellan Kön och Behavioral_problem",x = "Kön", y = "Behavioral_problem") +
   theme_classic()
 
 
##Födelseår
 ggplot(Valid_D1_players) +
   geom_point(aes(x = Födelseår ,y = Behavioral_problem)) + 
   labs( title = "Samband mellan Födelseår och Behavioral_problem",x = "Födelseår", y = "Behavioral_problem") +
   theme_classic()
 
modell_1  <- lm(Behavioral_problem ~ Kognitiv_stimulans + Ordförståelse + factor(Race) + factor(Kön) + Födelseår, data=Valid_D1_players)
modell_1
 
modell_1_sum <- summary(modell_1) 
modell_1_sum

modell_1_anova <- anova(modell_1) 
modell_1_anova

#-------------------------------------------------------------------------------

#The mighty and grate NA MAXING analysis (Sponsors by Thai) 






#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------






histo <- ggplot(modell_1$residuals) + 


#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
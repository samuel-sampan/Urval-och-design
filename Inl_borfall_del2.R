# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("tibble")
# ---------------------------------------# 
library("tidyverse")
library("readxl")
library("tibble")
# --------------------------------------- # 

childdata_big26 <- read.csv("//stuur01.it.liu.se/students/chrni417/Desktop/Bortfall_ del 2/childdata 2026.csv")

anyNA(childdata_big26)
sum( all(childdata_big26 == "NA"))


 list(
   #Identifikationsvariabler (dessa ska inte användas till något på denna labb)
  "CPUBID_XRND" = "barnets idkod"
  "MPUBID_XRND" = "mammans idkod"
  
  #Bakgrundsvariabler:
  "CRACE_XRND": race (1=Hispanic, 2=Black, 3=non-Hispanic, non-Black)
  "CSEX_XRND": kön (1=pojke, 2=flicka)
  "CYRB_XRND": födelseår
  
  #Undersökningsvariabler
  "BPI2014_2014": behavioral problem (betteendeproblem-index, totalpoäng)
  "DIGIT2014_2014": digit span (minnestest, totalpoäng)
  "COGNP2014_2014": kognitiv stimulans i hemmet (percentil)
  "MATH2014_2014": mattetest (totalpoäng)
  "RECOG2014_2014": ordförståelsetest (totalpoäng)
  "COMP2014_2014": läsförståelsetest (totalpoäng)
  "PPVT2014_2014": peabody picture vocabulary test (bild och innebördsförståelse, totalpoäng)
  
)

 
D1_plays <- childdata_big26[c("BPI2014_2014","COGNP2014_2014","RECOG2014_2014","CRACE_XRND","CSEX_XRND","CYRB_XRND" )]

Valid_D1_players <- D1_plays[
  
    !is.na(D1_plays$BPI2014_2014) &
    !is.na(D1_plays$COGNP2014_2014) &
    !is.na(D1_plays$RECOG2014_2014) &
    !is.na(D1_plays$CRACE_XRND) &
    !is.na(D1_plays$CSEX_XRND) &
    !is.na(D1_plays$CYRB_XRND),
  ]

colnames(Valid_D1_players ) <- c( "BPI2014_2014"= "Behavioral_problem",
                                  "Kognitiv_stimulans",
                                  "Ordförståelse",
                                  "Race",
                                  "Kön",
                                  "Födelseår")
#kognitiv_stimulans
 ggplot(Valid_D1_players) +
   geom_point(aes(x = Kognitiv_stimulans ,y = Behavioral_problem)) + 
   labs( title = "Samband mellan Kognitiv_stimulans och Behavioral_problem",x = "Kognitiv_stimulans", y = "Behavioral_problem") +
   theme_classic()
 
#ordförsåelse
 ggplot(Valid_D1_players) +
   geom_point(aes(x = Ordförståelse ,y = Behavioral_problem)) + 
   labs( title = "Samband mellan Ordförståelse och Behavioral_problem",
         x = "Ordförståelse", 
         y = "Behavioral_problem") +
   theme_classic()
 
#race
 ggplot(Valid_D1_players) +
   geom_boxplot(aes(x = factor(Race) ,y = Behavioral_problem)) + 
   labs( title = "Samband mellan Race och Behavioral_problem",x = "Race", y = "Behavioral_problem") +
   theme_classic()
 
 
#Kön
 ggplot(Valid_D1_players) +
   geom_boxplot(aes(x = factor(Kön) ,y = Behavioral_problem)) + 
   labs( title = "Samband mellan Kön och Behavioral_problem",x = "Kön", y = "Behavioral_problem") +
   theme_classic()
 
 
#Födelseår
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


histo <- ggplot(modell_1$residuals) + 
  
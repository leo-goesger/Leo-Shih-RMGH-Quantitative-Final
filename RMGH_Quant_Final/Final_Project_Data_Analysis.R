# libraries
install.packages("broom")
library(tidyverse)
library(here)
library(broom)
# load NWS csv
NWS_Full_Imp1 <- here("NWS_Full.csv") %>% read_csv()

# create selected-variable csv
NWS_Clean_Imp1 <- select(NWS_Full_Imp1, 
                       "SURVEYID", "AGE", "SEX", "HAPPY", "PERSONAL", "HHINC", 
                         "MARSTAT", "EDUC", "RELIG_IMP", "SAT_WORK", "MENT_HEALTH", "PHYS_HEALTH", "SLEEPHRS")
view(NWS_Clean_Imp1)

write.csv(NWS_Clean_Imp1, 
          "/Users/leoshih/Documents/GitHub/Leo-Shih-RMGH-Quantitative-Final/RMGH_Quant_Final/NWS_Clean_Imp1.csv", row.names = FALSE)

# Test Regression 1
Linear_Regression_1 <- lm(HAPPY ~ AGE + SEX + PERSONAL + HHINC + MARSTAT + EDUC + RELIG_IMP + SAT_WORK + SLEEPHRS, data = NWS_Clean_Imp1)
Testcor <- cor.test(NWS_Clean_Imp3$HAPPY, NWS_Clean_Imp3$PHYS_HEALTH, method = "pearson")

# Cleaning all NAs
NWS_Clean_Imp2 <- NWS_Clean_Imp1 %>% 
  subset(AGE < 95) %>%
  subset(SEX < 95) %>%
  subset(HAPPY < 95) %>%
  subset(PERSONAL < 95) %>%
  subset(HHINC < 95) %>%
  subset(MARSTAT < 95) %>%
  subset(EDUC < 95) %>%
  subset(RELIG_IMP < 95) %>%
  subset(SAT_WORK < 95) %>%
  subset(PHYS_HEALTH < 95) %>%
  subset(MENT_HEALTH < 95) %>%
  subset(SLEEPHRS < 95)
view(NWS_Clean_Imp2)

# create cleaned selected-variable csv
write.csv(NWS_Clean_Imp2, 
          "/Users/leoshih/Documents/GitHub/Leo-Shih-RMGH-Quantitative-Final/RMGH_Quant_Final/NWS_Clean_Imp1.csv", row.names = FALSE)

# dummy variables
MARSTAT_DUMMY <- (NWS_Clean_Imp3$MARSTAT == 1) %>% as.integer()
SEX_DUMMY <- (NWS_Clean_Imp3$SEX == 1) %>% as.integer()
NWS_Clean_Imp3 <- cbind(NWS_Clean_Imp3, SEX_DUMMY, MARSTAT_DUMMY)
view(NWS_Clean_Imp3)  

# reverse code happiness, mental health, physical health, 

NWS_Clean_Imp3$MENT_HEALTH <- 6-NWS_Clean_Imp3$MENT_HEALTH
NWS_Clean_Imp3$PHYS_HEALTH <- 6-NWS_Clean_Imp3$PHYS_HEALTH
view(NWS_Clean_Imp3)

# descriptive
summary(NWS_Clean_Imp3$HAPPY)
summary(NWS_Clean_Imp3$AGE)
summary(NWS_Clean_Imp3$SLEEPHRS)

# anova
SEX_DUMMY_ANOVA <- aov(HAPPY ~ SEX_DUMMY, data = NWS_Clean_Imp3) %>% tidy()
PERSONAL_ANOVA <- aov(HAPPY ~ PERSONAL, data = NWS_Clean_Imp3) #%>% tidy()
HHINC_ANOVA <- aov(HAPPY ~ HHINC, data = NWS_Clean_Imp3) #%>% tidy()
MARSTAT_DUMMY_ANOVA <- aov(HAPPY ~ MARSTAT_DUMMY, data = NWS_Clean_Imp3) #%>% tidy()
EDUC_ANOVA <- aov(HAPPY ~ EDUC, data = NWS_Clean_Imp3) #%>% tidy()
RELIG_IMP_ANOVA <- aov(HAPPY ~ RELIG_IMP, data = NWS_Clean_Imp3) #%>% tidy()
SAT_WORK_ANOVA <- aov(HAPPY ~ SAT_WORK, data = NWS_Clean_Imp3) #%>% tidy()

rbind(SEX_DUMMY_ANOVA, PERSONAL_ANOVA, HHINC_ANOVA, MARSTAT_DUMMY_ANOVA, 
      EDUC_ANOVA, RELIG_IMP_ANOVA, SAT_WORK_ANOVA) %>% 
  write.csv(here("ANOVA_Results.csv"))

ANOVA_Results <- here("ANOVA_Results.csv") %>% read_csv()

## correlation analysis
Cor_Sleep_Happy <- cor.test(NWS_Clean_Imp3$SLEEPHRS, NWS_Clean_Imp3$HAPPY, method = "pearson") #%>% tidy()
Cor_Sleep_Happy

Cor_Age_Happy <- cor.test(NWS_Clean_Imp3$AGE, NWS_Clean_Imp3$HAPPY, method = "pearson") #%>% tidy()
Cor_Age_Happy

Cor_Ment_Happy <- cor.test(NWS_Clean_Imp3$MENT_HEALTH, NWS_Clean_Imp3$HAPPY, method = "pearson") #%>% tidy()
Cor_Ment_Happy

Cor_Phys_Happy <- cor.test(NWS_Clean_Imp3$PHYS_HEALTH, NWS_Clean_Imp3$HAPPY, method = "pearson") #%>% tidy()
Cor_Phys_Happy

rbind(Cor_Sleep_Happy, Cor_Sleep_Happy, Cor_Age_Happy, 
      Cor_Ment_Happy, Cor_Phys_Happy) %>%
  write.csv(here("PearsonCor_Results.csv"))

PearsonCor_Results <- here("PearsonCor_Results.csv") %>% read_csv()

# linear regression
happy_linreg <- lm(HAPPY ~ AGE + SEX_DUMMY + PERSONAL + HHINC + MARSTAT_DUMMY + 
                     EDUC + RELIG_IMP + SAT_WORK + SLEEPHRS, data = NWS_Clean_Imp3) #%>% tidy()

ment_linreg <-  lm(MENT_HEALTH ~ AGE + SEX_DUMMY + PERSONAL + HHINC + MARSTAT_DUMMY + 
                     EDUC + RELIG_IMP + SAT_WORK + SLEEPHRS, data = NWS_Clean_Imp3) #%>% tidy()

phys_linreg <-  lm(PHYS_HEALTH ~ AGE + SEX_DUMMY + PERSONAL + HHINC + MARSTAT_DUMMY + 
                     EDUC + RELIG_IMP + SAT_WORK + SLEEPHRS, data = NWS_Clean_Imp3) #%>% tidy()

cbind(happy_linreg, ment_linreg, phys_linreg) %>%
  write.csv(here("linreg_Results.csv"))

linreg_Results <- here("linreg_Results.csv") %>% read_csv()
  
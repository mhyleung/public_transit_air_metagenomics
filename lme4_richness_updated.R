library(readr)
library(dplyr)
library(stringr)
library(lme4)
library(lubridate)

Alpha <- read_tsv("richness_rarefied_global_samples_subsamp.txt") %>%
    select(Sample, richness)
#standardize the response variable
#Alpha$Shannon2 <- scale(Alpha$Shannon, center = TRUE, scale = TRUE)
#Alpha <- Alpha %>% filter(!Alpha$Shannon == "0")
meta <- read_tsv("metadata_new.txt")
merge <- left_join(Alpha, meta)
#merge$log <- log10(merge$Shannon)
sub(" 20", "/", as.character(merge$Sampling_date)) -> merge$Samplig.date
x = as.Date(merge$Samplig.date,  "%d/%m/%y")
yday(x) -> merge$julian

names(merge)[6] <- c("Indoor_outdoor")
#observed_otus is the response variable, 1 is the fixed effect for the intercept, houseshold is the level 1 predictor, (1|) are the random effect for the intercepts
lmer.hyp <- lmer(richness~City + Indoor_outdoor + No_lines + julian + Ground + (1|Location), data=merge, REML=FALSE)
lmer.null <- lmer(richness~Indoor_outdoor + No_lines + julian + Ground + (1|Location), data=merge, REML=FALSE)

attr(getME(lmer.hyp,"X"),"col.dropped")
summary(lmer.hyp)
summary(lmer.null)

qqnorm(resid(lmer.hyp))
qqline(resid(lmer.hyp))
shapiro.test(resid(lmer.hyp))

Shapiro-Wilk normality test

data:  resid(lmer.hyp)
W = 0.99091, p-value = 0.1097

# test the fixed effect of city
anova(lmer.hyp,lmer.null)

Data: merge
Models:
lmer.null: richness ~ Indoor_outdoor + No_lines + julian + Ground + (1 |
lmer.null:     Location)
lmer.hyp: richness ~ City + Indoor_outdoor + No_lines + julian + Ground +
lmer.hyp:     (1 | Location)
Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
lmer.null  7 1341.8 1366.7 -663.92   1327.8
lmer.hyp  12 1228.3 1270.9 -602.14   1204.3 123.57      5  < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# test the fixed effect of indoor and outdoor air source
lmer.hyp <- lmer(richness~City + Indoor_outdoor + No_lines + julian + Ground + (1|Location), data=merge, REML=FALSE)
lmer.null <- lmer(richness~City + No_lines + julian + Ground + (1|Location), data=merge, REML=FALSE)
anova(lmer.hyp,lmer.null)
Data: merge
Models:
lmer.null: richness ~ City + No_lines + julian + Ground + (1 | Location)
lmer.hyp: richness ~ City + Indoor_outdoor + No_lines + julian + Ground +
lmer.hyp:     (1 | Location)
Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
lmer.null 11 1241.4 1280.5 -609.69   1219.4
lmer.hyp  12 1228.3 1270.9 -602.14   1204.3 15.114      1  0.0001012 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# test the fixed effect of the number of lines
lmer.hyp <- lmer(richness~City + Indoor_outdoor + No_lines + julian + Ground + (1|Location), data=merge, REML=FALSE)
lmer.null <- lmer(richness~City + Indoor_outdoor + julian + Ground + (1|Location), data=merge, REML=FALSE)
anova(lmer.hyp,lmer.null)
Data: merge
Models:
lmer.null: richness ~ City + Indoor_outdoor + julian + Ground + (1 | Location)
lmer.hyp: richness ~ City + Indoor_outdoor + No_lines + julian + Ground +
lmer.hyp:     (1 | Location)
Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
lmer.null 11 1230.1 1269.2 -604.06   1208.1
lmer.hyp  12 1228.3 1270.9 -602.14   1204.3 3.8441      1    0.04992 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#test the fixed effect of julian day
lmer.hyp <- lmer(richness~City + Indoor_outdoor + No_lines + julian + Ground + (1|Location), data=merge, REML=FALSE)
lmer.null <- lmer(richness~City + Indoor_outdoor + No_lines + Ground + (1|Location), data=merge, REML=FALSE)
anova(lmer.hyp,lmer.null)
Data: merge
Models:
lmer.null: richness ~ City + Indoor_outdoor + No_lines + Ground + (1 | Location)
lmer.hyp: richness ~ City + Indoor_outdoor + No_lines + julian + Ground +
lmer.hyp:     (1 | Location)
Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
lmer.null 11 1226.5 1265.6 -602.25   1204.5
lmer.hyp  12 1228.3 1270.9 -602.14   1204.3 0.233      1     0.6293

#test the fixed effect of ground level
lmer.hyp <- lmer(richness~City + Indoor_outdoor + No_lines + julian + Ground + (1|Location), data=merge, REML=FALSE)
lmer.null <- lmer(richness~City + Indoor_outdoor + No_lines + julian + (1|Location), data=merge, REML=FALSE)
anova(lmer.hyp,lmer.null)
Data: merge
Models:
lmer.null: richness ~ City + Indoor_outdoor + No_lines + julian + (1 | Location)
lmer.hyp: richness ~ City + Indoor_outdoor + No_lines + julian + Ground +
lmer.hyp:     (1 | Location)
Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
lmer.null 11 1226.5 1265.6 -602.26   1204.5
lmer.hyp  12 1228.3 1270.9 -602.14   1204.3 0.2505      1     0.6167

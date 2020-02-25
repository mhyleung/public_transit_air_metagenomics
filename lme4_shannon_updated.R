library(readr)
library(dplyr)
library(stringr)
library(lme4)
library(lubridate)

Alpha <- read_tsv("shannon_diversity_rarefied_global_samples_subsamp.txt") %>%
    select(Sample, Shannon)
#standardize the response variable
#Alpha$Shannon2 <- scale(Alpha$Shannon, center = TRUE, scale = TRUE)
Alpha <- Alpha %>% filter(!Alpha$Shannon == "0")
meta <- read_tsv("metadata_new.txt")
merge <- left_join(Alpha, meta)
#merge$log <- log10(merge$Shannon)
sub(" 20", "/", as.character(merge$Sampling_date)) -> merge$Samplig.date
x = as.Date(merge$Samplig.date,  "%d/%m/%y")
yday(x) -> merge$julian


names(merge)[6] <- c("Indoor_outdoor")
#observed_otus is the response variable, 1 is the fixed effect for the intercept, houseshold is the level 1 predictor, (1|) are the random effect for the intercepts
lmer.hyp <- lmer(Shannon~City + Indoor_outdoor + No_lines + julian + Ground + (1|Location), data=merge, REML=FALSE)
lmer.null <- lmer(Shannon~Indoor_outdoor + No_lines + julian + Ground + (1|Location), data=merge, REML=FALSE)


attr(getME(lmer.hyp,"X"),"col.dropped")
summary(lmer.hyp)
summary(lmer.null)

qqnorm(resid(lmer.hyp))
qqline(resid(lmer.hyp))
shapiro.test(resid(lmer.hyp))

Shapiro-Wilk normality test

data:  resid(lmer.hyp)
W = 0.99435, p-value = 0.4584

library(AICcmodavg)

AICc(lmer.hyp)
[1] 106.7154

# test the fixed effect of city
anova(lmer.hyp,lmer.null)

Data: merge
Models:
lmer.null: Shannon ~ Indoor_outdoor + No_lines + julian + Ground + (1 |
lmer.null:     Location)
lmer.hyp: Shannon ~ City + Indoor_outdoor + No_lines + julian + Ground +
lmer.hyp:     (1 | Location)
Df    AIC    BIC   logLik deviance  Chisq Chi Df Pr(>Chisq)
lmer.null  7 229.11 253.92 -107.553  215.106
lmer.hyp  12 105.43 147.97  -40.716   81.431 133.67      5  < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# test the fixed effect of indoor and outdoor air source
lmer.hyp <- lmer(Shannon~City + Indoor_outdoor + No_lines + julian + Ground + (1|Location), data=merge, REML=FALSE)
lmer.null <- lmer(Shannon~City + No_lines + julian + Ground + (1|Location), data=merge, REML=FALSE)
anova(lmer.hyp,lmer.null)
Data: merge
Models:
lmer.null: Shannon ~ City + No_lines + julian + Ground + (1 | Location)
lmer.hyp: Shannon ~ City + Indoor_outdoor + No_lines + julian + Ground +
lmer.hyp:     (1 | Location)
Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
lmer.null 11 108.43 147.43 -43.215   86.430
lmer.hyp  12 105.43 147.97 -40.716   81.431 4.9983      1    0.02537 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# test the fixed effect of the number of lines
lmer.hyp <- lmer(Shannon~City + Indoor_outdoor + No_lines + julian + Ground + (1|Location), data=merge, REML=FALSE)
lmer.null <- lmer(Shannon~City + Indoor_outdoor + julian + Ground + (1|Location), data=merge, REML=FALSE)
anova(lmer.hyp,lmer.null)
Data: merge
Models:
lmer.null: Shannon ~ City + Indoor_outdoor + julian + Ground + (1 | Location)
lmer.hyp: Shannon ~ City + Indoor_outdoor + No_lines + julian + Ground +
lmer.hyp:     (1 | Location)
Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
lmer.null 11 106.84 145.84 -42.421   84.842
lmer.hyp  12 105.43 147.97 -40.716   81.431 3.4104      1    0.06479 .
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#test the fixed effect of julian day
lmer.hyp <- lmer(Shannon~City + Indoor_outdoor + No_lines + julian + Ground + (1|Location), data=merge, REML=FALSE)
lmer.null <- lmer(Shannon~City + Indoor_outdoor + No_lines + Ground + (1|Location), data=merge, REML=FALSE)
anova(lmer.hyp,lmer.null)
Data: merge
Models:
lmer.null: Shannon ~ City + Indoor_outdoor + No_lines + Ground + (1 | Location)
lmer.hyp: Shannon ~ City + Indoor_outdoor + No_lines + julian + Ground +
lmer.hyp:     (1 | Location)
Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
lmer.null 11 104.78 143.78 -41.391   82.781
lmer.hyp  12 105.43 147.97 -40.716   81.431 1.3496      1     0.2453

#test the fixed effect of ground level
lmer.hyp <- lmer(Shannon~City + Indoor_outdoor + No_lines + julian + Ground + (1|Location), data=merge, REML=FALSE)
lmer.null <- lmer(Shannon~City + Indoor_outdoor + No_lines + julian + (1|Location), data=merge, REML=FALSE)
anova(lmer.hyp,lmer.null)
Data: merge
Models:
lmer.null: Shannon ~ City + Indoor_outdoor + No_lines + julian + (1 | Location)
lmer.hyp: Shannon ~ City + Indoor_outdoor + No_lines + julian + Ground +
lmer.hyp:     (1 | Location)
Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
lmer.null 11 105.20 144.20 -41.602   83.204
lmer.hyp  12 105.43 147.97 -40.716   81.431 1.7722      1     0.1831

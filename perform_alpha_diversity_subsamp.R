library(readr)
library(dplyr)
library(reshape2)
library(tidyr)

alpha <- read_tsv("merged_abundance_table_species_subsamp.txt") %>%
	gather(Variable, abundance, 2:(ncol(alpha))) %>%
	filter(abundance >0)
names(alpha)[1:2] <- c("Species", "Sample")

meta <- read_tsv("metadata_new.txt") %>%
	select(Sample, City)
	
merge <- left_join(alpha, meta)

richness <- merge %>%
	select(-abundance, -Species) %>%
	mutate(richness = 1) %>%
	group_by(Sample) %>%
	mutate(richness= sum(richness)) %>%
	ungroup() %>%
	unique()
write_tsv(richness,"richness_rarefied_global_samples_subsamp.txt")
	
# calculate the shannon diversity
library(vegan)
alpha[1:6,1:6]

alpha <- read.table("merged_abundance_table_species_subsamp.txt", header=T, sep="\t", fill =TRUE, quote="", row.names = NULL)
Sample <- colnames(alpha[2:ncol(alpha)])
Species <- alpha[1:nrow(alpha),1]
names(alpha) <- NULL
alpha[,1] <- NULL
#transpose the data frame
alpha_t <- data.frame(t(alpha))
alpha_t[is.na(alpha_t)] <- 0
colnames(alpha_t) <- Species
#calculate the shannon diversity for global samples
Shannon <- diversity(alpha_t, "shannon")
#merge the sample info with alpha diversity
data <- data.frame(cbind(Sample,Shannon))
#load the meta table
meta <- read_tsv("metadata_new.txt")
#add meta info
merge <- left_join(data, meta)
write_tsv(merge, "shannon_diversity_rarefied_global_samples_subsamp.txt")


shannon <- read_tsv("shannon_diversity_rarefied_global_samples_500k.txt") %>%
    select(-ID) %>%
    mutate(diversity = "Shannon diversity")
names(shannon)[1] <- c("value")

richness <- read_tsv("richness_rarefied_global_samples_500k.txt") %>%
    mutate(diversity = "Richness")
names(richness)[4] <- c("value")
merge <- rbind(shannon,richness)
Plot <- ggplot(merge, aes(x = City, y = value, fill = City))
Plot <- Plot + geom_boxplot()
Plot <- Plot + facet_wrap(~diversity)
Plot <- Plot + theme_bw()
Plot <- Plot + scale_fill_brewer(palette = "Dark2")
Plot <- Plot + theme(axis.title = element_text(size=14, face="bold"))
Plot <- Plot + ylab(paste0("Richness")) + xlab(paste0("City"))



# make the barchart for richness
library(ggplot2)
table <- read_tsv("richness_rarefied_global_samples_subsamp.txt")
Plot <- ggplot(table, aes(x = Sample, y = richness, color = City))
Plot <- Plot + geom_bar(stat="identity",position = "stack")
Plot <- Plot + theme_classic()
Plot <- Plot + theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1))
Plot <- Plot + ylab(paste0("Richness")) + xlab(paste0("Sample"))
Plot <- Plot + scale_color_brewer(palette = "Dark2")
Plot <- Plot + theme(axis.text.x = element_blank())
Plot <- Plot +  scale_y_continuous(expand = c(0, 0))
Plot <- Plot + theme(axis.ticks.x = element_blank())
Plot <- Plot + theme(axis.title = element_text(size=14, face="bold"))
ggsave("richenss_rarefied_global_samples_500k_barchart.png", dpi=1086)

#make the boxplot for richness
table <- read_tsv("richness_rarefied_global_samples_subsamp.txt")
Plot <- ggplot(table, aes(x = City, y = richness, fill = City))
Plot <- Plot + geom_boxplot()
Plot <- Plot + theme_bw()
Plot <- Plot + scale_fill_brewer(palette = "Dark2")
Plot <- Plot + theme(axis.title = element_text(size=14, face="bold"))
Plot <- Plot + ylab(paste0("Richness")) + xlab(paste0("City"))
ggsave("richenss_rarefied_global_samples_subsamp_boxplot.pdf", dpi=1086)

#Statistics
stats <- aov(richness~City, data=table)
summary(stats)
Df Sum Sq Mean Sq F value Pr(>F)
City          5   2604   520.8   65.97 <2e-16 ***
Residuals   252   1990     7.9
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#make the boxplot for richness
table <- read_tsv("shannon_diversity_rarefied_global_samples_subsamp.txt")
Plot <- ggplot(table, aes(x = City, y = Shannon, fill = City))
Plot <- Plot + geom_boxplot()
Plot <- Plot + theme_bw()
Plot <- Plot + scale_fill_brewer(palette = "Dark2")
Plot <- Plot + theme(axis.title = element_text(size=14, face="bold"))
Plot <- Plot + ylab(paste0("Shannon Diversity")) + xlab(paste0("City"))
ggsave("shannon_rarefied_global_samples_subsamp_boxplot.pdf", dpi=1086)

#Statistics
stats <- aov(Shannon~City, data=table)
summary(stats)
Df Sum Sq Mean Sq F value Pr(>F)
City          5  30.82   6.164   63.33 <2e-16 ***
Residuals   253  24.63   0.097
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



library(wilkoxmisc)
table <- read_tsv("shannon_diversity_rarefied_global_samples_subsamp.txt")
test <- kruskal.test(Shannon~City,data=table)

Kruskal-Wallis rank sum test

data:  Shannon by City
Kruskal-Wallis chi-squared = 155.56, df = 5, p-value < 2.2e-16
> test$p.value
[1] 8.748636e-32


table <- read_tsv("richness_rarefied_global_samples_subsamp.txt")
test <- kruskal.test(richness~City,data=table)

Kruskal-Wallis rank sum test

data:  richness by City
Kruskal-Wallis chi-squared = 154.01, df = 5, p-value < 2.2e-16
> test$p.value
[1] 1.872822e-31

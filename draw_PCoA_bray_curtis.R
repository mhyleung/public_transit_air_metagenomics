library(readr)
library(dplyr)
library(tidyr)
library(wilkoxmisc)
library(reshape2)

tax_cast <- read.table("merged_abundance_table_species_all_cities.cast.txt",header=T,row.names=1)
#distance <- distance(tax_cast, method = "bray-curtis") in R package "ecodist
library(vegan)
distance <- vegdist(tax_cast, method="bray")

UniFracMatrix <- as.matrix(distance)
Meta <- read_tsv("metadata_new.txt")
PCoA <- cmdscale(UniFracMatrix, k = 2, eig = TRUE)
DF <- data.frame(Sample = row.names(PCoA$points), PCoA1 = PCoA$points[,1], PCoA2 = PCoA$points[,2], row.names = NULL)
Eigenvalues <- eigenvals(PCoA)
VarianceExplained <- Eigenvalues /sum(Eigenvalues)
VarianceExplained1 <- 100 * signif(VarianceExplained[1], 2)
VarianceExplained2 <- 100 * signif(VarianceExplained[2], 2)
PCoA <- merge(DF, Meta, by = "Sample", all.x = TRUE)

#Merge Dataframe with metadata
AllSamples <-data.frame(Sample = row.names(UniFracMatrix))
AllSamples <- merge(AllSamples, Meta, by = "Sample", all.x = TRUE)
#Check that UniFrac matrix rows match samples table (for ANOSIM)
sum(row.names(UniFracMatrix)==AllSamples$Sample) == length(AllSamples$Sample)

BrayCurtis <- as.dist(distance)

#Specify X and Y axis
xlab <- paste0("PCoA1 (Variance Explained: 27%)")
ylab <- paste0("PCoA2 (Variance Explained: 16%)")

Plot <- ggplot(PCoA, aes(x = PCoA1, y = PCoA2, colour=City))
Plot <- Plot + geom_point(size=2) + stat_ellipse(geom = "polygon",alpha = 0.05, aes(fill = City))
#Plot <- Plot + geom_point(size=3)
#Plot <- Plot + theme_test()
Plot <- Plot + xlab(xlab)
Plot <- Plot + ylab(ylab)
Plot <- Plot + theme_classic()
Plot <- Plot + scale_color_brewer(palette = "Set2")
Plot <- Plot + theme(axis.title=element_text(size=14))
Plot <- Plot + theme(axis.text=element_text(size=12))
Plot <- Plot + theme(legend.title = element_blank(), legend.position="right")
Plot <- Plot + theme(legend.text = element_text(size=14, face = "bold"))
ggsave("metaphlan2_bray_curtis_by_city.pdf")

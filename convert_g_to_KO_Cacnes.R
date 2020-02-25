#Combine random forest top30 city-based functions with KO 

Cacnes <- read.tidy("top30_predictors_cacnes.txt")
KO <- read.tidy("panphlan_pacnes16_centroids_translated.fasta.emapper.annotations.txt")

Merge <- merge(Cacnes, KO, by.x="predictors",by.y="g00",all.x=TRUE)

write.tidy(Merge,"top30_predictors_cacnes_w_KO.txt")
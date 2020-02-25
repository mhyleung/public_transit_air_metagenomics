#Combine random forest top30 city-based functions with KO 

Mluteus <- read.tidy("top30_predictors_mluteus_new.txt")
KO <- read.tidy("panphlan_mluteus16_centroids_translated.fasta.emapper.annotations.txt")

Merge <- merge(Mluteus, KO, by.x="predictors",by.y="g00",all.x=TRUE)

write.tidy(Merge,"top30_predictors_mluteus_new_w_KO.txt")

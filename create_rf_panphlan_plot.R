#Open RF results for 2 species
Table <- read.tidy("top30_predictors_combined_w_KO_edit_new.txt")
Table$Description <- reorder(Table$Description, Table$MeanDecreaseAccuracy, .desc =TRUE)
Table$Species <- factor(Table$Species, levels =c("C. acnes","M. luteus"))

Plot <- ggplot(Table,aes(x=Description,y=MeanDecreaseAccuracy, fill=Class))
Plot <- Plot + geom_bar(stat="identity") + theme_classic()
Plot <- Plot + ylab(paste0("Mean Decrease Accuracy")) + theme(axis.title.y = element_blank(), axis.text.y = element_text(angle=22.5),legend.title=element_blank(), legend.position="bottom")
Plot <- Plot + coord_flip() + facet_wrap(~Species) + scale_fill_brewer(palette="Paired")
Plot <- Plot + guides(fill=guide_legend(ncol = 2))

ggsave("RF_panphlan_newer.pdf",width=9,height=8,units="in")

#Add prevalence data for KOs in question based on g00 (C. acnes)
Table <- read.tidy("top30_predictors_combined_w_KO_edit.txt")[c("predictors","KO","MeanDecreaseAccuracy","Species")]
Cacnes <- Table[which(Table$Species == "C. acnes") ,]
DenverCacnes <- read.tidy("pacnes_presence_wt_ref_Denver.txt")
DenverCacnes <- merge(Cacnes,DenverCacnes,by.y="g00",by.x = "predictors",all.x=TRUE)
write.tidy(DenverCacnes,"pacnes_presence_wt_ref_Denver_w_g00_for_RF.txt")

HKGCacnes <- read.tidy("pacnes_presence_wt_ref_HKG.txt")
HKGCacnes <- merge(Cacnes,HKGCacnes,by.y="g00",by.x = "predictors",all.x=TRUE)
write.tidy(HKGCacnes,"pacnes_presence_wt_ref_HKG_w_g00_for_RF.txt")

LondonCacnes <- read.tidy("pacnes_presence_wt_ref_London.txt")
LondonCacnes <- merge(Cacnes,LondonCacnes,by.y="g00",by.x = "predictors",all.x=TRUE)
write.tidy(LondonCacnes,"pacnes_presence_wt_ref_London_w_g00_for_RF.txt")

NYCCacnes <- read.tidy("pacnes_presence_wt_ref_NYC.txt")
NYCCacnes <- merge(Cacnes,NYCCacnes,by.y="g00",by.x = "predictors",all.x=TRUE)
write.tidy(NYCCacnes,"pacnes_presence_wt_ref_NYC_w_g00_for_RF.txt")

OsloCacnes <- read.tidy("pacnes_presence_wt_ref_Oslo.txt")
OsloCacnes <- merge(Cacnes,OsloCacnes,by.y="g00",by.x = "predictors",all.x=TRUE)
write.tidy(OsloCacnes,"pacnes_presence_wt_ref_Oslo_w_g00_for_RF.txt")

StockholmCacnes <- read.tidy("pacnes_presence_wt_ref_Stockholm.txt")
StockholmCacnes <- merge(Cacnes,StockholmCacnes,by.y="g00",by.x = "predictors",all.x=TRUE)
write.tidy(StockholmCacnes,"pacnes_presence_wt_ref_Stockholm_w_g00_for_RF.txt")

#Add prevalence data for KOs in question based on g00 (M. luteus)
Mluteus <- Table[which(Table$Species == "M. luteus") ,]
DenverMluteus <- read.tidy("mluteus_presence_wt_ref_Denver.txt")
DenverMluteus <- merge(Mluteus,DenverMluteus,by.y="g00",by.x = "predictors",all.x=TRUE)
write.tidy(DenverMluteus,"mluteus_presence_wt_ref_Denver_w_g00_for_RF.txt")

HKGMluteus <- read.tidy("mluteus_presence_wt_ref_HKG.txt")
HKGMluteus <- merge(Mluteus,HKGMluteus,by.y="g00",by.x = "predictors",all.x=TRUE)
write.tidy(HKGMluteus,"mluteus_presence_wt_ref_HKG_w_g00_for_RF.txt")

LondonMluteus <- read.tidy("mluteus_presence_wt_ref_London.txt")
LondonMluteus <- merge(Mluteus,LondonMluteus,by.y="g00",by.x = "predictors",all.x=TRUE)
write.tidy(LondonMluteus,"mluteus_presence_wt_ref_London_w_g00_for_RF.txt")

NYCMluteus <- read.tidy("mluteus_presence_wt_ref_NYC.txt")
NYCMluteus <- merge(Mluteus,NYCMluteus,by.y="g00",by.x = "predictors",all.x=TRUE)
write.tidy(NYCMluteus,"mluteus_presence_wt_ref_NYC_w_g00_for_RF.txt")

OsloMluteus <- read.tidy("mluteus_presence_wt_ref_Oslo.txt")
OsloMluteus <- merge(Mluteus,OsloMluteus,by.y="g00",by.x = "predictors",all.x=TRUE)
write.tidy(OsloMluteus,"mluteus_presence_wt_ref_Oslo_w_g00_for_RF.txt")

StockholmMluteus <- read.tidy("mluteus_presence_wt_ref_Stockholm.txt")
StockholmMluteus <- merge(Mluteus,StockholmMluteus,by.y="g00",by.x = "predictors",all.x=TRUE)
write.tidy(StockholmMluteus,"mluteus_presence_wt_ref_Stockholm_w_g00_for_RF.txt")


#Create barplot for significant RF by species
library(dplyr)
library(forcats)
Table <- read.tidy("RF_boxplot_new.txt")
Table$City <- factor(Table$City, levels = c("Denver","Hong Kong","London","New York", "Oslo","Stockholm"))
Table$KO <- factor(Table$KO, levels = c("K18230","K01243","K07492","K03325","K07783","K01551","K04095","K02016","K03436","K01197","K02283","K01719","K00097","K00219","K03496",
                                          "K02351","K07493","K07217","K00036","K06188","K05561","K06902","K02004","K07497","K07001","K01534","K02003","K06215"))

Plot <- ggplot(Table,aes(x=City,y=Prevalence,fill=Species))
Plot <- Plot + geom_bar(stat="identity") + facet_wrap(~KO,ncol=7) + theme_classic()
Plot <- Plot + scale_fill_manual(values=c("#cdc673","#8a2be2","#ee7600","#6e8b3d","#1874cd","#ee6363"))
Plot <- Plot + theme(axis.text.x = element_text(size=6, angle=270,hjust=0,vjust=0.5),axis.text.y=element_text(size=6))
Plot <- Plot + ylab(paste0("Prevalence (%)"))
Plot <- Plot + theme(strip.background =element_rect(fill="white"))
Plot <- Plot + theme(legend.position = "none") + scale_y_continuous(expand = c(0, 0))
ggsave("RF_prevalence.pdf",width=9,height=7,units="in")

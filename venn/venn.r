# sessionInfo ()
# R version 4.1.0 (2021-05-18)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=Chinese (Simplified)_China.936 
# [2] LC_CTYPE=Chinese (Simplified)_China.936   
# [3] LC_MONETARY=Chinese (Simplified)_China.936
# [4] LC_NUMERIC=C                              
# [5] LC_TIME=Chinese (Simplified)_China.936    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets 
# [6] methods   base     
library(RColorBrewer) ### to brewer.pal
library(pheatmap)
library(gplots)
library(magrittr)
library(ggpubr)
library(tidyverse)
library(ggsci)
library(scales)
colors=pal_npg('nrc')(10) ###library(scales)
colors
show_col(colors)
pal_simpsons("springfield")(16)
show_col(pal_simpsons("springfield")(16))

library(tibble)
library(ComplexHeatmap) ### to heatmap  
library(phyloseq)
library(vegan)
library(cowplot)
# if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
# #
# BiocManager::install("MicrobiotaProcess")
# install.packages(c("RColorBrewer","broom","pheatmap","gplots","magrittr","ggpubr","tidyverse","ggsci","scales","tibble","ComplexHeatmap","phyloseq","vegan"))
# BiocManager::install("phyloseq")
# #
# BiocManager::install("ComplexHeatmap")
# install.packages("sysfonts")
# library(sysfonts)
# font_families()
# font_add("serif", "C:/Users/Administrator/Desktop/serif-new-roman.ttf")
# font_families()
##############################
#library(showtext) # ???ذ?
#library(sysfonts) # ???ذ?
#font.families() # ??????ǰ???õ?????????
##font_add("serif", "D:/serif-new-roman.ttf") # myFont1?????????????ƣ?serifbd.ttf Ϊ serif New Roman????
#font.families() # ??????ǰ???õ?????????
#showtext_auto()
###############################
library(randomcoloR)
library(MicrobiotaProcess)
# ps_dada2 <- import_qiime2(otuqza='./table_filter_singleton.qza',
#                           taxaqza ='./tax.qza', 
#                           mapfilename ='./metadata2.txt'
#                           )
# 
merged2all<-read.table("./data/merged2years_its_otu.txt", 
                       quote="",head = TRUE, row.names = 1,sep = "\t",check.names = F)
head(merged2all)
dim(merged2all)

venn1<-read.table("./data/venn1.txt", 
                  quote="",head = TRUE, row.names = 1,sep = "\t",check.names = F)
venn1

merged2venn1<-merged2all%>%select(rownames(venn1))
merged2venn1_cleaned<-merged2venn1[rowSums(merged2venn1[])>0,]
dim(merged2venn1_cleaned)

merged2venn1_cleaned_merged <- (merge(t(merged2venn1_cleaned), venn1, by.x=0, by.y=0, all.x=TRUE))
merged2venn1_cleaned_merged <- column_to_rownames(merged2venn1_cleaned_merged,var = 'Row.names')

library(dplyr)
merged2venn1_final<-merged2venn1_cleaned_merged %>% 
  group_by(Group) %>% 
  summarise_each(list(sum))%>%
  data.frame()%>%
  column_to_rownames(var='Group')%>%
  t()

merged2venn1_final[merged2venn1_final != 0] <- 1
Group1 <- rownames(merged2venn1_final)[merged2venn1_final[,1] > 0]
Group2 <- rownames(merged2venn1_final)[merged2venn1_final[,2] > 0]

library("venn")
sample_list <- list(Group1 = Group1, Group2 = Group2)
color=c( "#3C5488B2","#00A087B2", 
         "#F39B7FB2","#91D1C2B2", 
         "#8491B4B2", "#DC0000B2", 
         "#7E6148B2","yellow", 
         "darkolivegreen1", "lightskyblue", 
         "darkgreen", "deeppink", "khaki2", 
         "firebrick", "brown1", "darkorange1", 
         "cyan1", "royalblue4", "darksalmon", 
         "darkgoldenrod1", "darkseagreen", "darkorchid")

venn<-venn(sample_list,zcolor = color[1:(length(sample_list))])
pdf("data/figs/Venn1.pdf",width = 6,height = 8)
venn(sample_list,zcolor = color[1:(length(sample_list))],font.family="serif")
dev.off()


venn1<-read.table("./data/N_venn4.txt", 
                  quote="",head = TRUE, row.names = 1,sep = "\t",check.names = F)
venn1

merged2venn1<-merged2all%>%select(rownames(venn1))
merged2venn1_cleaned<-merged2venn1[rowSums(merged2venn1[])>0,]
dim(merged2venn1_cleaned)

merged2venn1_cleaned_merged <- (merge(t(merged2venn1_cleaned), venn1, by.x=0, by.y=0, all.x=TRUE))
merged2venn1_cleaned_merged <- column_to_rownames(merged2venn1_cleaned_merged,var = 'Row.names')

library(dplyr)
merged2venn1_final<-merged2venn1_cleaned_merged %>% 
  group_by(N_venn4) %>% 
  summarise_each(list(sum))%>%
  data.frame()%>%
  column_to_rownames(var='N_venn4')%>%
  t()

merged2venn1_final[merged2venn1_final != 0] <- 1
Group1 <- rownames(merged2venn1_final)[merged2venn1_final[,1] > 0]
Group2 <- rownames(merged2venn1_final)[merged2venn1_final[,2] > 0]

library("venn")
sample_list <- list(N_venn4_Group1 = Group1, N_venn4_Group2 = Group2)
color=c( "#3C5488B2","#00A087B2", 
         "#F39B7FB2","#91D1C2B2", 
         "#8491B4B2", "#DC0000B2", 
         "#7E6148B2","yellow", 
         "darkolivegreen1", "lightskyblue", 
         "darkgreen", "deeppink", "khaki2", 
         "firebrick", "brown1", "darkorange1", 
         "cyan1", "royalblue4", "darksalmon", 
         "darkgoldenrod1", "darkseagreen", "darkorchid")

venn<-venn(sample_list,zcolor = color[1:(length(sample_list))])
pdf("data/figs/N_Venn4.pdf",width = 6,height = 8)
venn(sample_list,zcolor = color[1:(length(sample_list))],font.family="serif")
dev.off()

### for single samples
merged2venn1<-merged2all%>%select(c("N_NA_0628","N_BU_0628","N_NG_0628"))
merged2venn1_cleaned<-merged2venn1[rowSums(merged2venn1[])>0,]
dim(merged2venn1_cleaned)

merged2venn1_cleaned[merged2venn1_cleaned != 0] <- 1
N_NA_0628 <- rownames(merged2venn1_cleaned)[merged2venn1_cleaned[,1] > 0]
N_BU_0628 <- rownames(merged2venn1_cleaned)[merged2venn1_cleaned[,2] > 0]
N_NG_0628 <- rownames(merged2venn1_cleaned)[merged2venn1_cleaned[,3] > 0]

library("venn")
sample_list <- list(N_NA_0628 = N_NA_0628, N_BU_0628 = N_BU_0628,N_NG_0628=N_NG_0628)
color=c( "#3C5488B2","#00A087B2", 
         "#F39B7FB2","#91D1C2B2", 
         "#8491B4B2", "#DC0000B2", 
         "#7E6148B2","yellow", 
         "darkolivegreen1", "lightskyblue", 
         "darkgreen", "deeppink", "khaki2", 
         "firebrick", "brown1", "darkorange1", 
         "cyan1", "royalblue4", "darksalmon", 
         "darkgoldenrod1", "darkseagreen", "darkorchid")

venn<-venn(sample_list,zcolor = color[1:(length(sample_list))])
pdf("data/figs/3samples_venn1.pdf",width = 6,height = 8)
venn(sample_list,zcolor = color[1:(length(sample_list))],font.family="serif")
dev.off()


merged2venn1<-merged2all%>%select(c("N_NA_0802","N_BU_0802","N_NG_0802"))
merged2venn1_cleaned<-merged2venn1[rowSums(merged2venn1[])>0,]
dim(merged2venn1_cleaned)

merged2venn1_cleaned[merged2venn1_cleaned != 0] <- 1
N_NA_0802 <- rownames(merged2venn1_cleaned)[merged2venn1_cleaned[,1] > 0]
N_BU_0802 <- rownames(merged2venn1_cleaned)[merged2venn1_cleaned[,2] > 0]
N_NG_0802 <- rownames(merged2venn1_cleaned)[merged2venn1_cleaned[,3] > 0]

library("venn")
sample_list <- list(N_NA_0802 = N_NA_0802, N_BU_0802 = N_BU_0802,N_NG_0802=N_NG_0802)
color=c( "#3C5488B2","#00A087B2", 
         "#F39B7FB2","#91D1C2B2", 
         "#8491B4B2", "#DC0000B2", 
         "#7E6148B2","yellow", 
         "darkolivegreen1", "lightskyblue", 
         "darkgreen", "deeppink", "khaki2", 
         "firebrick", "brown1", "darkorange1", 
         "cyan1", "royalblue4", "darksalmon", 
         "darkgoldenrod1", "darkseagreen", "darkorchid")

venn<-venn(sample_list,zcolor = color[1:(length(sample_list))])
pdf("data/figs/3samples_venn2.pdf",width = 6,height = 8)
venn(sample_list,zcolor = color[1:(length(sample_list))],font.family="serif")
dev.off()


merged2venn1<-merged2all%>%select(c("1-N-NG4-1","1-N-HA","1-N-NJ"))
merged2venn1_cleaned<-merged2venn1[rowSums(merged2venn1[])>0,]
dim(merged2venn1_cleaned)

merged2venn1_cleaned[merged2venn1_cleaned != 0] <- 1
N_NG4_1 <- rownames(merged2venn1_cleaned)[merged2venn1_cleaned[,1] > 0]
N_HA <- rownames(merged2venn1_cleaned)[merged2venn1_cleaned[,2] > 0]
N_NJ <- rownames(merged2venn1_cleaned)[merged2venn1_cleaned[,3] > 0]

library("venn")
sample_list <- list(N_NG4_1 = N_NG4_1, N_HA = N_HA,N_NJ=N_NJ)
color=c( "#3C5488B2","#00A087B2", 
         "#F39B7FB2","#91D1C2B2", 
         "#8491B4B2", "#DC0000B2", 
         "#7E6148B2","yellow", 
         "darkolivegreen1", "lightskyblue", 
         "darkgreen", "deeppink", "khaki2", 
         "firebrick", "brown1", "darkorange1", 
         "cyan1", "royalblue4", "darksalmon", 
         "darkgoldenrod1", "darkseagreen", "darkorchid")

venn<-venn(sample_list,zcolor = color[1:(length(sample_list))])
pdf("data/figs/3samples_venn3.pdf",width = 6,height = 8)
venn(sample_list,zcolor = color[1:(length(sample_list))],font.family="serif")
dev.off()


merged2venn1<-merged2all%>%select(c("2-N-HA","2-N-NG4-1","2-N-NJ"))
merged2venn1_cleaned<-merged2venn1[rowSums(merged2venn1[])>0,]
dim(merged2venn1_cleaned)

merged2venn1_cleaned[merged2venn1_cleaned != 0] <- 1
N_HA <- rownames(merged2venn1_cleaned)[merged2venn1_cleaned[,1] > 0]
N_NG4_1 <- rownames(merged2venn1_cleaned)[merged2venn1_cleaned[,2] > 0]
N_NJ <- rownames(merged2venn1_cleaned)[merged2venn1_cleaned[,3] > 0]

library("venn")
sample_list <- list(N_NG4_1 = N_NG4_1, N_HA = N_HA,N_NJ=N_NJ)
color=c( "#3C5488B2","#00A087B2", 
         "#F39B7FB2","#91D1C2B2", 
         "#8491B4B2", "#DC0000B2", 
         "#7E6148B2","yellow", 
         "darkolivegreen1", "lightskyblue", 
         "darkgreen", "deeppink", "khaki2", 
         "firebrick", "brown1", "darkorange1", 
         "cyan1", "royalblue4", "darksalmon", 
         "darkgoldenrod1", "darkseagreen", "darkorchid")

venn<-venn(sample_list,zcolor = color[1:(length(sample_list))])
pdf("data/figs/3samples_venn4.pdf",width = 6,height = 8)
venn(sample_list,zcolor = color[1:(length(sample_list))],font.family="serif")
dev.off()

merged2venn1<-merged2all%>%select(c("3-N-HA","3-N-NG4-1","3-N-NJ"))
merged2venn1_cleaned<-merged2venn1[rowSums(merged2venn1[])>0,]
dim(merged2venn1_cleaned)

merged2venn1_cleaned[merged2venn1_cleaned != 0] <- 1
N_HA <- rownames(merged2venn1_cleaned)[merged2venn1_cleaned[,1] > 0]
N_NG4_1 <- rownames(merged2venn1_cleaned)[merged2venn1_cleaned[,2] > 0]
N_NJ <- rownames(merged2venn1_cleaned)[merged2venn1_cleaned[,3] > 0]

library("venn")
sample_list <- list(N_NG4_1 = N_NG4_1, N_HA = N_HA,N_NJ=N_NJ)
color=c( "#3C5488B2","#00A087B2", 
         "#F39B7FB2","#91D1C2B2", 
         "#8491B4B2", "#DC0000B2", 
         "#7E6148B2","yellow", 
         "darkolivegreen1", "lightskyblue", 
         "darkgreen", "deeppink", "khaki2", 
         "firebrick", "brown1", "darkorange1", 
         "cyan1", "royalblue4", "darksalmon", 
         "darkgoldenrod1", "darkseagreen", "darkorchid")

venn<-venn(sample_list,zcolor = color[1:(length(sample_list))])
pdf("data/figs/3samples_venn5.pdf",width = 6,height = 8)
venn(sample_list,zcolor = color[1:(length(sample_list))],font.family="serif")
dev.off()

### upset

merged2venn1<-merged2all%>%select(c("N_NA_0628","N_BU_0628","N_MU_0628","N_NG_0628","N_NH_0628"))
merged2venn1_cleaned<-merged2venn1[rowSums(merged2venn1[])>0,]
dim(merged2venn1_cleaned)

merged2venn1_cleaned[merged2venn1_cleaned != 0] <- 1
head(merged2venn1_cleaned)

m1 = make_comb_mat(merged2venn1_cleaned)
UpSet(m1)
UpSet(m1, top_annotation = upset_top_annotation(m1, add_numbers = TRUE),
              right_annotation = upset_right_annotation(m1, add_numbers = TRUE))


merged2venn1<-merged2all%>%select(c("N_NA_0802","N_BU_0802","N_MU_0802","N_NG_0802","N_NH_0802"))
merged2venn1_cleaned<-merged2venn1[rowSums(merged2venn1[])>0,]
dim(merged2venn1_cleaned)

merged2venn1_cleaned[merged2venn1_cleaned != 0] <- 1
head(merged2venn1_cleaned)

m1 = make_comb_mat(merged2venn1_cleaned)
UpSet(m1)
UpSet(m1, top_annotation = upset_top_annotation(m1, add_numbers = TRUE),
      right_annotation = upset_right_annotation(m1, add_numbers = TRUE))



merged2venn1<-merged2all%>%select(c("1-N-HA","1-N-BM","1-N-MG","1-N-NG4-1","1-N-NH","1-N-NJ"))
merged2venn1_cleaned<-merged2venn1[rowSums(merged2venn1[])>0,]
dim(merged2venn1_cleaned)

merged2venn1_cleaned[merged2venn1_cleaned != 0] <- 1
head(merged2venn1_cleaned)

m1 = make_comb_mat(merged2venn1_cleaned)
UpSet(m1)
UpSet(m1, top_annotation = upset_top_annotation(m1, add_numbers = TRUE),
      right_annotation = upset_right_annotation(m1, add_numbers = TRUE))


merged2venn1<-merged2all%>%select(c("2-N-HA","2-N-BM","2-N-MG","2-N-NG4-1","2-N-NH","2-N-NJ"))
merged2venn1_cleaned<-merged2venn1[rowSums(merged2venn1[])>0,]
dim(merged2venn1_cleaned)

merged2venn1_cleaned[merged2venn1_cleaned != 0] <- 1
head(merged2venn1_cleaned)

m1 = make_comb_mat(merged2venn1_cleaned)
UpSet(m1)
UpSet(m1, top_annotation = upset_top_annotation(m1, add_numbers = TRUE),
      right_annotation = upset_right_annotation(m1, add_numbers = TRUE))


merged2venn1<-merged2all%>%select(c("3-N-HA","3-N-BM","3-N-MG","3-N-NG4-1","3-N-NH","3-N-NJ"))
merged2venn1_cleaned<-merged2venn1[rowSums(merged2venn1[])>0,]
dim(merged2venn1_cleaned)

merged2venn1_cleaned[merged2venn1_cleaned != 0] <- 1
head(merged2venn1_cleaned)

m1 = make_comb_mat(merged2venn1_cleaned)
UpSet(m1)
UpSet(m1, top_annotation = upset_top_annotation(m1, add_numbers = TRUE),
      right_annotation = upset_right_annotation(m1, add_numbers = TRUE))

### heatmap

heatmap1_list<-c("N_NA_0628","N_BU_0628","N_MU_0628","N_NG_0628","N_NH_0628",
                 "N_NA_0802","N_BU_0802","N_MU_0802","N_NG_0802","N_NH_0802",
                 "1-N-HA","1-N-BM","1-N-MG","1-N-NG4-1","1-N-NH","1-N-NJ",
                 "2-N-HA","2-N-BM","2-N-MG","2-N-NG4-1","2-N-NH","2-N-NJ",
                 "3-N-HA","3-N-BM","3-N-MG","3-N-NG4-1","3-N-NH","3-N-NJ")

merged2venn1<-merged2all%>%select(heatmap1_list)
merged2venn1_cleaned<-merged2venn1[rowSums(merged2venn1[])>0,]
dim(merged2venn1_cleaned)
head(merged2venn1_cleaned)
merged2venn1_cleaned<-merged2venn1_cleaned[sort(rowSums(merged2venn1_cleaned), index=T, decreasing=TRUE)$ix, ]
merged2venn1_cleaned <- cbind(merged2venn1_cleaned, rowSums(merged2venn1_cleaned))
merged2venn1_cleaned_top50<-merged2venn1_cleaned[1:50,]
merged2venn1_cleaned_top50<-select(merged2venn1_cleaned_top50,-c(29))%>%
  t()

merged2venn1_cleaned_top50_log2<-log2(merged2venn1_cleaned_top50)
merged2venn1_cleaned_top50_log2[merged2venn1_cleaned_top50_log2==-Inf] <- 0
#Heatmap(ITS_OTU_log2)
head(merged2venn1_cleaned_top50_log2)
dim(merged2venn1_cleaned_top50_log2)
colnames(merged2venn1_cleaned_top50_log2)

#library(pheatmap)
ComplexHeatmap::pheatmap(merged2venn1_cleaned_top50_log2, legend = TRUE, 
                         cluster_rows = TRUE, 
                         cluster_cols = TRUE, 
                         heatmap_legend_param = list(title = "Log2(reads)", at = c(0,5,10,15,20)))


library(stringr)
library(circlize)
#col_fun = colorRamp2(c(-2, 0, 2), c("green", "white", "red"))
#col_fun(seq(-3, 3))
f1 = colorRamp2(seq(min(ITS_OTU_log2_ordered), max(ITS_OTU_log2_ordered), length = 3), c("white", "#EEEEEE", "red"))

col_times <-  pal_npg(c("nrc"))(length(unique(str_split_fixed(colnames(ITS_OTU_log2_ordered),"-",3)[,1])))
names(col_times) <- unique(str_split_fixed(colnames(ITS_OTU_log2_ordered),"-",3)[,1])

col_rivers <-  pal_npg(c("nrc"))(length(unique(str_split_fixed(colnames(ITS_OTU_log2_ordered),"-",3)[,2])))
col_rivers <- pal_npg(c("nrc"))(10)[c(1,3,4)]
names(col_rivers) <- unique(str_split_fixed(colnames(ITS_OTU_log2_ordered),"-",3)[,2])

mycolors <- distinctColorPalette(36)
names(mycolors) <- unique(str_split_fixed(colnames(ITS_OTU_log2_ordered),"-",3)[,3])


ha = HeatmapAnnotation(
  Sampling_times = str_split_fixed(colnames(ITS_OTU_log2_ordered),"-",3)[,1], 
  River = str_split_fixed(colnames(ITS_OTU_log2_ordered),"-",3)[,2], 
  Stream = str_split_fixed(colnames(ITS_OTU_log2_ordered),"-",3)[,3],
  col = list(Sampling_times = col_times,
             Stream = mycolors,
             River = col_rivers
  )
)

otu_colors <- distinctColorPalette(9)
names(otu_colors)<-paste0('Clade',rep(1:9))
otu_col_clade<-merge(otu_clade,data.frame(otu_colors),by.x = 'Clade',by.y = 0)
otu_col_clade$OTUs=otu_col_clade$otu_colors

Heatmap(merged2venn1_cleaned_top50_log2,
        #column_split = str_split_fixed(colnames(ITS_OTU_log2_ordered),"-",3)[,3],
        top_annotation=ha,
        left_annotation = rowAnnotation(clade=otu_clade$Clade),
        cluster_rows = FALSE,
        show_column_names = FALSE,
        show_row_names = TRUE,
        row_names_side="left",
        col=f1
)

ITS_OTU_log_t<-merge(t(ITS_OTU_log),sampledata,by.x=0, by.y=0, all.x=TRUE)
ITS_OTU_log_t
dim(ITS_OTU_log_t)

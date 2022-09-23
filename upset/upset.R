library(ComplexHeatmap)
library(magrittr)

extrafont::loadfonts()
options(stringsAsFactors = F)
setwd("~/Desktop/temp/kongxiangli/sorghum_new")

rm(list = ls())

files <- dir('degs', full.names = T)#pattern = '.*txt',
deL <- list()

for (f in files) {
  #pfx <- sub('.*counts.matrix.', '', f) %>% sub('.DESeq.*', '', .)
  pfx <- sub('degs/', '', f)
  df <- read.delim(f,sep = ",") 
  #%>% subset(pvalue < 0.05)
  deL[[pfx]]<-df$Gene.ID
  #deL[[ paste0(pfx, '_UP') ]] <- subset(df, log2FoldChange > log2(1)) %>% rownames
  #deL[[ paste0(pfx, '_DN') ]]  <- subset(df, log2FoldChange < -log2(1)) %>% rownames
}

m = make_comb_mat(deL)
deg_matrix<-list_to_matrix(deL)
write.csv(deg_matrix,'./deg_matrix.csv')
m2 = make_comb_mat(deL, top_n_sets = 5)

pdf('DiffExpr_upset_plot.pdf', 12, 6, family = 'serif')
UpSet(m2, top_annotation = upset_top_annotation(m2, add_numbers = TRUE, numbers_offset = unit(1, "mm"),
                                               numbers_rot = 0, numbers_gp = gpar(fontsize = 6)),
      right_annotation = upset_right_annotation(m2, add_numbers = TRUE))
dev.off()

ITS_OTU_t<-rownames_to_column(data.frame(t(ITS_OTU)),var="sample")
mpse_dada_its_otu_microcystis_r_s<-ITS_OTU_t %>%
  separate(sample, c("Date","river", "stream"), "-")%>%
  subset(select=-c(3))%>%
  group_by(river,Date) %>% 
  summarise_each(funs(sum))%>%
  data.frame(check.names = F)%>%
  unite(river_date, c(river,Date), sep='_')%>%
  column_to_rownames(.,'river_date')%>%
  t()
mpse_dada_its_otu_microcystis_r_s
otu_count_t_s_count<-otu_count_t_s
otu_count_t_s_count[otu_count_t_s_count[,] > 0] <- 1
otu_count_t_s_count

write.table(otu_count_t_s_count,'./figs/flower_plot_ITS_count.txt', quote=F, sep='\t', row.names = T, col.names = T)

sample_id <- colnames(otu_count_t_s)
otu_id<-rownames(otu_count_t_s)[otu_count_t_s[,1] > 0]#otu_id <- unique(ITS_OTU_r_s[,1])
otu_id <- otu_id[otu_id != '']
core_otu_id <- otu_id
otu_num <- length(otu_id)

for (i in 2:ncol(otu_count_t_s)) {
  otu_id<-rownames(otu_count_t_s)[otu_count_t_s[,i] > 0]#otu_id <- unique(ITS_OTU_r_s[,i])
  #otu_id <- otu_id[otu_id != '']
  core_otu_id <- intersect(core_otu_id, otu_id)
  otu_num <- c(otu_num, length(otu_id))
}
core_num <- length(core_otu_id)
sample_id
otu_num
core_num

library(plotrix)

#定义备选颜色
ellipse_col <- c('#6181BD4E','#F348004E','#64A10E4E','#9300264E','#464E044E',
                 '#049a0b4E','#4E0C664E','#D000004E','#FF6C004E','#FF00FF4E',
                 '#c7475b4E','#00F5FF4E','#BDA5004E','#A5CFED4E','#f0301c4E',
                 '#2B8BC34E','#FDA1004E','#54adf54E','#CDD7E24E','#9295C14E')
ellipse_col <- c(distinctColorPalette(40))
show_col(ellipse_col)
#构建作图函数（参考自 https://www.cnblogs.com/xudongliang/p/7884667.html）
flower_plot <- function(sample, otu_num, core_otu, start, a, b, r, ellipse_col, circle_col) {
  par( bty = 'n', ann = F, xaxt = 'n', yaxt = 'n', mar = c(1,1,1,1))
  plot(c(0,10),c(0,10),type='n')
  n   <- length(sample)
  deg <- 360 / n
  res <- lapply(1:n, function(t){
    draw.ellipse(x = 5 + cos((start + deg * (t - 1)) * pi / 180), 
                 y = 5 + sin((start + deg * (t - 1)) * pi / 180), 
                 col = ellipse_col[t],
                 border = ellipse_col[t],
                 a = a, b = b, angle = deg * (t - 1))
    text(x = 5 + 2.5 * cos((start + deg * (t - 1)) * pi / 180),
         y = 5 + 2.5 * sin((start + deg * (t - 1)) * pi / 180),
         otu_num[t])
    
    if (deg * (t - 1) < 180 && deg * (t - 1) > 0 ) {
      text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
           y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
           sample[t],
           srt = deg * (t - 1) - start,
           adj = 1,
           cex = 1
      )
    } else {
      text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
           y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
           sample[t],
           srt = deg * (t - 1) + start,
           adj = 0,
           cex = 1
      )
    }            
  })
  draw.circle(x = 5, y = 5, r = r, col = circle_col, border = NA)
  text(x = 5, y = 5, paste('Core:', core_otu))
}

flower_plot(sample = sample_id, otu_num = otu_num, core_otu = core_num, 
            start = 90, a = 0.5, b = 2, r = 1, ellipse_col = ellipse_col, circle_col = 'light blue')


ITS_OTU3<-ITS_OTU[,c(109:144)]
ITS_OTU3<-ITS_OTU3[rowSums(ITS_OTU3[])>0,]
dim(ITS_OTU3)
head(ITS_OTU3)
colnames(ITS_OTU3)[24] <- "3-N-NG41"

ITS_OTU6<-ITS_OTU[,c(215:250)]
ITS_OTU6<-ITS_OTU6[rowSums(ITS_OTU6[])>0,]
dim(ITS_OTU6)
head(ITS_OTU6)
colnames(ITS_OTU6)[25] <- "9-N-NG41"


ITS_OTU9<-ITS_OTU[,c(314:347)]
ITS_OTU9<-ITS_OTU9[rowSums(ITS_OTU9[])>0,]
dim(ITS_OTU9)
head(ITS_OTU9)
colnames(ITS_OTU9)[25] <- "9-N-NG41"

ITS_OTU6_t<-rownames_to_column(data.frame(t(ITS_OTU6)),var="sample")

otu_count_t_r<-ITS_OTU6_t %>%
  separate(sample, c("Date","river", "location"), "-")%>%
  subset(select=-c(1,3))%>%
  group_by(river) %>% 
  summarise_each(funs(sum))%>%
  data.frame(check.names = F)%>%
  column_to_rownames('river')%>%
  #  unite(river_location, c(river,location), sep='_')%>%
  #  column_to_rownames(.,'river_location')%>%
  t()

otu_count_t_r
otu_count_t_r_count<-otu_count_t_r
otu_count_t_r_count[otu_count_t_r_count[,] > 0] <- 1
otu_count_t_r_count

write.table(otu_count_t_r_count,'./figs/venn_plot_ITS_count_river_stage6.txt', quote=F, sep='\t', row.names = T, col.names = T)

library(VennDiagram)
venn.plot<-venn.diagram(list(G=rownames(otu_count_t_r)[otu_count_t_r[,1] > 0],
                             N=rownames(otu_count_t_r)[otu_count_t_r[,2] > 0],
                             Y=rownames(otu_count_t_r)[otu_count_t_r[,3] > 0]),
                        filename = NULL,
                        #'China_HK_swine_venn.tiff',compression="lzw",
                        cex = 2,
                        rotation=3,
                        #cat.default.pos = "outer",
                        cat.cex = 2,
                        #cat.dist = c(0.1, 0.1, 0.1),
                        cat.pos = c(-45, 45, 0),
                        fill = c(alpha("#E64B35FF",0.6),alpha("#3C5488FF",0.6),alpha("#00A087FF",0.6)),
                        #cat.default.pos = "text",
                        resolution = 300,units = 'px',lwd = 2) ### cat.default.pos = "outer" to let the caption outside
cowplot::plot_grid(venn.plot)

venn.plot<-venn.diagram(
  x = list(
    G=rownames(otu_count_t_r)[otu_count_t_r[,1] > 0],
    N=rownames(otu_count_t_r)[otu_count_t_r[,2] > 0],
    Y=rownames(otu_count_t_r)[otu_count_t_r[,3] > 0]
  ),
  #category.names = c("Booba (1995)" , "Nekfeu (663)" , "Brassens (471)"),
  #filename = 'IMG/venn.png',
  filename = NULL,
  output = TRUE ,
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  lwd = 1,
  col=c("#440154ff", '#21908dff', '#fde725ff'),
  fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha('#fde725ff',0.3)),
  cex = 0.5,
  fontfamily = "sans",
  cat.cex = 0.3,
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  cat.col = c("#440154ff", '#21908dff', '#fde725ff'),
  rotation = 1
)

venn.plot

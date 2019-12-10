library(circlize)
set.seed(999)
n <- 1000
a <- data.frame(factors = sample(letters[1:8], n, replace = TRUE), x = rnorm(n), y = runif(n))

###  https://jokergoo.github.io/circlize_book/book/
###绘图第一步是先初始化(circos.initialize),接下来绘制track，再添加基本元素。需要提一下的是，
###由于circlize绘制图是不断叠加的，因此如果我们一大段代码下来我们只能看到最终的图形，
####这里为了演示每端代码的结果，所以每次我都得初始化以及circlize.clear。
### track1

par(mar = c(1, 1, 1, 1), lwd = 0.1, cex = 0.6)
circos.par(track.height = 0.1)
circos.initialize(factors = a$factors, x = a$x) #初始化，factors来控制track数目，初始化里只有x， 没有y。这一步相当于ggplot()
circos.trackPlotRegion(factors = a$factors, y = a$y, 
                       panel.fun = function(x, y) { 
                         circos.axis()})
col <- rep(c("#FF0000", "#00FF00"), 4) #自定义一下颜色# 这里先解释一下，一个track有好几个cell，具体数目由factors决定的，向本数据集中factors有八个，因此绘制一个track，其包含八个cell。含有前缀circos.track的函数会在所有的cel里添加基本元素，而只有前缀circos.的函数可以在特定的track、cell里添加基本元素。具体看下演示。
circos.trackPoints(a$factors, a$x, a$y, col = col, pch = 16, cex = 0.5) #所有的cell里都绘制点图
circos.text(-1, 0.5, "left", sector.index = "a", track.index = 1) #在track 1中的标记为a的cell里添加text
circos.text(1, 0.5, "right", sector.index = "a")
#circos.clear()

#track2
#circos.trackHist添加柱状图，由于柱状图相对高级一点，
#因此circos.trackHist会自动创建一个track，无需我们circos.trackPlotRegion进行创建

bg.col <- rep(c("#EFEFEF", "#CCCCCC"), 4)
circos.trackHist(a$factors, a$x, bg.col = bg.col, col = NA)
#circos.clear()

#track3
#这里又得提一下，当我们绘制多个track时，
#我们添加基本元素时要指定添加到哪个track(track.index指定)、
#哪个cell(sector.index指定)里，如果不指定，那么将默认track是我们刚刚创建的那个。
#track.index、sector.index等参数可以通过get.cell.meta.data函数获取。

par(mar = c(1, 1, 1, 1), lwd = 0.1, cex = 0.6)
circos.par(track.height = 0.1)
circos.initialize(factors = a$factors, x = a$x)
circos.trackPlotRegion(factors = a$factors, y = a$y,
                       panel.fun = function(x, y) { 
                         circos.axis()})
col <- rep(c("#FF0000", "#00FF00"), 4)
circos.trackPoints(a$factors, a$x, a$y, col = col, pch = 16, cex = 0.5)
circos.text(-1, 0.5, "left", sector.index = "a", track.index = 1)
circos.text(1, 0.5, "right", sector.index = "a")
bg.col <- rep(c("#EFEFEF", "#CCCCCC"), 4)
circos.trackHist(a$factors, a$x, bg.col = bg.col, col = NA)
circos.trackPlotRegion(factors = a$factors, x = a$x, y = a$y, 
                       panel.fun = function(x, y) {
                         grey = c("#FFFFFF", "#CCCCCC", "#999999") 
                         sector.index = get.cell.meta.data("sector.index") #这个是第三个track，因为我们刚刚创建，这里这一步不用也可。
                         xlim = get.cell.meta.data("xlim")
                         ylim = get.cell.meta.data("ylim") 
                         circos.text(mean(xlim), mean(ylim), sector.index) 
                         circos.points(x[1:10], y[1:10], col = "red", pch = 16, cex = 0.6) 
                         circos.points(x[11:20], y[11:20], col = "blue", cex = 0.6)})
circos.clear()
# update第2个track中标记为d的sector
circos.updatePlotRegion(sector.index = "d", track.index = 2)
circos.points(x = -2:2, y = rep(0, 5))
xlim <- get.cell.meta.data("xlim")
ylim <- get.cell.meta.data("ylim")
circos.text(mean(xlim), mean(ylim), "updated")

#track4
circos.trackPlotRegion(factors = a$factors, y = a$y)
circos.trackLines(a$factors[1:100], a$x[1:100], a$y[1:100], type = "h")

# 接下来添加links，links可以是point到point、point到interval、interval到interval
circos.link("a", 0, "b", 0, h = 0.3) #point to point
circos.link("c", c(-0.5, 0.5), "d", c(-0.5, 0.5), col = "red", border = NA, h = 0.2) #intreval to interval
circos.link("e", 0, "g", c(-1, 1), col = "green", border = "black", lwd = 2, lty = 2) #point to interval

####circlize详述

#circlize的绘图规则是初始化(initialize)-创建track-添加图形元素-创建track-添加图形元素-…-circos.clear。
#具体参数设置以及解释由于内容太多，有兴趣的可以自己参考文档。 我认为比较重要的是要理解track、sector。
#由于基本所有的图形元素我们都是添加在sector里面，因此就需要指定track.index以及sector.index。
#接下来就用个例子来讲解一下如何操纵track、sector。
par(mar = c(1, 1, 1, 1))
factors <- letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 1)) #初始化# 绘制三个track，并显示具体信息
for (i in 1:3) { 
  circos.trackPlotRegion(ylim = c(0, 1))}
circos.info(plot = TRUE)
# 通过draw.sector()来高亮某一sector，比如a：
draw.sector(get.cell.meta.data("cell.start.degree", sector.index = "a"), 
            get.cell.meta.data("cell.end.degree", sector.index = "a"), rou1 = 1, col = "blue")
circos.clear()

# 高亮某一track， 比如第一个track：
circos.initialize(factors = factors, xlim = c(0, 1))
for (i in 1:3) { 
  circos.trackPlotRegion(ylim = c(0, 1))}
circos.info(plot = TRUE)
draw.sector(0, 360, rou1 = get.cell.meta.data("cell.top.radius", track.index = 1), 
            rou2 = get.cell.meta.data("cell.bottom.radius", track.index = 1), col = "green")
circos.clear()

# 高亮某一track某一sector，比如地2、3track中的e、f(sector)：
circos.initialize(factors = factors, xlim = c(0, 1))
for (i in 1:3) { circos.trackPlotRegion(ylim = c(0, 1))}
circos.info(plot = TRUE)
draw.sector(get.cell.meta.data("cell.start.degree", sector.index = "e"), 
            get.cell.meta.data("cell.end.degree", sector.index = "f"), 
            get.cell.meta.data("cell.top.radius", track.index = 2), 
            get.cell.meta.data("cell.bottom.radius", track.index = 3), col = "red")
circos.clear() #千万别忘了circos.clear，不然下次无法绘图。

###放大某一区域
df <- data.frame(factors = sample(letters[1:6], 100, replace = TRUE),
                 x = rnorm(100), 
                 y = rnorm(100), 
                 stringsAsFactors = FALSE)
# 放大a，b区域
zoom_df <- df %>% dplyr::filter(factors %in% c("a", "b"))
zoom_df$factors <- paste0("zoom_", zoom_df$factors)
df2 <- rbind(df, zoom_df)
xrange <- tapply(df2$x, df2$factors, function(x) max(x) - min(x))
normal_sector_index <- unique(df$factors)
zoomed_sector_index <- unique(zoom_df$factors)
sector.width <- c(xrange[normal_sector_index]/sum(xrange[normal_sector_index]), 
                  xrange[zoomed_sector_index]/sum(xrange[zoomed_sector_index]))
# 绘图
par(mar = c(1, 1, 1, 1))
circos.par(start.degree = 90)
circos.initialize(df2$factors, x = df2$x, sector.width = sector.width)
circos.trackPlotRegion(df2$factors, x = df2$x, y = df2$y, 
                       panel.fun = function(x, y) { 
                         circos.points(x, y, col = "red", pch = 16, cex = 0.5) 
                         xlim = get.cell.meta.data("xlim") 
                         ylim = get.cell.meta.data("ylim") 
                         sector.index = get.cell.meta.data("sector.index") 
                         circos.text(mean(xlim), mean(ylim), sector.index, niceFacing = TRUE)})
# 添加links
circos.link("a", get.cell.meta.data("cell.xlim", sector.index = "a"), "zoom_a", 
            get.cell.meta.data("cell.xlim", sector.index = "zoom_a"), border = NA, col = "red")
circos.clear()

#### 圈圈图+热图+进化树
set.seed(1234)
data <- matrix(rnorm(100 * 10), nrow = 10, ncol = 100)
col <- colorRamp2(c(-2, 0, 2), c("green", "black", "red"))
factors <- rep(letters[1:2], times = c(30, 70))
data_list <- list(a = data[, factors == "a"], b = data[, factors == "b"])
dend_list <- list(a = as.dendrogram(hclust(dist(t(data_list[["a"]])))), 
                  b = as.dendrogram(hclust(dist(t(data_list[["b"]])))))
circos.par(cell.padding = c(0, 0, 0, 0), gap.degree = 5)
circos.initialize(factors = factors, xlim = cbind(c(0, 0), table(factors)))
circos.track(ylim = c(0, 10), bg.border = NA, 
             panel.fun = function(x, y) {
               sector.index = get.cell.meta.data("sector.index") 
               d = data_list[[sector.index]] 
               dend = dend_list[[sector.index]] 
               d2 = d[, order.dendrogram(dend)] 
               col_data = col(d2)
               nr = nrow(d2)
               nc = ncol(d2) 
               for (i in 1:nr) { 
                 circos.rect(1:nc - 1, rep(nr - i, nc), 1:nc, rep(nr - i + 1, nc),
                             border = col_data[i, ], col = col_data[i, ]) }})
max_height <- max(sapply(dend_list, function(x) attr(x, "height")))
circos.track(ylim = c(0, max_height), 
             bg.border = NA, track.height = 0.3,
             panel.fun = function(x, y) { 
               sector.index = get.cell.meta.data("sector.index")
               dend = dend_list[[sector.index]]
               circos.dendrogram(dend, max_height = max_height)})
circos.clear()

#多图排列
#直接用layout设置
layout(matrix(1:9, 3, 3))
for (i in 1:9) {
  factors = letters[1:8]
  par(mar = c(0.5, 0.5, 0.5, 0.5)) 
  circos.par(cell.padding = c(0, 0, 0, 0)) 
  circos.initialize(factors = factors, xlim = c(0, 1)) 
  circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.05, 
                         bg.col = rand_color(8), bg.border = NA)
# 绘制links 
  for(i in 1:20){
    se = sample(letters[1:8], 2) 
    circos.link(se[1], runif(2), se[2], runif(2),
                col = rand_color(1, transparency = 0.4), border = NA)}
  } 
circos.clear()
  


data <- read.table("CIBERSORT.filter.txt",
                   sep="\t",header=T,row.names=1,check.names=F)

library(tidyverse)
data2 <- data %>% 
  select_if( ~any(.x != 0)) %>% 
  cor() %>% 
  replace(lower.tri(., TRUE), NA) %>% 
  data.frame(check.names=F)%>% 
  rownames_to_column() %>% 
  rename(celltypes = rowname) %>% 
  gather(key = "cells", value = "correlation",-1) %>% 
  na.omit() %>% 
  mutate(correlation = ifelse(correlation > 0, 1, 0)) %>% 
  separate(celltypes, sep = " ", into = "cell", remove = FALSE) %>% 
  mutate(cell = paste(cell, "cells", sep = " ")) %>% 
  select(-cells) %>% 
  distinct(celltypes, .keep_all = TRUE) %>% 
  spread(celltypes, correlation, fill = 0) %>% 
  column_to_rownames("cell") %>% 
  .[,apply(., 2, function(x) any(x != 0))] %>% 
  .[apply(., 1, function(x) any(x != 0)),]

library(circlize)  
data_circos <- as.matrix(data2)
set.seed(1234) #这步固定颜色的，保证你和我画出来的颜色一样，你也可以去掉试试
chordDiagram(data_circos,annotationTrack="grid",preAllocateTracks=list(track.height = 0.5),
             transparency = 0.75)
circos.trackPlotRegion(track.index=1, 
                       panel.fun=function(x,y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name=get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1], sector.name,facing="clockwise",
              niceFacing=TRUE,adj=c(0,0.5), cex = 0.75)
  },
  bg.border=NA)


load("example/abandance.RData")
load("example/otu.RData")

species = intersect(colnames(abandance), otu[[2]])
abandance = abandance[, species]

cate = rownames(abandance)[c(1:10)]

library(RColorBrewer)
col1 = brewer.pal(5, "Set1")
names(col1) = species
col2 = brewer.pal(length(cate), "Set3")
names(col2) = cate

df = NULL
for(type in cate) {
  for(s in species) {
    l = otu[[2]] == s
    n = sum(l)
    n = 1
    dd = data.frame(type = rep(type, n), species = rep(s, n), vaule1 = abandance[type, s], value2 = sum(otu[l, type]))
    df = rbind(df, dd)
  }
}

df[[1]] = as.character(df[[1]])
df[[2]] = as.character(df[[2]])

sector = NULL
sector_xlim = NULL
for(t in unique(df[[1]])) {
  sector = c(sector, t)
  sector_xlim = rbind(sector_xlim, c(0, sum(df[df[[1]] == t, 3])))
}
for(t in unique(df[[2]])) {
  sector = c(sector, t)
  sector_xlim = rbind(sector_xlim, c(0, sum(df[df[[2]] == t, 4])))
}

library(circlize)


circos.clear()
circos.par(cell.padding = c(0, 0, 0, 0), start.degree = 270, gap.degree = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 10, 1, 1, 1, 1, 10))
circos.initialize(factors = factor(sector, levels = sector), xlim = sector_xlim,
                  sector.width = c(sector_xlim[1:10,2]/sum(sector_xlim[1:10,2]), 1*sector_xlim[11:15,2]/sum(sector_xlim[11:15,2])))

circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
  sector.index = get.cell.meta.data("sector.index")
  if(sector.index %in% sector[11:15]) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    l = otu[[2]] == sector.index
    x = seq(0, by = 3, length = sum(l))
    x = x + mean(xlim) - mean(x)
    circos.rect(xlim[1], ylim[1], xlim[2], ylim[2], col = col1[sector.index], border = NA)
    circos.text(x, rep(0, sum(l)), otu[l, 1], col = "white", facing = "clockwise", niceFacing = TRUE, adj = c(-0.1, 0.5), cex = 0.6)
    
  }
}, bg.border = NA, track.height = 0.08)
circos.trackPlotRegion(ylim = c(1, 10), panel.fun = function(x, y) {
  sector.index = get.cell.meta.data("sector.index")
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  if(sector.index %in% sector[11:15]) {
    l = otu[[2]] == sector.index
    df = otu[l, sector[1:10]]
    x = seq(0, by = 3, length = sum(l))
    x = x + mean(xlim) - mean(x)
    for(i in seq_len(nrow(df))) {
      circos.lines(c(x[i], x[i]), c(1, ncol(df)), lwd = 0.5)
    }
    for(j in seq_len(ncol(df))) {
      circos.lines(c(x[1], x[nrow(df)]), c(j, j), lwd = 0.5)
    }
    for(i in seq_len(nrow(df))) {
      for(j in seq_len(ncol(df))) {
        circos.points(x[i], j, pch = 16, cex = df[i,j]/20)
      }
    }
    if(sector.index %in% sector[11:15]) {
      circos.text(rep(x[1], ncol(df)), seq_len(ncol(df)), colnames(df), cex = 0.6, facing = "bending.inside", niceFacing = TRUE, adj = c(1.1, 0.5))
    }
  }
}, bg.border = NA, track.height = 0.2)

circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
  sector.index = get.cell.meta.data("sector.index")
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  circos.text(mean(xlim), mean(ylim), sector.index, cex = 0.7, facing = "bending.inside", niceFacing = TRUE)
}, track.height = 0.05, bg.border = NA)

circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
  circos.axis(h = "top", labels.cex = 0.4, major.tick.percentage = 0.4)
}, track.height = 0.02, bg.col = c(col2, col1), track.margin = c(0, 0.01))

circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
}, track.height = 0.02, track.margin = c(0, 0.01))

accum_species = sapply(species, function(x) get.cell.meta.data("xrange", sector.index = x)); names(accum_species) = species
accum_cate = sapply(cate, function(x) get.cell.meta.data("xrange", sector.index = x)); names(accum_cate) = cate
for(i in seq_len(nrow(df))) {
  circos.link(df[i,1], c(accum_cate[df[i,1]], accum_cate[df[i,1]] - df[i, 3]),
              df[i,2], c(accum_species[df[i,2]], accum_species[df[i,2]] - df[i, 4]),
              col = paste0(col1[df[i,2]], "80"), border = NA)
  
  circos.rect(accum_cate[df[i,1]], 0, accum_cate[df[i,1]] - df[i, 3], 1, sector.index = df[i,1], col = col1[df[i,2]])
  circos.rect(accum_species[df[i,2]], 0, accum_species[df[i,2]] - df[i, 4], 1, sector.index = df[i,2], col = col2[df[i,1]])
  
  accum_cate[df[i,1]] = accum_cate[df[i,1]] - df[i, 3]
  accum_species[df[i,2]] = accum_species[df[i,2]] - df[i, 4]
}
circos.clear()

mat = matrix(sample(100, 25), 5)
rownames(mat) = letters[1:5]
colnames(mat) = letters[1:5]

library(circlize)
chordDiagram(mat, directional = TRUE, transparency = 0.5)


library(circlize)

mat = matrix(sample(1:100, 18, replace = TRUE), 3, 6)
rownames(mat) = letters[1:3]
colnames(mat) = LETTERS[1:6]
rn = rownames(mat)
cn = colnames(mat)

circos.par(gap.after = c(rep(2, nrow(mat)-1), 10, rep(2, ncol(mat)-1), 10))
chordDiagram(mat, annotationTrack = "grid", transparency = 0.5,
             preAllocateTracks = list(track.height = 0.1))
for(si in get.all.sector.index()) {
  circos.axis(h = "top", labels.cex = 0.3, sector.index = si, track.index = 2)
}
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  circos.lines(xlim, c(mean(ylim), mean(ylim)), lty = 3)
  for(p in seq(0, 1, by = 0.25)) {
    circos.text(p*(xlim[2] - xlim[1]) + xlim[1], mean(ylim), p, cex = 0.4, adj = c(0.5, -0.2), niceFacing = TRUE)
  }
  circos.text(mean(xlim), 1.4, sector.name, niceFacing = TRUE)
}, bg.border = NA)
circos.clear()


library(circlize)

bed1 = generateRandomBed(nr = 100)
bed1 = bed1[sample(nrow(bed1), 20), ]
bed2 = generateRandomBed(nr = 100)
bed2 = bed2[sample(nrow(bed2), 20), ]
circos.par("track.height" = 0.1, cell.padding = c(0, 0, 0, 0))
circos.initializeWithIdeogram()

circos.genomicLink(bed1, bed2, col = sample(1:5, 20, replace = TRUE), border = NA)
circos.clear()



arg_tax_is_plasmids_1174<-read.table('../../microbiome/data/arg_tax_is_plasmids_1174.txt', stringsAsFactors = FALSE, sep='\t', head=F, quote = "#")
##arg_tax_is_plasmids_1174_IS<-subset(arg_tax_is_plasmids_1174,!is.na(arg_tax_is_plasmids_1174$V10))
arg_tax_is_plasmids_1174_IS<-subset(arg_tax_is_plasmids_1174,V10!='')
head(arg_tax_is_plasmids_1174_IS)
dim(arg_tax_is_plasmids_1174_IS)

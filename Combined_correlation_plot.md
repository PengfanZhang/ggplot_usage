# Plot combined correlation plots with ggplot2

----

![image](https://user-images.githubusercontent.com/54542240/124382386-771e2900-dcc7-11eb-86c1-1aaec5d1d1bf.png)

This tutorial shows how to plot the above figure with ggplot2

Package ggcor has been developed for this purpose https://github.com/hannet91/ggcor.

Here I show how to plot this figure from scratch with ggplot2

```

---
title: "combined_corr"
output: html_document
---

```{r}
library(ggcorrplot)
library(ggplot2)
library(readr)

triangle_dat <- as.data.frame(read_delim("./Desktop/triangle_corr.txt", delim="\t"))

triangle_dat <- lapply(triangle_dat, function(x) gsub("*", "", x, fixed = TRUE))
triangle_dat <- as.data.frame(triangle_dat)
triangle_dat[, 2:ncol(triangle_dat)] <- lapply(triangle_dat[, 2:ncol(triangle_dat)], as.numeric)
triangle_dat <- as.data.frame(triangle_dat)
rownames(triangle_dat) <- triangle_dat[, 1]
triangle_dat <- triangle_dat[, -1]
 ggcorrplot(triangle_dat)


## create axis coordinates for triangle plot

# x axis
lower_tri <- triangle_dat[lower.tri(triangle_dat)]
xx <- rep(ncol(triangle_dat), ncol(triangle_dat)-1)
for (i in (ncol(triangle_dat)-1):1){
  xx <- c(xx, rep(i, i-1))
}
xx <- xx + 4

# y axis
yy <- c()
for (i in (ncol(triangle_dat)-1):1){
  for (j in seq(ncol(triangle_dat)-i, ncol(triangle_dat)-1)){
    yy <- c(yy, j)
  }
}
yy <- yy + 1

wwt <- data.frame(x=xx, y=yy, corr = lower_tri)
wwt$color <- 1
wwt$color[which(wwt$corr<0)] <- -1
#p <- ggplot() + geom_tile(aes(x = x, y = y),wwt,fill=NA,color='gray',size=0.5) + geom_point(aes(x, y, size=abs(corr), fill=corr), wwt, shape=21, color="white") + scale_fill_distiller(palette="RdYlBu") + scale_size(range = c(1, 8))
p <- ggplot() + geom_tile(aes(x = x, y = y),wwt,fill=NA,color='gray',size=0.5) + geom_point(aes(x, y, size=abs(corr), color=as.factor(color)), wwt) + scale_size_continuous(name="Absolute correlation", range = c(3, 10))

# axis for text in triangle plot
x_text <- seq(1, ncol(triangle_dat)) + 4
y_text <- seq(ncol(triangle_dat), 1)

t_text_dat <- data.frame(text=rev(colnames(triangle_dat)), x=x_text, y=y_text)

```
![image](https://user-images.githubusercontent.com/54542240/124382398-869d7200-dcc7-11eb-9ae3-78776758a7cc.png)

## plot points
```{r}
point_con <- as.data.frame(read_delim("./point_connect.txt", delim="\t"))

rownames(point_con) <- point_con[, 1]
point_con <- point_con[, -1]
sig_p_con <- point_con[lower.tri(point_con)]
sig_p_con1 <- rep('NS', length(sig_p_con))
sig_p_con1[grepl("*", sig_p_con, fixed = TRUE)] <- 'Sig'
point_con <- lapply(point_con, function(x) gsub("*", "", x, fixed = TRUE))
point_con <- as.data.frame(point_con)
point_con[, 2:ncol(point_con)] <- lapply(point_con[, 2:ncol(point_con)], as.numeric)
point_con <- as.data.frame(point_con)


## create axis for points

# x and y axis
x_ss <- min(wwt$x) - 3
x_p <- x_ss
y_p <- max(wwt$y) - 0.5
sec_part <- nrow(point_con)-2+1
for (i in 1:sec_part){
  x_p <- c(x_p, x_ss + (ncol(triangle_dat)-2)*(1-cos(i*5*pi/(12*sec_part))))
  y_p <- c(y_p, max(wwt$y) - 1 - (ncol(triangle_dat)-2)*sin(i*5*pi/(12*sec_part)))
}

point_dat <- data.frame(point = colnames(point_con), x=x_p, y=y_p)


# connect points


point_corr <- point_con[lower.tri(point_con)]

p1 <- c()
p2 <- c()
for (i in seq(ncol(point_con)-1)){
  p1 <- c(p1, rep(colnames(point_con)[i], ncol(point_con)-i))
  p2 <- c(p2, colnames(point_con)[(i+1):ncol(point_con)])
}

point_corr <- data.frame(p1 = p1, p2 = p2, corr = point_corr)

point_corr$x <- point_dat$x[match(point_corr$p1, point_dat$point)]
point_corr$x2 <- point_dat$x[match(point_corr$p2, point_dat$point)]
point_corr$y <- point_dat$y[match(point_corr$p1, point_dat$point)]
point_corr$y2 <- point_dat$y[match(point_corr$p2, point_dat$point)]

point_corr$color <- 1
point_corr$color[which(point_corr$corr<0)] <- -1
point_corr$sig <- sig_p_con1

p <- p + geom_point(aes(x=x, y=y, fill=point), point_dat, size=4, shape=21, color="white") + scale_fill_manual(values=c("#f60404", "#7a7a7a", "#99999b", "#000000", "#457b9d","#d8d8d8", "#fcfd0c")) + geom_text(aes(x=x, y=y-0.2), point_dat, label=point_dat$point, family = "Times") + geom_curve(aes(x=x, y=y, xend=x2, yend=y2, color=as.factor(color)), point_corr[which(point_corr$sig=="Sig"), ], size=point_corr$corr[which(point_corr$sig=="Sig")], curvature = -0.2) + geom_curve(aes(x=x, y=y, xend=x2, yend=y2, color=as.factor(color)), point_corr[which(point_corr$sig=="NS"), ], size=point_corr$corr[which(point_corr$sig=="NS")], curvature = -0.2, linetype=2)

```
![image](https://user-images.githubusercontent.com/54542240/124382410-92893400-dcc7-11eb-8cf6-a263a5b0044c.png)


## combine two plots
```{r}
p_t_con <- as.data.frame(read_delim("./curve_connect.txt", delim="\t"))

rownames(p_t_con) <- p_t_con[, 1]
p_t_con <- p_t_con[, -1]
  #rownames(p_t_con) <- colnames(point_con) ## here we may need to modify, this is unnecessary
  #p_t_con <- p_t_con[, colnames(triangle_dat)] ## reorder the matrix to make the columns in the same order as in triangle_dat, this is unnecessary
Sig_p_t <- as.data.frame(matrix(-1, nrow = nrow(p_t_con), ncol = ncol(p_t_con)))
Sig_p_t[which(grepl("*", p_t_con, fixed = TRUE), arr.ind = TRUE)] <- 1
colnames(Sig_p_t) <- colnames(p_t_con)
Sig_p_t$id <- rownames(p_t_con)
Sig_p_t_tab <- reshape2::melt(Sig_p_t)

p_t_con <- lapply(p_t_con, function(x) gsub("*", "", x, fixed = TRUE))
p_t_con <- as.data.frame(p_t_con)
p_t_con[, 2:ncol(p_t_con)] <- lapply(p_t_con[, 2:ncol(p_t_con)], as.numeric)
p_t_con <- as.data.frame(p_t_con)



p_t_con$id <- rownames(p_t_con)
p_t_corr <- reshape2::melt(p_t_con)
p_t_corr$x <- point_dat$x[match(p_t_corr$id, point_dat$point)]
p_t_corr$y <- point_dat$y[match(p_t_corr$id, point_dat$point)]
p_t_corr$xend <- t_text_dat$x[match(p_t_corr$variable, t_text_dat$text)]
p_t_corr$yend <- t_text_dat$y[match(p_t_corr$variable, t_text_dat$text)]
p_t_corr$color <- 1
p_t_corr$color[which(p_t_corr$value<0)] <- -1
p_t_corr$sig <- Sig_p_t_tab$value

p + geom_curve(aes(x=x, y=y, xend=xend-0.2, yend=yend, color=as.factor(color)), p_t_corr[which(p_t_corr$sig==1), ], size=abs(p_t_corr$value[which(p_t_corr$sig==1)]), curvature = 0.1) + geom_curve(aes(x=x, y=y, xend=xend-0.2, yend=yend, color=as.factor(color)), p_t_corr[which(p_t_corr$sig==-1), ], size=abs(p_t_corr$value[which(p_t_corr$sig==-1)]), curvature = 0.1, linetype=2) + geom_text(aes(x=x, y=y), t_text_dat, label=t_text_dat$text, family="Times") + theme_void() + scale_color_discrete(name="Correlation", labels=c("Positive", "Negative")) + theme(text=element_text(family="Times"))

```
![image](https://user-images.githubusercontent.com/54542240/124382386-771e2900-dcc7-11eb-86c1-1aaec5d1d1bf.png)

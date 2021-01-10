library(dplyr)
library(data.table)
library(shiny)
library(ggplot2)
options(scipen = 999)
# global.R
ex_df <- read.table("large_ex_Inference.txt", header = TRUE, stringsAsFactors = FALSE)
ex_df$Ancestry[grep(";", ex_df$Ancestry)] <- "Missing"

x.low <- min(ex_df$PC1)
x.high <- max(ex_df$PC1)
y.low <- min(ex_df$PC2)
y.high <- max(ex_df$PC2)

uniq.grp <- unique(ex_df$Ancestry)

# gg_color_hue <- function(n) {
# hues = seq(15, 375, length = n + 1)
#hcl(h = hues, l = 65, c = 100)[1:n]
#}


Palette <- c("#1F78B4", "#33A02C", "#E31A1C", "#FF7F00", "#6A3D9A", "#B15928", "#A6CEE3", 
             "#B2DF8A", "#FB9A99", "#FDBF6F", "#CAB2D6", "#FFFF99", "#999999")

if ("Missing" %in% uniq.grp ) {
legend.group <- c(sort(uniq.grp[uniq.grp != "Missing"]), "Missing") } else {
  legend.group <- sort(uniq.grp)
}

if ("Missing" %in% uniq.grp ) {
  n = length(uniq.grp)
  cols=c(Palette[1:(n-1)],"#999999")
} else {
  cols = Palette[1:n]
  }

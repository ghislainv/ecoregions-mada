#!/usr/bin/Rscript

# ==============================================================================
# author          :Ghislain Vieilledent
# email           :ghislain.vieilledent@cirad.fr, ghislainv@gmail.com
# web             :https://ghislainv.github.io
# license         :GPLv3
# ==============================================================================

## Libraries
require(ggplot2)
require(rasterVis)
require(rgdal)
require(ggspatial)  # for layer_spatial()
require(svglite) # to export as svg
require(sf)

## Import
ecoregion <- readOGR(dsn="gisdata/vectors",layer="madagascar_ecoregion_tenaizy_38s")
## Recode ecoregion
ecoregion.data <- ecoregion@data
ecoregion.data$code <- c("s","m","h","d") # spiny, mangroves, humid, dry
ecoregion@data <- ecoregion.data

## Text
xt <- c(850000,580000,530000,600000)
yt <- c(7920000,8060000,7250000,8460000)
t.df <- data.frame(text=c("Moist","Dry","Spiny","Mangroves"),x=xt,y=yt)

## Forest 2015
for2015 <- raster("gisdata/rasters/for2015.tif")

## Segments
seg.df <- data.frame(x=c(720000),y=c(8282000),
                     xend=c(600000),yend=c(8405000))

## ggplot theme
theme_base <- theme(axis.line=element_blank(),
                    axis.text.x=element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks=element_blank(),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    plot.margin=unit(c(0,0,0,0),"null"),
                    panel.spacing=unit(c(0,0,0,0),"null"),
                    plot.background=element_rect(fill="transparent"),
                    panel.background=element_rect(fill="transparent"),
                    panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),
                    panel.border=element_blank())

## Colors
red.t <- adjustcolor("red",alpha.f=0.5)
blue.t <- adjustcolor("blue",alpha.f=0.5)
green.t <- adjustcolor("dark green",alpha.f=0.5)
orange.t <- adjustcolor("orange",alpha.f=0.5)
black.t <- adjustcolor("black",alpha.f=0.5)
eco.col <- c("h"=green.t,"d"=orange.t,"s"=red.t,"m"="blue","1"=black.t)

## Plot
plot.ecoregion <- gplot(for2015, maxpixels=1e5) +
  layer_spatial(mapping=aes(group=code, fill=factor(code)), data=ecoregion) +
  geom_raster(aes(fill=factor(value))) +
  scale_fill_manual(values=eco.col, na.value="transparent") +
  geom_text(data=t.df, aes(x=x, y=y, label=text), size=5) +
  geom_segment(data=seg.df, aes(x=x, xend=xend, y=y, yend=yend), size=0.25) +
  theme_bw() + theme_base +
  theme(legend.position="null") +
  scale_y_continuous(limits=c(7165000,8685000),expand=c(0,0)) +
  scale_x_continuous(limits=c(300000,1100000),expand=c(0,0)) +
  coord_equal() + coord_sf(datum=st_crs(32738))
ggsave("ecoregion.png", plot.ecoregion, width=10, height=15, units="cm")
ggsave("ecoregion.svg", plot.ecoregion, width=10, height=15, units="cm")

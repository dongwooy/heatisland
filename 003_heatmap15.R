# Step 1: Install packages required for web-scrapping #########
ipak <- function(pkg){
      new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
      if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
      sapply(pkg, require, character.only = TRUE)
}

packages <- c("sp","rgdal","gstat","intervals","raster","automap")
ipak(packages)


input <- "G:/workfoldergis/research/heatisland/003_attributes/tmpnhmd/003_tmp15_map.shp"
input2 <- "G:/workfoldergis/research/heatisland/002_boundaries/000_gwj_5179.shp"
tmp <- readOGR(input)
city <- readOGR(input2)

#normality test - daytime temperature distribution
qqnorm(tmp$diurnal)
qqline(tmp$diurnal)
shapiro.test(log(tmp$diurnal))
#normality test - night time temperature distribution
qqnorm(tmp$nocturnal)
qqline(tmp$nocturnal)
shapiro.test(tmp$nocturnal)
#normality test - late night temperature distribution
qqnorm(tmp$nocturnal2)
qqline(tmp$nocturnal2)
shapiro.test(tmp$nocturnal2)

# variogram
tmd.v <- variogram(diurnal~1, tmp)
plot(tmd.v, pch=20)
plot(tmd.v, pch=20, cex=1.5)

tmn.v <- variogram(nocturnal ~ 1, tmp)
plot(tmn.v, pch=20)
tmn.vf.exp <- fit.variogram(tmn.v, vgm(0.99,"Ste", 5020, 0.3,kappa=1))
plot(tmn.v, tmn.vf.exp, pch=20, cex=1.5)

tmn2.v <- variogram(nocturnal2~1, tmp)
plot(tmn2.v, pch=20)
plot(tmd2.v, pch=20, cex=1.5)

bb <- autofitVariogram(nocturnal ~ 1, tmp)
plot(bb)



tm.reg <- spsample(city, 100000, type="regular")
tm.grid <- SpatialPixels(tm.reg)

ok.exp <- krige(diurnal ~ 1, tmp, tm.grid)
plot(ok.exp)

ok2.exp <- krige(nocturnal ~ 1, tmp, tm.grid)
plot(ok2.exp)

ok3.exp <- krige(nocturnal2 ~ 1, tmp, tm.grid)
plot(ok3.exp)

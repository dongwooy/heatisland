# Step 1: Install packages required for web-scrapping #########
ipak <- function(pkg){
      new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
      if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
      sapply(pkg, require, character.only = TRUE)
}

packages <- c("sp","rgdal","gstat","intervals","raster")
ipak(packages)


input <- "G:/workfoldergis/research/heatisland/003_attributes/tmpnhmd/003_tmp14_map.shp"
input2 <- "G:/workfoldergis/research/heatisland/002_boundaries/000_gwj_5179.shp"
tmp <- readOGR(input)
city <- readOGR(input2)

#normality test - daytime temperature distribution
qqnorm(tmp$diurnal)
qqline(tmp$diurnal)
shapiro.test(tmp$diurnal)
#normality test - nighttime temperature distribution
qqnorm(tmp$nocturnal)
qqline(tmp$nocturnal)
shapiro.test(tmp$nocturnal)

# variogram
tmd.v <- variogram(diurnal ~ 1, tmp)
plot(tmd.v, pch=20)
tmd.vf.exp <- fit.variogram(tmd.v, vgm(1, "Ste", 2000, 1,kappa=1))
plot(tmd.v, tmd.vf.exp, pch=20, cex=1.5)

tmn.v <- variogram(nocturnal ~ 1, tmp)
plot(tmn.v, pch=20)
tmn.vf.exp <- fit.variogram(tmn.v, vgm(1,"Mat", 4000, 0.5,kappa=0.2))
plot(tmn.v, tmn.vf.exp, pch=20, cex=1.5)


tm.reg <- spsample(city, 100000, type="regular")
tm.grid <- SpatialPixels(tm.reg)
ok.exp <- krige(diurnal ~ 1, tmp, tm.grid, tmd.vf.exp)
plot(ok.exp)

ok2.exp <- krige(nocturnal ~ 1, tmp, tm.grid, tmn.vf.exp)
plot(ok2.exp)

spplot(ok.exp["var1.pred"])
spplot(ok.exp["var1.var"])

spplot(ok2.exp["var1.pred"])
spplot(ok2.exp["var1.var"])

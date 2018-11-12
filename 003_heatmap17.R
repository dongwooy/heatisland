# Step 1: Install packages required for web-scrapping #########
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("sp","rgdal","gstat","intervals","raster")
ipak(packages)

input <- ".../003_tmp17_map.shp"
input2 <- ".../000_gwj_5179.shp"
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

#variogram
tmd.v <- variogram(diurnal ~ 1, tmp)
plot(tmd.v, pch=20)
tmd.vf.exp <- fit.variogram(tmd.v, vgm(1, "Ste", 2000, 1,kappa=0.1))
plot(tmd.v, tmd.vf.exp, pch=20, cex=1.5)

tmn.v <- variogram(nocturnal ~ 1, tmp)
plot(tmn.v, pch=20)
tmn.vf.exp <- fit.variogram(tmn.v, vgm(0.6,"Exp", 200, 0.01))
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

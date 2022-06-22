library(rWind)
library(raster)
library(gdistance)
library(lubridate)

lat_Kampala <- 0.3476
long_Kampala <- 32.5825
coord_Kampala <- c(long_Kampala, lat_Kampala)

lat_Karachi <- 24.8607
long_Karachi <- 67.0011
coord_Karachi <- c(long_Karachi, lat_Karachi)

long_Chandigarh <- 76.7794
lat_Chandigarh <- 30.7333
coord_Chandigarh <- c(long_Chandigarh, lat_Chandigarh)

long_Nairobi <- 36.8219
lat_Nairobi <- -1.2921
coord_Nairobi <- c(long_Nairobi, lat_Nairobi)

long_Kabale <- 29.9856 
lat_Kabale <- -1.2420
coord_Kabale <- c(long_Kabale, lat_Kabale)

list_coord <- list(coord_Kabale, coord_Karachi, coord_Chandigarh)

loc <- do.call(rbind, list_coord)
colnames(loc) <- c("lon", "lat")
rownames(loc) <- c("Kabale", "Karachi", "Chandigarh")

dt <- c(seq(ymd_hms(paste(2017,3,3,00,00,00, sep="-")),
            ymd_hms(paste(2017,5,31,12,00,00, sep="-")),by="5 days"),
        seq(ymd_hms(paste(2018,3,3,00,00,00, sep="-")),
            ymd_hms(paste(2018,5,31,12,00,00, sep="-")),by="5 days"),
        seq(ymd_hms(paste(2019,3,3,00,00,00, sep="-")),
            ymd_hms(paste(2019,5,31,12,00,00, sep="-")),by="5 days"))

n_dt <- length(dt)
paths <- list(1:n_dt)
n_loc <- nrow(loc)
cost_list <- array(NA_real_, dim = c(n_loc, n_loc, n_dt))

ptm <- proc.time()

w_c <- wind.dl_2(dt, long_Kabale - 2, long_Chandigarh + 2, lat_Kabale - 2, lat_Chandigarh + 2)

path_layers <- wind2raster(w_c)

Conductance <- flow.dispersion(path_layers, type = "passive",
                               output = "transitionLayer")

proc.time() - ptm

for (i in 1:n_dt){
  cost_list[ , , i] <- costDistance(Conductance[[i]], loc)
  if (costDistance(Conductance[[i]], loc[1, ] , loc[3, ]) != Inf){
    paths[[i]] <- shortestPath(Conductance[[i]], loc[1, ], loc[3, ],
                               output="SpatialLines")
  }
}

this_path <- paths[[i]]@lines[[1]]

df <- SpatialLinesDataFrame(this_path)

connectivity <- 1 / cost_list


conn_avg <- apply(connectivity, c(1, 2), mean, na.rm = TRUE)
rownames(conn_avg) <- rownames(loc)
colnames(conn_avg) <- rownames(loc)

paths_clean <- paths[!sapply(paths, is.null)]
paths_merged <- paths_clean[[2]]
for (h in 2:length(paths_clean)) {
  paths_merged <- rbind(paths_merged,paths_clean[[h]])
}

library(spatstat)
paths_psp <- as(paths_merged, "psp")
lines_kernel <- density(paths_psp, sigma=0.4, dimyx=c(350,410))
kernel <- raster(lines_kernel)
kernel[kernel<(maxValue(kernel)*0.1)]<-NA

ext <- c(40, 100, -5, 35)
kernel <- extend(kernel, ext)
image.plot(kernel, col=bpy.colors(1000), zlim = c(0, 10),
           main = "Least cost paths density", xlab="Longitude",
           ylab="Lattitude")
lines(getMap(resolution = "high"), lwd=2)

#---















































ptm <- proc.time()
w <- wind.dl(2019, 2, 12, 12, long_Kampala - 2, long_Karachi + 2, lat_Kampala - 2, lat_Karachi + 2)
wind_layer <- wind2raster(w)
Conductance <- flow.dispersion(wind_layer, output = "transitionLayer")

AtoB<- shortestPath(Conductance,
                    c(long_Kampala, lat_Kampala), c(long_Karachi, lat_Karachi), output="SpatialLines")
BtoA<- shortestPath(Conductance,
                    c(long_Karachi, lat_Karachi), c(long_Kampala, lat_Kampala), output="SpatialLines")

proc.time() - ptm



library(fields)
library(shape)
library(rworldmap)
image.plot(wind_layer$speed, main="least cost paths by wind direction and speed",
           col=terrain.colors(10), xlab="Longitude", ylab="Lattitude", zlim=c(0,7))

lines(getMap(resolution = "low"), lwd=4)
points(-5.5, 37, pch=19, cex=3.4, col="red")
points(-5.5, 35, pch=19, cex=3.4, col="blue")
lines(AtoB, col="red", lwd=4, lty=2)
lines(BtoA, col="blue", lwd=4, lty=2)
alpha <- arrowDir(w)
Arrowhead(w$lon, w$lat, angle=alpha, arr.length = 0.05, arr.type="curved")
text(-5.75, 37.25,labels="Spain", cex= 2.5, col="red", font=2)
text(-5.25, 34.75,labels="Morocco", cex= 2.5, col="blue", font=2)
legend("toprigh", legend = c("From Spain to Morocco", "From Morocco to Spain"),
       lwd=4 ,lty = 1, col=c("red","blue"), cex=0.9, bg="white")




















loc <- matrix(c(-9.4729, -11.0422, -14.0443, -13.0395, -14.0000, -15.6000,
                -16.6000, -17.8600, -24.3800, -25.1800, 30.3331, 28.3376,
                26.2368, 26.3028, 28.4000, 28.0000, 28.2700, 28.7300, 14.9300,
                17.0700), 10, 2)
colnames(loc) <- c("lon", "lat")
rownames(loc) <- c("Morocco1", "Morocco3", "WSahara1", "WSahara2",
                   "Fuerteventura", "Gran_Canaria", "Tenerife", "La_Palma",
                   "Fogo", "San_Antonio")


dt <- c(seq(ymd_hms(paste(2012,5,3,00,00,00, sep="-")),
            ymd_hms(paste(2012,8,31,12,00,00, sep="-")),by="5 days"),
        seq(ymd_hms(paste(2013,5,3,00,00,00, sep="-")),
            ymd_hms(paste(2013,8,31,12,00,00, sep="-")),by="5 days"),
        seq(ymd_hms(paste(2014,5,3,00,00,00, sep="-")),
             ymd_hms(paste(2014,8,31,12,00,00, sep="-")),by="5 days"),
        seq(ymd_hms(paste(2015,5,3,00,00,00, sep="-")),
            ymd_hms(paste(2015,8,31,12,00,00, sep="-")),by="5 days"),
        seq(ymd_hms(paste(2016,5,3,00,00,00, sep="-")),
            ymd_hms(paste(2016,8,31,12,00,00, sep="-")),by="5 days"),
        seq(ymd_hms(paste(2017,5,3,00,00,00, sep="-")),
            ymd_hms(paste(2017,8,31,12,00,00, sep="-")),by="5 days"))

paths <- list(1:150)
cost_list <- array(NA_real_, dim=c(10,10,150))

ptm <- proc.time()

w_c <- wind.dl_2(dt,-27,-7,14,31)
proc.time() - ptm

path_layers <- wind2raster(w_c)

ptm <- proc.time()
Conductance <- flow.dispersion(path_layers, type="passive",
                               output="transitionLayer")
proc.time() - ptm


x <- costDistance(Conductance[[i]],loc)

connectivity <- 1 / x

for (i in 1:150){
  cost_list[,,i] <- costDistance(Conductance[[i]],loc)
  if (costDistance(Conductance[[i]],loc[3,] , loc[9,]) != Inf){
    paths[[i]] <- shortestPath(Conductance[[i]], loc[3,], loc[9,],
                               output="SpatialLines")
  }
}
connectivity <- 1/cost_list


conn_avg <- apply(connectivity, c(1, 2), mean, na.rm = TRUE)
rownames(conn_avg) <- rownames(loc)
colnames(conn_avg) <- rownames(loc)

mat <- matrix(0,10,10)
mat[5:10,1:4] <- t(conn_avg[1:4,5:10])
colnames(mat) <- c(colnames(t(conn_avg[1:4,5:10])),
                   rownames(t(conn_avg[1:4,5:10])))
rownames(mat) <- c(colnames(t(conn_avg[1:4,5:10])),
                   rownames(t(conn_avg[1:4,5:10])))
mat[5:10,1:4]

library(qgraph)
gr <- as.factor(c("Africa (Mr, WS)","Africa (Mr, WS)","Africa (Mr, WS)",
                  "Africa (Mr, WS)","Western Canary Islands (Frt)",
                  "Central Canary Islands (Tnr, G_C)",
                  "Central Canary Islands (Tnr, G_C)",
                  "Eastern Canary Islands (L_P)","Cape Verde (Fog, S_A)",
                  "Cape Verde (Fog, S_A)"))
qgraph(t(mat), layout= "circle", groups=gr, theme="colorblind", vsize=8,
       edge.width=1.7)
text( 0.5,-1, round(mean(mat[5,1:4]), 4) * 100, cex=1.5, font=2)
text( 0,-1.2, round(mean(mat[6,1:4]), 4) * 100, cex=1.5, font=2)
text( -0.5,-1, round(mean(mat[7,1:4]), 4) * 100, cex=1.5, font=2)
text( -1,-0.5, round(mean(mat[8,1:4]), 4) * 100, cex=1.5, font=2)
text( -1,0.5, round(mean(mat[9,1:4]), 4) * 100, cex=1.5, font=2)
text( -0.5,1, round(mean(mat[10,1:4]), 4) * 100, cex=1.5, font=2)












wind.dl_2("2018/3/15 9:00:00",-10,5,35,45)

dt <- seq(ymd_hms(paste(2018,1,1,00,00,00, sep="-")),
          ymd_hms(paste(2018,1,2,21,00,00, sep="-")),by="3 hours")
ww <- wind.dl_2(dt,-10,5,35,45)
tidy (ww)
# Solargraph simulation with R
# www.overfitting.net
# https://www.overfitting.net/2022/04/simulacion-de-solarigrafias-con-r.html


# Degrees to radians
deg2rad=function(deg) {deg*pi/180.0}

# Elevation and azimuth per day in 2022 (5min sampling) from:
# https://www.sunearthtools.com/dp/tools/pos_sun.php#annual
elevazim=read.table("year2022_elev_azim_GMT+1_NoDST.csv",
                    header=T, sep=";", dec=".")


# PLOT SUN TRAJECTORIES FOR 3 PINHOLE CAMERAS

# Pinhole cameras focal lengths
f=2*(6.5/2)  # 6.5cm pinhole camera
Rlata=f/2  # can camera (beer can diameter=6.5 cm)
Rsemilata=f  # half can camera

# Prepare Azim/Elev data
elevazimtmp=elevazim[1:365,2:ncol(elevazim)]
azimtmp=elevazimtmp[col(elevazimtmp)%%2==0]
elevtmp=elevazimtmp[col(elevazimtmp)%%2==1]
dim(azimtmp)=c(nrow(elevazimtmp), ncol(elevazimtmp)/2)
dim(elevtmp)=dim(azimtmp)

# Flat pinhole camera
png(file="pinhole1.png", width=512, height=640)
for (i in 1:nrow(azimtmp)) {
    x0=f*tan(deg2rad(azimtmp[i,]-180))
    y0=f*tan(deg2rad(elevtmp[i,]))/cos(deg2rad(azimtmp[i,]-180))
    MAXX=f*deg2rad(90)
    x0[x0 > MAXX]=NA
    x0[x0 < -MAXX]=NA
    y0[y0 < 0]=NA
        
    if (i==1) {
        plot(x0, y0, asp=1,
             xlim=c(-MAXX,MAXX), ylim=c(0,25), type='l',
             main='Flat pinhole camera', xlab='X (cm)', ylab='Y (cm)',
             col=rgb(0,0,1,0.08))        
    } else {
        lines(x0, y0, type='l', col=rgb(0,0,1,0.08)) 
    }
}
abline(h=0, v=c(-MAXX,0,MAXX), lty='dotted')
dev.off()

# Can pinhole camera
png(file="pinhole2.png", width=512, height=640)
for (i in 1:nrow(azimtmp)) {
    x0=2*Rlata*deg2rad(azimtmp[i,]-180)
    y0=2*Rlata*tan(deg2rad(elevtmp[i,]))*cos(deg2rad(azimtmp[i,]-180))
    MAXX=2*Rlata*deg2rad(90)
    x0[x0 > MAXX]=NA
    x0[x0 < -MAXX]=NA
    y0[y0 < 0]=NA
    
    if (i==1) {
        plot(x0, y0, asp=1,
             xlim=c(-MAXX,MAXX), ylim=c(0,25), type='l',
             main='Can pinhole camera', xlab='X (cm)', ylab='Y (cm)',
             col=rgb(0,0,1,0.08))        
    } else {
        lines(x0, y0, type='l', col=rgb(0,0,1,0.08)) 
    }
}
abline(h=0, v=c(-MAXX,0,MAXX), lty='dotted')
dev.off()

# Half can pinhole camera
png(file="pinhole3.png", width=512, height=640)
for (i in 1:nrow(azimtmp)) {
    x0=Rsemilata*deg2rad(azimtmp[i,]-180)
    y0=Rsemilata*tan(deg2rad(elevtmp[i,]))
    MAXX=Rsemilata*deg2rad(90)
    x0[x0 > MAXX]=NA
    x0[x0 < -MAXX]=NA
    y0[y0 < 0]=NA

    if (i==1) {
        plot(x0, y0, asp=1,
             xlim=c(-MAXX,MAXX), ylim=c(0,25), type='l',
             main='Half can pinhole camera', xlab='X (cm)', ylab='Y (cm)',
             col=rgb(0,0,1,0.08))         
    } else {
        lines(x0, y0, type='l', col=rgb(0,0,1,0.08)) 
    }
}
abline(h=0, v=c(-MAXX,0,MAXX), lty='dotted')
dev.off()



# DRAW ANALEMMAS

# Keep samples at exact hours
azimana=azimtmp[col(azimtmp)%%12==0]
elevana=elevtmp[col(elevtmp)%%12==0]
dim(azimana)=c(365, 24)
dim(elevana)=c(365, 24)

png(file="pinhole_analemma1.png", width=512, height=640)
for (i in 1:24) {
    x0=f*tan(deg2rad(azimana[,i]-180))
    y0=f*tan(deg2rad(elevana[,i]))/cos(deg2rad(azimana[,i]-180))
    MAXX=f*deg2rad(90)
    x0[x0 > MAXX]=NA
    x0[x0 < -MAXX]=NA
    y0[y0 < 0]=NA
    
    if (i==1) {
        plot(x0, y0, asp=1,
             xlim=c(-MAXX,MAXX), ylim=c(0,25), type='l',
             main='Flat pinhole camera analemmas', xlab='X (cm)', ylab='Y (cm)',
             col=rgb(0,0,1,1))         
    } else {
        lines(x0, y0, type='l', col=rgb(0,0,1,1)) 
    }
}
abline(h=0, v=c(-MAXX,0,MAXX), lty='dotted')
dev.off()

png(file="pinhole_analemma2.png", width=512, height=640)
for (i in 1:24) {
    x0=2*Rlata*deg2rad(azimana[,i]-180)
    y0=2*Rlata*tan(deg2rad(elevana[,i]))*cos(deg2rad(azimana[,i]-180))
    MAXX=2*Rlata*deg2rad(90)
    x0[x0 > MAXX]=NA
    x0[x0 < -MAXX]=NA
    y0[y0 < 0]=NA
    
    if (i==1) {
        plot(x0, y0, asp=1,
             xlim=c(-MAXX,MAXX), ylim=c(0,25), type='l',
             main='Can pinhole camera analemmas', xlab='X (cm)', ylab='Y (cm)',
             col=rgb(0,0,1,1))         
    } else {
        lines(x0, y0, type='l', col=rgb(0,0,1,1)) 
    }
}
abline(h=0, v=c(-MAXX,0,MAXX), lty='dotted')
dev.off()

png(file="pinhole_analemma3.png", width=512, height=640)
for (i in 1:24) {
    x0=Rsemilata*deg2rad(azimana[,i]-180)
    y0=Rsemilata*tan(deg2rad(elevana[,i]))
    MAXX=Rsemilata*deg2rad(90)
    x0[x0 > MAXX]=NA
    x0[x0 < -MAXX]=NA
    y0[y0 < 0]=NA
    
    if (i==1) {
        plot(x0, y0, asp=1,
             xlim=c(-MAXX,MAXX), ylim=c(0,25), type='l',
             main='Half can pinhole camera analemmas', xlab='X (cm)', ylab='Y (cm)',
             col=rgb(0,0,1,1))         
    } else {
        lines(x0, y0, type='l', col=rgb(0,0,1,1)) 
    }
}
abline(h=0, v=c(-MAXX,0,MAXX), lty='dotted')
dev.off()

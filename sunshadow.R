# Sun shadow calculation with R
# www.overfitting.net
# https://www.overfitting.net/

# https://www.sunearthtools.com/dp/tools/pos_sun.php#annual

library(tiff)
library(plotly)

# Degrees to radians
deg2rad=function(deg) {deg*pi/180.0}

# Elevation and azimuth per day in 2002 (5min sampling)
elevazim=read.table("year2002_elev_azim.csv", header=T, sep=";", dec=".")  # CSV de Excel


solsver=elevazim[elevazim$DIA=='21/06/2022',]  # summer solstice
solsver=solsver[,2:ncol(solsver)]  # keep azim/elev data
NSOLSVER=as.integer(rownames(solsver))  # row 172

solsinv=elevazim[elevazim$DIA=='21/12/2022',]  # winter solstice
solsinv=solsinv[,2:ncol(solsinv)]  # keep azim/elev data
NSOLSINV=as.integer(rownames(solsinv))  # row 355



# BASIC PLOTS

# Plot curves for days 21 (21-jun=summer solstice, 21-dec=winter solstice)
elevazimdays21=elevazim[substr(elevazim$DIA,1,2)=='21',]
elevazimdays21=elevazimdays21[,2:ncol(elevazimdays21)]  # keep azim/elev data

# CARTESIAN PLOT
# We define an empty polar plot and its layout
fig=plot_ly(type='scatter', mode='lines')
fig=fig %>% layout(
    title='Azimuth and elevation on 21th of the month',
    xaxis=list(
        range=c(0,360),
        tickvals=seq(0, 360, 45),
        title='Azimuth (deg)'
    ),
    yaxis=list(
        range=c(0,90),
        tickvals=seq(0, 90, 10),
        title='Elevation (deg)'
    ),
    showlegend=FALSE
)

# Now we populate it with curves
for (i in 1:nrow(elevazimdays21)) {
    day21=elevazimdays21[i,]  # day 21 of the month
    azim=day21[col(day21)%%2==0]
    elev=day21[col(day21)%%2==1]   
    fig=fig %>% add_trace(x=azim, y=elev, color=I("blue"))
}
fig


# POLAR PLOT
# We define an empty polar plot and its layout
fig=plot_ly(type='scatterpolar', mode='lines')
fig=fig %>% layout(
    polar=list(
        title='Elevation and Azimuth every 21th of the month',
        radialaxis=list(
            angle=90,
            range=c(0, 90),
            ticktext=seq(90, 0, -10),
            tickvals=seq(0, 90, 10),
            title='Elevation (deg)'
        ),
        angularaxis=list(
            rotation=90,
            direction='clockwise',
            ticktext=c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N"),
            tickvals=seq(0, 360, 45),
            title='Azimuth (deg)'
        )
    ),
    showlegend=FALSE
)

# Now we populate it with curves
for (i in 1:nrow(elevazimdays21)) {
    day21=elevazimdays21[i,]  # day 21 of the month
    azim=day21[col(day21)%%2==0]
    elev=day21[col(day21)%%2==1]   
    fig=fig %>% add_trace(r=90-elev, theta=azim, color=I("blue"))
}
fig



# BASIC ANALYSIS
elevazimdata=as.matrix(elevazim[,2:ncol(elevazim)])  # keep azim/elev data
azim=elevazimdata[col(elevazimdata)%%2==0]
elev=elevazimdata[col(elevazimdata)%%2==1]
dim(azim)=c(nrow(elevazimdata), ncol(elevazimdata)/2) # restitute matrix format
dim(elev)=dim(azim)


# Max sunrise/sunset angle differences
azimsolsver=azim[NSOLSVER,]
azimsolsinv=azim[NSOLSINV,]
elevsolsver=elev[NSOLSVER,]
elevsolsinv=elev[NSOLSINV,]

# Plot Azimuth and Elevation vs hour

# Summer solstice
time=seq(0,24,length.out=289)[1:288]
maxelev=max(elevsolsver[!is.na(elevsolsver)])
hmaxelev=time[which(elevsolsver==maxelev)]
plot(time, azimsolsver, ylim=c(0,315), type='l',
     main='Summer solstice Azimuth / Elevation',
     xlab=paste0('Hour (max Elev=',maxelev,'deg at ',hmaxelev,'h)'),
     ylab='Azimuth (deg) / Elevation (deg)',
     col='red', xaxt='n', yaxt='n')
lines(time, elevsolsver, type='l', col='blue')
abline(h=c(0,180), v=hmaxelev, lty='dotted')
axis(1, at=seq(0, 23, by=1), cex.axis=0.5)
axis(2, at=seq(0, 315, by=45))
legend("topleft", legend=c("Azim", "Elev"),
       col=c("red", "blue"), lty=1:1, cex=0.8)

# Winter solstice
time=seq(0,24,length.out=289)[1:288]
maxelev=max(elevsolsinv[!is.na(elevsolsinv)])
hmaxelev=time[which(elevsolsinv==maxelev)]
plot(time, azimsolsinv, ylim=c(0,315), type='l',
     main='Winter solstice Azimuth / Elevation',
     xlab=paste0('Hour (max Elev=',maxelev,'deg at ',hmaxelev,'h)'),
     ylab='Azimuth (deg) / Elevation (deg)',
     col='red', xaxt='n', yaxt='n')
lines(time, elevsolsinv, type='l', col='blue')
abline(h=c(0,180), v=hmaxelev, lty='dotted')
axis(1, at=seq(0, 23, by=1), cex.axis=0.5)
axis(2, at=seq(0, 315, by=45))
legend("topleft", legend=c("Azim", "Elev"),
       col=c("red", "blue"), lty=1:1, cex=0.8)


print(paste0("Max difference in sunrise angle: ",
    min(azimsolsinv[!is.na(azimsolsinv)])-min(azimsolsver[!is.na(azimsolsver)]),
    " deg"))

print(paste0("Max difference in sunset angle: ",
    max(azimsolsver[!is.na(azimsolsver)])-max(azimsolsinv[!is.na(azimsolsinv)]),
    " deg"))


# Daylight hours (from number of >0 5min slices)
elevtmp=elev[1:365,]
elevtmp[is.na(elevtmp)]=0
elevtmp[elevtmp<0]=0
elevtmp[elevtmp>0]=1
daylighthours=apply(elevtmp, 1, sum)*5/60  # count Elevation>0 slices
plot(daylighthours, ylab=paste0('Daylight hours (max=', max(daylighthours),
    'h, min=', min(daylighthours),'h)'), xlab='Day (1-365)',
    main='Daylight hours',
    xlim=c(0,364), ylim=c(0,15),
    type='l', col='blue', xaxt='n', yaxt='n')
axis(1, at=seq(0, 365, by=30), cex.axis=0.7)
axis(2, at=seq(0, 15, by=1), cex.axis=0.7)
abline(v=c(NSOLSVER,NSOLSINV), lty='dotted')


# Check Elevation for negative values
# Conclusion: data is nor perfectly normalised -> quick approximation
for (i in 1:(nrow(elev)-1)) {
    nneg=0
    npos=0
    nzero=0
    nnull=0
    for (j in 1:ncol(elev)) {
        if (!is.na(elev[i,j]) & (elev[i,j]>0)) npos=npos+1
        if (!is.na(elev[i,j]) & (elev[i,j]<0)) nneg=nneg+1
        if (!is.na(elev[i,j]) & (elev[i,j]==0)) nzero=nzero+1
        if (is.na(elev[i,j])) nnull=nnull+1
    }
    if (1) {  #(nneg!=2) {
        # print(paste0("Row ", i, ": npos=", npos, ", nneg=", nneg,
        # ", nzero=", nzero,", nnull=", nnull,
        # ". Total=", npos+nneg+nzero+nnull))
        print(npos)
    }
}



# Read structure
SCALE=13  # px/m
HEIGHT=3  # 3m
ANGLE=1  # ANGLE=1 scales light power by Elevation, ANGLE=0 counts light hours
GAMMA=2.2  # plotting gamma lift
GAMMA=1  # plotting gamma lift

img=1-readTIFF("house.tif", native=FALSE, convert=FALSE)
DIMX=ncol(img)
DIMY=nrow(img)

# Plot scene contour
imgcontour=img*0
imgcontour[2:(DIMY-1),2:(DIMX-1)]=
    abs(img[1:(DIMY-2),2:(DIMX-1)] -
            img[2:(DIMY-1),2:(DIMX-1)]) +
    abs(img[2:(DIMY-1),1:(DIMX-2)] -
            img[2:(DIMY-1),2:(DIMX-1)])
imgcontour[imgcontour != 0]=1
writeTIFF(imgcontour, "housecontour.tif", bits.per.sample=8,
          compression="LZW")
icontour=which(imgcontour==1)


# Read months
DARKEN=4
month=array(0,c(dim(img),12))
for (m in 1:12) {
    month[,,m]=readTIFF(paste0("m",m,".tif"),
                        native=FALSE, convert=FALSE)^DARKEN
}


# LOOP THROUGH YEAR

# dia=172, 180 tramos 5min (15h) de contribución
# dia=355, 108 tramos 5min (9h)  de contribución

room=array(0,365)
iorg=which(img>0, arr.ind=TRUE)  # there is a roof

# Calculating light contributions on each day
iMAX=array(0,c(2,365))
for (dia in 1:365) {
    elevday=elev[dia,]
    elevdaytmp=elevday[!is.na(elevday) & elevday>0]
    iMAX[1,dia]=length(elevdaytmp)  # number of contributions
    iMAX[2,dia]=sum(sin(deg2rad(elevdaytmp)))  # sum of contributions
}

if (ANGLE) {
    MAXNORM=max(iMAX[2,])  # max sum of contributions
} else {
    MAXNORM=max(iMAX[1,])  # max number of contributions
}


for (dia in 1:365) {  # loop through year
    azimday=azim[dia,]
    elevday=elev[dia,]

    azimdaytmp=azimday[!is.na(azimday) & elevday>0]
    elevdaytmp=elevday[!is.na(elevday) & elevday>0]
    
    # Accumulated light on day
    if (ANGLE) {
        imgoutacum=img*0+iMAX[2,dia]  # sum of contributions on day
    } else {
        imgoutacum=img*0+iMAX[1,dia]  # number of contributions on day
    }
    
    for (k in 1:length(azimdaytmp)) {  # loop through day (all 5min slices)
        d=HEIGHT/tan(deg2rad(elevdaytmp[k]))*SCALE  # d in pixels
        dx=d*cos(deg2rad(270-azimdaytmp[k]))
        dy=d*sin(deg2rad(270-azimdaytmp[k]))
        
        j=iorg  # reset indexes
        j[,2]=j[,2]+round(dx)  # offset X (columns)
        j[,1]=j[,1]-round(dy)  # offset Y (rows)
        
        # Keep only offset coords falling into the array
        keep=j[,2]>=1 & j[,2]<=DIMX & j[,1]>=1 & j[,1]<=DIMY
        j=j[keep]
        dim(j)=c(length(j)/2,2) # restore 2D format

        # Subtract shadows
        imgoutacum[j]=imgoutacum[j]-ifelse(ANGLE,sin(deg2rad(elevdaytmp[k])),1)
        
        # Shadows animation
        if (dia==-1) {
            imgout=img*0
            imgout[j]=img[iorg]
            imgout[icontour]=1-imgout[icontour]
            writeTIFF(1-imgout, paste0("houseshadow_",
                ifelse(k<10,"00",ifelse(k<100,"0","")),k,".tif"),
                bits.per.sample=8, compression="LZW")
        }
    }
    
    # room[dia]=mean(imgoutacum[(450/2-50):(450/2-20),(512/2-15):(512/2+15)])

    imgoutacum=imgoutacum/MAXNORM
    imgoutacum[icontour]=0  #(imgoutacum[icontour]+0.5)%%1
    
    # Add month
    if (dia==-1) {
        m=as.integer(dia*12/367)+1  # month to which day belongs
        imes=which(month[,,m] < imgoutacum)
        imgoutacum[imes]=month[,,m][imes]
    }

    writeTIFF(imgoutacum^(1/GAMMA), paste0("houseshadowdia_",
            ifelse(dia<10,"00",ifelse(dia<100,"0","")),dia,".tif"),
            bits.per.sample=16, compression="LZW")
    print(dia)
}


# Daylight hours (from number of >0 5min slices)
plot(room, main='Living room average sunlight', xlab='Day (1-365)',
     ylim=c(0,max(room)), ylab='', type='l', lty='dotted', col='blue', xaxt='n')
lines(room_circle, col='red')
axis(1, at=seq(0, 365, by=30), cex.axis=0.7)
legend("bottomright", legend=c("Donut", "Semidonut"),
       col=c("red", "blue"), lty=1:1, cex=0.8)





# DIBUJAR TRAYECTORIAS DEL SOL EN FOTOGRAFÍA
# - Curvas diarias
# - Forma elíptica (dato: radio)
# - Analema (posición a la misma hora a lo largo del año)


########################################################################
# OFFSET

# Cómo desplazar una matriz por un offset de forma vectorizada
a=array(0, c(7,9))
DIMX=ncol(a)
DIMY=nrow(a)

b=a
a[2:5,3:5]=1

i=which(a>0, arr.ind=TRUE)  # there is roof
j=i

# Apply desired offset according to shadow
dx=-3
dy=2
j[,2]=j[,2]+dx  # offset X (columns)
j[,1]=j[,1]-dy  # offset Y (rows)

# Keep only offset coords falling into the array
keep=j[,2]>=1 & j[,2]<=DIMX & j[,1]>=1 & j[,1]<=DIMY
j=j[keep]
i=i[keep]
dim(i)=c(length(i)/2,2) # restitute 2D format
dim(j)=dim(i)

# Dump data already offset
b[j]=a[i]

a
b

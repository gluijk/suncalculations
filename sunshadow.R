# Sun trajectory and shadow casting
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

mar19=elevazim[elevazim$DIA=='19/03/2022',]  # winter solstice
mar19=mar19[,2:ncol(mar19)]  # keep azim/elev data



# BASIC PLOTS

library(plotly)

# Plot curves for days 21 (21-jun=summer solstice, 21-dec=winter solstice)
elevazimdays21=elevazim[substr(elevazim$DIA,1,2)=='21',]
elevazimdays21=elevazimdays21[,2:ncol(elevazimdays21)]  # keep azim/elev data

# CARTESIAN PLOT
# We define an empty polar plot and its layout
fig=plot_ly(type='scatter', mode='lines')
fig=fig %>% layout(
    title='Elevation and Azimuth every 21th of the month',
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
            ticktext=c("N", "NE", "E", "SE", "S", "SO", "O", "NO", "N"),
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
plot(azim, elev, type='l')


# Max sunrise/sunset angle differences
azimsolsver=azim[NSOLSVER,]
azimsolsinv=azim[NSOLSINV,]
elevsolsver=elev[NSOLSVER,]
elevsolsinv=elev[NSOLSINV,]

print(paste0("Max difference in sunrise angle: ",
    min(azimsolsinv[!is.na(azimsolsinv)])-min(azimsolsver[!is.na(azimsolsver)]),
    " deg"))

print(paste0("Max difference in sunset angle: ",
    max(azimsolsver[!is.na(azimsolsver)])-max(azimsolsinv[!is.na(azimsolsinv)]),
    " deg"))


# Daylight hours (from number of >0 5min slices)
elevtmp=elev
elevtmp[is.na(elevtmp)]=0
elevtmp[elevtmp<0]=0
elevtmp[elevtmp>0]=1
plot(apply(elevtmp, 1, sum)*5/60, ylab='Daylight hours', xlab='Day (1-365)',
     xlim=c(1,365), ylim=c(0,15), type='l', col='blue')


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

iorg=which(img>0, arr.ind=TRUE)  # there is a roof
j=i


# VERANO
azimsolsvertmp=azimsolsver[!is.na(azimsolsver) & elevsolsver>0]
elevsolsvertmp=elevsolsver[!is.na(elevsolsver) & elevsolsver>0]

imgout=img*0
imgoutacum=imgout
for (k in 1:length(azimsolsvertmp)) {  # i represents each 5min slice
    d=HEIGHT/tan(deg2rad(elevsolsvertmp[k]))*SCALE  # d in pixels
    OFFSETX=d*cos(deg2rad(270-azimsolsvertmp[k]))
    OFFSETY=d*sin(deg2rad(270-azimsolsvertmp[k]))
    
    i=iorg
    j=iorg
    
    j[,2]=j[,2]+round(OFFSETX)  # offset X (columns)
    j[,1]=j[,1]-round(OFFSETY)  # offset Y (rows)
    
    # Keep only offset coords falling into the array
    keep=!(j[,2]<1 | j[,2]>DIMX | j[,1]<1 | j[,1]>DIMY)
    j=j[keep]
    i=i[keep]
    dim(i)=c(length(i)/2,2) # restitute 2D format
    dim(j)=dim(i)
    
    imgoutacum[j]=imgoutacum[j]+
        img[i]*ifelse(ANGLE,sin(deg2rad(elevsolsvertmp[k])),1)
    imgout[j]=img[i]
    imgout[icontour]=1-imgout[icontour]
    writeTIFF(1-imgout, paste0("houseshadow_",
                               ifelse(k<10,"00",ifelse(k<100,"0","")),k,".tif"), bits.per.sample=8,
              compression="LZW")
    imgout=imgout*0
    print(k)
}

imgoutacum=imgoutacum/max(imgoutacum)
imgoutacum[icontour]=1  #-imgoutacum[icontour]
writeTIFF(1-imgoutacum, "houseshadow.tif", bits.per.sample=16,
          compression="LZW")


# INVIERNO
azimsolsinvtmp=azimsolsinv[!is.na(azimsolsinv) & elevsolsinv>0]
elevsolsinvtmp=elevsolsinv[!is.na(elevsolsinv) & elevsolsinv>0]

imgout=img*0
imgoutacum=imgout
for (k in 1:length(azimsolsinvtmp)) {  # i represents each 5min slice
    d=HEIGHT/tan(deg2rad(elevsolsinvtmp[k]))*SCALE  # d in pixels
    OFFSETX=d*cos(deg2rad(270-azimsolsinvtmp[k]))
    OFFSETY=d*sin(deg2rad(270-azimsolsinvtmp[k]))
    
    i=iorg
    j=iorg
    
    j[,2]=j[,2]+round(OFFSETX)  # offset X (columns)
    j[,1]=j[,1]-round(OFFSETY)  # offset Y (rows)
    
    # Keep only offset coords falling into the array
    keep=!(j[,2]<1 | j[,2]>DIMX | j[,1]<1 | j[,1]>DIMY)
    j=j[keep]
    i=i[keep]
    dim(i)=c(length(i)/2,2) # restitute 2D format
    dim(j)=dim(i)
    
    imgoutacum[j]=imgoutacum[j]+
        img[i]*ifelse(ANGLE,sin(deg2rad(elevsolsinvtmp[k])),1)
    imgout[j]=img[i]
    imgout[icontour]=1-imgout[icontour]
    writeTIFF(1-imgout, paste0("houseshadow_",
                               ifelse(k<10,"00",ifelse(k<100,"0","")),k,".tif"), bits.per.sample=8,
              compression="LZW")
    imgout=imgout*0
    print(k)
}

imgoutacum=imgoutacum/max(imgoutacum)
imgoutacum[icontour]=1  #-imgoutacum[icontour]
writeTIFF(1-imgoutacum, "houseshadow.tif", bits.per.sample=16,
          compression="LZW")


# BUCLE

MAXNORM=100.804877429025  # es el max(imgoutacum) del dia=171

# for (dia in NSOLSVER:NSOLSINV) {
for (dia in 1:365) {
    azimday=azim[dia,]
    elevday=elev[dia,]

    azimdaytmp=azimday[!is.na(azimday) & elevday>0]
    elevdaytmp=elevday[!is.na(elevday) & elevday>0]
    
    imgout=img*0
    imgoutacum=imgout
    for (k in 1:length(azimdaytmp)) {  # i represents each 5min slice
        d=HEIGHT/tan(deg2rad(elevdaytmp[k]))*SCALE  # d in pixels
        OFFSETX=d*cos(deg2rad(270-azimdaytmp[k]))
        OFFSETY=d*sin(deg2rad(270-azimdaytmp[k]))
        
        i=iorg
        j=iorg
        
        j[,2]=j[,2]+round(OFFSETX)  # offset X (columns)
        j[,1]=j[,1]-round(OFFSETY)  # offset Y (rows)
        
        # Keep only offset coords falling into the array
        keep=!(j[,2]<1 | j[,2]>DIMX | j[,1]<1 | j[,1]>DIMY)
        j=j[keep]
        i=i[keep]
        dim(i)=c(length(i)/2,2) # restitute 2D format
        dim(j)=dim(i)
        
        imgoutacum[j]=imgoutacum[j]+
            img[i]*ifelse(ANGLE,sin(deg2rad(elevdaytmp[k])),1)
        imgout[j]=img[i]
        imgout[icontour]=1-imgout[icontour]
        # writeTIFF(1-imgout, paste0("houseshadow_",
        #                            ifelse(k<10,"00",ifelse(k<100,"0","")),k,".tif"), bits.per.sample=8,
        #           compression="LZW")
        imgout=imgout*0
        # print(k)
    }
    
    MAX=max(imgoutacum)
    imgoutacum=imgoutacum/MAX
    imgoutacum=1-imgoutacum
    imgoutacum=imgoutacum/(MAXNORM/MAX)
    imgoutacum=1-imgoutacum 
    imgoutacum[icontour]=1  #-imgoutacum[icontour]
    writeTIFF((1-imgoutacum)^(1/2.2), paste0("houseshadowdia_",
            ifelse(dia<10,"00",ifelse(dia<100,"0","")),dia,".tif"),
            bits.per.sample=16, compression="LZW")
    print(dia)
}


# dia=172, 180 tramos 5min (15h) de contribución, max(imgoutacum)=100.5367
# dia=355, 108 tramos 5min (9h)  de contribución, max(imgoutacum)= 27.4276
# dia=171, 180 tramos 5min (15h) de contribución, max(imgoutacum)=100.8049




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
OFFSETX=-3
OFFSETY=2
j[,2]=j[,2]+OFFSETX  # offset X (columns)
j[,1]=j[,1]-OFFSETY  # offset Y (rows)

# Keep only offset coords falling into the array
keep=!(j[,2]<1 | j[,2]>DIMX | j[,1]<1 | j[,1]>DIMY)
j=j[keep]
i=i[keep]
dim(i)=c(length(i)/2,2) # restitute 2D format
dim(j)=dim(i)

# Dump data already offset
b[j]=a[i]

a
b
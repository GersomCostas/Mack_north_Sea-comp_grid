###################################
## SCRIPT : ANNUAL EGG PRODUCTION FOR NEA MACKEREL. GRIDDING NORTH SEA AREA 
## version November 2017
##
## Gersom Costas 
## gersom.costas@ieo.es
#
###################################
#
##DESCRIPTION

# The international mackerel and horse mackerel egg surveys (WGMEGS) cover spawning areas for NEA mackerel and horse mackerel stocks. The spatial and temporal distribution of sampling is designed to ensure an adequate coverage of both mackerel  and horse mackerel spawning areas. 
#The Northeast Atlantic shelf area is subdivided into 'western' and 'southern' areas for the purposes of estimating egg production and SSB. 
#The western area for NEA mackerel is from 44 degrees N (45 degrees N in the west) to 63 degrees N. It includes Biscay, the Celtic Sea and the shelf edge to the northwest of Scotland. 
#The 'southern' area for mackerel is regarded as being from 36 degrees N to 44 degrees N in the east and 45 degrees N in the west . It extends from Cape Trafalgar in the Gulf of Cadiz, around the coast of Portugal to 11 degrees W, the Cantabrian Sea and southern Biscay.
#The 'North Sea' area for mackerel is regarded as being from 52.75 degrees N to 60.25 degrees N and  -1.75 degrees in the east and 8.25 degrees  in the west . It extends in North Sea

#The plankton survey grid was designed according to the procedure described in AEPM manual (ICES, 2016). The basic sampling unit is 0.5 degree longitude * 0.5 degree latitude,  half of an ICES rectangle. 
#But in the Cantabrian coast and in Gulf of Cadiz, the standard half ICES rectangle was changed to a quarter degree latitude by one degree longitude because transects in those regions were done perpendicular to the 200 m depth contour line.

####################################################

##  SPATIAL GRID  NORTH SEA AREA.

###  North Sea:

# Latitude: lower or equal 60.25 degree N   y bigger  or equal  52.75 degree N
# Longitude: bigger or equal -1.75 degree W   y lower or equal  8.25 degree W

####################################################


# clear workspace

rm(list = ls())


##################
# load libraries
##################

if(!require(plyr)) install.packages("plyr") ; require(plyr)
if(!require(sp)) install.packages("sp") ; require(sp)
if(!require(maptools)) install.packages("maptools") ; require(maptools)
if(!require(dplyr)) install.packages("dplyr") ; require(dplyr)
if(!require(maps)) install.packages("maps") ; require(maps)
if(!require(mapdata)) install.packages("mapdata") ; require(mapdata)




# Importing  file of proportion in rectangles area that correspond sea(removing covered area by land from rectangle area)
# Rectangle area   (cos(RECT$lat*pi/180)*30*1853.2)*30*1853.2 

RECTall <- read.csv("data/rect_searatio_all.csv")

summary(RECTall)


#choose North Sea rectangles 
#North sea rectangle dimensions:  0.5 degree latitud x 0.5 degree longitude 
 
RECT_nsea<-RECTall%>%filter(lat>=52.75,lon<=8.25)%>%filter(lat<=60.25,lon>=-1.75)%>%droplevels()

#RECT_west$Area_minus_land<-RECT_west$Area*RECT_west$sea_ratio
summary(RECT_nsea)

RECT_nsea_df<-RECT_nsea

# standardised name for overlap function

RECT<-RECT_nsea

# Covert to Spatial pixel

gridded(RECT_nsea) = ~lon+lat


# Convert to Spatial Polygon

RECT_p <- as(RECT_nsea, "SpatialPolygons")

# PLOTING 

plot(RECT_p)

slotNames(RECT_p)# slot names


# Original Rectangle names

row.names(RECT_p) 


# Use the spChFIDs() method in maptools to change the  IDs of at least one of your objects to disambiguate them 

RECT_p <- spChFIDs(RECT_p, as.character(RECT_nsea@data[, 3]))

row.names(RECT_p) 


## join spatialpoligonos ( step by step)

rownames(RECT_nsea_df)<-RECT_nsea_df$RECT


## Projection

proj4string(RECT_p) <- CRS("+proj=longlat   +ellps=WGS84 +datum=WGS84")

RECT_p<-SpatialPolygonsDataFrame(RECT_p, data=RECT_nsea_df)

#View grid

plot(RECT_p)



# ploting land + grid


png("images/nsea_survey_grid.png",
    width = 5, height = 7, units = "in", pointsize = 10,
    bg = "white", res = 800,
    type = "cairo-png")

par(mar=c(2,2,2,2) + 0.1)

map(database = "worldHires",   xlim = c(-4,10), ylim = c(50,61),fill=T, type="n")

plot(RECT_p, border="grey",  xlim = c(-4,10), ylim = c(50,61))

degAxis(2, at = c(seq(50,61, by=2)),cex.axis = 0.5,las=2)

degAxis(1, at = c(seq(-4,10, by=2)), cex.axis = 0.5, las=2)

map(database = "worldHires",  xlim = c(-4,10), ylim = c(50,61),fill=T, col="darkgreen",add=T)

title("Mackerel North Sea area grid")

box()

dev.off()




rm( RECT_nsea, RECTall)


save.image("AEPM_grid_mack_NorthSea.RData") 








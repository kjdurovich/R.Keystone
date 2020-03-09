library(maps)
library(mapdata)

#Make map
m <- {map(database="world",region="USA",  col="gray80", fill=TRUE, xlim = c(-142, -115), ylim = c(43,52.5))
map(database="world",region="Canada",  col="gray80", fill=TRUE, xlim=c(-142,-115),
    ylim=c(49,55), add = TRUE)}

#Add scale
map.scale(-122, 52,  ratio=FALSE, relwidth=0.18, cex=.7)

#Add points on map
points(y= 43.3665, x= -124.2179, pch=19)       #Coos Bay
points(y= 51.2999988, x= -127.666664, pch=19)  #Smith Sound
points(y= 48.9804, x= -122.8287, pch=19)       #Semiahmoo Bay 
points(y= 49.5596, x= -123.2355, pch=19)       #Porteau Cove Provincial Park

#Add text
text(-125.9179,43.3665, "Coos Bay", cex = .7)
text(-129.96664, 51.2999988, "Smith Sound", cex=.7)
text(-120.00, 48.6804, "Semiahmoo Bay", cex=.7)
text(-121.00,49.5596, "Porteau Cove", cex = .7)


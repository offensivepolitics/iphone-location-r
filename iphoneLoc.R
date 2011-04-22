library(RSQLite)
library(RgoogleMaps)

findLocationDB <- function (basePath) {
	filename <- NA

	drv <- dbDriver("SQLite")

	for(testFileName in list.files(basePath) ) {
		# brute-force the connection
		con <- dbConnect(drv,paste(basePath,testFileName,sep=''))

		# try and list the tables. 
		# this will fail if the file is not a sqlite db
		tableList <- tryCatch(dbListTables(con), error=function(e) e)
	
		# if class of tableList is character then we've got a sqlite DB 
		if(any(class(tableList) == "character")) {
			# look for the CellLocation table
			if(length(grep("CellLocation", tableList))>0) {
			# we've found it. save this filename
				filename <- paste(basePath,"/",testFileName,sep='')
				dbDisconnect(con)
				break
			}
		}
		dbDisconnect(con)
	}

	dbUnloadDriver(drv)
	
	return(filename)
}

fetchLatLongTimestamp <- function(dbLocation,loc.table.name,accuracy=1.0) {
	
	ldata <- NA

	con <- dbConnect("SQLite", dbLocation)

	ldata <- dbReadTable(con, loc.table.name)

	# drop data where lat == 0.0 && long == 0.0
	ldata <- ldata[-which(ldata$Latitude == 0.0 & ldata$Longitude == 0.0),]

	# convert the mac timestamp to unix timestamp
        ldata$datetime <- as.POSIXlt(ldata$Timestamp, origin="2001-01-01")

	# downsample the lat long by accuracy to obscure the location
	ldata$Latitude <- ldata$Latitude / accuracy
	ldata$Longitude <- ldata$Longitude / accuracy
	
	ldata <- ldata[,c("Latitude","Longitude", "datetime")]

	dbDisconnect(con)

	return(ldata)
}

## change this to the full path of a backup of an ios 4 device
backupPath <- "C:/Documents and Settings/jjh/Application Data/apple computer/MobileSync/Backup/a6ddb1824738f61a15b3e3c87e3e8172599b7134/"

dbLoc <- findLocationDB(backupPath)

if(!is.na(dbLoc)) {
	print(sprintf("Found location database in path: %s!",dbLoc))

	## for Verizon phones
	# locs <- fetchLatLongTimestamp(dbLoc, "CdmaCellLocation")
	## for AT&T phones
	ldata <- fetchLatLongTimestamp(dbLoc, "CellLocation")

	## plot a map of all the positions
	bb <- qbbox(ldata$Latitude, ldata$Longitude)
	# zoomlevel 4 works for my data (US only) 
	zoomlevel <- 4
	# grab the map
	map <- GetMap.bbox(bb$lonR, bb$latR,zoom=zoomlevel,maptype="mobile")
	png("all-tracks.png", width=640,height=640)
	# plot the points as circles 
	PlotOnStaticMap(map,lon=ldata$Longitude,lat=ldata$Latitude,col="blue",verbose=0)
	dev.off()

	## limit the data to 4/1-4/4. I was in las vegas at the time.
	ldata.lv <- ldata[which(ldata$datetime >= as.POSIXlt('2011-04-01 23:00:00') & ldata$datetime <= as.POSIXlt('2011-04-04 14:00:00')),]
	bb.lv <- qbbox(ldata.lv$Latitude, ldata.lv$Longitude)
	# zoom level of 12 center nicely on the strip
	zoom.lv <- 12
	map.lv <- GetMap.bbox(bb.lv$lonR, bb.lv$latR,zoom=12,destfile="lv.png",maptype="mobile")
	png("lv-tracks.png",width=640,height=640)
	PlotOnStaticMap(map.lv,lon=ldata.lv$Longitude,lat=ldata.lv$Latitude,col="blue",verbose=0)
	dev.off()
	
} else {
	print(sprintf("Could not find location database in path: %s",backupPath))
}


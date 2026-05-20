setwd("T:/MARSH/Trend Analyses")
rm(list=ls(all=TRUE))
memory.limit(4095)
source("I:/R-functions/functions.r")
require(lattice)
library(sqldf)

######################################################################################################################################
# OVERVIEW
#
# This program is mostly a 2010 R translation of a previous SAS "build" program, of 
# various authorship (named variously like "MMPBld.sas", "ON MMPBld2009.sas", "MMPBld2010.sas", 
# "QCMMPBld2010.sas"). It should be run once a year to create files to input into trend analyses 
# and other projects. The main output (for birds and amphibians) is a species table, one 
# column per species, and one row per station / year, plus various covariates including 
# habitat, vegetation, route information, and visit conditions data. The final species table 
# is called Birdstns_noobserve.csv and Amphstns.csv for birds and amphibians respectively 
# (outputed to the above working directory). To make these files, various data tables 
# are integrated together, including: 
#
#	vwMmpRouteMaster - sql database on bscdata, route level information
#	vwMMPDataQuality - sql database on bscdata (not used in 2010+ analyses)
#	MARSH5.CSV - on T:/MARSH/Trend Analyses, station level information on habitat data for 1995
#	VEGCOV5.cov- on T:/MARSH/Trend Analyses, station level information on vegetation data for 1995, similar to MARSH5.COV above
#	vwMMPMarshCharacteristics - sql database on bscdata, station level information on habitat data for 1996+
#	vwMMPMarshVegetation - sql database on bscdata, station level information information on vegetation data for 1996+
#	vwMMPBirdStations - sql database on bscdata, information per station visit
#	vwMMPBirdData_TargetsAdded - sql database on bscdata, point count information per station visit
#	MMP.Species.Classification.MASTER.csv - on T:/MARSH/IBI/, species-level information for which bird species are visitors, which need to be renamed
#	mmp_codes - sql database on bscdata, species-level information for bird species guilds / marsh use codes
#	vwMmpAmphStations - sql database on bscdata, information per station visit
#	vwMMPAmphData - sql database on bscdata, point count infromation per station visit
#
# These various datasets are at different locations (SQL database on BSC server), and on 
# the T drive. Furthermore, they have different levels in the MMP hierarchy: some have a 
# row per route, a row per station/year, a row per visit/station/year, a row per species
# observation, etc. Much of the processing in this script is to properly merge these 
# different datasets across their different levels, into one final product which has
# a row per station/year, and a column per species
#
# Furthermore, this script handles a lot of errors and duplications within the different data
# sets, such as: differences in protocols between pre- and post- 2008 bird surveys, station
# level information only being recorded for station A (in some cases), improper recordings 
# of visits, etc. 
#
# OUTPUTS: This script is crucial for the MMP Trend Analyses program (see ***), as well as the 
# AOC community assessments, and other one-off projects.


################################################################################
################################################################################
#            SECTION ONE - Read in raw data
################################################################################
################################################################################

################################################################################
# 1.a IMPORT ROUTE LEVEL INFORMATION
################################################################################
# INPUT:  vwMmpRouteMaster, from the BSCDATA database.
# PROCESSSING: minor renaming, capitalizing of column values, etc.
# OUTPUT 1: mmproutes - database which contains one row per route, such as 
#	latitude and longitude, basin location, AOC designation, etc.
# OUTPUT 2: QCroutes: a list of routes which are in Quebec --> need to process BIRD surveys
#	    from Quebec differently than Great Lakes information BIRD surveys 
#	    (see Section 3C below

  mmproutemaster <- bscdata.readSQLTable("bscdata", "vwMmpRouteMaster", maxrec = 0)

  mmproutemaster <- rename(mmproutemaster, c(
	Route_Number = "route",
	AOC_CODE = "aoccode",
	latitude = "lat",
	longitude = "long",
	Zone_no = "latzone",
	Distance = "distance",
	Size_ha = "sizeha",
	Basin = "basin",
	Region = "region",
	Marsh_Route_Name = "rtename",
	Closest_Town = "nrtown",
	County = "county",
	Statprov_code = "provstat",
	Remediation = "remede",
	Type_Remediation = "remedtyp",
	Marsh_Complex = "mcomplex",
	Wetland_Classification = "wetclass",
	Recent_Map_Year = "mapyr",
	GLCWC_route = "glcwc",
	Durham_route = "durham",
	marsh_status = "coastal",
	BCR = "bcr"))

  mmproutemaster$provstat <- as.character(mmproutemaster$provstat)

# standardize Quebec routes specification in the provstat column
  mmproutemaster$provstat[unique(c(which(mmproutemaster$provstat == "PQ"),grep("QC", mmproutemaster$route)))] <- "QC"
  QCroutes <- subset(mmproutemaster, provstat == "QC")$route
  removeQC <- "NO"
  removeQC <- winDialog("yesno", message="Do you want to KEEP (default) Routes from Quebec?")
  if(removeQC != "YES"){ mmproutemaster <- subset(mmproutemaster,mmproutemaster$provstat != "QC")}
 
  mmproutes <- sqldf("select route,aoccode,lat,long,latzone,distance,sizeha,basin,bcr, region,rtename,nrtown,county,provstat,remede,remedtyp,mcomplex,wetclass,mapyr,glcwc,durham,coastal from mmproutemaster")
  mmproutes$region = toupper(mmproutes$region)
  mmproutes$basin = toupper(mmproutes$basin)
  mmproutes$reg <-ifelse(mmproutes$region == "S",1,
		ifelse(mmproutes$region == "N",3,
		ifelse(mmproutes$region == "C",2,NA)))

  mmproutes$latzone <- 	ifelse(mmproutes$lat < 43,1, ifelse(mmproutes$lat > 47,3,2))
  mmproutes$latzone[is.na(mmproutes$latzone)] <- mmproutes$reg[is.na(mmproutes$latzone)]
  mmproutes$latzone[is.na(mmproutes$lat)]<-NA

# Label if Region from Paradox file and Latzone as defined here do not agree;
  err1 <- mmproutes$reg != mmproutes$latzone | is.na(mmproutes$latzone) | is.na(mmproutes$distance) | is.na(mmproutes$long) | is.na(mmproutes$lat)
  print("ERR:Latzone and Region disagree,Distance missing, or AOC missing. 
	\n -- please see file 'error1.csv' for which rows need attention")

# export route information to .csv
  write.csv(mmproutes[err1,],"error - MMP build - Missing Route information.csv")
  write.csv(mmproutes,"MMProutes.csv",row.names=FALSE)

## MMP.ROUTES COMPLETE --


################################################################################
# 1b. IMPORT AMPH,BIRD DATA QUALITY, 1994-1998
################################################################################
# INPUT: vwMMPDataQuality from BSCDATA database
# PROCESSING: minor renaming and standardizing of data quality codes across different years and designations
# OUTPUT: mmpdatqual, a dataframe not really used (as of 2011). It is included here only because it was
#	included in previous scripts. See the two "PGM NOTES" below, from the original SAS program.
#
# PGM NOTE: DQ assignment revised in 1998/99 - need to assign DQs to most of 1998
#  amphibian data, primarily based on dates (done below);
#
# PGM NOTE: DQ assignment revised in 1998/99 - need to assign DQs to most of 1998
#  amphibian data, primarily based on dates (done below);

mmpdatqual <- bscdata.readSQLTable("bscdata", "vwMMPDataQuality", maxrec = 0)
mmpdatqual <- rename(mmpdatqual, c(Route_Number ="route", Year_surv = "year", user_id = "observer"))
mmpdatqual$nastns <- ifelse(mmpdatqual$nastns == 0,NA,mmpdatqual$nastns) 
mmpdatqual$nbstns <- ifelse(mmpdatqual$nbstns == 0,NA,mmpdatqual$nbstns) 

# Fill in values for DQ codes based on those from all previous years;
mmpdatqual$dqamph <- ifelse(mmpdatqual$year<1998,mmpdatqual$amphqual,
	ifelse(mmpdatqual$dqampht=="A",1,
	ifelse(mmpdatqual$dqampht=="B",2,
	ifelse(mmpdatqual$dqampht=="C",3, 
	ifelse(mmpdatqual$dqampht=="D",4,NA)))))
mmpdatqual$dqbird <- ifelse(mmpdatqual$year<1998,mmpdatqual$birdqual,
	ifelse(mmpdatqual$dqbirdt =="A",1,
	ifelse(mmpdatqual$dqbirdt =="B",2,
	ifelse(mmpdatqual$dqbirdt =="C",3, 
	ifelse(mmpdatqual$dqbirdt =="D",4,NA)))))

#Fill in values Habitat DQ based on Veg DQ or (for some of 1998) letter codes;
mmpdatqual$dqhabit <- ifelse(mmpdatqual$year<1998,mmpdatqual$vegqual,
	ifelse(mmpdatqual$dqhabt=="A",1,
	ifelse(mmpdatqual$dqhabt=="B",2,
	ifelse(mmpdatqual$dqhabt=="C",3, 
	ifelse(mmpdatqual$dqhabt=="D",4,mmpdatqual$vegqual)))))
write.csv(mmpdatqual,"mmpdatqual.csv",row.names= F)


################################################################################
# 1c IMPORT HABITAT DATA 1995 (APPLIES TO BOTH BIRD AND AMPHIBIANS)
################################################################################
# INPUT: MARSH5.CSV , a .csv file on the T drive (T:/MARSH/Trend Analyses), which is only for 1995,
#	which has habitat information at the station-level
# PROCESSING: minor renaming and capitalizing of the file, to make consistent with post-1995 habitat data.
# OUTPUT: habit95, 1995 information at the station level
# 
# PGM NOTES: 	1) Habitat protocol changed after 1995 season
#         	2) Some conversions below to make compatible with 1996+ data;

habit95 <- read.csv("MARSH5.csv",header=T)
habit95$pexpose <- habit95$pexpmud+habit95$pexprock
habit95$ntree <- habit95$nltree+habit95$ndtree
habit95$station <- toupper(habit95$station)
habit95$type <-toupper(habit95$type)
habit95$ntree <- ifelse(habit95$ntree > 20,50,ifelse(habit95$ntree > 11,11,ifelse(habit95$ntree > 1,1,0)))

err2 <- is.na(habit95$ntree) | is.na(habit95$pexpose) 
print("ERR:Missing values for PExpose(Pexpmud,Pexprock) or NTree(Nltree,Ndtree) 
	\n -- Review data and assign 0 if possible")

write.csv(habit95[err2,], "error - MMP build - Missing Expose mud,rock or Tree info.csv")

################################################################################
## 1d.  IMPORT VEGETATION DATA 1995 (APPLIES TO BOTH BIRD AND AMPHIBIANS)
################################################################################
# INPUT: VEGCOV5.cov, old vegetation data for 1995 only, similar to MARSH5.COV above.
# PROCESSING: renaming fields, recategorizing fields, etc., to make consistent with vegetation data
# 	collected after 1996
# OUTPUT: veg95, a data.frame with one row per station, for 1995 only
#
# PGM NOTES: 	1) 1995 data needs some reformatting to match 1996 structure
#         	2)nle=Narrow-leaf Emergents, ble=Broad-leaf emergents, tre=Tall Robust Emergents;

veg95 <- read.csv("VEGCOV5.csv",header=T)
# Convert Braun-Blanquet codes to Percent cover (using midpoint of B-B codes);#
	VV <- which(names(veg95) %in% c("wildrice", "bur_reed", "grasses", "sedge", "rushes", 
		"brush", "pickweed", "arrowhd",  "smartwd","purploos", "wwillow","cattail", "comreed"))
	for(v in VV){
		var <- veg95[,v]
		newvar <- ifelse(var=="1" | var=="p",2.5,ifelse(var=="2",15,ifelse(var=="3",37.5,ifelse(var=="4",62.5,ifelse(var=="5",87.5,0)))))
		veg95[,v] <- newvar}
# Combine grass/sedge and rush/bullrush;
	veg95$grasses <- veg95$grasses + veg95$sedge
	veg95$grasses <- ifelse(veg95$grasses >100,100,veg95$grasses)
	veg95$rushes <- veg95$rushes + veg95$brush
	veg95$rushes <- ifelse(veg95$rushes > 100, 100,veg95$rushes)
# Recode shrubs and trees from Braun-Blanquet to PShrubs (midpoints of B-B codes);
	var <- veg95[,"shrub95"]
	veg95$pshrub95 <- ifelse(var=="1" | var=="p",2.5,ifelse(var=="2",15,ifelse(var=="3",37.5,ifelse(var=="4",62.5,ifelse(var=="5",87.5,0)))))
	var <- veg95[,"trees"]
	veg95$ptree95 <- ifelse(var=="1" | var=="p",2.5,ifelse(var=="2",15,ifelse(var=="3",37.5,ifelse(var=="4",62.5,ifelse(var=="5",87.5,0)))))		
# Recode floating veg to fit 1996 onward (0=none,1=slight,2=moderate,3=dense,.=unknown);
	veg95$float95 <- ifelse(veg95$float95 == 2 | veg95$float95 == 3,2,ifelse(veg95$float95==4 | veg95$float95== 5,3,veg95$float95))
	
	veg95$station <- toupper(veg95$station)
	veg95$type <- toupper(veg95$type)
	write.csv(veg95,"veg95.csv",row.names=F)

################################################################################
## 1e. IMPORT HABITAT,1996 onward (APPLIES TO BOTH BIRD AND AMPHIBIANS)
################################################################################
# INPUT: vwMMPMarshCharacteristics from BSCDATA database, for 1996 onward
# PROCESSING: minor renaming of fields, capitalizing of field values
# OUTPUT: habread, data.frame with one row per station, per year. Similar to habit95 above, but for 1996 onward
#
# PGM NOTES:1)1997 PTree,PShrub used for first time
#             FIX:??(if possible) convert 95,96 to Percents?

  mmphabitat96master <- bscdata.readSQLTable("bscdata", "vwMMPMarshCharacteristics", maxrec = 0)
  mmphabitat96master <- rename(mmphabitat96master, c(
	 route_number= "route",
	 year_surv= "year",
	 user_id= "observer",
	 station= "station",
	 type_surv= "type",
	 emergent_veg= "pemerge",
	 open_water= "popenw",
	 exposed_mud_sand_rock= "pexpose",
	 trees= "ptree",
	 shrubs= "pshrub",
	 no_trees= "ntree",
	 no_shrubs= "nshrubs",
	 floating_plant_cover= "fplant",
	 wetland_permanency= "wetperm",
	 marsh_size= "sizet",
	 edge_code= "sampedge",
	 Land_Use_Human_influence= "landuse",
	 DQ9697m= "dq9697m",
	 area_ot_type= "areatype"))

	habread <- mmphabitat96master
	habread$station <- toupper(habread$station)
	habread$type <- toupper(habread$type)
	habread$route <- toupper(habread$route)
	habread$float <- ifelse(habread$fplant == "no",0,ifelse(habread$fplant =="sl",1,ifelse(habread$fplant =="mo",2,ifelse(habread$fplant == "de",3,NA))))
	
################################################################################
# 1f. IMPORT VEGETATION DATA, 1996 onward (APPLIES TO BOTH BIRD AND AMPHIBIANS)
################################################################################
# INPUT: vwMMPMarshVegetation from BSCDATA database, for 1996 onward
# PROCESSING: minor renaming of fields, capitalizing of field values
# OUTPUT: vegcov, one row per station, per year. Similar to veg95 above, but for 1996 onward

mmpvegetation96master <- bscdata.readSQLTable("bscdata", "vwMMPMarshVegetation", maxrec = 0)
mmpvegetation96master <- rename(mmpvegetation96master, c(
	  route_number = "route",
	  year_surv = "year",
	  user_id = "observer",
	  Station = "station",
	  type_surv = "type",
	  wild_rice = "wildrice",
	  bur_reed = "bur_reed",
	  grasses_gedges = "grasses",
	  rushes_bulrushes = "rushes",
	  pickeral_weed = "pickweed",
	  arrowhead = "arrowhd",
	  smartweed = "smartwd",
	  purple_loosestrife = "purploos",
	  water_willow = "wwillow",
	  cattail = "cattail",
	  reeds = "comreed",
	  other_1 = "other1",
	  other_2 = "other2",
	  other_3 = "other3",
	# Non_Emerg = "nonemerg",
	  other_1_Type = "o1_type",
	  other_2_type = "o2_type",
	  other_3_type = "o3_type",
	  "Old Data" = "olddat",
	  dw9697v = "dq9697v"))
mmpvegetation96master$route <- toupper(mmpvegetation96master$route)
mmpvegetation96master$station <- toupper(mmpvegetation96master$station)
mmpvegetation96master$type <- toupper(mmpvegetation96master$type)
vegcov <- mmpvegetation96master[,names(mmpvegetation96master) %in% c("o1_type", "o2_type", "o3_type", "olddat","dq9697v")==FALSE]
rm(mmpvegetation96master)
rm(mmphabitat96master)

################################################################################
################################################################################
# SECTION TWO - COMBINE DISPARATE DATA (habitat, vegetation, pre- and post- 1995
#	(APPLIES TO BOTH BIRD AND AMPHIBIANS)
################################################################################
################################################################################;

################################################################################
# 2a. REMOVE REPEATED STATIONS
################################################################################
#
# New step as of 2010. This uses an algorithm to need deal with point counts which have
# different observers, but are otherwise listed as having the same station/type/route/year
# INPUT: takes in veg95, habit95, vegcov and habread (pre- and post- 1995 habitat
# 	and vegetation data
# PROCESSING: step through all routes/stations with a repeat, and follow this algorithm:
#  	if one column's value is NA, then take the other
#  	if variable is a number, then take averge of both
#  	if the variable can have many classes, then add, using  the ";" as the seperator
#  	if its a Y/N answer, then take Y if one is Y
#  	otherwise, sadly, need to just take one value  :(
# OUTPUT: Makes a function that is applied below

# find repeated stations / sites / routes / types 
findRepeats <- function(dat) {
	reps <- which(dat$ID %in% sqldf("select ID, count(ID) as 'tally' from dat group by ID having tally > 1")$ID)
	if(length(reps) < 1){ return(dat) } else {
		repdat <- dat[reps,]
		blankdat <- sqldf("select * from repdat group by ID")	
		#  if one is NA, then take the other
		#  if variable is a number, then take averge of both
		#  if the variable can have many classes, then add, using  the ";" as the seperator
		#  if its a Y/N answer, then take Y if one is Y
		#  otherwise, sadly, need to just take one value  :(
		for(i in 1:ncol(repdat)){				# step through all routes/stations with repeats
			for(j in blankdat$ID){				# step through 
				d <- repdat[which(repdat$ID == j),i]
				if(is.numeric(repdat[,i])) {blankdat[which(blankdat$ID == j),i] <- ifelse(any(!is.na(d)),mean(d[!is.na(d)]),NA)}	# if its a numeric value, take mean
				# otherwise, its non numeric, and we need have special manipulations
				else { 	
					if(any(grepl(",", as.character(unique(dat[,i])))) | any(grepl(";", as.character(unique(dat[,i]))))) 	# if the attributes sum
						{
						dat[,i] <-as.character(dat[,i]); blankdat[,i] <- as.character(blankdat[,i])
						blankdat[which(blankdat$ID == j),i] <- ifelse(any(!is.na(d)),paste(d[!is.na(d)],collapse=";"),NA)}
					else{
						if(all(c("y","n","un") %in% unique(dat[,i]))) 		# if its a yes/no variable
							{blankdat[which(blankdat$ID == j),i] <- ifelse("y" %in% d,"y",ifelse("n" %in% d,"n",ifelse(any(!is.na(d)),d[!is.na(d)][1],NA)))}
						else {blankdat[which(blankdat$ID == j),i] <- d[1]}	# defaults to just taking first value	
						}
					}
				}
			}
		dat <- rbind(dat[-reps,],blankdat)
		return(dat[order(dat$ID),])
		}
	} # END FIND REPEATS FUNCTION


# make temporary IDs to use for combining data
veg95$ID <- paste(veg95$route,veg95$year,veg95$station,veg95$type,sep="")
habit95$ID <- paste(habit95$route,habit95$year,habit95$station,habit95$type,sep="")
vegcov$ID <- paste(vegcov$route,vegcov$year,vegcov$station,vegcov$type,sep="")
habread$ID <- paste(habread$route,habread$year,habread$station,habread$type,sep="")

veg95 <- findRepeats(veg95)		# remove routes/stations/types that are represented twice
habit95 <- findRepeats(habit95)		# remove routes/stations/types that are represented twice
vegcov <- findRepeats(vegcov)		# remove routes/stations/types that are represented twice
habread <- findRepeats(habread)		# remove routes/stations/types that are represented twice

################################################################################
# 2b. COMBINE HABITAT AND VEGETATION DATA (1995)
################################################################################
# INPUT: veg95 AND habit95 data.frames, one row per station for 1995
# PROCESSING: SQL join which merges the two data.frames
# OUTPUT: dat95 , data.frames one row per station for 1995, both vegetation and habitat information

missRowsVeg <- numeric(0)
missRowsHab <- numeric(0)
missRowsVeg <- which(veg95$ID %in% habit95$ID == FALSE)
if(length(missRowsVeg) > 0){
	print(veg95[missRowsVeg,]) 
	print("Warning: the 1995 Habitat data is missing some Routes or Stations or Types which
	are in the 1995 Vegetation data (Probably because some Habitat columns are missing data).")
	}

missRowsHab <- which(habit95$ID %in% veg95$ID == FALSE)
if(length(missRowsHab) > 1){
	print(habit95[missRowsHab,]) 
	print("Warning: the 1995 Veg data is missing some Routes or Stations or Types which
	are in the 1995 Habitat data (Probably because some Veg columns are missing data).")
	}

dat95a <- sqldf("select veg95.route,veg95.year,veg95.observer,veg95.station,veg95.type,veg95.wildrice,veg95.bur_reed,
	veg95.grasses,veg95.rushes,veg95.pickweed,veg95.arrowhd,veg95.smartwd,veg95.purploos,veg95.wwillow,veg95.cattail,
	veg95.comreed,habit95.pemerge,habit95.popenw,habit95.pexpose,veg95.ptree95 as 'ptree',habit95.ntree,veg95.pshrub95 as 'pshrub',
	veg95.float95 as 'float',habit95.wetperm,habit95.sizet,habit95.sampedge,habit95.landuse
	from veg95 left join habit95 on veg95.ID = habit95.ID")
	dat95a$ID <- paste(dat95a$route,dat95a$year,dat95a$station,dat95a$type,sep="")
dat95b <- sqldf("select habit95.route,habit95.year,habit95.observer,habit95.station,habit95.type,dat95a.wildrice,dat95a.bur_reed,
	dat95a.grasses,dat95a.rushes,dat95a.pickweed,dat95a.arrowhd,dat95a.smartwd,dat95a.purploos,dat95a.wwillow,dat95a.cattail,
	dat95a.comreed,habit95.pemerge,habit95.popenw,habit95.pexpose,dat95a.ptree,habit95.ntree,dat95a.pshrub,
	dat95a.float,habit95.wetperm,habit95.sizet,habit95.sampedge,habit95.landuse
	from habit95 left join dat95a on habit95.ID = dat95a.ID")
	dat95b$ID <- paste(dat95b$route,dat95b$year,dat95b$station,dat95b$type,sep="")
	
dat95 <- rbind(dat95a,dat95b[which(dat95b$ID %in% dat95a$ID == FALSE),])
	

################################################################################
# 2c. COMBINE HABITAT AND VEGETATION DATA (1995)
################################################################################
# INPUT: vegcov AND habread data.frames, one row per station for 1996+ data
# PROCESSING: SQL join which merges the two data.frames
# OUTPUT: data, data.frames one row per station for 1996+, both vegetation and habitat information

missRowsVeg <- numeric(0)
missRowsHab <- numeric(0)
	
missRowsVeg <- which(vegcov$ID %in% habread$ID == FALSE)
if(length(missRowsVeg) > 0){
	print(vegcov[missRowsVeg,]) 
	print("Warning: the 1996+ Habitat data is missing some Routes or Stations or Types which
	are in the 1996+ Vegetation data (Probably because some Habitat columns are missing data).")
	}

missRowsHab <- which(habread$ID %in% vegcov$ID == FALSE)
if(length(missRowsHab) > 0){
	print(habread[missRowsHab,]) 
	print("Warning: the 1996+ Veg data is missing some Routes or Stations or Types which
	are in the 1996+ Habitat data (Probably because some Veg columns are missing data).")
	}

dataa <- sqldf("select vegcov.route,vegcov.year,vegcov.observer,vegcov.station,vegcov.type,vegcov.wildrice,vegcov.bur_reed,
	vegcov.grasses,vegcov.rushes,vegcov.pickweed,vegcov.arrowhd,vegcov.smartwd,vegcov.purploos,vegcov.wwillow,vegcov.cattail,
	vegcov.comreed,habread.pemerge,habread.popenw,habread.pexpose,habread.ptree,habread.ntree,habread.pshrub,
	habread.float,habread.wetperm,habread.sizet,habread.sampedge,habread.landuse
	from vegcov left join habread on vegcov.ID = habread.ID")
	dataa$ID <- paste(dataa$route,dataa$year,dataa$station,dataa$type,sep="")
datab <- sqldf("select habread.route,habread.year,habread.observer,habread.station,habread.type,dataa.wildrice,dataa.bur_reed,
	dataa.grasses,dataa.rushes,dataa.pickweed,dataa.arrowhd,dataa.smartwd,dataa.purploos,dataa.wwillow,dataa.cattail,
	dataa.comreed,habread.pemerge,habread.popenw,habread.pexpose,habread.ptree,habread.ntree,habread.pshrub,
	dataa.float,habread.wetperm,habread.sizet,habread.sampedge,habread.landuse
	from habread left join dataa on habread.ID = dataa.ID")
	datab$ID <- paste(datab$route,datab$year,datab$station,datab$type,sep="")
data <- rbind(dataa,datab[which(datab$ID %in% dataa$ID == FALSE),])
	

################################################################################
# 2d. COMBINE HABITAT AND VEGETATION DATA FROM 1995 and 1996+
################################################################################
# INPUT: dat95 (from 2b above) and data (2c above) which are habitat and vegetation data
# 	for 1995 and 1996+ respectively
# PROCESSING: removes duplicates common to either data set (takes 1996 data by default),
#	then merges the two together (function rbind)
# OUPUT: habitat1, one row per station, per year, all years, with both vegetation
#	and habitat information

# remove duplicates in 1996+ data and 1995 data. Take the 1996 data
duplicates <- which(dat95$ID == unique(dat95$ID)[which(unique(dat95$ID) %in% unique(data$ID))])
habitat1 <- rbind(dat95[-duplicates,],data)

################################################################################
# 2e. ADD DATA QUALITY TO habitat1 (veg and habitat) DATA
################################################################################
mmpdatqual$ID <- paste(mmpdatqual$route,mmpdatqual$year,mmpdatqual$station,mmpdatqual$type,sep="")
habitat <- sqldf("select habitat1.*,mmpdatqual.dqhabit from habitat1 left join mmpdatqual on habitat1.ID = mmpdatqual.ID")

write.csv(habitat, "MMP build habitat.csv",row.names = F)
rm(habread, vegcov, dat95, veg95, habit95, dataa, datab, dat95a, dat95b)
####  ---MMP.HABITAT COMPLETE--- ###



################################################################################
# SECTION 3: BIRD DATA
################################################################################
# PGM NOTE: 1994 bird data not yet entered
################################################################################

################################################################################
# 3A IMPORT BIRD VISIT DATA, 1995 onward
################################################################################
#
# INPUT: vwMMPBirdStations, SQL table from BSCDATA
# PROCESSING: minor capitalization of route and station fields
# OUTPUT: bvisit, dataframe informatin for each visit to a station (e.g., weather 
#	info, time and date, etc.
# NOTE: this is a crucial data frame, which has one row for each VISIT to a station
#	It is important because if NO observations are recorded in the point counts
# 	data.frame below

bvisit <- bscdata.readSQLTable("bscdata", "vwMMPBirdStations", maxrec = 0)
bvisit <- rename(bvisit, c(
	 Route_Number = "route",
	 Year_surv = "year",
	 Survey_no = "visit",
	 Station = "station",
	 Month_no = "month",
	 Day_no = "day",
	 user_id = "observer",
	 Start_Time = "stime",
	 Wind = "wind",
	 Cloud = "cloud",
	 Air_Temp = "airtemp",
	 Comment = "comment"))
bvisit$station = toupper(bvisit$station)
bvisit$route = toupper(bvisit$route)
bvisit<-bvisit[,-c(which(names(bvisit) == "comment"))]	


##########################################################
# 3B.  BIRD DATA, 1994
##########################################################
# NOTE: May need to combine 94 bird stn data with later data if it cannot be incorporated to
#       a cumulative dataset;
#######################################################################################################
# 3C. BIRD DATA, 1995 onward###########################################################################
#
# INPUT: vwMMPBirdData_TargetsAdded, SQL database of bird point counts 
# OUTPUT: mmpbirddatamaster, data.frame of point counts.
# NOTE: the column "noobserve" is the final, processed column to compare observations across years
#
# NOTE, AS OF 2010, the data is being queried differently for focal species and non-focal species, in
# an effort to maintain consistency of pre-2008 data with post-2008. For the focal species (American Bittern, 
# American Coot, Black Rail, Common Moorhen, King Rail, Least Bittern, Pied-billed Grebe, Sora, Virginia Rail, 
# Yellow Rail), we're only pulling data for No_observed2, No_observed3, n_target_sp (which are the columns for 
# during and after the callback being played) plus  No_observed4 which is the only column for data pre-2008.
# For non-focals, we're using No_observed, No_observed2 (which is the 5 minutes BEFORE the callback, and 5 
# minutes during the callback), plus No_observed4.

# Note: The Quebec MMP used to do 5 minutes of silent listening, then 5 minutes of playback (different from Great Lakes)
# so, the work around doesn't apply for Quebec

# first declare the focal species
  FocalSpecies <- c("AMBI","AMCO","BLRA","COMO","KIRA","LEBI","PBGR","SORA","VIRA","YERA")

mmpbirddatamaster <- bscdata.readSQLTable("bscdata", "vwMMPBirdData_TargetsAdded", maxrec = 0)
mmpbirddatamaster$Species_Code <- toupper(mmpbirddatamaster$Species_Code)
mmpbirddatamaster$No_observed <- ifelse(is.na(mmpbirddatamaster$No_observed),0,mmpbirddatamaster$No_observed)
mmpbirddatamaster$No_observed2 <- ifelse(is.na(mmpbirddatamaster$No_observed2),0,mmpbirddatamaster$No_observed2)
mmpbirddatamaster$No_observed3 <- ifelse(is.na(mmpbirddatamaster$No_observed3),0,mmpbirddatamaster$No_observed3)
mmpbirddatamaster$No_observed4 <- ifelse(is.na(mmpbirddatamaster$No_observed4),0,mmpbirddatamaster$No_observed4)
mmpbirddatamaster$n_target_sp <- ifelse(is.na(mmpbirddatamaster$n_target_sp),0,mmpbirddatamaster$n_target_sp)
# mmpbirddatamaster$noobserve <- mmpbirddatamaster$No_observed2 + mmpbirddatamaster$No_observed3 + mmpbirddatamaster$No_observed4 + mmpbirddatamaster$n_target_sp_5_15
mmpbirddatamaster$noobserve <- ifelse(mmpbirddatamaster$Route_Number %in% QCroutes, 
					mmpbirddatamaster$No_observed + mmpbirddatamaster$No_observed2 + mmpbirddatamaster$No_observed4 + mmpbirddatamaster$n_target_sp_0_10,  # query for Quebec (1st five and 2nd five minutes)
				ifelse(mmpbirddatamaster$Species_Code %in% FocalSpecies, 
					mmpbirddatamaster$No_observed2 + mmpbirddatamaster$No_observed3 + mmpbirddatamaster$No_observed4 + mmpbirddatamaster$n_target_sp_5_15, 
					mmpbirddatamaster$No_observed + mmpbirddatamaster$No_observed2 + mmpbirddatamaster$No_observed4))

mmpbirddatamaster<-rename(mmpbirddatamaster, c(
	Route_Number = "route",
	Year_surv = "year",
	user_id = "observer",
	station = "station",
	Survey_no = "visit",
	Species_Code = "species",
	Outside_Flythrough = "outfly"))
mmpbirddatamaster$station <- toupper(mmpbirddatamaster$station)
mmpbirddatamaster$route <- toupper(mmpbirddatamaster$route)



################################################################################
# 3X CHECK IF THERE ARE STATIONS MISSING IN THE BIRD STATION VISIT FILE
################################################################################
#
# INPUT: mmpbirddatamaster, bvisit
# PROCESSING: check if there is any count data (in mmpbirddatamaster) for visits which
#	are erroneously missing from the Station Visit info (bvisit).
# OUTPUT: bvisit, dataframe informatin for each visit to a station (e.g., weather 
#	info, time and date, etc.
# NOTE: bvisit is supposed to have one row per visit to a station, even if there
#	ended up being no birds observed on the visit. In reality, sometimes 
# 	there is count info but no visit info in bvisit. Most of these cases seem
#	to be where information was recorded for station A of a route, but not for
# 	stations B,C,D... etc.

# collect all unique route/station/year/visit info from the point count data (mmbirddatamaster)
birddatavisits <- unique(mmpbirddatamaster[,c("form_id","route","year","observer","visit","station","Month_no","Day_no","Start_Time")])
birddatavisits <-rename(birddatavisits, c(
	Month_no 	= "month",
	Day_no		= "day",
	Start_Time	= "stime"))

# join to the birddatavisits information in the bvisit file
birddatavisits <- unique(rbind(birddatavisits, bvisit[,names(bvisit) %in% names(birddatavisits)]))

# join bvisit's weather conditions (when they are recorded) back to birddatavists
missingCols <- names(bvisit)[names(bvisit) %in% names(birddatavisits) == FALSE]
sqlstatement <- paste("SELECT birddatavisits.*,",
		paste("bvisit.",missingCols,collapse=",",sep=""),	# paste in names of columns in visit data not in bird count data
		" FROM birddatavisits LEFT JOIN bvisit ON birddatavisits.route = bvisit.route AND birddatavisits.year = bvisit.year AND birddatavisits.station = bvisit.station AND birddatavisits.visit = bvisit.visit and birddatavisits.form_id = bvisit.form_id")
bvisit <- sqldf(sqlstatement)
rm(birddatavisits)
##########################################################################
# 3F.  CORRECT MISSING BIRD VISIT INFO
##########################################################################
# Occassionally, a wetlands habitat/vegation information was recorded only for station A, and the rest of the
# stations were assumed to have the same values as station A.
# The following collects the habitat/condition information for 1997 stations A, and inserts them into missing values for stations B,C,D,...,etc.
#  data temp (keep=route year observer station visit);
#
# INPUT: BVISIT, from 3D and 3A
# OUTPUT: BVISIT

# convert stations to character strings
bvisit$station <- as.character(bvisit$station)
bvisit$route <- as.character(bvisit$route)

# collect all unique surveys
day_surveyed <- unique(bvisit[,c("route","year","visit","form_id")])
needs_attn <- function(column) any(is.na(column))		# function which checks if any data is missing in a column

isNumBvisit <- as.numeric(which(unlist(lapply(bvisit,FUN = function(x) is.numeric(x)))))

# cycles through all stations and visits and replaces higher station NA values with values from station A
for(i in 1:nrow(day_surveyed)){
	subdata <- subset(bvisit,bvisit$route == day_surveyed[i,"route"] & bvisit$year == day_surveyed[i,"year"] & bvisit$visit == day_surveyed[i,"visit"] & bvisit$form_id == day_surveyed[i,"form_id"])[,c("station","year", "month","observer","day", "stime", "wind", "cloud", "airtemp","Precip","noise_level")]
	
	# is there more than 1 station in route ?
	if(nrow(subdata) > 1) {

	# collect visit data from stations B,C,D,E,... etc.,
	Bvisit_data <- subdata[which(subdata$station != min(subdata$station)),]

	# which columns have missing values in stations B,C,D,... etc
	missing <- apply(Bvisit_data,2,needs_attn)
	
	# are there any missing values
	if(any(missing)){

	# check if station A has those values to fill in missing values
	if(any(!is.na(subdata[which(subdata$station == min(subdata$station)),missing])))
		{
		# collect visit data from first station, and make into template matrix into which other non-missing data from B,C,D,E... will be inserted
		newVisitDat <- matrix(subdata[which(subdata$station == min(subdata$station))[1],],ncol=ncol(Bvisit_data),nrow=nrow(Bvisit_data),byrow=T,dimnames=list(row.names(Bvisit_data),names(subdata)))
		
		# insert values which are NOT missing from stations B,C,D,...,etc. into temporary data
		newVisitDat[as.vector(!is.na(Bvisit_data))] <- as.matrix(Bvisit_data)[as.vector(!is.na(Bvisit_data))]
		newVisitDat <- rbind(subdata[which(subdata$station == min(subdata$station)),],as.data.frame(newVisitDat))
		newVisitDat <- data.frame(apply(newVisitDat,2,unlist),row.names = row.names(subdata),stringsAsFactors = FALSE)

		# convert back to numeric data
		newVisitDat[,which(names(newVisitDat) %in% names(bvisit)[isNumBvisit])] <- apply(newVisitDat[,which(names(newVisitDat) %in% names(bvisit)[isNumBvisit])],2,as.numeric)
		
		# insert replaced values back into "bvisit" data
		bvisit[which(bvisit$route == day_surveyed[i,"route"] & bvisit$year == day_surveyed[i,"year"] & bvisit$visit == day_surveyed[i,"visit"] & bvisit$form_id == day_surveyed[i,"form_id"]), names(subdata)] <- newVisitDat
	}}}}
	

# display missing data values to user
errVisit<- which(apply(bvisit[,c("wind", "cloud", "airtemp","Precip","noise_level")],1,FUN = function(x) any(is.na(x))))
print(bvisit[errVisit,])
print("ERR: the above data is missing info on the wind, cloud or air temperature. See 'error_missing_visit_info.csv'")
write.csv(bvisit[errVisit,],"error_missing_visit_info_BIRDS.csv")

errDate <- which(apply(bvisit[,c("day","month","year","stime")],1,FUN = function(x) any(is.na(x))))
print(bvisit[errDate,])
print("ERR: the above data is missing info on the day, month, or start time of survey. See 'error_missing_date_info.csv'")
write.csv(bvisit[errDate,],"error_missing_date_info_BIRDS.csv")




##############################################################################
# 3D. BIRD DATA: PROCESS MIS-LABELED  STATION AND VIST INFORMATION
##############################################################################
# Extra step to check for miss-counted visits in Station Visit data and bird count data
# INPUT: bvisit from 3B, data.frame of visits to bird stations
# PROCESSING: 	some stations seem to have more than their listed number of visits
#	e.g., a point count occurred and was listed as being the same point count
#	from an earlier time. OPTIONS: merge the two visits together as one visit (DEFAULT)
#	or re-enumeriate disparate visits so that they are e.g., 1,2,3 and not 1,2a,2b	
# OUTPUT: 	bvisit, slightly cleaned up

bvisit <- data.frame(sqldf("select * from bvisit order by route, year, observer, station, visit"),stringsAsFactors = FALSE)

# check that the highest number of visits in the data is the same as the actual amount of surveys
bvisitTally <- sqldf("select *, max(visit) as 'maxVisit', count(station) as 'tally' from bvisit group by route,year,station")

# data with contradictory declared amounts of visits and actual visits
visitErr <- bvisitTally[which(bvisitTally$maxVisit < bvisitTally$tally),]

if(length(visitErr) > 0){
	# the following are problematic data... too many visits, or duplicate station visits...
	bvisitErr <- sqldf("select bvisit.*,visitErr.maxVisit as 'maxVisit', visitErr.tally as 'tally' from bvisit join visitErr on bvisit.route = visitErr.route AND bvisit.year = visitErr.year AND bvisit.station = visitErr.station")[,c(names(bvisit)[1:9],"maxVisit","tally")]

	# check if there is bird data for all the odd visit data
	birddatVis <- sqldf("select form_id, route,year,Month_no, Day_no, Start_Time, observer,station, visit,sum(noobserve) as 'nbirds' from mmpbirddatamaster group by route,year,station,observer,form_id,visit")
	checkBdat <- sqldf("select bvisitErr.*, birddatVis.nbirds,birddatVis.Month_no,birddatVis.Day_no from bvisitErr left join birddatVis on bvisitErr.route = birddatVis.route AND bvisitErr.year = birddatVis.year AND bvisitErr.station = birddatVis.station AND bvisitErr.form_id = birddatVis.form_id AND bvisitErr.observer = birddatVis.observer")

	# flag (for discarding) any duplicate visit data which do not have any bird data attached to it
	# we can safely discard such visits, because they are redundant
	discards <- which(is.na(checkBdat$nbirds))
	Dindices <- numeric(0)
	for(dd in 1:length(discards)){
		Dindices <- c(Dindices,which(as.character(bvisit$route) == checkBdat$route[discards[dd]] & bvisit$year == checkBdat$year[discards[dd]] & bvisit$form_id == checkBdat$form_id[discards[dd]] & bvisit$station == checkBdat$station[discards[dd]] & bvisit$visit == checkBdat$visit[discards[dd]] & bvisit$observer == checkBdat$observer[discards[dd]]))
		}
	
	bvisit <- bvisit[1:nrow(bvisit) %in% Dindices==FALSE,]
	}

# prompt for decision about whether to discard, merge, or re-enumerate duplicate visits
if(length(visitErr) > 0){
	print(checkBdat) 
	print("in some stations & years, there are more than the declared number of visits")
	print("you must decide if you want to... ")
	print("...MERGE visits labeled as the same visit (type M)")
	print("...RE-NUMBER extra visits (type R)")
	}
	ANSduplicates <- "M"
	if(length(visitErr) > 0){ 
		ANSduplicates <- winDialogString("Some stations have more than declared # of visits. Do you (M) merge visits, or (R) re-number them?","M")
		}
	if(all(ANSduplicates != "R",ANSduplicates != "r")){ANSduplicates <- "M"}

# pre-processing step to make sure we don't transform numeric data into character data
if(length(visitErr) > 0){ isNumBirddat <- as.numeric(which(unlist(lapply(mmpbirddatamaster,FUN = function(x) is.numeric(x)))))
			  isNumBvisit <- as.numeric(which(unlist(lapply(bvisit,FUN = function(x) is.numeric(x)))))
			}

# merge visit data labeled as the same visit, in both bird and visit databases
# WARNING: this involves selecting and discarding visit environment data, based on a decision algorithm:
# Decision flowchart to select duplicate visit data:
# ... if one of the duplicates has no accompanying visit data, select other
# ... if one has the majority of bird data, choose it
# ... if one has any missing values in visit data, choose other
# ... else, choose visit data from earlier survey

if(ANSduplicates == "M" & length(visitErr) > 0){
	# which extra visits actually have bird data associated with them
	merges <- checkBdat[which(!is.na(checkBdat$nbirds)),]
	merges2 <- sqldf("select * from merges group by route,year,station,visit")
	
	# visit data of concern
	Vvar <-c("month","day","stime","wind","cloud","airtemp","Habitat_Changed","Water_Temp","Precip")

	# cycle through routes/years/stations with the same visit number
	for(mm in 1:nrow(merges2)){
		DONE <- FALSE
		while(DONE == FALSE){
		# indices in Visit data of the offending routes/station/visits
		Vindices <- which(bvisit$route == merges2$route[mm] & bvisit$year == merges2$year[mm] & bvisit$station == merges2$station[mm] & bvisit$visit == merges2$visit[mm])
		subdata <- bvisit[Vindices,]
		
		if(nrow(subdata) == 1) {DONE <- TRUE
		} else {
		subdata[,isNumBvisit] <- apply(subdata[,isNumBvisit],2,as.numeric)
		
		# Decision flowchart to select duplicate visit data:
		# ... if one of the duplicates has no accompanying visit data, select other
		# ... if one has the majority of bird data, choose it
		# ... if one has any missing values in visit data, choose other
		# ... else, choose visit data from earlier survey
		
		chooseVdat <- subdata # this will contain the final data that is selected

		# ... if one of the duplicates has no accompanying visit data, select other
		chooseVdat <- chooseVdat[as.numeric(which(apply(chooseVdat[,Vvar],1,FUN = function(x) all(is.na(x)))==FALSE)),]
		if(nrow(chooseVdat) == 1) { DONE <- TRUE }
		if(nrow(chooseVdat) == 0) { chooseVdat <- subdata }
		
		# ... if one has more bird data, choose it for its visit data 
		BDataIndices <- which(mmpbirddatamaster$route == merges2$route[mm] & mmpbirddatamaster$year == merges2$year[mm] & mmpbirddatamaster$station == merges2$station[mm] & mmpbirddatamaster$visit == merges2$visit[mm])
		tempBirddat1 <- mmpbirddatamaster[BDataIndices,]
		tempBirddat1[,isNumBirddat] <- apply(tempBirddat1[,isNumBirddat],2,as.numeric)
		tempBirddat <- sqldf("select *, sum(noobserve) as 'totalBirds' from tempBirddat1 group by form_id,observer,Month_no,Day_no,Start_Time") 
		
		# reorder tempamphdat to the same order as the visitation data (chooseVdat)
 		whatisdiff <- apply(chooseVdat[,c("form_id","route","year","month","day","stime","observer")],2,FUN = function(x) ifelse(length(unique(x))>1,2,1))
		orderr <- order(chooseVdat[names(whatisdiff)[match(2,whatisdiff)]])
		orderr2 <- order(tempBirddat[names(whatisdiff)[match(2,whatisdiff)]])
		tempBirddat <-tempBirddat[orderr2,]
		if(identical(orderr,order(tempBirddat[names(whatisdiff)[match(2,whatisdiff)]]))==FALSE){
			tempBirddat <-tempBirddat[orderr,]
			}

		chooseVdat <- chooseVdat[which(tempBirddat$totalBirds == max(tempBirddat$totalBirds)),] 
		if(nrow(chooseVdat) == 1) { DONE <- TRUE }
		else{	# ... if one has fewer missing values in visit data, choose it	
			chooseVdat <- chooseVdat[which(as.numeric(apply(chooseVdat[,Vvar],1,FUN = function(x) length(which(is.na(x)))))
				==min(as.numeric(apply(chooseVdat[,Vvar],1,FUN = function(x) length(which(is.na(x))))))),]
			
			if(nrow(chooseVdat) == 1) { DONE <- TRUE }
			else{	# ... else just choose the earlier date for the visit data
				chooseVdat <- chooseVdat[which(julian(as.Date(paste(chooseVdat$year,chooseVdat$month,chooseVdat$day,sep="-"))) + chooseVdat$stime/2400.0
				== min(julian(as.Date(paste(chooseVdat$year,chooseVdat$month,chooseVdat$day,sep="-"))) + chooseVdat$stime/2400.0)),]
				#chooseVdat <- chooseVdat[which(julian(chooseVdat$month,chooseVdat$day,chooseVdat$year,origin = c(month=1,day=1,year=chooseVdat$year[1])) + chooseVdat$stime/2400.0
				#== min(julian(chooseVdat$month,chooseVdat$day,chooseVdat$year,origin = c(month=1,day=1,year=chooseVdat$year[1])) + chooseVdat$stime/2400.0)),]	
				}	
			}
		
		# replace the visit data of the duplicates with the selected data, & move temporary matrix into the Bvisit data 		
		bvisit[Vindices,Vvar] <- data.frame(matrix(apply(chooseVdat[,Vvar],2,as.character),nrow=nrow(bvisit[Vindices,]),ncol = length(Vvar),byrow=TRUE,dimnames=list(row.names(subdata),Vvar)),stringsAsFactors=FALSE)
				
		# replace the visit data of the duplicates with the finally selected data, & move temporary matrix into the mmpbirddatamaster data
		mmpbirddatamaster[BDataIndices,c("Month_no","Day_no","Start_Time")] <- data.frame(matrix(apply(chooseVdat[,c("month","day","stime")],2,as.character),nrow=nrow(tempBirddat1),ncol = 3,byrow=TRUE,dimnames=list(row.names(tempBirddat1),c("Month_no","Day_no","Start_Time"))),stringsAsFactors=FALSE)
			}		
		DONE <- TRUE
		}}
	bvisit[,isNumBvisit] <- apply(bvisit[,isNumBvisit],2,as.numeric)	# final step to restore NUMERIC class to columns
	mmpbirddatamaster[,isNumBirddat] <- apply(mmpbirddatamaster[,isNumBirddat],2,as.numeric)
	}

# Alternative, instead of merging the Bvisit data by the visit number, we re-enumerate the duplicate visits as entirely different visits	
if(ANSduplicates == "R" & length(visitErr) > 0)
	{		
	enumer <- checkBdat[which(!is.na(checkBdat$nbirds)),]

	# order visit data by month, day, start time, in order to re-enumerate based on date of visit
	enumer2 <- sqldf("select * from enumer order by route, year,station, month, day, stime")
	enumer3 <- enumer2

	# re-enumerate
	for(ee in 2:nrow(enumer2)){
		if(enumer2$route[ee-1] == enumer2$route[ee] & enumer2$year[ee-1] == enumer2$year[ee] & enumer2$station[ee-1] == enumer2$station[ee])
			{ enumer2$visit[ee] <- enumer2$visit[ee-1]+1 }}
	
	# replace new visit values in bvisit and mmpbirddatamaster data
	for(mm in 1:nrow(enumer3)){	
		
		# find corresponding data in bvisit
		Vindices <- which(bvisit$route == enumer3$route[mm] & bvisit$year == enumer3$year[mm] & bvisit$station == enumer3$station[mm] & bvisit$observer == enumer3$observer[mm] & bvisit$visit == enumer3$visit[mm])
		
		# replace visit with the new visit number, as reenumerated in enumer2
		bvisit[Vindices,]$visit <- enumer2$visit[mm]

		# find corresponding data in mmpbirddatamaster
		Bindices <- which(mmpbirddatamaster$route == enumer3$route[mm] & mmpbirddatamaster$year == enumer3$year[mm] & mmpbirddatamaster$station == enumer3$station[mm] & mmpbirddatamaster$observer == enumer3$observer[mm] & mmpbirddatamaster$visit == enumer3$visit[mm])
		
		# replace mmpbirddatamaster with the new visit number, as reenumerated in enumer2
		mmpbirddatamaster[Bindices,]$visit <- rep(enumer2$visit[mm],length(mmpbirddatamaster[Bindices,]$visit))		
	}	}
	

## END PROCESSING TO DEAL WITH DUPLICATE VISITS

##########################################################################
# 3E. BIRD DATA : ADD RICHNESS METRICS, AND HABITAT GUILD
##########################################################################
# InPUT: mmpbirddatamaster, data.frame of avian point counts
#	 MMP.Species.Classification.MASTER.csv, csv which classifies species, among other things, as whether or not 
#	 	they are "lumps" or "splits" within the time frame of the program.
#	 "mmp_codes", SQL table from BSCDATA database, which classifies species into various mutually exclusive guilds
# PROCESSING: reclassifies species, based on "lumping" and "splitting" suggested in the  MMP.Species.Classification.MASTER.csv file
#	  classifies species according to various guilds.
#	  removes "Visitors" from the data table
#	  calculates the richness of various guilds
# OUTPUT: birddat2, data.frame of avian point counts (only breeding birds)
# NOTE: Nesters, Aerial foragers, Fish foragers, Visitors.  Birds are classed in one category (highest, closest affinity to wetlands);
# 	(in reality, they may belong to more than one category)

# richness metrics
pres <- ifelse(mmpbirddatamaster$noobserve > 0,1,0)
nbirds <- mmpbirddatamaster$noobserve
birddat1 <- cbind(mmpbirddatamaster,data.frame(pres = pres,nbirds = nbirds))

  sp.class <- read.csv("T:/MARSH/IBI/MMP.Species.Classification.MASTER.csv")

# need to reclassify split/lumped species codes in bird data to those in the Reclass column of sp.class
  for(newsp in as.character(sp.class$spcd)[sp.class$Reclass != "no"]){
	birddat1$species <- ifelse(birddat1$species == newsp,as.character(sp.class$Reclass)[sp.class$spcd == newsp],birddat1$species)}

# mmpbirdcodemaster <- bscdata.readSQLTable("bscdata", "mmp_codes", maxrec = 0)
  mmpbirdcodemaster <- rename(sp.class, c(
	spcd = "species",
	Nester = "nest",
	Aerial_Forager = "aerfor",
	Water_Forager = "forage",
	Visitor = "visit",
	Ind = "indicator"))

# NOTE, species will only be classified to ONE category. So, if a species is an indicator species, and it is a 
# marsh nester too, this WON'T be noted in its bdclass, which will only be scored as "N"

nester <- ifelse(is.na(mmpbirdcodemaster$nest),0,ifelse(mmpbirdcodemaster$nest==1,1,0))
airfor <- ifelse(is.na(mmpbirdcodemaster$aerfor),0,ifelse(mmpbirdcodemaster$aerfor==1,1,0))
visitor <- ifelse(is.na(mmpbirdcodemaster$visit),0,ifelse(mmpbirdcodemaster$visit==1,1,0))
fishfor <- ifelse(is.na(mmpbirdcodemaster$forage),0,ifelse(mmpbirdcodemaster$forage==1,1,0))
indic <- ifelse(is.na(mmpbirdcodemaster$indicator),0,ifelse(mmpbirdcodemaster$indicator==1,1,0))
bdclass <- ifelse(nester == 1,"N",ifelse(airfor == 1,"A",ifelse(visitor == 1,"V",ifelse(fishfor == 1,"F",ifelse(indic == 1,"I","noclass")))))
moot <- ifelse(mmpbirdcodemaster$species == "MOOT",1,0)
mt <- ifelse(mmpbirdcodemaster$species == "AMCO" | mmpbirdcodemaster$species == "COMO",1,0)
birdcode <- data.frame(species = mmpbirdcodemaster$species,nester = nester, airfor = airfor,visitor = visitor, fishfor = fishfor, indic = indic,bdclass = bdclass, moot = moot, mt = mt)

birdcode <- birdcode[order(birdcode$species),]		# sort by species name

# merge bird code data with the bird presense / nbird data
birddat2 <- sqldf("select birddat1.*,birdcode.nester,birdcode.airfor,birdcode.visitor,birdcode.fishfor,birdcode.indic,birdcode.bdclass,birdcode.moot,birdcode.mt from birddat1 join birdcode on birddat1.species = birdcode.species")
rm(birddat1)
# remove Visitors (e.g., only want breeding birds)
birddat2 <- subset(birddat2,visitor == 0)


##################################################################################
# 3G: MERGE bird Visit DATA WITH VEG/HABITAT DATA (FROM SECTIONS 1 & 2)
##################################################################################

habitatB <- subset(habitat,habitat$type == "B" | habitat$type == "AB" | habitat$type == "BA" | is.na(habitat$type))
habitatB <- sqldf("select * from habitatB order by route,year,station")
bvisit <- sqldf("select * from bvisit order by route,year,station,visit")

# NOTE this drops duplicates of the same station/visit/year/route
bvishab <- sqldf("SELECT bvisit.*,habitatB.type,habitatB.wildrice,habitatB.bur_reed,habitatB.grasses,habitatB.rushes,
		habitatB.pickweed,habitatB.arrowhd,habitatB.smartwd,habitatB.purploos,habitatB.wwillow,habitatB.cattail,
		habitatB.comreed,habitatB.pemerge,habitatB.popenw,habitatB.pexpose,habitatB.ptree,habitatB.ntree,
		habitatB.pshrub,habitatB.float,habitatB.wetperm,habitatB.sizet,habitatB.sampedge,habitatB.landuse 
		FROM bvisit LEFT JOIN habitatB ON bvisit.route = habitatB.route AND bvisit.year = habitatB.year 
		AND bvisit.station = habitatB.station AND bvisit.observer = habitatB.observer 
		group by route,year,station,visit")


##################################################################################
# 3h: MERGE bird Visit and Habitat Data with Data Quality (mmpdatqual)
##################################################################################

# NOTE this drops duplicates of the same station/visit/year/route
bvishabqual <- sqldf("SELECT bvishab.*, mmpdatqual.nbstns,mmpdatqual.dqhabit, 
		mmpdatqual.dqbird FROM bvishab LEFT JOIN mmpdatqual ON 
		bvishab.route = mmpdatqual.route AND bvishab.year = mmpdatqual.year AND 
		bvishab.observer = mmpdatqual.observer group by route,year,station,visit")

####################################################################
# 3I: Filter Bird Visit/Habitat/Quality by relevant routes in mmproutes)
####################################################################

# collect all covariates (route info, station info, visit info, vegetation, habitat, etc
bAllother <- subset(bvishabqual,bvishabqual$route %in% unique(mmproutes$route))
bAllother <- sqldf("select bAllother.*, mmproutes.rtename, mmproutes.aoccode,mmproutes.lat,
		mmproutes.long,mmproutes.latzone,mmproutes.distance,mmproutes.sizeha,
		mmproutes.basin,mmproutes.region,mmproutes.nrtown,mmproutes.county,
		mmproutes.provstat,mmproutes.remede,mmproutes.remedtyp,mmproutes.mcomplex,
		mmproutes.wetclass,mmproutes.mapyr,mmproutes.glcwc,mmproutes.durham,
		mmproutes.coastal,mmproutes.reg, mmproutes.basin, mmproutes.bcr FROM
		bAllother JOIN mmproutes ON bAllother.route = mmproutes.route")
write.csv(bAllother,"BirdCovariates.csv",row.names=F)
# bAllother <- read.csv("BirdCovariates.csv",header=T)

# Filter Bird data by relevant routes in mmproutes)

birddat3 <- subset(birddat2,birddat2$route %in% unique(mmproutes$route))[c("route","station","visit","year","Month_no","Day_no","Start_Time","species","No_observed","No_observed2","No_observed3","No_observed4","n_target_sp","n_target_sp_0_5","n_target_sp_5_10","n_target_sp_10_15","n_target_sp_0_10","n_target_sp_5_15","outfly","noobserve","pres","nbirds","nester","airfor","visitor","fishfor","indic","bdclass","moot","mt")]
write.csv(birddat3,"birddat3.csv",row.names=F)
# birddat3 <- read.csv("birddat3.csv",header=T)

########################################################################
# 3J: Join BIRD POINT COUNTS (birddat3) with habitat data
########################################################################
# NOTE this step is supplied, only because it was done previously.
# This product does not feed into further constructions
#
# mmpbirddata <- sqldf("SELECT birddat3.*, bAllother.* from birddat3 LEFT JOIN bAllother on birddat3.route = bAllother.route AND birddat3.year = bAllother.year AND birddat3.station = bAllother.station AND birddat3.visit = bAllother.visit")
# file <- paste("mmpbirddata",min(mmpbirddata$year),"-",max(mmpbirddata$year),".csv",sep="")
# write.csv(mmpbirddata,file)


#############################################################################################################
# 3K: Create a Bird Species table, per visit/station, per year as well
#
# Unlike previously, this creates a main birdstn file, which is equivalent to the older methods. 
# In the future, it should build 1 minute intervals for detectibility modelling.
#############################################################################################################
rm(mmpbirddatamaster,birddat2,bvishab,bvishabqual)

splist <- unique(birddat3$species)
sumcol <- c("noobserve")		# select which columns to produce species tables for
					# noobserve is for the full 10 minute interval during and after call back
					# noobserve makes the output equivalent to the old SAS build program

birddat3$ID <- paste(birddat3$route,birddat3$year,birddat3$station,birddat3$visit,sep="")
bAllother$ID <- paste(bAllother$route,bAllother$year,bAllother$station,bAllother$visit,sep="")

# first, check if there are any TYPE's labeled NA, which do include bird data
noType <- bAllother$ID[which(is.na(bAllother$type))]
bAllother$type <- ifelse(!is.na(bAllother$type),as.character(bAllother$type),ifelse(bAllother$ID %in% unique(birddat3$ID),"B","B"))
# NOTE: if a site is listed in vmMMPBirdStation, then it is supposedly gauranteed to have been surveyed


# remove any data that is not labeled as type B, or AB
sitedat <- subset(bAllother,bAllother$type == "B" | bAllother$type =="AB" | bAllother$type == "BA")

for(colObs in sumcol)			# iterate through columns of relevance
	{
	sptable <- data.frame(ID = sitedat$ID)
	for(sp in splist)		# iterate through species 
		{			# create species sums for each visit/route/station/year
		d <- subset(birddat3,birddat3$species == sp)
		dsum <- sqldf(paste("select ID,sum(noobserve) as '",sp,"' from d group by ID",sep=""))	
		sptable <- sqldf(paste("select sptable.*,dsum.",sp," from sptable left join dsum on sptable.ID = dsum.ID",sep=""))
		}
	
	# now sum the number of species classes as well
	spclasssum <- sqldf("select ID,sum(nester) as 'nester', sum(airfor) as 'airfor', sum(visitor) as 'visitor',sum(fishfor) as 'fishfor',sum(indic) as 'indic',sum(moot) as 'moot', sum(mt) as 'mt' from birddat3 group by ID")
	sptable <- sqldf("select sptable.*,spclasssum.nester,spclasssum.airfor,spclasssum.visitor,spclasssum.fishfor,spclasssum.indic,spclasssum.moot,spclasssum.mt from sptable left join spclasssum on sptable.ID = spclasssum.ID")
	
	# replace NA's with zeros
	sptable[,-1] <-apply(sptable[,-1],2,FUN = function(x) as.numeric(ifelse(is.na(x),0,x)))

	# now remerge back with sitedat
	sitedat <- sqldf("select * from sitedat order by ID")
	sptable <- sqldf("select * from sptable order by ID")
	sptableByVisit <- cbind(sitedat,sptable[,names(sptable) != "ID"])  # this has a column for each species, and a row for each VISIT
	filename <- paste("Birdstns_",colObs,"_ByVisit.csv",sep="")
	write.csv(sptableByVisit,filename,row.names = F)

	# Build an SQL statement to take maximum observations at each station
	sqlstatment_maxBirds <- paste("select", paste(c("route","year","station"),sep="",collapse=","),", count(visit) as 'nVisit',",
					paste("max(",splist,") as '",splist,"'",sep="",collapse=","),
					" from sptableByVisit group by year,route,station")
	maxBirds <- sqldf(sqlstatment_maxBirds)				# this has a species column, lisiting observations by station (not visit)
	# re-count richness of "nesters"  "airfors"  "visitors" "fishfors" "indics" "moots"    "mts"
	birdcode2 <- birdcode[birdcode$species %in% names(maxBirds),]
	maxBirds$nnester <- as.numeric(rowSums(apply(maxBirds[,as.character(birdcode2$species[which(birdcode2$nester == 1)])],2,FUN = function(x) ifelse(x > 0,1,0))))
	maxBirds$nairfor <- as.numeric(rowSums(apply(maxBirds[,as.character(birdcode2$species[which(birdcode2$airfor == 1)])],2,FUN = function(x) ifelse(x > 0,1,0))))
	maxBirds$nfishfor <- as.numeric(rowSums(apply(maxBirds[,as.character(birdcode2$species[which(birdcode2$fishfor == 1)])],2,FUN = function(x) ifelse(x > 0,1,0))))
	maxBirds$nindic <- as.numeric(rowSums(apply(maxBirds[,as.character(birdcode2$species[which(birdcode2$indic == 1)])],2,FUN = function(x) ifelse(x > 0,1,0))))
	maxBirds$nmoot <- ifelse(maxBirds$MOOT > 0,1,0)
	maxBirds$nmt <- as.numeric(rowSums(apply(maxBirds[,as.character(birdcode2$species[which(birdcode2$mt == 1)])],2,FUN = function(x) ifelse(x > 0,1,0))))

	# re-add environmental data, route, and dataquality 
	maxBirdsH <- sqldf(paste("select maxBirds.*,",paste("habitatB.",names(habitatB)[names(habitatB) %in% c("ID","route","year","station","observer","type","basin","dqhabit") ==FALSE],sep="",collapse=",")," from maxBirds left join habitatB on maxBirds.route = habitatB.route AND maxBirds.year = habitatB.year AND maxBirds.station = habitatB.station group by route,year,station",sep=""))
	maxBirdsR <- sqldf(paste("select maxBirdsH.*,",paste("mmproutes.",names(mmproutes)[names(mmproutes) %in% c("route") ==FALSE],sep="",collapse=",")," from maxBirdsH left join mmproutes on maxBirdsH.route = mmproutes.route group by route,year,station",sep=""))
	maxBirdsDQ <- sqldf("select maxBirdsR.*, mmpdatqual.nbstns,mmpdatqual.dqhabit,mmpdatqual.dqbird from maxBirdsR left join mmpdatqual on maxBirdsR.route = mmpdatqual.route AND maxBirdsR.year = mmpdatqual.year group by route,year,station")

	# reorder data.frame to produce Birdstns.csv (equivalent to previous SAS build program)
	namesord <- c(which(names(maxBirdsDQ) %in% c(as.character(splist),"nnester","nairfor","nfishfor","nvisitor","nindic","nmt","nmoot")==FALSE),which(names(maxBirdsDQ) %in% c(as.character(splist),"nnester","nairfor","nfishfor","nvisitor","nindic","nmt","nmoot")))
	Birdstns <- maxBirdsDQ[,namesord]
	rm(maxBirdsH); rm(maxBirdsR); rm(maxBirdsDQ)
	
	finalFileName <- paste("Birdstns_",colObs,".csv",sep="")
	write.csv(Birdstns,finalFileName,row.names=F)
	}

########################################################################
##   END BIRD DATA
########################################################################





########################################################################
# 4.                           AMPHIBIANS
########################################################################

#########################################
# 4a. AMPHIB VISIT (STN) DATA 1995 onward
#########################################
# Note: Validify has values of 0,1,2,3. Used for the validation work in 1995;

amphstn <- bscdata.readSQLTable("bscdata", "vwMMPAmphStations", maxrec = 0)
amphstn <- rename(amphstn, c(Route_Number = "route",
	Year_surv = "year",
	Survey_no = "visit",
	Station = "station",
	Month_no = "month",
	Day_no = "day",
	user_id = "observer",
	Start_Time = "stime",
	Wind = "wind",
	Cloud = "cloud",
	Air_Temp = "airtemp",
	Water_Temp = "watertemp",
	Precip = "precip",
	Comment = "comment",
	Remarks = "remarks"))

amphstn<- amphstn[,which(names(amphstn) %in% c("comment","remarks")==FALSE)]
amphstn$station <- toupper(amphstn$station)

#########################################
# 4b. AMPHIB DATA 1995 onward
#########################################
# Note: Vcode (1,2,3) and Vcount (1-6,10) are the codes and counts for the 1995 validation work;

amphdat1 <- bscdata.readSQLTable("bscdata", "vwMMPAmphData", maxrec = 0)
amphdat1 <- rename(amphdat1, c(Route_Number = "route",
	Year_surv = "year",
	Survey_no = "visit",
	Month_no = "month",
	Day_no = "day",
	Start_Time = "stime",
	station = "station",
	user_id = "observer",
	Species_Code = "species",
	Code = "code",
	Count_ind = "count",
	V_Code = "vcode",
	V_Count = "vcount",
	In_sector = "ins"))

amphdat1 <- amphdat1[,names(amphdat1) %in% c("vcount","vcode")==FALSE]
amphdat1$station <- toupper(amphdat1$station)
amphdat1$ins <- as.character(amphdat1$ins)
amphdat1$species <- toupper(amphdat1$species)

# score NONE species observations as a 0 code (currently a mix of NA and 0)
amphdat1$code <- ifelse(amphdat1$species == "NONE" & is.na(amphdat1$code),0,amphdat1$code)
amphdat1$count <- ifelse(amphdat1$species == "NONE" & is.na(amphdat1$count),0,amphdat1$count)

# NOTE, according to Kathy jones, WITHIN 100 M was only supposed to be used for habitat, NOT trend analysese filtering
amphdat1$ins <- ifelse(is.na(amphdat1$ins) & amphdat1$year < 1998,NA,ifelse(amphdat1$ins == "Y" | amphdat1$ins == "y",1,0))

################################################################################
# 4X CHECK IF THERE ARE STATIONS MISSING IN THE AMPH STATION VISIT FILE
################################################################################
#
# INPUT: amphdat1, amphstn
# PROCESSING: check if there is any count data (in amphdat1) for visits which
#	are erroneously missing from the Station Visit info (amphstn).
# OUTPUT: amphstn, dataframe informatin for each visit to a station (e.g., weather 
#	info, time and date, etc.
# NOTE: amphstn is supposed to have one row per visit to a station, even if there
#	ended up being no amphibians observed on the visit. In reality, sometimes 
# 	there is count info but no visit info in amphstn. Most of these cases seem
#	to be where information was recorded for station A of a route, but not for
# 	stations B,C,D... etc.

# collect all unique route/station/year/visit info from the point count data (mmbirddatamaster)
amphdatavisits <- unique(amphdat1[,c("form_id","route","year","observer","visit","station","month","day","stime")])

# join to the amphdatavisits information in the bvisit file
amphdatavisits <- unique(rbind(amphdatavisits, amphstn[,names(amphstn) %in% names(amphdatavisits)]))

# join bvisit's weather conditions (when they are recorded) back to amphdatavists
missingCols <- names(amphstn)[names(amphstn) %in% names(amphdatavisits) == FALSE]
sqlstatement <- paste("SELECT amphdatavisits.*,",
		paste("amphstn.",missingCols,collapse=",",sep=""),	# paste in names of columns in visit data not in bird count data
		" FROM amphdatavisits LEFT JOIN amphstn ON amphdatavisits.route = amphstn.route AND amphdatavisits.year = amphstn.year AND amphdatavisits.station = amphstn.station AND amphdatavisits.visit = amphstn.visit and amphdatavisits.form_id = amphstn.form_id")
amphstn <- sqldf(sqlstatement)
rm(amphdatavisits)

##################################################################################
# 4D. CORRECT MISSING AMPH VISIT INFO
##################################################################################

# Occassionally, a wetlands habitat/vegation information was recorded only for station A, and the rest of the
# stations were assumed to have the same values as station A.
# The follow collects the habitat/condition information for 1997 stations A, and inserts them into missing values for stations B,C,D,...,etc.
#  data temp (keep=route year observer station visit);
#
# INPUT: amphstn
# OUTPUT: amphstn

# make station and routes a character string
amphstn$station <- as.character(amphstn$station)
amphstn$route <- as.character(amphstn$route)

# collect all unique surveys
day_surveyed <- unique(amphstn[,c("route","year","visit","form_id")])
needs_attn <- function(column) any(is.na(column))		# function which checks if any data is missing in a column

isNumamphstn <- as.numeric(which(unlist(lapply(amphstn,FUN = function(x) is.numeric(x)))))

# cycles through all stations and visits and replaces higher station NA values with values from station A
for(i in 1:nrow(day_surveyed)){
	subdata <- subset(amphstn,amphstn$route == day_surveyed[i,"route"] & amphstn$year == day_surveyed[i,"year"] & amphstn$visit == day_surveyed[i,"visit"] & amphstn$form_id == day_surveyed[i,"form_id"])[,c("station","year", "observer","month", "day", "stime", "wind", "cloud", "airtemp","precip","noise_level")]
	
	# is there more than 1 station in route ?
	if(nrow(subdata) > 1) {

	# collect visit data from stations B,C,D,E,... etc.,
	amphstn_data <- subdata[which(subdata$station != min(subdata$station)),]

	# which columns have missing values in stations B,C,D,... etc
	missing <- apply(amphstn_data,2,needs_attn)
	
	# are there any missing values?
	if(any(missing)){

	# check if station A has those values to fill in missing values
	if(any(!is.na(subdata[which(subdata$station == min(subdata$station)),missing])))
		{
		# collect visit data from first station, and make into template matrix into which other non-missing data from B,C,D,E... will be inserted
		newVisitDat <- matrix(subdata[which(subdata$station == min(subdata$station))[1],],ncol=ncol(amphstn_data),nrow=nrow(amphstn_data),byrow=T,dimnames=list(row.names(amphstn_data),names(subdata)))
		
		# insert values which are NOT missing from stations B,C,D,...,etc. into template matrix
		newVisitDat[as.vector(!is.na(amphstn_data))] <- as.matrix(amphstn_data)[as.vector(!is.na(amphstn_data))]
		newVisitDat <- rbind(subdata[which(subdata$station == min(subdata$station)),],as.data.frame(newVisitDat))
		newVisitDat <- data.frame(apply(newVisitDat,2,unlist),row.names = row.names(subdata),stringsAsFactors = FALSE)

		# convert template matrix back to numeric data
		newVisitDat[,which(names(newVisitDat) %in% names(amphstn)[isNumamphstn])] <- apply(newVisitDat[,which(names(newVisitDat) %in% names(amphstn)[isNumamphstn])],2,as.numeric)
		
		# insert replaced values back into "amphstn" data
		amphstn[which(amphstn$route == day_surveyed[i,"route"] & amphstn$year == day_surveyed[i,"year"] & amphstn$visit == day_surveyed[i,"visit"] & amphstn$form_id == day_surveyed[i,"form_id"]), names(subdata)] <- newVisitDat
	}}}}
	

# display missing data values to user
errVisit <- which(apply(amphstn[,c("wind", "cloud", "airtemp","precip","noise_level")],1,FUN = function(x) any(is.na(x))))
print(amphstn[errVisit,c("route","year","visit","wind", "cloud", "airtemp","precip","noise_level")])
print("ERR: the above data is missing info on wind, cloud or air temperature . See 'error_missing_visit_info.csv'")
write.csv(amphstn[errVisit,],"error_missing_visit_info_AMPH.csv")

errDate <- which(apply(amphstn[,c("day","month","year","stime")],1,FUN = function(x) any(is.na(x))))
print(amphstn[errDate,c("route","year","visit","day","month","year","stime")])
print("ERR: the above data is missing info on the day, month, or start time of survey. See 'error_missing_day_info.csv'")
write.csv(amphstn[errDate,],"error_missing_day_info_ANMPH.csv")



##############################################################################
# 4c.. AMPH DATA: PROCESS MIS-LABELED STATION AND VIST INFORMATION
##############################################################################
# Extra step to check for mislabeled in Station Visit data and amphibian visit data
# INPUT: amphstn from 4a, data.frame of visits to amphibians stations
# PROCESSING: some stations seem to have more than their listed number of visits
#	e.g., a point count occurred and was listed as being the same point count
#	from an earlier time. OPTIONS: merge the two visits together as one visit (DEFAULT)
#	or re-enumeriate disparate visits so that they are e.g., 1,2,3,4 and not 1,2,3a,3b	
# OUTPUT: amphstn, slightly cleaned up

amphstn <- data.frame(sqldf("select * from amphstn order by route, year, observer, station, visit"),stringsAsFactors = FALSE)

# check that the highest number of visits in the data is the same as the actual amount of surveys
avisitTally <- sqldf("select *, max(visit) as 'maxVisit', count(station) as 'tally' from amphstn group by route,year,station")

# data with contradictory declared amounts of visits and actual visits
visitErr2 <- avisitTally[which(avisitTally$maxVisit < avisitTally$tally),]

if(length(visitErr2) > 0){

	# the following are problematic data... too many visits, or duplicate station visits...
	avisitErr <- sqldf("select amphstn.*,visitErr2.maxVisit as 'maxVisit', visitErr2.tally as 'tally' from amphstn join visitErr2 on amphstn.route = visitErr2.route AND amphstn.year = visitErr2.year AND amphstn.station = visitErr2.station")[,c(names(amphstn)[1:9],"maxVisit","tally")]

	# check if there is amph data for all the odd visit data
	amphdatVis <- sqldf("select form_id, route,year,month, day, stime, observer,station, visit,sum(code) as 'nCode',sum(count) as 'nCount' from amphdat1 group by route,year,station,observer,form_id,visit")
	checkAdat <- sqldf("select avisitErr.*, amphdatVis.* from avisitErr left join amphdatVis on avisitErr.route = amphdatVis.route AND avisitErr.year = amphdatVis.year AND avisitErr.station = amphdatVis.station AND avisitErr.form_id = amphdatVis.form_id AND avisitErr.observer = amphdatVis.observer")
	checkAdat$route <- as.character(checkAdat$route)

	write.csv(checkAdat,"visitErrorsAmphs.csv")

	# flag (for discarding) any duplicate visit data that doesn't have ANY amphibian data attached to it
	discards <- as.numeric(which(apply(checkAdat[,(ncol(avisitErr)+1):ncol(checkAdat)],1,FUN = function(x) all(is.na(x)))))
	
	Dindices <- numeric(0)
	for(dd in 1:length(discards)){
		Dindices <- c(Dindices,which(as.character(amphstn$route) == as.character(checkAdat$route[discards[dd]]) & amphstn$year == checkAdat$year[discards[dd]] & amphstn$form_id == checkAdat$form_id[discards[dd]] & amphstn$station == checkAdat$station[discards[dd]] & amphstn$visit == checkAdat$visit[discards[dd]] & amphstn$observer == checkAdat$observer[discards[dd]]))
		}
	
	amphstn <- amphstn[1:nrow(amphstn) %in% Dindices==FALSE,]
	rm(amphdatVis)
	}

# prompt for decision about whether to discard, merge, or re-enumerate duplicate visits
if(length(visitErr2) > 0){
	print(checkAdat) 
	print("in some stations & years, there are more than the declared number of visits")
	print("you must decide if you want to... ")
	print("...MERGE visits labeled as the same visit (type M)")
	print("...RE-NUMBER extra visits (type R)")
	}
	ANSduplicates <- "M"
	if(length(visitErr2) > 0){ 
		ANSduplicates <- winDialogString("Some stations have more than declared # of visits. Do you (M) merge visits, or (R) re-number them?","M")
		}
	if(all(ANSduplicates != "R",ANSduplicates != "r")){ANSduplicates <- "M"}

# pre-processing step to make sure we don't transform numeric data into character data
if(length(visitErr2) > 0){ isNumAmphdat <- as.numeric(which(unlist(lapply(amphdat1,FUN = function(x) is.numeric(x)))))
			  isNumamphstn <- as.numeric(which(unlist(lapply(amphstn,FUN = function(x) is.numeric(x)))))
			}

# merge visit data labeled as the same visit, in both amphibian and visit databases
# WARNING: this involves selecting and discarding visit environment data, based on a decision algorithm
if(ANSduplicates == "M" & length(visitErr2) > 0){
	merges <- checkAdat[1:nrow(checkAdat) %in% discards==FALSE,]
	merges2 <- sqldf("select * from merges group by route,year,station,visit")
	merges2$route <- as.character(merges2$route)
	
	# visit data of concern
	Vvar <-c("month","day","stime","wind","cloud","airtemp","watertemp","precip")

	# cycle through routes/years/stations with the same visit number
	for(mm in 1:nrow(merges2)){
		DONE <- FALSE
		while(DONE == FALSE){
		# indices in Visit data of the offending routes/station/visits
		Vindices <- which(amphstn$route == as.character(merges2$route[mm]) & amphstn$year == merges2$year[mm] & amphstn$station == merges2$station[mm] & amphstn$visit == merges2$visit[mm])
		subdata <- amphstn[Vindices,]
		
		if(nrow(subdata) == 1) {DONE <- TRUE
		} else {
		subdata[,isNumamphstn] <- apply(subdata[,isNumamphstn],2,as.numeric)
		
		# Decision flowchart to select duplicate visit data:
		# ... if one of the duplicates has no accompanying visit data, select other
		# ... if one has the majority of amphibian data, choose it
		# ... if one has any missing values in visit data, choose other
		# ... else, choose visit data from earlier survey
		
		chooseVdat <- subdata # this will contain the final data that is selected

		# ... if one of the duplicates has no accompanying visit data, select other
		chooseVdat <- chooseVdat[as.numeric(which(apply(chooseVdat[,Vvar],1,FUN = function(x) all(is.na(x)))==FALSE)),]
		if(nrow(chooseVdat) == 1) { DONE <- TRUE}
		if(nrow(chooseVdat) == 0) { chooseVdat <- subdata }
		
		# ... if one has more amphibian data, choose it for its visit data 
		ADataIndices <- which(amphdat1$route == merges2$route[mm] & amphdat1$year == merges2$year[mm] & amphdat1$station == merges2$station[mm] & amphdat1$visit == merges2$visit[mm])
		tempamphdat1 <- amphdat1[ADataIndices,]
		tempamphdat1[,isNumAmphdat] <- apply(tempamphdat1[,isNumAmphdat],2,as.numeric)
		tempamphdat1$code <- as.numeric(ifelse(is.na(tempamphdat1$code),0,as.numeric(tempamphdat1$code)))
		tempamphdat <- sqldf("select *, sum(code) as 'totalAmphs' from tempamphdat1 group by form_id,observer,month,day,stime") 
		
		# reorder tempamphdat to the same order as the visitation data (chooseVdat)
 		whatisdiff <- apply(chooseVdat[,c("form_id","route","year","month","day","stime","observer")],2,FUN = function(x) ifelse(length(unique(x))>1,2,1))
		orderr <- order(chooseVdat[names(whatisdiff)[match(2,whatisdiff)]])
		orderr2 <- order(tempamphdat[names(whatisdiff)[match(2,whatisdiff)]])
		tempamphdat <-tempamphdat[orderr2,]
		if(identical(orderr,order(tempamphdat[names(whatisdiff)[match(2,whatisdiff)]]))==FALSE){
			tempamphdat <-tempamphdat[orderr,]
			}
		chooseVdat <- chooseVdat[which(tempamphdat$totalAmphs == max(tempamphdat$totalAmphs)),] 
		if(nrow(chooseVdat) == 1) { DONE <- TRUE } else{
			# ... if one has fewer missing values in visit data, choose it	
			chooseVdat <- chooseVdat[which(as.numeric(apply(chooseVdat[,Vvar],1,FUN = function(x) length(which(is.na(x)))))
				==min(as.numeric(apply(chooseVdat[,Vvar],1,FUN = function(x) length(which(is.na(x))))))),]
			
			if(nrow(chooseVdat) == 1) { DONE <- TRUE } else{ 
				# ... else just choose the earlier date for the visit data
				chooseVdat <- chooseVdat[which(julian(as.Date(paste(chooseVdat$year,chooseVdat$month,chooseVdat$day,sep="-"))) + chooseVdat$stime/2400.0
				== min(julian(as.Date(paste(chooseVdat$year,chooseVdat$month,chooseVdat$day,sep="-"))) + chooseVdat$stime/2400.0)),]
				#chooseVdat <- chooseVdat[which(julian(chooseVdat$month,chooseVdat$day,chooseVdat$year,origin = c(month=1,day=1,year=chooseVdat$year[1])) + chooseVdat$stime/2400.0
				#== min(julian(chooseVdat$month,chooseVdat$day,chooseVdat$year,origin = c(month=1,day=1,year=chooseVdat$year[1])) + chooseVdat$stime/2400.0)),]	
				}	
			}
		
		
		# replace the visit data of the duplicates with the selected data, & move temporary matrix into the amphstn data 		
		amphstn[Vindices,Vvar] <- data.frame(matrix(apply(chooseVdat[,Vvar],2,as.character),nrow=nrow(amphstn[Vindices,]),ncol = length(Vvar),byrow=TRUE,dimnames=list(row.names(subdata),Vvar)),stringsAsFactors=FALSE)
				
		# replace the visit data of the duplicates with the finally selected data, & move temporary matrix into the amphdat1 data
		amphdat1[ADataIndices,c("month","day","stime")] <- data.frame(matrix(apply(chooseVdat[,c("month","day","stime")],2,as.character),nrow=nrow(tempamphdat1),ncol = 3,byrow=TRUE,dimnames=list(row.names(tempamphdat1),c("Month_no","Day_no","Start_Time"))),stringsAsFactors=FALSE)
		}		
		DONE <- TRUE
		}}
	amphstn[,isNumamphstn] <- apply(amphstn[,isNumamphstn],2,as.numeric)	# final step to restore NUMERIC class to columns
	amphdat1[,isNumAmphdat] <- apply(amphdat1[,isNumAmphdat],2,as.numeric)
	}

# Alternative, instead of merging the amphstn data by the visit number, we re-enumerate the duplicate visits as entirely different visits	
if(ANSduplicates == "R" & length(visitErr2) > 0)
	{		
	enumer <- checkAdat[1:nrow(checkAdat) %in% discards==FALSE,]

	# order visit data by month, day, start time, in order to re-enumerate based on date of visit
	enumer2 <- sqldf("select * from enumer order by route, year,station, month, day, stime")
	enumer2$route <- as.character(enumer2$route)
	enumer3 <- enumer2

	# re-enumerate
	for(ee in 2:nrow(enumer2)){
		if(enumer2$route[ee-1] == enumer2$route[ee] & enumer2$year[ee-1] == enumer2$year[ee] & enumer2$station[ee-1] == enumer2$station[ee])
			{ enumer2$visit[ee] <- enumer2$visit[ee-1]+1 }}
	
	# replace new visit values in amphstn and amphdat1 data
	for(mm in 1:nrow(enumer3)){	
		
		# find corresponding data in amphstn
		Vindices <- which(amphstn$route == enumer3$route[mm] & amphstn$year == enumer3$year[mm] & amphstn$station == enumer3$station[mm] & amphstn$observer == enumer3$observer[mm] & amphstn$visit == enumer3$visit[mm])
		
		# replace visit with the new visit number, as reenumerated in enumer2
		amphstn[Vindices,]$visit <- enumer2$visit[mm]

		# find corresponding data in amphdat1
		Aindices <- which(amphdat1$route == enumer3$route[mm] & amphdat1$year == enumer3$year[mm] & amphdat1$station == enumer3$station[mm] & amphdat1$observer == enumer3$observer[mm] & amphdat1$visit == enumer3$visit[mm])
		
		# replace amphdat1 with the new visit number, as reenumerated in enumer2
		amphdat1[Aindices,]$visit <- rep(enumer2$visit[mm],length(amphdat1[Aindices,]$visit))		
		}
	}

## END PROCESSING TO DEAL WITH DUPLICATE VISITS
	


################################################################
# MERGE Amphibian Visit DATA (amphstn) WITH VEG/HABITAT DATA
###############################################################

habitatA <- subset(habitat,habitat$type == "A" | habitat$type == "AB" | habitat$type == "BA" | is.na(habitat$type))
habitatA <- sqldf("select * from habitatA order by route,year,station")
amphstn <- sqldf("select * from amphstn order by route,year,station,visit")

# NOTE this drops duplicates of the same station/visit/year/route
avishab <- sqldf("select amphstn.*,habitatA.type,habitatA.wildrice,habitatA.bur_reed,habitatA.grasses,habitatA.rushes,habitatA.pickweed,habitatA.arrowhd,habitatA.smartwd,habitatA.purploos,habitatA.wwillow,habitatA.cattail,habitatA.comreed,habitatA.pemerge,habitatA.popenw,habitatA.pexpose,habitatA.ptree,habitatA.ntree,habitatA.pshrub,habitatA.float,habitatA.wetperm,habitatA.sizet,habitatA.sampedge,habitatA.landuse from amphstn left join habitatA on amphstn.route = habitatA.route AND amphstn.year = habitatA.year AND amphstn.station = habitatA.station AND amphstn.observer = habitatA.observer group by route,year,station,visit")

####################################################################
# MERGE Amph Visit and Habitat Data with Data Quality (mmpdatqual)
####################################################################

# NOTE this drops duplicates of the same station/visit/year/route
avishabqual <- sqldf("select avishab.*, mmpdatqual.nastns,mmpdatqual.dqhabit, mmpdatqual.dqamph from avishab left join mmpdatqual on avishab.route = mmpdatqual.route AND avishab.year = mmpdatqual.year AND avishab.observer = mmpdatqual.observer group by route,year,station,visit")

######################################################################################
# Filter Amphibian Visit/Habitat/Quality (aAllother) by relevant routes in mmproutes
#####################################################################################

aAllother <- subset(avishabqual,avishabqual$route %in% unique(mmproutes$route))
aAllother <- sqldf("select aAllother.*, mmproutes.rtename, mmproutes.aoccode,mmproutes.lat,mmproutes.long,mmproutes.latzone,mmproutes.distance,mmproutes.sizeha,mmproutes.basin,mmproutes.region,mmproutes.nrtown,mmproutes.county,mmproutes.provstat,mmproutes.remede,mmproutes.remedtyp,mmproutes.mcomplex,mmproutes.wetclass,mmproutes.mapyr,mmproutes.glcwc,mmproutes.durham,mmproutes.coastal,mmproutes.reg, mmproutes.bcr from aAllother join mmproutes on aAllother.route = mmproutes.route")
write.csv(aAllother,"AmphCovariates.csv",row.names=F)
# aAllother <- read.csv("AmphCovariates.csv",header=T)


###############################################################################
# Label Indicator Amphibian species (function, can be used in subsequent steps)
###############################################################################

# list of indicator species
IndicSp <- c("NLFR","MIFR","CHFR","SPPE")

labelIndicators <- function(speciescolumn,codecolumn,indicatorspecies){
	# function to quickly create columns scoring the richness of indicator species
	# input the 	1) column for the species (or many columns if a species table)
	#		2) column for the code (or many columns if a species table)
	#		3) and, the vector of indicator species (IndicSp above)
	#
	# check if we're dealing with a species table, or stacked species observations
	if(!is.data.frame(speciescolumn) & !is.data.frame(codecolumn))
		{		# dealing with stacked species observations
		return(ifelse(speciescolumn %in% IndicSp & codecolumn >0,1,0))
		} else {
	if(identical(dim(speciescolumn),dim(codecolumn)))
		{		# dealing with species table
		return(as.numeric(rowSums(apply(codecolumn[,indicatorspecies],2,FUN = function(x) ifelse(x > 0,1,0)))))
	}}	}

# add number of indicator species to amphdat2
amphdat1 <- cbind(amphdat1,data.frame(nindic = labelIndicators(amphdat1$species,amphdat1$code,IndicSp)))

########################################################################
# Filter Amphibian count data by relevant routes in mmproutes
########################################################################

amphdat2 <- subset(amphdat1,amphdat1$route %in% unique(mmproutes$route))[c("route","station","visit","year","month","day","stime","species","code","count","ins")]

# add number of indicator species to amphdat2
amphdat2 <- cbind(amphdat2,data.frame(nindic = labelIndicators(amphdat2$species,amphdat2$code,IndicSp)))

write.csv(amphdat2,"amphdat2.csv",row.names=F)
# amphdat2 <- read.csv("amphdat2.csv",header=T)

			
########################################################################
# Join amphibian with habitat data
########################################################################
# NOTE this step is supplied only because it was done previously.
# This product does not feed into further constructions
#
#mmpamphdata <- sqldf("select amphdat2.*, aAllother.* from amphdat2 left join aAllother on amphdat2.route = aAllother.route AND amphdat2.year = aAllother.year AND amphdat2.station = aAllother.station AND amphdat2.visit = aAllother.visit")
#file <- paste("mmpamphdata",min(mmpamphdata$year),"-",max(mmpamphdata$year),".csv",sep="")
#write.csv(mmpamphdata,file)

#############################################################################################################
# Create an Amphibian Species table, per visit/station, per year as well
#
# This creates a main amphstn file, which is equivalent to the older methods, as well as per visit
#############################################################################################################
rm(amphstn, amphdat, amphdat1)
amphdat2$species <- toupper(amphdat2$species)

# collect all the species names
asplist <- unique(amphdat2$species)

# remove NONE from the vector of species names
asplist <- asplist[asplist != "NONE"]

amphdat2$ID <- paste(amphdat2$route,amphdat2$year,amphdat2$station,amphdat2$visit,sep="")
aAllother$ID <- paste(aAllother$route,aAllother$year,aAllother$station,aAllother$visit,sep="")

# first, check if there are any TYPE's labeled NA, which do include amphibian data
noType <- aAllother$ID[which(is.na(aAllother$type))]
aAllother$type <- ifelse(!is.na(aAllother$type),as.character(aAllother$type),ifelse(aAllother$ID %in% unique(amphdat2$ID),"A","A"))

# remove any data that is not labeled as type B, or AB
sitedat <- subset(aAllother,aAllother$type == "A" | aAllother$type =="AB" | aAllother$type == "BA")

	asptable <- data.frame(ID = sitedat$ID)
	for(sp in asplist)		# iterate through species 
		{			# create species sums for each visit/route/station/year
		d <- subset(amphdat2,amphdat2$species == sp)
		dsum <- sqldf(paste("select ID,max(code) as '",sp,"' from d group by ID",sep=""))	
		asptable <- sqldf(paste("select asptable.*,dsum.",sp," from asptable left join dsum on asptable.ID = dsum.ID",sep=""))
		}

	# replace NA's with zeros
	asptable[,-1] <-apply(asptable[,-1],2,FUN = function(x) as.numeric(ifelse(is.na(x),0,x)))
	
	# now sum the richness of indicator species --> see the function above "labelIndicators"
	asptable$nindic <- labelIndicators(asptable[,asplist],asptable[,asplist],IndicSp)

	# sum overall species richness "npres"
	asptable$npres <- as.numeric(rowSums(apply(asptable[,asplist],2,FUN = function(x) ifelse(x > 0,1,0))))	
	
	# now remerge back with sitedat
	sitedat <- sqldf("select * from sitedat order by ID")
	asptable <- sqldf("select * from asptable order by ID")
	asptableByVisit <- cbind(sitedat,asptable[,names(asptable) != "ID"])  # this has a column for each species, and a row for each VISIT
	filename <- paste("Amphstns_ByVisit.csv",sep="")
	write.csv(asptableByVisit,filename,row.names = F)
	# Build an SQL statement to take maximum observations at each station
	sqlstatment_maxAmphs <- paste("select", paste(c("route","year","station"),sep="",collapse=","),", count(visit) as 'nVisit',",
					paste("max(",asplist,") as '",asplist,"'",sep="",collapse=","),
					" from asptableByVisit group by year, route, station") #Fixed, was previously group by ID
	maxAmphs <- sqldf(sqlstatment_maxAmphs)				# this has a species column, lisiting observations by station (not visit)

	# re-count richness of indicator species and ALL species
	maxAmphs$nindic <- labelIndicators(maxAmphs[,asplist],maxAmphs[,asplist],IndicSp)
	maxAmphs$npres <- as.numeric(rowSums(apply(maxAmphs[,asplist],2,FUN = function(x) ifelse(x > 0,1,0))))	
	
	# re-add environmental data, route, and dataquality 
	maxAmphsH <- sqldf(paste("select maxAmphs.*,",paste("habitatA.",names(habitatA)[names(habitatA) %in% c("ID","route","year","station","observer","type","basin","dqhabit") ==FALSE],sep="",collapse=",")," from maxAmphs left join habitatA on maxAmphs.route = habitatA.route AND maxAmphs.year = habitatA.year AND maxAmphs.station = habitatA.station group by route,year,station",sep=""))
	maxAmphsR <- sqldf(paste("select maxAmphsH.*,",paste("mmproutes.",names(mmproutes)[names(mmproutes) %in% c("route") ==FALSE],sep="",collapse=",")," from maxAmphsH left join mmproutes on maxAmphsH.route = mmproutes.route group by route,year,station",sep=""))
	maxAmphsDQ <- sqldf("select maxAmphsR.*, mmpdatqual.nastns,mmpdatqual.dqhabit,mmpdatqual.dqamph from maxAmphsR left join mmpdatqual on maxAmphsR.route = mmpdatqual.route AND maxAmphsR.year = mmpdatqual.year group by route,year,station")

	# reorder data.frame to produce Birdstns.csv (equivalent to previous SAS build program)
	namesord <- c(which(names(maxAmphsDQ) %in% c(as.character(asplist),"npres","nindic")==FALSE),which(names(maxAmphsDQ) %in% c(as.character(asplist),"nindic","npres")))
	Amphstns <- maxAmphsDQ[,namesord]
	rm(maxAmphsH); rm(maxAmphsR); rm(maxAmphsDQ)
	
	finalFileName <- paste("Amphstns.csv",sep="")
	write.csv(Amphstns,finalFileName,row.names=F)

########################################################################
##   END AMPHIBIAN DATA
########################################################################










# USER-FRIENDLY SUMMARY OF TREND OUTPUT FILES
#
# Script to take model outputs from the trend analysis script "MMP Trend Analysis - Bayesian. R"
# The input files need to be named specifically (generally located in the "T:/MARSH/Trend Analyses/")
# Some of the necessary species-specific plotting files are in "temp_R_files_TRENDS"
# The script generates various table which are more user friendly to interpret
# Input a prefix, (e.g., Bird or Amph), then the following files are generated:
#
# PREFIX Overall Change in Species Counts.csv 
# PREFIX Overall Change in Species Presence.csv 
# PREFIX Annual Abundances Indices.csv 
# PREFIX Annual Presence Indices.csv 
# PREFIX Overall Change in Species Presence per Basin.csv 
# PREFIX Overall Change in Species Abundance per Basin.csv 

# .~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`
# 		Latest Author Robert William Rankin (robertw.rankin@gmail.com)
# .~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`

##################################################################
# SETUP
##################################################################
  library(sqldf)
  library(Hmisc)	# required for making error bars in plotting output
  library(plyr)
  setwd("T:/MARSH/Trend Analyses/")
  source("I:/R-functions/functions.r")

  prefix <- "Bird"	# default prefix, the next line prompts you for a prefix
  prefix <- winDialogString("Please enter a prefix for the output files (like 'Bird' or 'Amph' ... without quotation marks).","Bird")

# DECLARE the results file to import results results
  #fileName <- "Trend Amphs Analysis - GLMM.csv"
  #fileName <- "Trend Birds Analysis - GLMM.csv"
  #fileName <- "Trend Bird Analysis 2010.csv"
  fileName <- "Trend Amphs Analysis.csv"

# DECLARE the folder where temporary output files are stored (something like "temp_R_files_TRENDS" or "temp_R_files_TRENDS - GLMM" for GLMM
  #plottingFolder <- "temp_R_files_TRENDS - GLMM"
  plottingFolder <- "temp_R_files_TRENDS"
      
# check if the output files already exist, and prompt user if they want to replace them, or rename current
# output files with a unique suffix

outputNames <- c(paste(prefix,"Overall Change in Species Counts.csv",sep=" "),
	paste(prefix,"Overall Change in Species Presence.csv",sep=" "),
	paste(prefix,"Annual Abundances Indices.csv",sep=" "),
	paste(prefix,"Annual Presence Indices.csv",sep=" "),
	paste(prefix,"Overall Change in Species Presence per Basin.csv",sep=" "),
	paste(prefix,"Overall Change in Species Abundance per Basin.csv",sep=" "))

# index of outputNames which do exist
whichFilesExist <- file.exists(outputNames)

# mesage to ask user if they want to replace the output files or enter a suffix (avoids having the same name)
promptMessage <- paste("WARNING: Some output files already exist. Enter a unique suffix. Enter nothing to allow overwriting.")
suffix <- ""
if(any(whichFilesExist)){
	suffix <- winDialogString(promptMessage, "")
	}

##################################################################
# IMPORT RESULTS
##################################################################
 
# go through each line of the output file: try to extract every 2 lines together (1st line is a header, 2nd is data)
# This finds the "headers" by locating lines with the following entries: species,model,family,random,subset,duration
# This finds the accompanying data by locating lines with a four-letter species code in the first column, such as SEWR
# if a line begins with neither, than ignores that line and tells you there is an error (messed up data) for you to 
# review manually

GOREAD <- TRUE		# dummy variable to end while lopp
index <- 0		# initial number of rows to skip
rs <- list()	# resulting list of data
goodLines <- numeric(0)	# record of "good" lines which have data. Otherwise, the data is messed-up in some way
while(GOREAD){
	# read 1 line of data
	readUnit1 <- scan(fileName, skip = index, nlines=1, what = list("raw"),sep=",", quiet =TRUE)

	# check if we've reached the end of the file (empty row -- i.e., STOP reading lines)
	if(length(readUnit1[[1]]) == 0) { GOREAD <- FALSE	# trigger end of while loop
	} else {
		
		index <- index + 1 	# move index up one line
	
		# check if the data is a header line or actual results data (i.e., the first columns should be "species","model"
		if(all(readUnit1[[1]][1:2] == c("species","model"))){

			# if the 1st line is a header, then proceed to read next line (expecting it to be data)
			readUnit2 <- scan(fileName, skip = index, nlines=1, what = list("raw"),sep=",",quiet =TRUE)
		
			# check if the next line of data has a species code for the first column
			# ... if so, then it has the actual data to make the previous header
			if(nchar(readUnit2[[1]][1]) == 4){
				index <- index + 1 	# line of the file to read

				# bind the 1st line (header) to the second line (data)
				tempDat <- data.frame(matrix(readUnit2[[1]],ncol = length(readUnit2[[1]])), stringsAsFactors = FALSE)
				names(tempDat) <- readUnit1[[1]]	
			
				# import resulting table into rs list
				rs[[length(rs)+1]] <- tempDat

				# finally, make a note that these two lines where successfully read
				goodLines <- c(goodLines,c(index,index-1))
				}
			}
		}
	}
		
# Process the reported "goodLines" to see which are "bad lines" and prompt the user to deal with the messed-up lines
goodLines <- goodLines[order(goodLines)]
badLines <- (1:(index-1))[which(1:(index-1) %in% goodLines == FALSE)]

# alert user of the messed-up lines
if(length(badLines)>0){
	winDialog("ok", paste("WARNING: there seems to be errors in your input file around row(s)",paste(badLines,collapse=","),". Review Manually and fix/delete weird rows"))
	}

##################################################################
# PRE-PROCESS RESULTS: MAKE DIFFERENT TABLES PER MODEL FAMILY / FORMULA
##################################################################


# Seperate the different models by formula  
# make a new list to store the model outputs
  rs2 <- list()	

  for(mm in 1:length(rs)){ 

	# step through each results table (e.g., "headers" and data together)
	currTable <- rs[[mm]]
	currDatF <- currTable[1,which(currTable[1,] != "")]

	# check if the model is NOT already in the  new list (rs2)
	if(paste(as.character(currTable[1,3]),as.character(currTable[1,4]),as.character(currTable[1,2]),sep="") %in% names(rs2) == FALSE) {
		
		# make a new data.frame, using the data, and the header
		rs2[[paste(as.character(currTable[1,3]),as.character(currTable[1,4]),as.character(currTable[1,2]),sep="")]] <- currDatF

	} else {   # if the data.frame is already in the new list (rs2)
		existTable <- rs2[[paste(as.character(currTable[1,3]),as.character(currTable[1,4]),as.character(currTable[1,2]),sep="")]] # extract exisiting table in the list
		if("error" %in% names(existTable)==FALSE) {existTable$error <- rep(NA,nrow(existTable))}
		
		# check if the headers align, or if there are new ones to add
		missH <- names(currDatF)[which(names(currDatF) %in% names(existTable) == FALSE)]
		if(length(missH) > 0) {
			# add new header to existing table
			existTable<- cbind(existTable,matrix(rep(NA,length(missH) * nrow(existTable)),ncol = length(missH),dimnames = list(row.names(existTable),missH)))
			}
		# rearrange new data to conform to existing data
		addrow <- existTable[1,]
		addrow[names(currDatF)] <- currDatF[1,names(currDatF)]
		addrow[names(addrow) %in% names(currDatF) == FALSE] <- NA
		rs2[[paste(as.character(currTable[1,3]),as.character(currTable[1,4]),as.character(currTable[1,2]),sep="")]] <- rbind(existTable,addrow)	
		}
	}


##################################################################
# PRE-PROCESS RESULTS: ARE THERE DUPLICATE MODELS?
##################################################################
# duplciates defined as same formula, family, and subset

# check for duplications, and take model with best fit
# cycle through the different tables (one per different model specification)
  for(j in 1:length(rs2)){

	# extract model table
	modtab <- rs2[[j]]

	# check if rows have the same species (& subset), implying there has been duplication
	duplicSp <- which(paste(modtab$species,modtab$subset,sep="") %in% names(which(table(paste(modtab$species,modtab$subset,sep=""))>1)))

	if(length(duplicSp) > 0){

		# extract model results for those species which are NOT duplicates
		newtab <- modtab[which(1:nrow(modtab) %in% duplicSp == FALSE),]

		# collect unique combinations of species and subsets
		duplicCombos <- unique(modtab[duplicSp,c("species","subset")])

		for(sp in 1:nrow(duplicCombos)){

			# extract model results for those species which have duplicates
			subtab <- subset(modtab,species == duplicCombos$species[sp] & subset == duplicCombos$subset[sp])
			
			# check if one model result is actually an error
			if(any(is.na(subtab$error)) & any(!is.na(subtab$error))){

				# if one is an error, but the second isn't, take the non-error value
				takei <- which(is.na(subtab$error))
			} else {
				if(all(!is.na(subtab$error))){

				# check if both models are errors, then return both
				takei <- nrow(subtab)
				} else {
					# check if there are any errors in the pBayesian, and assing value of 1
					subtab$pBayesian[is.na(subtab$pBayesian)] <- 1
					
					# for the GLMM analysis, no model fit criteria is produced
					# ... so we return the first model arbitrarily
				
					if(any(subtab$pBayesian == "-")){
						takei <- subtab[1,]

					} else {
					
						# check if one of the solutions has a poor fit, while the other doesn't
						if(any(subtab$pBayesian < 0.05 | subtab$pBayesian > 0.95 | is.na(subtab$pBayesian)) & !all(subtab$pBayesian < 0.05 | subtab$pBayesian > 0.95 | is.na(subtab$pBayesian))){
						
							# take one with okay model fit
							takei <- which((subtab$pBayesian > 0.05 & subtab$pBayesian < 0.95))
							takei <- which(subtab$DIC == min(subtab$DIC))[1]
						} else {
						
							# take the one with lowest DIC
							takei <- which(subtab$DIC == min(subtab$DIC))[1]
							}
						}
					}
				}
			newtab <- rbind(newtab,subtab[takei,])
			}
		rs2[[j]] <- newtab
		}
	}
		
rs3 <- rs2
plotdat <- list()

			
##################################################################
# MAKE TABLES FOR PRESENTING RESULTS
##################################################################
		 
# Tables for reporting: Linear Trend Estimate, Poisson
  i <- 1:length(rs2)

# Tables for reporting: Linear Trend Estimate, Poisson
# find results table in rs2 which has the annual trends, for poisson
  yearsI <- which(i %in% grep("poisson",names(rs2)) & i %in% grep("year",names(rs2)) & i %in% grep("basin",names(rs2)) == FALSE & regexpr("as.factor(year)",names(rs2),fixed=TRUE) < 0)
  if(length(yearsI) > 0){
  	trendsDat <- rs2[[yearsI]]		# extract data table
        trendsDat[,c("year","yearlowCI","yearupCI")] <- apply(trendsDat[,c("year","yearlowCI","yearupCI")],2,FUN = function(x) ifelse(!is.na(x),as.numeric(x)-1,NA))
	rs3[[yearsI]] <- trendsDat
	trendsDat$CI <- paste("(",round(trendsDat$yearlowCI,4),",",round(trendsDat$yearupCI,4),")",sep="")
        printDat <- trendsDat[,c("species","year","subset","duration","CI","yearp","yearpdecline","pBayesian","error")]

	# change names of printDat
	names(printDat) <- c("species","annual trend","subset","duration","CI","p","probability of decline","model-fit","error-comment")
	
	# check if the model fits poorly or not
	poorfits <- which((printDat[,"model-fit"] > 0.975 | printDat[,"model-fit"] < 0.025) & printDat[,"model-fit"] > 0)
	commentt <- "poor fit: consider using average trend over all-basins, or perhaps logisitc regression"
	if(length(poorfits)>0){
		printDat[poorfits,"error-comment"] <- ifelse(is.na(printDat[poorfits,"error-comment"]),commentt,paste(printDat[poorfits,"error-comment"],"; ",commentt,sep=""))
		}
	# order table by species codes
	sporder <- order(printDat$species)
	printDat <- printDat[sporder,]

	# write the table name
	write.table(c("Annual change in species counts"),paste(prefix,"Overall Change in Species Counts",suffix,".csv",sep=" "),sep=",",row.names=F,col.names=F)
	
	# write the results
	write.table(printDat,paste(prefix,"Overall Change in Species Counts",suffix,".csv",sep=" "),sep=",",row.names=F,append=T)
	
	# write concluding comments describing the results
	write.table(matrix(c(paste("trends were analyzed at the level of",rs2[[yearsI]]$random[1]),
		"annual trend = proportion change in counts per year (multiply by 100% for percent)",
		"CI = 95% Bayesian Credibility Interval",
		"'probability of decline' is estimated from the Bayesian posterior distribution as the proportion of chains show a negative trend",
		"model-fit = 'Bayesian P value' from a posterior-predictive check. Values below 0.025 and above 0.975 are evidence of poor model fit"),ncol=1),
		paste(prefix,"Overall Change in Species Counts",suffix,".csv",sep=" "),sep=",",row.names=F,col.names=F,append=T)
		}


# Tables for reporting: Linear Trend Estimate, Logistic
# find results table in rs2 which has the annual trends, for Logistic
  yearsIlog <- which((i %in% grep("categorical",names(rs2)) | i %in% grep("binomial",names(rs2))) & i %in% grep("year",names(rs2)) & i %in% grep("basin",names(rs2)) == FALSE & regexpr("as.factor(year)",names(rs2),fixed=TRUE) < 0)
  if(length(yearsIlog) > 0){
  	trendsDat <- rs2[[yearsIlog]]		# extract data table
        trendsDat[,c("year","yearlowCI","yearupCI")] <- apply(trendsDat[,c("year","yearlowCI","yearupCI")],2,FUN = function(x) ifelse(!is.na(x),exp(boot::logit(as.numeric(x)))-1,NA))
	rs3[[yearsIlog]] <- trendsDat
	trendsDat$CI <- paste("(",round(trendsDat$yearlowCI,4),",",round(trendsDat$yearupCI,4),")",sep="")
        printDat <- trendsDat[,c("species","year","subset","duration","CI","yearp","yearpdecline","pBayesian","error")]

	# change names of printDat
	names(printDat) <- c("species","annual trend","subset","duration","CI","p","probability of decline","model-fit","error-comment")
	
	# check if the model fits poorly or not
	poorfits <- which((printDat[,"model-fit"] > 0.975 | printDat[,"model-fit"] < 0.025) & printDat[,"model-fit"] > 0)
	commentt <- "poor fit: consider using average trend over all-basins, or perhaps logisitc regression"
	
	if(length(poorfits)>0){
		printDat[poorfits,"error-comment"] <- ifelse(is.na(printDat[poorfits,"error-comment"]),commentt,paste(printDat[poorfits,"error-comment"],"; ",commentt,sep=""))
		}

	# order table by species codes
	sporder <- order(printDat$species)
	printDat <- printDat[sporder,]

	# write the table name
	write.table(c("Overall Change in Species Presence"),paste(prefix,"Overall Change in Species Presence",suffix,".csv",sep=" "),sep=",",row.names=F,col.names=F)
	
	# write the results
	write.table(printDat,paste(prefix,"Overall Change in Species Presence",suffix,".csv",sep=" "),sep=",",row.names=F,append=T)
	
	# write concluding comments describing the results
	write.table(matrix(c(paste("trends were analyzed at the level of",rs2[[yearsIlog]]$random[1]),
		"annual trend = proportion change in odds ratio of species being present, per year (multiply by 100% for percent)",
		"CI = 95% Bayesian Credibility Interval",
		"probability of decline is estimated from the Bayesian posterior distribution as the number of chains show a negative trend",
		"model-fit = 'Bayesian P value' from a posterior-predictive check. Values below 0.025 and above 0.975 are evidence of poor model fit"),ncol=1),
		paste(prefix,"Overall Change in Species Presence",suffix,".csv",sep=" "),sep=",",row.names=F,col.names=F,append=T)
	}


# Table for reporting: Annual Abundances Indices, Poisson
  CyearsI <- which(i %in% grep("poisson",names(rs2)) & regexpr("as.factor(year)",names(rs2),fixed=TRUE) > 0)
  if(length(CyearsI) > 0){
  	indicesDat <- rs2[[CyearsI]]
	
	# change the names into recognizable years (WARNING: assumes that the oldest year is 1995
	# first, find which names need to be changed	
	changeNames <- which(regexpr("as.factor(year)",names(indicesDat),fixed=TRUE) > 0)
	yearValuesC <- unique(as.numeric(sapply(names(indicesDat)[changeNames],FUN = function(x) substr(x, nchar("as.factor(year)")+1, nchar(x)), USE.NAMES=FALSE)))
	yearValuesC <- yearValuesC[!is.na(yearValuesC)]
	#yearNames <- yearValuesC - min(yearValuesC) + 1995
	yearNames <- yearValuesC
	names(indicesDat)[changeNames] <- sapply(names(indicesDat)[changeNames],FUN = function(x) paste("year",substr(x, nchar("as.factor(year)")+1, nchar(x)),sep=""), USE.NAMES=FALSE)
	for(x in 1:length(yearValuesC)){names(indicesDat)[changeNames] <- gsub(paste("year",yearValuesC[x],sep=""), yearNames[x], names(indicesDat)[changeNames])}	
	rs3[[CyearsI]]	<- indicesDat

	# scale each abundance estimate relative to the 1995 counts
	# define the columns which should be scaled
	whichToScale <- c(as.character(yearNames),paste(yearNames,"lowCI",sep=""),paste(yearNames,"upCI",sep=""))
	
	# for some reason, need to ensure that the data is numeric
	indicesDat[,whichToScale] <- apply(indicesDat[,whichToScale],2, FUN = function(x) as.numeric(x))

	# find the "baseline" year... should be 1995, or if 1995 is year, then use earliest year with non-zero value
	#baseline <- apply(indicesDat[,as.character(yearNames)],1, FUN = function(x) x[min(which(as.numeric(x)!=0))])
	
	# scale all estimates by the baseline
	#indicesDat[,whichToScale] <- indicesDat[,whichToScale]/matrix(rep(as.numeric(baseline),length(whichToScale)),ncol=length(whichToScale),byrow=FALSE)
	
	rs3[[CyearsI]] <- indicesDat
	
	# make a table for reporting
	printDat2 <- indicesDat[,c("species","subset","duration",yearNames)]
	lowCIs <- paste(yearNames,"lowCI",sep="")
	hiCIs <- paste(yearNames, "upCI",sep="")
	minHiCI <- rep(10**6,nrow(printDat2))			# dummy variable to collect the lowest upper confidence interval
	maxLowCI <- rep(10**-6,nrow(printDat2))			# dummy variable to collect the max upper confidence interval
	for(jj in yearNames){
		minHiCI <- ifelse(minHiCI < indicesDat[,paste(jj,"upCI",sep="")], minHiCI, indicesDat[,paste(jj,"upCI",sep="")])
		maxLowCI <- ifelse(maxLowCI > indicesDat[,paste(jj,"lowCI",sep="")], maxLowCI, indicesDat[,paste(jj,"lowCI",sep="")])
		printDat2[,paste(jj,"CI",sep="")] <- paste("(",round(indicesDat[,paste(jj,"lowCI",sep="")],4),",",round(indicesDat[,paste(jj,"upCI",sep="")],4),")",sep="")
		}

	# determine if there is significant variation between years (e.g., at least one 95% confidence interval does not overlap with the others)
	printDat2$pvalue <- ifelse(maxLowCI >= minHiCI,"< 0.05","N.S.")

	# order the columns
	printDat2 <- printDat2[,c(1,order(names(printDat2)[-1])+1)]

	# add model fit and any errors
	printDat2 <- cbind(printDat2,indicesDat[,c("pBayesian","error")])
	names(printDat2)[which(names(printDat2) %in% c("pBayesian","error"))] <- c("model-fit","error-comment")
	
	# order the species names
	sporder <- order(printDat2$species)
	printDat2 <- printDat2[sporder,]

	# write the table name
	write.table(c("Annual Abundances Indices"),paste(prefix,"Annual Abundances Indices",suffix,".csv",sep=" "),sep=",",row.names=F,col.names=F)
	
	# write the results
	write.table(printDat2,paste(prefix,"Annual Abundances Indices",suffix,".csv",sep=" "),sep=",",row.names=F,append=T)
	
	# write concluding comments describing the results
	write.table(matrix(c(paste("trends were analyzed at the level of",rs2[[CyearsI]]$random[1]),
		"Annual indices are the model average counts-per-sampling-unit",
		"CI = 95% Bayesian Credibility Interval",
		"p-value = classical test of significant difference between annual indices",
		"model-fit = 'Bayesian P value' from a posterior-predictive check. Values below 0.025 and above 0.975 are evidence of poor model fit"),ncol=1),
		paste(prefix,"Annual Abundances Indices",suffix,".csv",sep=" "),sep=",",row.names=F,col.names=F,append=T)
	
	}


# Table for reporting: Annual Abundances Indices, Logistic
  CyearsIlog <- which((i %in% grep("categorical",names(rs2)) | i %in% grep("binomial",names(rs2))) & regexpr("as.factor(year)",names(rs2),fixed=TRUE) > 0)
  if(length(CyearsIlog) > 0){
  	indicesDat <- rs2[[CyearsIlog]]
	
	# change the names into recognizable years (WARNING: assumes that the oldest year is 1995
	# first, find which names need to be changed	
	changeNames <- which(regexpr("as.factor(year)",names(indicesDat),fixed=TRUE) > 0)
	yearValuesC <- unique(as.numeric(sapply(names(indicesDat)[changeNames],FUN = function(x) substr(x, nchar("as.factor(year)")+1, nchar(x)), USE.NAMES=FALSE)))
	yearValuesC <- yearValuesC[!is.na(yearValuesC)]
	#yearNames <- yearValuesC - min(yearValuesC) + 1995
	yearNames <- yearValuesC
	names(indicesDat)[changeNames] <- sapply(names(indicesDat)[changeNames],FUN = function(x) paste("year",substr(x, nchar("as.factor(year)")+1, nchar(x)),sep=""), USE.NAMES=FALSE)
	for(x in 1:length(yearValuesC)){names(indicesDat)[changeNames] <- gsub(paste("year",yearValuesC[x],sep=""), yearNames[x], names(indicesDat)[changeNames])}	
	rs3[[CyearsIlog]]	<- indicesDat

	# scale each abundance estimate relative to the 1995 counts
	# define the columns which should be scaled
	whichToScale <- c(as.character(yearNames),paste(yearNames,"lowCI",sep=""),paste(yearNames,"upCI",sep=""))
	
	# for some reason, need to ensure that the data is numeric
	indicesDat[,whichToScale] <- apply(indicesDat[,whichToScale],2, FUN = function(x) as.numeric(x))

	# find the "baseline" year... should be 1995, or if 1995 is year, then use earliest year with non-zero value
	#baseline <- apply(indicesDat[,as.character(yearNames)],1, FUN = function(x) x[min(which(as.numeric(x)!=0))])
	
	# scale all estimates by the baseline
	# here, we convert each year(and its CI) to the odds ratio scale (exp(logit(p)) 
	# then the  index is the Odds ratio of the odds at time=N / odds at time=1995
	#indicesDat[,whichToScale] <- apply(indicesDat[,whichToScale],2, FUN = function(x) exp(boot::logit(x)))/matrix(rep(exp(boot::logit(as.numeric(baseline))),length(whichToScale)),ncol=length(whichToScale),byrow=FALSE)
	
	rs3[[CyearsIlog]] <- indicesDat
	
	# make a table for reporting
	printDat2 <- indicesDat[,c("species","subset","duration",yearNames)]
	lowCIs <- paste(yearNames,"lowCI",sep="")
	hiCIs <- paste(yearNames, "upCI",sep="")
	minHiCI <- rep(10**6,nrow(printDat2))			# dummy variable to collect the lowest upper confidence interval
	maxLowCI <- rep(10**-6,nrow(printDat2))			# dummy variable to collect the max upper confidence interval
	for(jj in yearNames){
		minHiCI <- ifelse(minHiCI < indicesDat[,paste(jj,"upCI",sep="")], minHiCI, indicesDat[,paste(jj,"upCI",sep="")])
		maxLowCI <- ifelse(maxLowCI > indicesDat[,paste(jj,"lowCI",sep="")], maxLowCI, indicesDat[,paste(jj,"lowCI",sep="")])
		printDat2[,paste(jj,"CI",sep="")] <- paste("(",round(indicesDat[,paste(jj,"lowCI",sep="")],4),",",round(indicesDat[,paste(jj,"upCI",sep="")],4),")",sep="")
		}
	# determine if there is significant variation between years (e.g., at least one 95% confidence interval does not overlap with the others)
	printDat2$pvalue <- ifelse(maxLowCI >= minHiCI,"< 0.05","N.S.")
	
	# order the columns
	printDat2 <- printDat2[,c(1,order(names(printDat2)[-1])+1)]

	# add model fit and any errors
	printDat2 <- cbind(printDat2,indicesDat[,c("pBayesian","error")])
	names(printDat2)[which(names(printDat2) %in% c("pBayesian","error"))] <- c("model-fit","error-comment")
	
	# order the species names
	sporder <- order(printDat2$species)
	printDat2 <- printDat2[sporder,]

	# write the table name
	write.table(c("Annual Presence Indices"),paste(prefix,"Annual Presence Indices",suffix,".csv",sep=" "),sep=",",row.names=F,col.names=F)
	
	# write the results
	write.table(printDat2,paste(prefix,"Annual Presence Indices",suffix,".csv",sep=" "),sep=",",row.names=F,append=T)
	
	# write concluding comments describing the results
	write.table(matrix(c(paste("trends were analyzed at the level of",rs2[[CyearsIlog]]$random[1]),
		"Annual index is the model average probability of species being present at sampling-unit",
		"CI = 95% Bayesian Credibility Interval",
		"p-value = classical test of significant difference between annual indices",
		"model-fit = 'Bayesian P value' from a posterior-predictive check. Values below 0.025 and above 0.975 are evidence of poor model fit"),ncol=1),
		paste(prefix,"Annual Presence Indices",suffix,".csv",sep=" "),sep=",",row.names=F,col.names=F,append=T)
	
	}


# Table Reporting trends per basin, Poisson
  basinsI <- which(i %in% grep("poisson",names(rs2)) & regexpr("as.factor(year)",names(rs2),fixed=TRUE) < 0 & i %in% grep("basin",names(rs2)))
  if(length(basinsI) > 0){
  	basinDat <- rs2[[basinsI]]
	
	# find the names of basins
	bn <- which(1:ncol(basinDat) %in% grep("lowCI",names(basinDat)) & 1:ncol(basinDat) %in% grep("year",names(basinDat))==FALSE) 
	basinNames <- substr(names(basinDat)[bn],1,regexpr("lowCI",names(basinDat)[bn])-1)
	basinNames <- basinNames[order(basinNames)]
	basinNames <- basinNames[which(basinNames != "mainTrend")]
	# make table for printing, then add data from each basin to it
	printDat3 <- basinDat[,c("species","subset","duration","pBayesian")]

	# cycle through each "basin"
	for(bas in basinNames){
		tempDat <- basinDat[,grep(bas,names(basinDat))]
		trendIndices <- grep("year",names(tempDat))

		# take only the trend data (e.g., has an interaction with year)
		tempDat <- tempDat[,trendIndices]

		trendNames <- paste("year:",bas,c("","lowCI","upCI"),sep="")
		
		# make trends a %change
		tempDat[,trendNames] <- apply(tempDat[,trendNames],2,FUN = function(x) as.numeric(ifelse(!is.na(x),as.numeric(x)-1,NA)))
		
		# make Credibility Intervals
		tempDat[,paste(bas,"CI",sep="")] <- paste("(",round(tempDat[,paste("year:",bas,"lowCI",sep="")],4),",",round(tempDat[,paste("year:",bas,"upCI",sep="")],4),")",sep="")
		
		names(tempDat)[match(paste("year:",bas,c("","p","pdecline"),sep=""),names(tempDat))] <- paste(bas,c("","p"," probability of decline"),sep="")
		
		# attach basin data to printDat3
		printDat3 <- cbind(printDat3,tempDat[,paste(bas,c("","CI","p"," probability of decline"),sep="")])
		}
	# now deal with the estimated aggregate "mainTrend" # ONLY FOR BAYESIAN ANALYSIS
	if(length(grep("mainTrend",names(basinDat))) != 0){
	
		tempDat <- basinDat[,grep("mainTrend",names(basinDat))]
		trendNames <- paste("mainTrend",c("","lowCI","upCI"),sep="")
		
		# make trends a %change
		tempDat[,trendNames] <- apply(tempDat[,trendNames],2,FUN = function(x) as.numeric(ifelse(!is.na(x),as.numeric(x)-1,NA)))
		
		# make Credibility Intervals
		tempDat[,paste("mainTrend","CI",sep="")] <- paste("(",round(tempDat[,paste("mainTrend","lowCI",sep="")],4),",",round(tempDat[,paste("mainTrend","upCI",sep="")],4),")",sep="")
		
		names(tempDat)[match(paste("mainTrend",c("","p","pdecline"),sep=""),names(tempDat))] <- paste("mainTrend",c("","p"," probability of decline"),sep="")
		
		# attach basin data to printDat3
		printDat3 <- cbind(printDat3,tempDat[,paste("mainTrend",c("","CI","p"," probability of decline"),sep="")])
		}
	
	printDat3$error <- basinDat$error
	names(printDat3)[match("pBayesian",names(printDat3))] <- "model-fit"

	# order the species names
	sporder <- order(printDat3$species)
	printDat3 <- printDat3[sporder,]

	# write the table name
	write.table(c("Annual Abundances Indices per Basin"),paste(prefix,"Overall Change in Species Abundance per Basin",suffix,".csv",sep=" "),sep=",",row.names=F,col.names=F)
	
	# write the results
	write.table(printDat3,paste(prefix,"Overall Change in Species Abundance per Basin",suffix,".csv",sep=" "),sep=",",row.names=F,append=T)
	
	# write concluding comments describing the results
	write.table(matrix(c(paste("trends were analyzed at the level of",rs2[[basinsI]]$random[1]),
		"Change in species abundance, per Basin",
		"CI = 95% Bayesian Credibility Interval",
		"model-fit = 'Bayesian P value' from a posterior-predictive check. Values below 0.025 and above 0.975 are evidence of poor model fit",
		"'main trend' refers to the weighted average of all basin trend, estimated from the posterior distribution"),ncol=1),
		paste(prefix,"Overall Change in Species Abundance per Basin",suffix,".csv",sep=" "),sep=",",row.names=F,col.names=F,append=T)
	
	}
	

# Table Reporting trends per basin, Logistic
  basinsIlog <- which((i %in% grep("categorical",names(rs2)) | i %in% grep("binomial",names(rs2))) & regexpr("as.factor(year)",names(rs2),fixed=TRUE) < 0 & i %in% grep("basin",names(rs2)))
  if(length(basinsIlog) > 0){
  	basinDat <- rs2[[basinsIlog]]
	
	# find the names of basins
	bn <- which(1:ncol(basinDat) %in% grep("lowCI",names(basinDat)) & 1:ncol(basinDat) %in% grep("year",names(basinDat))==FALSE) 
	basinNames <- substr(names(basinDat)[bn],1,regexpr("lowCI",names(basinDat)[bn])-1)
	basinNames <- basinNames[order(basinNames)]
	basinNames <- basinNames[which(basinNames != "mainTrend")]
	# make table for printing, then add data from each basin to it
	printDat3 <- basinDat[,c("species","subset","duration","pBayesian")]

	# cycle through each "basin"
	for(bas in basinNames){
		tempDat <- basinDat[,grep(bas,names(basinDat))]
		trendIndices <- grep("year",names(tempDat))

		# take only the trend data (e.g., has an interaction with year)
		tempDat <- tempDat[,trendIndices]

		trendNames <- paste("year:",bas,c("","lowCI","upCI"),sep="")
		
		# make trends a %change
		tempDat[,trendNames] <- apply(tempDat[,trendNames],2,FUN = function(x) as.numeric(ifelse(!is.na(x),exp(boot::logit(as.numeric(x)))-1,NA)))
		
		# make Credibility Intervals
		tempDat[,paste(bas,"CI",sep="")] <- paste("(",round(tempDat[,paste("year:",bas,"lowCI",sep="")],4),",",round(tempDat[,paste("year:",bas,"upCI",sep="")],4),")",sep="")
		
		names(tempDat)[match(paste("year:",bas,c("","p","pdecline"),sep=""),names(tempDat))] <- paste(bas,c("","p"," probability of decline"),sep="")
		
		# attach basin data to printDat3
		printDat3 <- cbind(printDat3,tempDat[,paste(bas,c("","CI","p"," probability of decline"),sep="")])
		}
	# now deal with the estimated aggregate "mainTrend" # ONLY FOR BAYESIAN ANALYSIS
	if(length(grep("mainTrend",names(basinDat))) != 0){
		tempDat <- basinDat[,grep("mainTrend",names(basinDat))]
		trendNames <- paste("mainTrend",c("","lowCI","upCI"),sep="")

		# make trends a %change
		tempDat[,trendNames] <- apply(tempDat[,trendNames],2,FUN = function(x) as.numeric(ifelse(!is.na(x),exp(boot::logit(as.numeric(x)))-1,NA)))
		
		# make Credibility Intervals
		tempDat[,paste("mainTrend","CI",sep="")] <- paste("(",round(tempDat[,paste("mainTrend","lowCI",sep="")],4),",",round(tempDat[,paste("mainTrend","upCI",sep="")],4),")",sep="")
		
		names(tempDat)[match(paste("mainTrend",c("","p","pdecline"),sep=""),names(tempDat))] <- paste("mainTrend",c("","p"," probability of decline"),sep="")
		
		# attach basin data to printDat3
		printDat3 <- cbind(printDat3,tempDat[,paste("mainTrend",c("","CI","p"," probability of decline"),sep="")])
		}
	
	printDat3$error <- basinDat$error
	names(printDat3)[match("pBayesian",names(printDat3))] <- "model-fit"

	# order the species names
	sporder <- order(printDat3$species)
	printDat3 <- printDat3[sporder,]

	# write the table name
	write.table(c("Annual Presence Indices per Basin"),paste(prefix,"Overall Change in Species Presence per Basin",suffix,".csv",sep=" "),sep=",",row.names=F,col.names=F)
	
	# write the results
	write.table(printDat3,paste(prefix,"Overall Change in Species Presence per Basin",suffix,".csv",sep=" "),sep=",",row.names=F,append=T)
	
	# write concluding comments describing the results
	write.table(matrix(c(paste("trends were analyzed at the level of",rs2[[basinsIlog]]$random[1]),
		"Abundance index set to 1 for baseline year",
		"CI = 95% Bayesian Credibility Interval",
		"model-fit = 'Bayesian P value' from a posterior-predictive check. Values below 0.025 and above 0.975 are evidence of poor model fit",
		"'main trend' refers to the weighted average of all basin trend, estimated from the posterior distribution"),ncol=1),
		paste(prefix,"Overall Change in Species Presence per Basin",suffix,".csv",sep=" "),sep=",",row.names=F,col.names=F,append=T)
	
	}

#######################################################################################
# plotting function: requires outputs from models for linear trend, plus annual indices
#######################################################################################
########### SO FAR I'VE SUCCESSFULLY INCORPORATED SUBSET INTO EACH ANALYSIS
# I've also changed it so that the actual years remain as 1997 etc. in the annual index modell... which means
# I need to change the importation and recognition of PREDICTED trend values. 
# otherwise, need to match annual indices and overall trends with both species and year!!!


  library(Hmisc)
# create new directory to save plots
  dir.create(paste(getwd(),"/plot",suffix,sep=""))

   trenddat <- data.frame()	# main effects models
   indicedat <- data.frame()	# annual indices
 
# emoty data.frame for species names and model families for plotting
  specFam <- data.frame()

# check if Poisson year-as-main-effect models have been produced, and if so, add them to trenddat
  if(length(yearsI) > 0){ trenddat <- rbind(trenddat, rs3[[yearsI]])
  specFam <- rbind(specFam, trenddat[,c("species","family","subset")])
  	}
# check if Poisson annual indices models have been produced, and if so, add them to trenddat
  if(length(CyearsI) > 0){ indicedat <- rbind(indicedat, rs3[[CyearsI]])
   specFam <- rbind(specFam, indicedat[,c("species","family","subset")])
  	}
# check if Logistical year-as-main-effect models have been produced, and if so, add them to trenddat
  if(length(yearsIlog) > 0) { trenddat <- rbind(trenddat,rs3[[yearsIlog]])
      specFam <- rbind(specFam, trenddat[,c("species","family","subset")])
    	}
# check if Logistical annual indices models have been produced, and if so, add them to trenddat
  if(length(CyearsIlog) > 0) { indicedat <- rbind(indicedat, rs3[[CyearsIlog]])
       specFam <- rbind(specFam, indicedat[,c("species","family","subset")])
    	}

# collect unique species and model families and subsets
  specFam <- unique(specFam)

# cycle through species-family combos, and plot
  for(i in 1:nrow(specFam)){

	sp <- specFam$species[i]
	fam <- specFam$family[i]
	subsetter <- specFam$subset[i]

	ymax <- numeric(0)	# variable to collect extremes in values for defining the size of the plot frame
	ymin <- numeric(0)	# variable to collect extremes in values for defining the size of the plot frame
	xlabels <- numeric(0)	# variable to collect labels (years) of x axis

	plottext <- sp		# what to plot on screen
	
	# first try to collect data for the overall trend
	subdat <- subset(trenddat,species == sp & family == fam & subset== subsetter)
	if(nrow(subdat) != 0){
		
		# attempt to import the overall trend + credibility interval (saved as an individual file during modelling)
		CIfile <- NULL
		ymax <- 0; ymin <- 99	# dummy reset of variables
		#CIfileName <- paste(plottingFolder,"/PLOT",paste(sp," ~ year",ifelse(grep("nStations",subdat$model) == 1,' + log(nStations)',""),ifelse(length(grep("station",paste(deparse(subdat$random))))>0,"_~route + station",paste("_~",subdat$random,sep="")),"_",fam,"_",subsetter,sep=""),".csv",sep="") 		
		#try(CIfile <- read.csv(CIfileName), silent = TRUE)
		CIfileName <- paste(plottingFolder,"/PLOT",paste(sp," ~ ",subdat$model,ifelse(length(grep("station",paste(deparse(subdat$random))))>0,"_~route + station",paste("_~",subdat$random,sep="")),"_",fam,"_",subsetter,sep=""),".csv",sep="") 		
		try(CIfile <- read.csv(CIfileName), silent = TRUE)

		
		# confirm that file actually exists
		if(!is.null(CIfile)){
			
			# for x-axis on plot
			xlabels <- CIfile$year
			

			# collect extremes for plotting extent
			ymax <- max(c(CIfile$CIlow,CIfile$CIup))
			ymin <- min(c(CIfile$CIlow,CIfile$CIup))

			# collect statistics to plot as text
			plottext <- c(plottext,paste("\nTrend: ",round(subdat$year,4),"\np value: ",subdat$yearp,sep=""))
			}
		}

	# second attempt to collect the overall trend
	subdat2 <- subset(indicedat,species == sp & family == fam & subset == subsetter)
	if(nrow(subdat2) != 0){
		subdat2[1,is.na(subdat2[1,])]<-0

		# collect years lower CI values
		CIlowIndices <- which(1:ncol(subdat2) %in% grep("lowCI",names(subdat2)))
		CIlowNames <- names(subdat2)[CIlowIndices]

		# collect years upper CI values
		CIhiIndices <- which(1:ncol(subdat2) %in% grep("upCI",names(subdat2)))
		CIhiNames <- names(subdat2)[CIhiIndices]

		# collect estimated annual indice
		yearNames <- unique(as.numeric(sapply(CIlowNames,FUN = function(x) substr(x, 1,nchar(x) - nchar("lowCI")), USE.NAMES=FALSE)))
		xlabels <- unique(c(xlabels, yearNames))
		
		# collect extremes for plotting extent
		upIntervals <- c(ymax,as.numeric(subdat2[,c(CIlowNames, CIhiNames, as.character(yearNames))]))
		downIntervals <- c(ymin,as.numeric(subdat2[,c(CIlowNames, CIhiNames, as.character(yearNames))]))
		
		ymax <- max(upIntervals[!is.na(upIntervals) & !is.infinite(upIntervals)])
		ymin <- min(downIntervals[!is.na(upIntervals) & !is.infinite(upIntervals)])
		}
	
	# open connection with the printing device (here its a .png driver)
	# png makes a raster image .png
	# postscript makes a vector image .eps
	 png(file=paste("plot",suffix,"/",sp,"-",subsetter,"-",fam,".png",sep=""), bg="transparent", height = 600, width = 700)
	# postscript(file=paste("plot",suffix,"/",sp,"-",subsetter,"-",fam,".png",sep=""), bg="transparent",height = 600, width = 700)
	# win.metafile(file=paste("plot",suffix,"/",sp,"-",subsetter,"-",fam,".png",sep=""))
	# jpeg(file=paste("plot",suffix,"/",sp,"-",subsetter,"-",fam,".png",sep=""), bg="transparent", height = 600, width = 700)


	xlabels <- xlabels[order(xlabels)]

	par(mar = c(2, 4, 1, 1) + 0.1,
		cex.axis = 1,
		cex.lab = 1,
		#xaxp = c(min(xlabels), max(xlabels), n = 5),xlog =FALSE
		xaxs = "i", yaxs = "i", # removes extra whitespace on ends of axes
		lab = c(length(xlabels),5,length(xlabels)),
		bty = "n" # remove border around plot
		#las = 2  # changes the orientation of the axis text 2 = vertical
		)
	
	# create initial blank plotting space
	plot(xlabels,rep(0,length(xlabels)),
		ylab = ifelse(fam == "poisson","counts per station", "probability of being present"),
		xlab = "",
		ylim = c(0,ymax), cex=0.001,col = "white",
		#family = "sans"
		)

	# place descriptive text in the upper right corner
	text(xlabels[order(xlabels)][floor(length(xlabels)*0.1)], 
		ymax*0.90,paste(plottext,collapse=""),
		#family = "sans", 
		font = 2, pos = 4, cex = 1.05)
	
	if(nrow(subdat) != 0){
		
		# add main trend estimate
		lines(CIfile$year,CIfile$fit,lwd = 3, 
		# main trend line is red if significantly negative, and blue if significantly positive
		col = ifelse((subdat[,"yearp"] < 0.05 | subdat[,"yearp"] == "*") & as.numeric(subdat[,"year"]) <0, "red", ifelse((subdat[,"yearp"] < 0.05 | subdat[,"yearp"] == "*") & as.numeric(subdat[,"year"]) >0, "blue", "black")))
		
		# add 95% credibility intervals
		lines(CIfile$year,CIfile$CIup,lty = 2,lwd = 2, col = "gray45")
		lines(CIfile$year,CIfile$CIlow, lty =2, lwd = 2, col = "gray45")
			}

	if(nrow(subdat2) != 0){
		errbar(x = xlabels, y = as.numeric(subdat2[,as.character(yearNames)]), 
			yplus = as.numeric(subdat2[,as.character(CIhiNames)]), yminus = as.numeric(subdat2[,as.character(CIlowNames)]),
			add=TRUE,
			type = "b", # connect points with a line
			col = "grey55",
			lty=5)
			}
	dev.off()
	rm(plottext,CIfile, subdat, subdat2,xlabels)
		} # end cycle through species/models

#######################################
# END PLOTTING:
# ... plots are for diagnostic purposes only. Don't worry if there are lots of errors. 


#######################################################################################################
# Produce SQL Table for Nature Counts
#######################################################################################################
# RELOAD information

# This line just makes all the results NULL, to be filled with the actual results (if they exist)
  mainPois <- mainLogi <- IndiPois <- IndiLogi <- BasiPois <- BasiLogi <- NULL

# reload outputted results files
  try(mainPois <- read.csv(paste(prefix,"Overall Change in Species Counts",suffix,".csv",sep=" "), skip=1, stringsAsFactors=FALSE), silent=TRUE)
  try(mainLogi <- read.csv(paste(prefix,"Overall Change in Species Presence",suffix,".csv",sep=" "), skip=1, stringsAsFactors=FALSE), silent=TRUE)
  try(IndiPois <- read.csv(paste(prefix,"Annual Abundances Indices",suffix,".csv",sep=" "), skip=1,check.names=FALSE, stringsAsFactors=FALSE),silent=TRUE)
  try(IndiLogi <- read.csv(paste(prefix,"Annual Presence Indices",suffix,".csv",sep=" "), skip=1,check.names=FALSE, stringsAsFactors=FALSE),silent=TRUE)
  try(BasiPois <- read.csv(paste(prefix,"Overall Change in Species Abundance per Basin",suffix,".csv",sep=" "), skip=1, stringsAsFactors=FALSE), silent=TRUE)
  try(BasiLogi <- read.csv(paste(prefix,"Overall Change in Species Presence per Basin",suffix,".csv",sep=" "), skip=1, stringsAsFactors=FALSE), silent=TRUE)

# remove captions (describe various columns of the data set)
  try(mainPois <- mainPois[which(nchar(as.character(mainPois[,1])) < 10),], silent=TRUE)
  try(mainLogi <- mainLogi[which(nchar(as.character(mainLogi[,1])) < 10),], silent=TRUE)
  try(IndiPois <- IndiPois[which(nchar(as.character(IndiPois[,1])) < 10),], silent=TRUE)
  try(IndiLogi <- IndiLogi[which(nchar(as.character(IndiLogi[,1])) < 10),], silent=TRUE)
  try(BasiPois <- BasiPois[which(nchar(as.character(BasiPois[,1])) < 10),], silent=TRUE)
  try(BasiLogi <- BasiLogi[which(nchar(as.character(BasiLogi[,1])) < 10),], silent=TRUE)

# add family names to the results (to make one data.frame)
  if(!is.null(mainPois)){mainPois$model_type <- rep("poisson",nrow(mainPois))}
  if(!is.null(mainLogi)){mainLogi$model_type <- rep("logistic",nrow(mainLogi))}
  if(!is.null(IndiPois)){IndiPois$model_type <- rep("poisson",nrow(IndiPois))}
  if(!is.null(IndiLogi)){IndiLogi$model_type <- rep("logistic",nrow(IndiLogi))}
  if(!is.null(BasiPois)){BasiPois$model_type <- rep("poisson",nrow(BasiPois))}
  if(!is.null(BasiLogi)){BasiLogi$model_type <- rep("logistic",nrow(BasiLogi))}
  
# stack poisson and logisitic models ontop of each other to make one data.frame (where appropriate, add columns which are missing in one of the data.frames)
  if(!is.null(mainPois) & !is.null(mainLogi)) { mainMods <- rbind(mainPois, mainLogi)} else {mainMods <- eval(parse(text = paste(ifelse(!is.null(mainPois),"mainPois",ifelse(!is.null(mainLogi),"mainLogi","NULL")),sep=" ")))}
  if(any(is.null(IndiPois), is.null(IndiLogi))) { IndiMods <- eval(parse(text = paste(ifelse(!is.null(IndiPois),"IndiPois",ifelse(!is.null(IndiLogi),"IndiLogi","NULL")),sep=" "))) } else { IndiMods <- plyr::rbind.fill(IndiPois, IndiLogi) }
  if(any(is.null(BasiPois), is.null(BasiLogi))) { BasiMods <- eval(parse(text = paste(ifelse(!is.null(BasiPois),"BasiPois",ifelse(!is.null(BasiLogi),"BasiLogi","NULL")),sep=" "))) } else { BasiMods <- plyr::rbind.fill(BasiPois, BasiLogi) }
  
# if a basin*year interaction model was run, split up the file by basins and add them to the overall trend file (mainMods)
  if(any(!is.null(BasiPois), !is.null(BasiLogi))){
	# collect names of basins analyzed
	bn <- which(1:ncol(BasiMods) %in% grep("CI",names(BasiMods))) 
	basinNames <- substr(names(BasiMods)[bn],1,regexpr("CI",names(BasiMods)[bn])-1)
	basinNames <- basinNames[which(basinNames %in% c("mainTrend","basinOther")==FALSE)]

	# create fake data.frame to load basin data into (if mainMods weren't generated)
	if(is.null(mainMods)) { mainMods <- data.frame() }	
	
	# cycle through each Basin in BasiMods, extract relevant columns, and attach to bottom of mainMods
	for(i in basinNames){
		tempbasindat <- BasiMods[,c("species","subset","duration","model.fit","model_type","error",i,paste(i,c("CI","p",".probability.of.decline"),sep=""))]
		tempbasindat$subset <- rep(i,nrow(tempbasindat))
		names(tempbasindat) <- c("species","subset","duration","model.fit","model_type","error.comment","annual.trend","CI","p","probability.of.decline")
		mainMods <- rbind(mainMods, tempbasindat[ , ifelse(rep(all(names(tempbasindat) %in% names(mainMods)),ncol(tempbasindat)), names(mainMods), names(tempbasindat))])
	}	}


	
##############################################################################
# PROCESS TREND INFORMATION (REMOVE DUPLICATES)
##############################################################################

# choose between Logisitic, Poisson, and all-Basin estimate trend models (if they exist). The strategy is to choose the best fitting models
# Trend Models
# ... collect all unique species/subset assemblages
# ... collect all models of same species, subset, for poisson, logisitic, main overall trend (if they exist)
# ...	select one that is good fitting, if all other are poor fitting
# ...		then select one with tightest confidence intervals

# collect all unique combinations of species and regional subsets (for main trend models)
sp <- NULL
sp <- unique(eval(parse(text = 
	paste("rbind(",paste(c(
		ifelse(!is.null(BasiPois),"BasiPois[,c('species','subset')]", NA),	# check if per Basin Poisson models were run, add NA if not
		ifelse(!is.null(BasiLogi),"BasiLogi[,c('species','subset')]",NA),	# check if per Basin Logistic models were run, add NA if not
		ifelse(!is.null(mainMods),"mainMods[,c('species','subset')]",NA)),	# check if overall (main) trend models were run, add NA if not
	collapse=","),")",sep=""))))							# stack all the different models on top of each other, then take unique species/subsets

sp <- sp[complete.cases(sp),]	# remove NA

# WARNING: IF THE FOLLOWING CODE TO MAKE "trendMods" FAILS, YOU CAN SIMPLY
# USE THE mainMods DATAFRAME INSTEAD. HOWEVER, YOU'LL GENERATE MORE THAN ONE
# TREND PER SPECIES, AND WILL NEED TO EDIT OUT THE DUPLICATES MANUALLY.

# cycle through each species-subset combination for the (for main trend models)
trendMods <- data.frame()
if(!is.null(sp)){
for(i in 1:nrow(sp)){

	# collect all models 
	spDat <- data.frame(eval(parse(text = paste("rbind(",paste(c(
		ifelse(is.null(mainMods),NA, "as.matrix(subset(mainMods, species==sp$species[i] & subset==sp$subset[i])[,c('species','annual.trend','subset','duration','CI','p','model.fit','model_type')])"),					# collect overall models
		ifelse(is.null(BasiMods$mainTrend),NA, "as.matrix(subset(BasiMods, species==sp$species[i] & subset==sp$subset[i])[,c('species','mainTrend','subset','duration','mainTrendCI','mainTrendp','model.fit','model_type')])")),	# collect 'mainTrend' estimated from averaging basin models (if exists) (only for BAYESIAN!)
	collapse=","),")"))), stringsAsFactors = FALSE)	
	
	names(spDat) <- c('species','annual.trend','subset','duration','CI','p','model.fit','model_type')

	# remove model's with NA
	spDat <- subset(spDat, !is.na(spDat$model.fit) & !is.na(spDat$annual.trend))

	if(nrow(spDat)>0){
	# exclude models with evidence of poor fit # ONLY FOR BAYESIAN
	if(is.numeric(spDat$model.fit)){											
	if(any((spDat$model.fit > 0.975 | spDat$model.fit <0.025)) & all((spDat$model.fit > 0.975 | spDat$model.fit < 0.025))==FALSE) {
		# take well fitting models
		  spDat <- subset(spDat, (spDat$model.fit > 0.975 | spDat$model.fit < 0.025)==FALSE) }
		}

	# take model with the tighest confidence interval
	spDat <- subset(spDat, eval(parse(text = paste("c(",paste("sd(c",spDat$CI,")",sep="",collapse=","),") == min(",paste("sd(c",spDat$CI,")",sep="",collapse=","),")"))))
	
	# default to poisson model, if there are more than 1 models left
	if(any(spDat$model_type == "poisson")){ spDat <- subset(spDat, model_type == "poisson")[1,] } else { spDat <- spDat[1,]}
	
	trendMods <- rbind(trendMods, spDat)
	}}}

##############################################################################
# PROCESS ANNUAL INDICES INFORMATION (REMOVE DUPLICATES)
##############################################################################
# WARNING: IF THE FOLLOWING CODE TO MAKE "indexMods" FAILS, YOU CAN SIMPLY
# USE THE IndiMods DATAFRAME INSTEAD. HOWEVER, YOU'LL GENERATE MORE THAN ONE
# INDEX PER SPECIES, AND WILL NEED TO EDIT OUT THE DUPLICATES MANUALLY.

spDat <- sp <- NULL
if(!is.null(IndiMods)){
	
	# collect unique species/subset combos
	sp <- unique(IndiMods[,c('species','subset')])

	# cycle through unique combos of species/subsets
	indexMods <- data.frame()	# resulting model
	for(i in 1:nrow(sp)){

		# extract model information
		spDat <- subset(IndiMods, species==sp$species[i] & subset == sp$subset[i])
		
		# exclude models with evidence of poor fit # ONLY FOR BAYESIAN
  		if(is.numeric(spDat$model.fit)){											
		if(any((spDat$model.fit > 0.975 | spDat$model.fit <0.025)) & all((spDat$model.fit > 0.975 | spDat$model.fit < 0.025))==FALSE) {
			# take well fitting models
		  	spDat <- subset(spDat, (spDat$model.fit > 0.975 | spDat$model.fit < 0.025)==FALSE) }
			}

		# default to poisson model, if there are more than 1 models left
		if(any(spDat$model_type == "poisson")){ spDat <- subset(spDat, model_type == "poisson")[1,] } else { spDat <- spDat[1,]}
		
		indexMods <- rbind(indexMods,spDat)
		}
	}
		

##########################################################################################
# MAKE "TREND" SQL TABLE
##########################################################################################

# make columns names and columns data (for main trends) compatable with Nature Counts SQL files
results_trends <- data.frame(
	results_code = 	rep("MMP", nrow(trendMods)),
	area_code = 	ifelse( trendMods$subset == "GreatLakes1",	"GREATLAKES",
			ifelse( trendMods$subset == "provstatON", 	"PROV-ON",
			ifelse( trendMods$subset == "provstatPQ", 	"PROV-PQ",
			ifelse( trendMods$subset == "basinH", 		"HURON",
			ifelse( trendMods$subset == "basinO", 		"ONTARIO",
			ifelse( trendMods$subset == "basinM", 		"MICHIGAN",
			ifelse( trendMods$subset == "basinE", 		"ERIE",
			ifelse( trendMods$subset == "basinS", 		"SUPERIOR",
			ifelse( trendMods$subset == "provstatONANDbcr13","BCR13-ON",
			ifelse( trendMods$subset == "provstatONANDbcr12", "BCR12-ON",
			ifelse( trendMods$subset == "bcr13", 		"BCR13",
			ifelse( trendMods$subset == "bcr12", 		"BCR12",
			ifelse( trendMods$subset == "bcr23", 		"BCR23", trendMods$subset))))))))))))),
	species_id =	bscdata.getSpeciesID(datasource="bmdedata", "MMP", as.character(trendMods$species), aggregate=TRUE)$species_id,
	season = 	rep("breeding", nrow(trendMods)),
	period = 	paste(abs(eval(parse(text = paste("c(",paste(trendMods$duration,collapse=","),")")))),"-years",sep=""),
	years = 	trendMods$duration,	
	trnd = 		ifelse(trendMods$model_type == "logistic", log(as.numeric(trendMods$annual.trend) + 1), 			# transform back to logit scale
			ifelse(trendMods$model_type == "poisson", log(as.numeric(trendMods$annual.trend) + 1), trendMods$annual.trend)),# transform back to log scale	
	trnd_order =	rep(1, nrow(trendMods)),	
	dq = 		rep("", nrow(trendMods)),	
	model_type = 	trendMods$model_type)

# add the p-value (different fields, depending on whether its a string or a numeric value)	
if(is.numeric(trendMods$p)) { results_trends$p <- trendMods$pval }
if(is.character(trendMods$p)) { results_trends$p <- trendMods$pval_str }


# Send Trend results to the BSCdata SQL server
SQLfileName <- paste(prefix,"_MMP_trends.sql",sep="")
write.csv(results_trends,paste(SQLfileName,".csv",sep=""),row.names = F)

# WARNING: UNTIL THE FOLLOWING COMMAND IS TESTED AND VERIFIED BY DENIS LEPAGE, THE 
# ABOVE .CSV FILE SHOULD BE SENT TO DENIS TO LOAD INTO THE SQL SERVER
# bscdata.sqlSave(("results_trends", results_annual_indices, textfile=SQLfileName, append=T)

##########################################################################################
# MAKE "ANNUAL INDICES" SQL TABLE
##########################################################################################

# the input data frame (indexMods) is on the count/presense scale, we need to convert it the model scale for SQL
# also, the input frame (indexMods) is in "wide" format (column per year), we need to convert it to "long" format

# extract time of models
  newestYear <- format(Sys.time(), "%Y")
  # newestYear <- 2011
  allyears <- 1995:newestYear
  #allyears <- 1995:2011

# names of columns corresponding to years
  allyears <- as.character(names(indexMods)[names(indexMods) %in% as.character(allyears)])
  allyearsCI <- paste(allyears,"CI",sep="")	# name of confidence intervals
  otherColumns <- names(indexMods)[names(indexMods) %in% c(allyears, allyearsCI) == FALSE]

# iterate through years and stake them on top of each other
  indexMods2 <- data.frame()
  for(i in 1:length(allyears)){
	# extract data for year i
	currDat <- cbind(indexMods[,otherColumns], data.frame(index = indexMods[,allyears[i]], indexCI = indexMods[,allyearsCI[i]]))		
	indexMods2 <- rbind(indexMods2, currDat)	# stack onto
	} # THE result is one column for the indices, with a row per species/year/subset (i.e., "long" format)
	
# transform the Confidence interval column from a "(xxx, xxx)" to two seperate columns
  indexMods2$CIlow <- sapply(indexMods2$indexCI, FUN = function(x) eval(parse(text = paste("min",x,sep=""))))
  indexMods2$CIhi  <- sapply(indexMods2$indexCI, FUN = function(x) eval(parse(text = paste("max",x,sep=""))))

# remove NA's
  indexMods2 <- indexMods2[!is.na(indexMods2$index),]
  results_annual_indices <- data.frame(
	results_code = 	rep("MMP", nrow(indexMods2)),
	area_code = 	ifelse( indexMods2$subset == "GreatLakes1",	"GREATLAKES",
			ifelse( indexMods2$subset == "provstatON", 	"PROV-ON",
			ifelse( indexMods2$subset == "provstatPQ", 	"PROV-PQ",
			ifelse( indexMods2$subset == "basinH", 		"HURON",
			ifelse( indexMods2$subset == "basinO", 		"ONTARIO",
			ifelse( indexMods2$subset == "basinM", 		"MICHIGAN",
			ifelse( indexMods2$subset == "basinE", 		"ERIE",
			ifelse( indexMods2$subset == "basinS", 		"SUPERIOR",
			ifelse( indexMods2$subset == "provstatONANDbcr13","BCR13-ON",
			ifelse( indexMods2$subset == "provstatONANDbcr12", "BCR12-ON",
			ifelse( indexMods2$subset == "bcr13", 		"BCR13",
			ifelse( indexMods2$subset == "bcr12", 		"BCR12",
			ifelse( indexMods2$subset == "bcr23", 		"BCR23", indexMods2$subset))))))))))))),
	species_id =	bscdata.getSpeciesID(datasource="bmdedata", "MMP", indexMods2$species, aggregate=TRUE)$species_id,
	season = 	rep("breeding", nrow(indexMods2)),
	period = 	paste(abs(eval(parse(text = paste("c(",paste(indexMods2$duration,collapse=","),")")))),"-years",sep=""),
	years = 	indexMods2$duration,	
	index =		ifelse(indexMods2$model_type == "logistic", boot::logit(indexMods2$index), 		# transform back to logit scale
			ifelse(indexMods2$model_type == "poisson", log(indexMods2$index), indexMods2$index)),	# transform back to log scale		
	lower_ci =	ifelse(indexMods2$model_type == "logistic", boot::logit(indexMods2$CIlow), 		# transform back to logit scale
			ifelse(indexMods2$model_type == "poisson", log(indexMods2$CIlow), indexMods2$CIlow)),	# transform back to log scale		
	upper_ci =		ifelse(indexMods2$model_type == "logistic", boot::logit(indexMods2$CIhi), 		# transform back to logit scale
			ifelse(indexMods2$model_type == "poisson", log(indexMods2$CIhi), indexMods2$CIhi)),	# transform back to log scale		
	model_type = 	indexMods2$model_type)
	
# Deal with infinities
# because we transform back to the link-scale, some of the large numbers become infinities
# Here, we replace them with minimum values and maximum values
	
negInf <- which(is.infinite(results_annual_indices$index) & results_annual_indices$index < 0)
posInf <- which(is.infinite(results_annual_indices$index) & results_annual_indices$index > 0)
results_annual_indices$index[negInf] <- min(results_annual_indices$index[!is.infinite(results_annual_indices$index)])
results_annual_indices$index[posInf] <- max(results_annual_indices$index[!is.infinite(results_annual_indices$index)])

posInf <- which(is.infinite(results_annual_indices$lower_ci) & results_annual_indices$lower_ci > 0)
negInf <- which(is.infinite(results_annual_indices$lower_ci) & results_annual_indices$lower_ci < 0)
results_annual_indices$lower_ci[negInf] <- min(results_annual_indices$lower_ci[!is.infinite(results_annual_indices$lower_ci)])
results_annual_indices$lower_ci[posInf] <- max(results_annual_indices$lower_ci[!is.infinite(results_annual_indices$lower_ci)])

posInf <- which(is.infinite(results_annual_indices$upper_ci) & results_annual_indices$upper_ci > 0)
negInf <- which(is.infinite(results_annual_indices$upper_ci) & results_annual_indices$upper_ci < 0)
results_annual_indices$upper_ci[posInf] <- max(results_annual_indices$upper_ci[!is.infinite(results_annual_indices$upper_ci)])
results_annual_indices$upper_ci[negInf] <- min(results_annual_indices$upper_ci[!is.infinite(results_annual_indices$upper_ci)])


# Send Trend results to the BSCdata SQL server
SQLfileName <- paste(prefix,"_MMP_indices.sql",sep="")
write.csv(results_annual_indices,paste(SQLfileName,".csv",sep=""),row.names = F)

# WARNING: UNTIL THE FOLLOWING COMMAND IS TESTED AND VERIFIED BY DENIS LEPAGE, THE 
# ABOVE .CSV FILE SHOULD BE SENT TO DENIS TO LOAD INTO THE SQL SERVER
#bscdata.sqlSave("results_annual_indices", results_annual_indices, textfile=SQLfileName, append=T)




######################################################################################3
# END
######################################################################################




#############################################################################################3
# AUTHOR: ROBERT WILLIAM RANKIN robertw.rankin@gmail.com April 2011
#
# This script is a complimentary Generalized Linear Mixed Model (GLMM) version of the Bayesian 
# MMP Trend Analysis. Ideally, the bayesian analysis should be run instead of this one. But, given
# computational and time limitations, this is a quicker alternative. Like the Bayesian Trend
# analysis, it includes a mixed model, produces plots. However, it doesn't model overdispersion
# and merely inflates (all) standard errors by the estimated scale parameter. Therefore, this 
# script probably has weaker Power than the Bayesian analysis (less able to detect significant
# trends). Also, the Logisitic regression may be prone to singularities in GLMM.
#
# If you are used to the Bayesian script, there are some subtle differences to the GLMM analysis
# such as: logistic regression uses the "binomial family" instead of "categorical"; offsetting
# of the # of stations per route is done with the "offset" command in the model formula, rather as
# a "fixed prior" in the Bayesian analysis.

# .~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`
# 		Latest Author Robert William Rankin (robertw.rankin@gmail.com)
# .~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`


setwd("T:/MARSH/Trend Analyses")
rm(list=ls(all=TRUE))
memory.limit(4095)
library(sqldf)
library(lme4)

############################################################
# Set up data
############################################################
# filename for the exported results
  fileName <- "Trend Birds Analysis - GLMM"	# NAME OF OUTPUT FILE (.csv)  			(use this file for further summaries)
  tempfolder <- "temp_R_files_TRENDS - GLMM"	# NAME OF FOLDER TO SCORE TEMPORARY FILES 	(needed for further summaries, e.g., plotting)

# import data, Maximum counts for species (by column) summarized to the station level 
  d <- read.csv("Birdstns_noobserve.csv",header=TRUE)
  d$basin <- toupper(d$basin)
  d$station <- toupper(d$station)

# create new field to potentially restrict analyses only to Great Lakes (all basins) plus st.lawrence sites (not in Quebec)
  d$GreatLakes <- ifelse(d$basin %in% c("H","O","M","E","S","L") & regexpr("QC", d$route)<0,1,0)

  globalBasins <- c("H","O","M","E","S","L")

# restrict trend analysis of individual basins.  The following are used subsequently.
  indieBasins <- c("H","O","M","E","S")

# restrict trend analysis to stations with at least 'minVisits'
  minVisits <- 2

# restrict individual species trend analysis to at least 'minObs' number of detections
  minObs <- 20

# remove routes visited less than minObs above
  routesummary <- sqldf("select route,year from d group by route,year")
  routesummary <- sqldf(paste("select route,year, count(route) as 'tally' from routesummary group by route having tally >=",minVisits,sep=""))
  routeDoneOnce <- unique(d$route)[which(unique(d$route) %in% routesummary$route == FALSE)]
  d <- subset(d,route %in% routeDoneOnce == FALSE) 
  
  #d2 <- d[which(!is.na(d$basin) & d$basin %in% c(globalBasins)),]	# remove NA's and irrelevant basins
  d2 <- d
  d2$basin <- factor(d2$basin)						# ensure basin is a factor
  d2$station <- factor(paste(d2$route,d2$station,sep=""))		# ensure station is a factor
  
# function to transform count data into presense/absense data
  presonly <- function(data,x) cbind(data[,which(names(data) %in% x == FALSE)],apply(as.matrix(data[,x],ncol=length(x),dimnames=list(row.names(data),x)),2,FUN = function(q) ifelse(q>0,1,0)))

# Make your OWN species list
  allspecies <- c('ABDU','ACFL','ALFL','AMBI','AMCO','AMCR','AMGO','AMKE','AMRE','AMRO','AMWI','AMWO','ATSP','AWPE','BAEA','BAGO','BANS','BAOR','BARS','BAWW','BBCU','BBPL','BBWA','BBWO','BCCH','BCNH','BCVI','BDOW','BEKI','BGGN','BHCO','BHVI','BLBW','BLJA','BLKI','BLPW','BLRA','BLTE','BOBO','BOCH','BOGU','BOWA','BRBL','BRCR','BRTH','BTBW','BTNW','BUFF','BWHA','BWTE','BWWA','CAGO','CANV','CARW','CATE','CAWA','CCSP','CEDW','CERW','CHSP','CHSW','CLSW','CMWA','COEI','COGO','COGR','COHA','COLO','COME','COMO','CONI','CORA','COTE','COYE','CSWA','DCCO','DEJU','DOWO','DUNL','EABL','EAKI','EAME','EAPH','EATO','EAWP','EUDO','EUST','EUWI','EVGR','FISP','FOSP','FOTE','GADW','GBBG','GBHE','GCFL','GCKI','GGOW','GHOW','GLGU','GLIB','GOEA','GRAJ','GRCA','GREG','GRHE','GRSC','GRSP','GRYE','GWTE','HAWO','HERG','HETH','HOFI','HOGR','HOLA','HOME','HOSP','HOWR','INBU','KILL','KIRA','LAGU','LASP','LBHE','LCSP','LEBI','LEFL','LEOW','LESA','LESC','LETE','LEYE','LIGU','LISP','LOWA','LTDU','MALL','MAWA','MAWR',
'MERL','MODO','MODU','MOOT','MOWA','MUSW','NAWA','NHOW','NOBO','NOCA','NOCR','NOFL','NOGO','NOHA','NOMO','NOPA','NOPI','NOWA','NRWS','NSHO','NSTS','NSWO','OROR','OSFL','OSPR','OVEN','PAWA','PBGR','PEFA','PHVI','PIGR','PISI','PIWA','PIWO','PRAW','PROW','PUFI','PUMA','RBGR','RBGU','RBME','RBNU','RBWO','RCKI','REDH','REVI','RHWO','RLHA','RNDU','RNGR','RNPH','ROPI','RSHA','RTHA','RTHU','RUBL','RUDU','RUGR','RUTU','RWBL','SACR','SAND','SAVS','SBDO','SCTA','SEOW','SEPL','SESA','SEWR','SNEG','SNGO','SORA','SOSA','SOSP','SPSA','SSHA','SWSP','SWTH','TEWA','TRES','TRUS','TUSW','TUTI','TUVU','UPSA','VEER','VESP','VIRA','WAVI','WBNU','WCSP','WEVI','WHIM','WIFL','WIPH','WISN','WITU','WIWA','WIWR','WODU','WOTH','WPWI','WTSP','WWCR','WWSC','YBCH','YBCU','YBFL','YBSA','YCNH','YERA','YHBL','YRWA','YTVI','YTWA','YWAR') 
  allspecies <- allspecies[apply(presonly(d2,allspecies)[,allspecies],2,FUN = function(x) sum(x) >= minObs)]	# restricts to species with > minObs
  allspecies <- call("paste", allspecies)	# crucial step to make the list callable

# list of species with a "binary distribution" i.e., presense absense for logisitic regression
  splist2 <- names(which(apply(d2[,eval(allspecies)],2,FUN = function(x) sum(x > 1)/sum(x >0) <= 0.1)))
  splist2 <- call("paste", splist2)

#######################################################################################################################
# SET UP MODELS (FORMULAS) TO RUN
#######################################################################################################################
# SPECIFY THE TYPES OF ANALYSES YOU WANT TO RUN
# Key things to consider:
# 1) run analysis: TRUE / FALSE to toggle whether that type of analysis should indeed be run
# 2) formula: only supports the following formulas			
#		~ year (+/- offset)  		for overall main TREND	# NOTICE THAT THE lme4 formula handles offsets differently than bayesian
#		~ as.factor(year) (+/- offset)	for annual INDICES	# NOTICE THAT THE lme4 formula handles offsets differently than bayesian
#		~ year*basin	  (+/- offset)	for basin-level TRENDS interactions (preferable to running each basin on its own)
# 3) family: only supports counts ("poisson") and logistic ("binomial")
# 4) random: specify what groups have a random intercept
#	recommend: 	"~ route" with an offset in formula for counts, or 
#			"~ route + route:station" for logisitic
# 5) subset: specify an SQL statement which restricts the analysis to a certain group (e.g., Province, State, BCR)
#		- you must then ensure that this column is available in the imported data
#		e.g. could be "provstat = 'ON'", "BCR = '13'"
#		be mindful to put the subset name (e.g., 'ON') in single-tick quotation marks (e.g., 'ON')
# 6) restrict: specify a list of species to restrict analysis to  
#		"allspecies" use all species (which meet criteria of > minObs)
#		"splist2" refers to species with 90% of observations as 1's (suitable for logisitic)
#		make your own list!!!

fl<- c(
#Run 	#formula					#family		 #random		 	#subset			#restrict	
TRUE,	"~ year + offset(log(nStations))",		"poisson", 	"~route",			"GreatLakes='1'",	allspecies 
,TRUE,	"~ as.factor(year)  + offset(log(nStations)) -1","poisson", 	"~route",			"GreatLakes='1'",	allspecies 	
,TRUE,	"~ year*basin + offset(log(nStations))-1 -year","poisson", 	"~route",			"GreatLakes='1'",	allspecies 
,TRUE, "~ year",					"binomial", 	"~route",			"GreatLakes='1'",	splist2 
,TRUE, "~ as.factor(year) -1",				"binomial", 	"~route",			"GreatLakes='1'",	splist2 	
,TRUE, "~ year*basin -1 -year",				"binomial", 	"~route",			"GreatLakes='1'",	splist2 
,TRUE,	"~ as.factor(year)  + offset(log(nStations)) -1","poisson", 	"~route",			"basin='H'",		allspecies 	
,TRUE,	"~ as.factor(year) -1",				"binomial", 	"~route",			"basin='H'",		splist2 	
,TRUE,	"~ as.factor(year)  + offset(log(nStations)) -1","poisson", 	"~route",			"basin='O'",		allspecies 	
,TRUE,	"~ as.factor(year) -1",				"binomial", 	"~route",			"basin='O'",		splist2 	
,TRUE,	"~ as.factor(year)  + offset(log(nStations)) -1","poisson", 	"~route",			"basin='M'",		allspecies 	
,TRUE,	"~ as.factor(year) -1",				"binomial", 	"~route",			"basin='M'",		splist2 	
,TRUE,	"~ as.factor(year)  + offset(log(nStations)) -1","poisson", 	"~route",			"basin='E'",		allspecies 	
,TRUE,	"~ as.factor(year) -1",				"binomial", 	"~route",			"basin='E'",		splist2 	
,TRUE,	"~ as.factor(year)  + offset(log(nStations)) -1","poisson", 	"~route",			"basin='S'",		allspecies 	
,TRUE,	"~ as.factor(year) -1",				"binomial", 	"~route",			"basin='S'",		splist2 	
,TRUE,	"~ year + offset(log(nStations))",		"poisson", 	"~route",			"provstat = 'ON'",	allspecies 
,TRUE,	"~ as.factor(year)  + offset(log(nStations)) -1","poisson", 	"~route",			"provstat = 'ON'",	allspecies 	
,TRUE,	"~ year",					"binomial", 	"~route",			"provstat = 'ON'",	splist2 
,TRUE,	"~ as.factor(year) -1",				"binomial", 	"~route",			"provstat = 'ON'",	splist2 	
,TRUE,	"~ year + offset(log(nStations))",		"poisson", 	"~route",			"provstat='ON' AND bcr='13'",	allspecies 
,TRUE,	"~ as.factor(year)  + offset(log(nStations)) -1","poisson", 	"~route",			"provstat='ON' AND bcr='13'",	allspecies 	
,TRUE,	"~ year",					"binomial", 	"~route",			"provstat='ON' AND bcr='13'",	splist2 
,TRUE,	"~ as.factor(year) -1",				"binomial", 	"~route",			"provstat='ON' AND bcr='13'",	splist2 	
,TRUE,	"~ year + offset(log(nStations))",		"poisson", 	"~route",			"bcr='12'",		allspecies 
,TRUE,	"~ as.factor(year)  + offset(log(nStations)) -1","poisson", 	"~route",			"bcr='12'",		allspecies 	
,TRUE,  "~ year",					"binomial", 	"~route",			"bcr='12'",		splist2 
,TRUE,	"~ as.factor(year) -1",				"binomial", 	"~route",			"bcr='12'",		splist2 	
,TRUE,	"~ year + offset(log(nStations))",		"poisson", 	"~route",			"bcr='23'",		allspecies 
,TRUE,	"~ as.factor(year)  + offset(log(nStations)) -1","poisson", 	"~route",			"bcr='23'",		allspecies 	
,TRUE,	"~ year",					"binomial", 	"~route",			"bcr='23'",		splist2 
,TRUE,	"~ as.factor(year) -1",				"binomial", 	"~route",			"bcr='23'",		splist2 	
)


# convert the model specifications into a list of formulas 		
formulalist <- list()

# cycle through each model specification in fl, set up formulas for each species (if it has enough observations)
# NOTE: the above table looks like table, but in of course is one long vector. The following code translates this 
# ... user-friendly input into something R can use. For example, each of the model specifications needs to be expanded
# ... for each species in the species-list
for(i in 0:((length(fl)-1)/6)){		# skip by 6, because the above model specification "table" has 6 "columns",
					# (but it is actually a long vector, not a table. So, every 6th value is a new "row"
	mod <- fl[(6*i+1):(i*6+6)]

	# check if the current model "i" has run set as TRUE or FALSE. Continue if TRUE
	if(as.logical(mod[[1]])){
	
		# collect all the species to be analyzed by the current model specification
		specieslist <- eval(mod[[6]])

		# cycle though each species, and set up the model formulas
		for(sp in specieslist){
		
			# check if there are enough species observations for each subset
			if(mod[[5]] != ""){
				tempdat <- sqldf(paste("select ",sp," from d2 where ",mod[[5]]))
				if(sum((tempdat[,1]>0)*1) >= minObs){
					formulalist[[length(formulalist)+1]] <- c(paste(sp,mod[[2]]),mod[[4]],mod[[3]],sp, mod[[5]])
					}
			# if there is no subset, include all species in specieslist as model
			} else {
				formulalist[[length(formulalist)+1]] <- c(paste(sp,mod[[2]]),mod[[4]],mod[[3]],sp, mod[[5]])
	}	}	}	}

# save(formulalist, file = "temp trend analysis formulalist.RData") # code if you want to save and reload the formulalist
  
# create new temporary directory to store models and data required for plotting
if(file.exists(tempfolder)==FALSE) dir.create(tempfolder)


#######################################################################################################################
# CHECK WHICH MODELS ARE ALREADY RUN
#######################################################################################################################
# create new temporary directory to store models and data required for plotting
if(file.exists(paste(fileName,".csv",sep=""))) {
	IMPORTT <- "YES"
	IMPORTT <- toupper(winDialog("yesno",paste("The file",paste(fileName,".csv",sep=""),"already exisits. Do you want to exclude models already completed and stored in the file?(Yes). Click 'No' to run all models again")))

	if(IMPORTT == "YES"){
	
		# user claimed to want to check which models were already run, so import results and remove those completed models from formulalist
		ans1 <- read.csv(paste(fileName,".csv",sep=""),header=FALSE, stringsAsFactors=FALSE)
		formulalist2 <- formulalist
		formulalist2 <- lapply(formulalist2, FUN = function(x) {x[5] <- gsub(" ","", gsub("=","",gsub("'","",x[5]))); x}) 	# standardize formulalist and results
		formulalist2 <- lapply(formulalist2, FUN = function(x) {x[1] <- substr(x[1],regexpr("~ ",x[1])+2, nchar(x[1])); x}) 	# standardize formulalist and results
		formulalist2 <- lapply(formulalist2, FUN = function(x) {x[1] <- gsub(" ","", gsub(" ","",x[1]));; x}) 			# standardize formulalist and results
		formulalist2 <- lapply(formulalist2, FUN = function(x) {x[2] <- substr(x[2],regexpr("~",x[2])+1, nchar(x[2])); x}) 	# standardize formulalist and results
		names(ans1)[1:8] <- as.character(subset(ans1, V1 =="species")[1,1:8])
		ans1 <- subset(ans1, species != "species")
		ans1$model <- gsub(" ","", gsub(" ","",ans1$model))
		newformulalist <- list()
		for(i in 1:length(formulalist2)){
			# check if the formula in formulalist2 has been run in the results
			modelrun <- any(sapply(FUN = function(y) {yy <- ans1[y,];x <- formulalist2[[i]]; all(x[1] == yy$model & x[2]==yy$random & x[3] == yy$family & x[4] == yy$species & x[5] == yy$subset)}, 1:nrow(ans1)))
			if(modelrun==FALSE) {newformulalist[[length(newformulalist)+1]] <- formulalist[[i]]}	#add to new formulalist if it hasn't been run 
			}
		formulalist <- newformulalist
		}
	}

#######################################################################################################################
# TREND ANALYSIS: DEFINE FUNCTION TO CONDUCT ANALYSIS (globfunc)
#######################################################################################################################

globfunc <- function(iii) {

		# function for presense / absense quick summaries
		presonly <- function(data,x) cbind(data[,which(names(data) %in% x == FALSE)],apply(as.matrix(data[,x],ncol=length(x),dimnames=list(row.names(data),x)),2,FUN = function(q) ifelse(q>0,1,0)))
		
		# a is the formulalist which has all the model specifications
		a <- formulalist[[iii]]
		sp <- a[4]			# species of focus
		randeff <- as.formula(a[2])	# formula for random effect
		fam <- a[3]			# family (poisson, or logisitic = binomial)
		subsetSQL <- a[5]		# SQL statment to limit the analysis (e.g., limit to BCR 12, or Ontario, etc.)
		subsetter <- gsub(" ","", gsub("=","",gsub("'","",subsetSQL)))	# to add as a file extension
		
		# make a proper formula for lme4
		# NOTE, unlike in MCMCglmm, where there is a slot for the random effect formula, in lme4, we have to add
		# the random effect formula to the main formula, like + (1|route) 
		baseform <- a[1]		# fixed effects model formula
		form <- formula(paste(baseform,"+ (1|",paste(randeff)[2],")"))
		
		# restrict data to specified SQL subset
		if(nchar(subsetter)>1){ dyndat <- sqldf(paste("select * from d2 where",subsetSQL))
			dyndat <- dyndat[,c("year","route","station","basin",sp)]	# focus species table to reduce strain on memory
		} else { dyndat <- d2[,c("year","route","station","basin",sp)] }
		
		# note the survey duration e.g., (199X - 20XX)		
		survPeriod <- paste(min(dyndat$year),"-",max(dyndat$year),sep="")

		# rescale year as a continuous variable (but leave alone if the model is for annual indices)
		if("as.factor(year)" %in% attr(terms(form), "term.labels")==FALSE){
			dyndat$year <- dyndat$year-ceiling(mean(unique(d2$year)))
			}		

		# if the family is logisitic/binomial, transform all the data to 0s and 1s
		if(fam == "binomial") { dyndat[,sp]<-as.numeric(ifelse(dyndat[,sp]>0,1,0)) }
		
		# two step SQL data reduction: 
		# 1) remove routes only surveyed once, 
		# 2) remove routes which NEVER had a single species occurence (e.g., no trend to estimate)
		keeproutes <- sqldf(paste("select route, count(year) as 'nYears', sum(totobs) as 'totcounts' from (select route, year, sum(",sp,") as 'totobs' from dyndat group by year,route) group by route having nYears > 1 AND totcounts > 0"))$route
		dyndat <- subset(dyndat, route %in% keeproutes)	
		
		m <- NULL		# dummy variable for the model itself
		
		# specify required inverse link-function "tx()" for resulting effect estimations, 
		# as well as criteria "CritDecl" to judge if there is a decline
		if(fam == "poisson"){ tx <- function(x) exp(x)
				      CritDecl <- 1
				      rfunc <- function(n,l) rpois(n=n,lambda=l) 	
				      pearsonR <- function(true,fitte) (true - fitte)/sqrt(fitte)	 
		} else {if(fam == "binomial"){ 
			tx <- function(x) boot::inv.logit(x)
			CritDecl <- 0.5
			rfunc <- function(n,p) round((p-runif(n=n,min=0,max=1))+0.5)
			pearsonR <- function(true,fitte) (true - fitte)/sqrt(fitte*(1-fitte))
			} else { tx <- function(x) x 
				CritDecl <- 0
				}}
	
		# preprocessing Basin information (if its an effect) to only use Basins for which there is > minObs observations
		dyndat <- subset(dyndat,basin %in% globalBasins)	# restrict Main trend to these basins specified in globalBasins
		basinTally <- sqldf("select basin, count(route) as basinTally from dyndat group by basin having basinTally > 10")
		if(nrow(basinTally)!=0){
			okBasins <- sqldf(paste("select basin, count(station) as 'basinTally' from (select basin,route,year,station from dyndat where",sp,"> 0) group by basin having basinTally >",minObs))$basin
		} else {okBasins <- c("none")}
		dyndat$basin <- as.character(dyndat$basin)
		dyndat$basin <-ifelse(dyndat$basin %in% okBasins & dyndat$basin %in% indieBasins,dyndat$basin,"Other")
		
		# the following basins should have sufficient data to report trends on (union with IndieBasins to clear what basins should be reported individually)
		okBasins <- sqldf(paste("select basin, count(station) as 'basinTally' from (select basin,route,year,station from dyndat where",sp,"> 0) group by basin having basinTally >",minObs))$basin	
		if(length(unique(dyndat$basin))==1 & length(grep("basin",deparse(form)))>0){
			resnames <- c("species","model","family","random","subset","duration","error")
			res <- c(sp,paste(formula(baseform))[3]		,fam,as.character(randeff)[2],subsetter,survPeriod,"too_few_obs_per_basin")
			listres <- matrix(res,ncol = length(res))
			colnames(listres) <- resnames
			write(colnames(listres),paste(fileName,".csv",sep=""),append=TRUE,sep=",",ncol = length(listres))
			write(listres,paste(fileName,".csv",sep=""),append=TRUE,sep=",", ncol = length(listres))
			return(listres)
		} else {	
		
		# check if there is an offset for the number of stations ("nStations") in the formula
		# if so, then the model needs to summarize all data to the route level
		if(a[2] == "~route" & length(grep("nStations",deparse(form)))>0){
			# need to transform data to summarize at the route level, plus need an offset (number of stations per route)
			dyndat <- sqldf(paste("select route,year,count(station) as 'nStations', sum(",sp,") as'",sp,"',basin from dyndat group by route,year",sep=""))			
			}

		model.terms <- colnames(model.matrix(formula(baseform),dyndat))	# find the model terms	
		
		# run model
		m <- glmer(form, data = dyndat, family = fam)
		if(!is.null(m)) {
		
		# save model for reloading purposes (i.e., it can be viewed later)
		#m$Xform <- formulalist[[iii]]; m$Xsp <- a[4]; m$Xrandeff <- a[2]; m$XsubsetSQL <- subsetSQL; m$Xsubsetter <- subsetter; m$Xfam <- fam
		#modName <- paste(tempfolder,"/MODEL",paste(deparse(form),ifelse(length(grep("station",paste(deparse(randeff))))>0,"~route + station",paste(deparse(randeff))),fam,subsetter,sep="_"),".RData",sep=""); modName <- gsub("*","xXx",modName, fixed=TRUE)
		#save(m, file = modName)
		
		# find the names of trend parameters: with a year (trend) as an interaction or main effect (BUT we don't want to return "as.factor(year)" because that is an annual indice NOT a trend
		trendparam <- row.names(summary(m)@coefs)[which(1:nrow(summary(m)@coefs) %in% grep("year",row.names(summary(m)@coefs)) & apply(as.matrix(m@X),2, FUN = function(x) any(x != 1 & x != 0)))]	
		
		# collect results: plus do some processing
		#	- inflate s.e., estimates (if there is evidence of overdispersion)
		#	- make 95% confidence intervals (inflated, if necessary)
		inflate <- ifelse(m@deviance["sigmaML"] >= 1, m@deviance["sigmaML"], 1)		# find dispersion parameter
		effectsMatrix <- data.frame(
			estimate = summary(m)@coefs[,"Estimate"],
			lowCI = summary(m)@coefs[,"Estimate"] - summary(m)@coefs[,"Std. Error"]*inflate*1.96,		# lower confidence interval (inflated if necessary)
			upCI = summary(m)@coefs[,"Estimate"] + summary(m)@coefs[,"Std. Error"]*inflate*1.96
			)
		
		# Remake significance test
		# ... because we artificially inflate Confidence intervals (by dispersion parameter), we can't trust the parameter p-values
		# ... here, we designate a parameter as significant if its 95% confidence intervals (inflated) do NOT bound 0
		# ... We can tell if they overlap 0: (-) * (+) = (-), while (-)*(-) = (+) and (+) * (+) = (+)
		effectsMatrix$p <- ifelse(effectsMatrix$lowCI * effectsMatrix$upCI < 0, "NS", "*")
		
		# transform estimates onto count/presense scale (in GLMM, they are modelled on the link scale)
		effectsMatrix[,c("estimate","lowCI","upCI")] <- apply(effectsMatrix[,c("estimate","lowCI","upCI")],2,FUN = function(x) tx(x))
			
		# In the Bayesian analysis, we estimate a Bayesian probability of a decline
	 	# ... we can't do that here for GLMM, so we make a dummy column with NA's just to keep things consistent
		if(length(trendparam) != 0){
			effectsMatrix$pdecline <- rep(NA,nrow(effectsMatrix))
			effectsMatrix[trendparam,]$pdecline <- rep(-99,length(trendparam))
			}
	
		# output plotting confidence intervals for sp ~ year models
		# ... check if its a simple year effect model (less than 3 terms) and not an annual index.
		if(length(model.terms)<=3 & length(grep("year",model.terms))>0){
			## works if I log the predictions, subtract the offset, so, must divide the predictions by nStations
			
			# predict for new each unique year
			yearsToEastimate <- unique(dyndat$year)[order(unique(dyndat$year))]
			yearEstimates <- model.matrix(~year,data.frame(year = yearsToEastimate)) %*% fixef(m)
			
			# estimate std.err of annual predictions
			stderr <- sqrt(diag(vcov(m)))
			names(stderr) <- names(fixef(m))
			
			# estimate 95% confidence intervals of annual predictions (inflated for overdispersion)
			CIlow <- yearEstimates - 1.96*inflate*stderr["year"]
			CIhi <- yearEstimates + 1.96*inflate*stderr["year"]

			printpreds <- data.frame(year = unique(dyndat$year)[order(unique(dyndat$year))] + ceiling(mean(unique(d2$year))),
						fit = tx(yearEstimates),
						CIlow = tx(CIlow),
						CIup = tx(CIhi))
			
			write.csv(printpreds,paste(tempfolder,"/PLOT",paste(baseform,ifelse(length(grep("station",paste(deparse(randeff))))>0,"~route + station",paste(deparse(randeff))),fam,subsetter,sep="_"),".csv",sep=""))
			}

		# vectorize mainEffects for results and reporting
		effectsMatrix <- effectsMatrix[which(row.names(effectsMatrix) != "offset(log(nStations))"),]	# remove offset, if its there
		resnames <- c("species","model","family","random","subset","duration","DIC","pBayesian",paste(rep(row.names(effectsMatrix),each=ncol(effectsMatrix)),c("",names(effectsMatrix)[-1]),sep=""))
		res <- c(sp,paste(formula(baseform))[3]	, fam, as.character(randeff)[2], subsetter, survPeriod, "-","-",unlist(lapply(t(effectsMatrix),FUN = function(x) ifelse(is.numeric(x),round(x,4),x))))
		resnames <- resnames[!is.na(res)]		
		res <- res[!is.na(res)]
		listres <- matrix(res,ncol = length(res))
		colnames(listres) <- resnames
		write(colnames(listres),paste(fileName, ".csv",sep=""),append=TRUE,sep=",",ncol = length(listres))
		write(listres,paste(fileName,".csv",sep=""),append=TRUE,sep=",", ncol = length(listres))
		return(listres)
 		} else {
		# if the originally modelling produced an error, return NAs
		resnames <- c("species","model","family","random","subset","duration","error")
		res <- c(sp,paste(formula(baseform))[3],fam,as.character(randeff)[2], subsetter,survPeriod,"error")
		listres <- matrix(res,ncol = length(res))
		colnames(listres) <- resnames
		return(listres)
		write(colnames(listres),paste(fileName,".csv",sep=""),append=TRUE,sep=",",ncol = length(listres))
		write(listres,paste(fileName,".csv",sep=""),append=TRUE,sep=",", ncol = length(listres))
		}}
	}



# cycle through formulas and run each species model
for(iii in 1:length(formulalist)){
	try(globfunc(iii))
	}


# TRY IN PARALLEL MODE 
#	library(snowfall)
#  	sfInit(parallel = TRUE, cpus = 2)	# set the number of cpus equal to the number of processors of your computer (go to START -> CONTROL PANELS -> SYSTEM -> DEVICES
#  	sfLibrary(MCMCglmm)
#  	sfLibrary(sqldf)
#  	sfExport("d2","formulalist","globalBasins","indieBasins","minObs","minVisits", "fileName")
#  	#sfres <- sapply(1:length(formulalist),globfunc)
#  	sfres <- sfLapply(1:length(formulalist),globfunc)
#  	sfStop()


# DEMONSTRATION OF HOW THE OFFSET WORKS in GLMM 
# x <- rep(c("A","B"),each = 30)
# groups <- rep(letters[1:10], each = 6)	# random intercept
# trudiff <- rep(c(0,2),each = 30)	# apparent diff is 2, but if we offset "offsetv", then the real difference is only 1
# trudiff.randomeffect <- trudiff + rep(rnorm(n = 10, mean = 0,sd=0.5), each = 6)	# add random effect
# offsetv <- rep(c(1,2), each = 30)
# y <- rpois(n = 60, lambda = trudiff.randomeffect + 3)
# modNOoffset <- glmer(y ~ x + (1|groups), family = "poisson")	# model without offset
# exp(coef(modNOoffset)$groups[1,"xB"])	# should be ~2
# modoffset <- glmer(y ~ x + (1|groups) + offset(log(offsetv)), family = poisson)	# model with offset - log of number of sampling trails
# exp(coef(modoffset)$groups[1,"xB"])	# should half the unadjusted estimate





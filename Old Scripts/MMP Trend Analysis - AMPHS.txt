#############################################################################################3
#
# This script runs Bayesian Mixed models for trend analyses (here for birds). There is also a
# a complimentary Generalized Linear Mixed Model (GLMM) version of the trend analysis,
# in case this analysis takes too long. This script can run for many days if all possible models
# all regional subsets are run.
#
# The output is specified by the user below. ( fileName <- "Trend Birds 2010"). Plus, the is a
# folder called "temp_R_files_TRENDS" which says .csv files useful for plotting trends plus 95%
# confidence intervals, as well as the saved R models which can be reread into R for further 
# investigations.
#
# The output has a line per model. It is not easy to interpret for humans, and is meant to feed
# into the script: MMP Trend Analysis SUMMARY.txt  to generate human-friendly model outputs, plus
# SQL tables to load up onto Nature Counts.
#
# The main input is "Amphstns.csv" which is a species table built in another R script	
# T:\MARSH\Trend Analyses\MMP Build Program 2010 R.R
#
# The user must specify a couple of things:
#	- working directory: 	"T:/MARSH/Trend Analyses"
#	- input data file:	"Birdstns_noobserve.csv"
#	- what types of models to run (see fl table below) (e.g., indices vs. trends, subsetting by 
#		basins, bcrs, etc. The DEFAULT is to run ALL which takes a LONG LONG time 
#		(maybe 24 hours days in 2011 on a very fast computer)
#	- species:	- default is to run ALL species for logistic regression.
#		
#
#
# It is not uncommon for Windows to produce random pop-up errors during the analysis. If this is the case,
# don't worry, you haven't lost all the results, because they are being saved continuously. Just
# reload the script and it will prompt you if you want to reload existing results, rather than re-do
# everything from the beginning.
#	
# The code for the Bayesian P-value is from "Marc Kery. 2010. Introductions to WinBUGS for Ecologists,
# Acedemic Press", adapted for use in the native R language and the MCMCglmm package.
#  To understand the R package MCMCglmm, refer to the MCMCglmm tutorial called
# "MCMCglmm CourseNotes.pdf", located at CRAN, and other R mirrors such as:
# http://hosho.ees.hokudai.ac.jp/~kubo/Rdoc/library/MCMCglmm/doc/
#
# WARNING: this script gives you the option of running in parallel mode or serial mode. Parallel mode
# is the default, BUT you must know the number of processors on your computer. If you have 4 processors
# this script will run x4 faster than the serial mode. Use the serial mode IF you only have 1 processor
# and if you only have ~ 1 GB or RAM.
#
# .~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`
# 		Latest Author Robert William Rankin (robertw.rankin@gmail.com)
# .~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`*Oo~._.~oO*`

rm(list=ls(all=TRUE))
memory.limit(4095)
setwd("T:/MARSH/Trend Analyses")
library(sqldf)
library(MCMCglmm)

############################################################
# Set up data
############################################################
# filename for the exported results
  fileName <- "Trend Amphs Analysis"

# import data, Maximum counts for species (by column) summarized to the station level 
  d <- read.csv("Amphstns.csv",header=TRUE)
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
  splist2 <- c('CHFR','AMTO','NLFR','SPPE','BULL','GRFR','GRTR','PIFR','WOFR','CGTR','FOTO','BCFR')
  splist2 <- call("paste", splist2)


###################################################################################
# SPECIFY THE TYPES OF ANALYSES YOU WANT TO RUN
###################################################################################
# Key things to consider:
# 1) run analysis: TRUE / FALSE to toggle whether that type of analysis should indeed be run
# 2) formula: only supports the following formulas
#		~ year (+/- offset)  		for overall main TREND
#		~ as.factor(year) (+/- offset)	for annual INDICES
#		~ year*basin	  (+/- offset)	for basin-level TRENDS interactions (preferable to running each basin on its own)
# 3) family: only supports counts ("poisson") and logistic ("categorical")
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
#Run 	#formula				#family		 #random		 	#subset			#restrict	
TRUE,	"~ year",				"categorical", 	"~route",			"GreatLakes='1'",	splist2 
,TRUE,	"~ as.factor(year) -1",			"categorical", 	"~route",			"GreatLakes='1'",	splist2 	
,TRUE,	"~ year*basin -1 -year",		"categorical", 	"~route",			"GreatLakes='1'",	splist2 
,TRUE,	"~ as.factor(year) -1",			"categorical", 	"~route",			"basin='H'",		splist2 	
,TRUE,	"~ as.factor(year) -1",			"categorical", 	"~route",			"basin='O'",		splist2 	
,TRUE,	"~ as.factor(year) -1",			"categorical", 	"~route",			"basin='M'",		splist2 	
,TRUE,	"~ as.factor(year) -1",			"categorical", 	"~route",			"basin='E'",		splist2 	
,TRUE,	"~ as.factor(year) -1",			"categorical", 	"~route",			"basin='S'",		splist2 	
)


# convert the model specifications into a list of formulas to 		
formulalist <- list()

# cycle through each model specification in fl, set up formulas for each species (if it has enough observations)
for(i in 0:((length(fl)-1)/6)){
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
  
# create new temporary directory to store models and data required for plotting
if(file.exists('temp_R_files_TRENDS')==FALSE) dir.create('temp_R_files_TRENDS')



#######################################################################################################################
# CHECK WHICH MODELS ARE ALREADY RUN
#######################################################################################################################
# create new temporary directory to store models and data required for plotting
if(file.exists(paste(fileName,".csv",sep=""))) {
	IMPORTT <- "YES"
	IMPORTT <- toupper(winDialog("yesno",paste("The file",paste(fileName,".csv",sep=""),"already exisits. Do you want to exclude models already completed and stored in the file?(Yes). Click 'No' to run all models again")))

	if(IMPORTT == "YES"){
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


# make the function which runs all the analyses
globfunc <- function(iii) {

		# function for presense / absense quick summaries
		presonly <- function(data,x) cbind(data[,which(names(data) %in% x == FALSE)],apply(as.matrix(data[,x],ncol=length(x),dimnames=list(row.names(data),x)),2,FUN = function(q) ifelse(q>0,1,0)))
		
		# a is the formulalist which has all the model specifications
		a <- formulalist[[iii]]
		sp <- a[4]			# species of focus
		form <- as.formula(a[1])	# model formula
		randeff <- as.formula(a[2])	# formula for random effect
		fam <- a[3]			# family (poisson, or logisitic = categorical)
		subsetSQL <- a[5]		# SQL statment to limit the analysis (e.g., limit to BCR 12, or Ontario, etc.)
		subsetter <- gsub(" ","", gsub("=","",gsub("'","",subsetSQL)))	# to add as a file extension
		
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

		# if the family is logisitic/categorical, transform all the data to 0s and 1s
		if(fam == "categorical") { dyndat[,sp]<-as.numeric(ifelse(dyndat[,sp]>0,1,0)) }
		
		# two step SQL data reduction: 1) remove routes only surveyed once, 2) remove routes which NEVER had a single species occurence (e.g., no trend to estimate)
		keeproutes <- sqldf(paste("select route, count(year) as 'nYears', sum(totobs) as 'totcounts' from (select route, year, sum(",sp,") as 'totobs' from dyndat group by year,route) group by route having nYears > 1 AND totcounts > 0"))$route
		dyndat <- subset(dyndat, route %in% keeproutes)	
		
		m <- NULL		# dummy variable for the model itself
		nInit <- ifelse(randeff =="~route + route:station" | fam == "categorical",100000,20000)	# initial number of iterations for bayesian sampling
		sliceT <- ifelse(randeff =="~route + route:station" | fam == "categorical",TRUE,FALSE)		# use slice sampling  to update the latent variables rather then MH updates
		autoCorCrit <- ifelse(fam =="categorical",0.21,0.21)
		bburn <- ifelse(randeff =="~route + route:station" | fam == "categorical",85000,10000)
		
		# specify required inverse link-function "tx()" for resulting effect estimations, 
		# as well as criteria "CritDecl" to judge if there is a decline
		if(fam == "poisson"){ tx <- function(x) exp(x)
				      CritDecl <- 1
				      rfunc <- function(n,l) rpois(n=n,lambda=l) 	
				      pearsonR <- function(true,fitte) (true - fitte)/sqrt(fitte)	 
		} else {if(fam == "categorical"){ 
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
			res <- c(sp,as.character(form)[3],fam,as.character(randeff)[2],subsetter,survPeriod,"too_few_obs_per_basin")
			listres <- matrix(res,ncol = length(res))
			colnames(listres) <- resnames
			write(colnames(listres),paste(fileName,".csv",sep=""),append=TRUE,sep=",",ncol = length(listres))
			write(listres,paste(fileName,".csv",sep=""),append=TRUE,sep=",", ncol = length(listres))
			return(listres)
		} else {	
		
		# check if there is an offset for stations in the formula: if so, then need to summarize counts by routes
		if(a[2] == "~route" & length(grep("nStations",deparse(form)))>0){
			# need to transform data to summarize at the route level, plus need an offset (number of stations per route)
			dyndat <- sqldf(paste("select route,year,count(station) as 'nStations', sum(",sp,") as'",sp,"',basin from dyndat group by route,year",sep=""))			
			}

		# SETUP PRIORS	
		# B is a list for parameter values in model, with a list of MU for initial estimated mean, and a list V for initial estimated variance (set high so as to be non-informative)
		# if it is poisson regression, the rest of the priors (random effects) are run as defaul in MCMCglmm
		# if it is logistic regression, then we must also specify priors for...
		#	... R is a list of priors for the variance of the random effects groupings (fixed to 1, as recommended by MCMCglmm author, so its not estimated)
		#	... G is a list of the G structure, one list per random effect. Variances (V) and degree of belief parameter (nu) for the inverse-Wishart.
		# NOTE, if there is an offset, (log(nstations)), then we must not allow the model to estimate its mean and variance. Rather we fix it to a value of 1 and
		# 	... and declare that we are highly confident that it is indeed this value.

		model.terms <- colnames(model.matrix(form,dyndat))	# model terms	
		priors <- list(B= list(mu = matrix(1*(model.terms == "log(nStations)"),length(model.terms)),V = diag(length(model.terms))*1e+6))
		if(fam == "categorical"){
			priors$B$V <- diag(length(model.terms))*(3 + pi^2/3)	# non-informative prior for logistic regression
			priors$R <- list(V = 1, fix = 1)
			priors$G <- list(G1 = list(V = 1,nu = 0.002),G2 = list(V = 1,nu = 0.002))
			}
		# offsetting stations: need to "fix" the prior so that it doesn't vary (very small variance)
		if(a[2] == "~route" & length(grep("nStations",deparse(form)))>0){
			diag(priors$B$V)[model.terms == "log(nStations)"]<-(1e-6)		# need to fix offset as unvarying
			}

		# run first model
		m <- MCMCglmm(form, random = randeff, data = dyndat, family = fam,  prior = priors, nitt = nInit, burn = bburn, thin = floor((nInit-bburn)/1000), pr = TRUE, verbose = FALSE, slice = sliceT)
		if(!is.null(m)) {
		
		# check auto correlation of variables estimates in MCMC chain, and increase number of iterations if any correlation is > 0.21
		maxCor <- max(as.vector(cbind(autocorr(m$Sol[,ncol(m$X)])[2:5,,],autocorr(m$VCV)[2:5,-ncol(m$VCV),-ncol(m$VCV)])))
		maxInits <- 0
		while(maxCor > autoCorCrit & maxInits < 5){
			nInit <- floor(nInit * (1+maxCor))	# increase iterations, and below, increase proportionally the thinning interval 'thin'
			bburn <- floor(bburn * (1+maxCor))
			m2<-NULL; m2$DIC <- m$DIC+1	# fake model
			try(m2 <- MCMCglmm(form, random = randeff, data = dyndat, family = fam, prior = priors, nitt = nInit, burn = bburn, thin = floor((nInit-bburn)/1000),pr = TRUE,verbose = FALSE, slice = sliceT),silent=TRUE)
			if(m2$DIC <= m$DIC){m <- m2}	
			maxInits <- maxInits + 1
			maxCor <- max(as.vector(cbind(autocorr(m$Sol[,ncol(m$X)])[2:5,,],autocorr(m$VCV)[2:5,-ncol(m$VCV),-ncol(m$VCV)])))
			}
		# save model for reloading purposes (i.e., it can be viewed later)
		m$Xform <- formulalist[[iii]]; m$Xsp <- a[4]; m$Xrandeff <- a[2]; m$XsubsetSQL <- subsetSQL; m$Xsubsetter <- subsetter; m$Xfam <- fam
		modName <- paste("temp_R_files_TRENDS/MODEL",paste(deparse(form),ifelse(length(grep("station",paste(deparse(randeff))))>0,"~route + station",paste(deparse(randeff))),fam,subsetter,sep="_"),".RData",sep=""); modName <- gsub("*","xXx",modName, fixed=TRUE)
		save(m, file = modName)
		
		# find the names of trend parameters with a year (trend) as an interaction or main effect
		trendparam <- row.names(summary(m)$solutions)[which(1:nrow(summary(m)$solutions) %in% grep("year",row.names(summary(m)$solution)) & apply(as.matrix(m$X),2, FUN = function(x) any(x != 1 & x != 0)))]	
		
		# rescale effects estimates to the "count" scale, or to the "presense-absense" scale (e.g., apply the tx(x) function earlier)
		effectsMatrix <- cbind(apply(summary(m)$solution[,c("post.mean","l-95% CI","u-95% CI")],2,FUN = function(x) tx(x)),data.frame(pMCMC = summary(m)$solution[,"pMCMC"]))
			
		# estimate Bayesian probability of a decline (assuming means are estimated, not the contrasts)
		if(length(trendparam)!=0){
				effectsMatrix$pdecline <- rep(NA,nrow(effectsMatrix))
				effectsMatrix[trendparam,"pdecline"] <- as.numeric(apply(data.frame(m$Sol[,trendparam]),2,FUN = function(x) sum(tx(x) < CritDecl))/nrow(m$Sol))   # for POISSON, year coefficients of < 0 means, for logisit < 0.5
				}
		
		# 'Bayesian p-value' (a.k.a., Goodness-of-fit statistic) a posterior predictive check to assess model fit
		Presiduals.obs <-  numeric(0)
		Presiduals.new <- numeric(0)
			if(is.null(dim(m$VCV))){ varfunc <- function(x) sqrt(m$VCV[x]) } else { varfunc <- function(x) sqrt(sum(m$VCV[x,])) }
			
			for (i in 1:nrow(m$Sol)) {
				pred.0 <- as.numeric(cBind(m$X,m$Z) %*% m$Sol[i,])			# predict estimates using both Fixed and Random effects, for each iteration of the MCMC 
				pred.1 <- rnorm(nrow(dyndat), pred.0, varfunc(i))			# random values based on each iterations fitted effects and random effects
				Presiduals.obs <- c(Presiduals.obs, sum(pearsonR(dyndat[,sp],tx(pred.0))**2))					# calculate sq pearson residuals (obs - exp)/sqrt(exp)
				Presiduals.new <- c(Presiduals.new, sum(pearsonR(rfunc(length(pred.1),tx(pred.1)),tx(pred.0))**2))		# (fake) sq pearson residuals (obs - exp)/sqrt(exp)
				}
			# Bayesian P-value for Goodness-of-fit:
			# number of times the random data fits better than your model 
			mpval <- mean(Presiduals.new > Presiduals.obs)	
			
		
		# next, need to estimate a weighted-average "main effect" for basin models
		if(any(grep("basin",row.names(effectsMatrix)))){
			splitBasins <- row.names(effectsMatrix)[grep("year",row.names(effectsMatrix))]
			prBasins <- table(dyndat$basin)/nrow(dyndat)		# proportion of data in different basins
			mainEffect <- matrix(m$Sol[,splitBasins] %*% prBasins,ncol=1)
			class(mainEffect) <- "mcmc"
			effectsMatrix["mainTrend",] <- c(tx(posterior.mode(mainEffect)), tx(coda::HPDinterval(mainEffect)), 2 * pmax(0.5/dim(mainEffect)[1], pmin(colSums(mainEffect > 0)/nrow(mainEffect), 1 - colSums(mainEffect > 0)/dim(mainEffect)[1])), colSums(mainEffect < 0)/nrow(mainEffect))
			}
		# output plotting confidence intervals for sp ~ year models
		if(length(model.terms)<=3 & length(grep("year",model.terms))>0){
			## works if I log the predictions, subtract the offset, so, must divide the predictions by nStations

			postpreds <- apply(m$Sol[,1:ncol(m$X)],1,FUN = function(x) as.numeric(cBind(m$X) %*% x))
			if(length(grep("nStations",deparse(form)))>0){ offset <- log(dyndat$nStations)
				} else { offset = 0 }
			postpredsAgg <- t(apply(postpreds,2, FUN = function(x) aggregate(x-offset,by = list(dyndat$year),mean)[,"x"]))
			yearEstimates <- tx(apply(postpredsAgg,2,mean))
			class(postpredsAgg)<-"mcmc"
			yearCIs<-coda::HPDinterval(postpredsAgg)	# estimate Credibility intervals
			printpreds <- data.frame(year = unique(dyndat$year)[order(unique(dyndat$year))] + ceiling(mean(unique(d2$year))),
						fit = yearEstimates,
						CIlow = tx(yearCIs[,"lower"]),
						CIup = tx(yearCIs[,"upper"]))
			
			write.csv(printpreds,paste("temp_R_files_TRENDS/PLOT",paste(deparse(form),ifelse(length(grep("station",paste(deparse(randeff))))>0,"~route + station",paste(deparse(randeff))),fam,subsetter,sep="_"),".csv",sep=""))
			}

		# vectorize mainEffects for results and reporting
		names(effectsMatrix)[2:4] <- c("lowCI","upCI", "p")
		effectsMatrix <- effectsMatrix[which(row.names(effectsMatrix) != "log(nStations)"),]
		resnames <- c("species","model","family","random","subset","duration","DIC","pBayesian",paste(rep(row.names(effectsMatrix),each=ncol(effectsMatrix)),c("",names(effectsMatrix)[-1]),sep=""))
		res <- c(sp,as.character(form)[3], fam, as.character(randeff)[2], subsetter, survPeriod, m$DIC,round(mpval,7),unlist(lapply(t(effectsMatrix),FUN = function(x) round(x,4))))
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
		res <- c(sp,as.character(form)[3],fam,as.character(randeff)[2], subsetter,survPeriod,"error")
		listres <- matrix(res,ncol = length(res))
		colnames(listres) <- resnames
		return(listres)
		write(colnames(listres),paste(fileName,".csv",sep=""),append=TRUE,sep=",",ncol = length(listres))
		write(listres,paste(fileName,".csv",sep=""),append=TRUE,sep=",", ncol = length(listres))
		}}
	}

###############################################################
# TRY IN Serial MODE 
###############################################################
# WARNING: this is a slower option, if you can't do parallel processing
# Both modes have the same result

# sfres <- sapply(length(formulalist):1,globfunc)

###############################################################
# TRY IN PARALLEL MODE (much faster)
###############################################################
# WARNING: you need to know the number of processors on your computer
	library(snowfall)
		

  	sfInit(parallel = TRUE, cpus = 4)
  	sfLibrary(MCMCglmm)
  	sfLibrary(sqldf)
  	sfExport("d2","formulalist","globalBasins","indieBasins","minObs","minVisits", "fileName")
  	#sfres <- sapply(1:length(formulalist),globfunc)
  	sfres <- sfLapply(c(1:length(formulalist)),globfunc)
  	sfStop()




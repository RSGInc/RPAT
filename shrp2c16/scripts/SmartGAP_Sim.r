#===============
#SmartGAP_Sim.r
#===============

#This program was developed as part of the SHRP 2 C16 project, The Effect of Smart Growth Policies on Travel Demand 
#See http://apps.trb.org/cmsfeed/TRBNetProjectDisplay.asp?ProjectID=2355
#The prime contractor for this project is RSG, Inc., http://www.rsginc.com
#Authors: Maren Outwater, Colin Smith, Michael Geilich, Chris Hoffman, Jeremy Wallis (all RSG), Christopher Gray, Jerry Walters (both Fehr and Peers), Rich Kuzmyak (Renaissance Planning Group), Dr. Robert Cervero, Dr. Kara Kockelman      

#This software is a significant modification of GreenSTEP, developed by Oregon Department of Transportation
#GreenSTEP was authored by Brian Gregor, Brian.J.Gregor@odot.state.or.us
#We acknowledge his great work in developing GreenSTEP and wish to thank him for his contribution to the practice by making GreenSTEP available as open source software 

#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GHU General Public License for more details.
#You should have received a copy of this GNU General Public License along with this program (see "license.txt"). If not, see <http://www.gnu.org/licenses/>.

# Copyright 2011-2012 RSG, Inc.


#Description
#===========
#This module performs all of the household microsimulation calculations for determining household income, vehicle ownership, household travel and vehicle characteristics and use. The results are aggregated to arrays by county, income and development type and saved to disk.

household <- function() {
#================================
print("HOUSEHOLD AND FIRM MODEL")
#================================
	
	#Create Directory Structure for Outputs
	#======================================
	for( runtype in RunTypes$RunType ) {
		
		yr <- RunTypes$RunYear [ RunTypes$RunType == runtype ]
		print( paste(runtype, yr, sep= " ") )
		
		# Make a directory to save the data for the run type in
		RunTypeDir <- paste( "outputs/Type", runtype, sep="" )
		if( !file.exists( RunTypeDir ) ) {
			dir.create( RunTypeDir )
		}
		# end of loop on runtypes
	}
	
	#Create synthetic households for each forecast year
	#==================================================
	
	#Load the inputs
	#Load the population file
	Pop..CoAp <- read.csv( inputLocation( "pop_by_age.csv" ) )
	
	#Loop through the runtypes
	for( runtype in RunTypes$RunType ) {
		
		yr <- RunTypes$RunYear [ RunTypes$RunType == runtype ]
		print( paste(runtype, yr, sep= " ") )	
		
		#Just iterate through the synthetic population for the "E" and "ELESNP" runtypes 
		if( runtype %in% c("E","ELESNP") ){
			
			StartTime <- Sys.time()
			#print to screen
			print( paste( "Synthesizing population for ", yr, sep = "" ) )
			#fix seed as synthesis involves sampling
			set.seed(1)
			
			# Totals by age group
			Pop.Ap <- colSums( Pop..CoAp[Pop..CoAp$Year == yr,3:8] )
			#synthesize households for the whole region
			Hsld.HhAp <- createHhByAge( Pop.Ap , HtProb.HtAp )[[1]]
			
			# Save the results for the year
			SaveFileName <- paste( "outputs/Hsld", yr, ".RData", sep="" )
			save( Hsld.HhAp, file=SaveFileName, compress=TRUE )
			rm( Pop.Ap, Hsld.HhAp, SaveFileName )
			
			#print to screen
			print( StartTime )
			print( Sys.time() )
			
			#end of if
		}	
		#end of loop of run types	
	}

	#Create synthetic businesses for each forecast year
	#==================================================
	#Load the inputs
	#Load the employment data file
	Cbp..CoIsEs <- read.csv( inputLocation( "employment.csv" ), as.is = TRUE )
	#Load the employment growth factor
	TempInput.. <- read.csv( inputLocation( "employment_growth.csv" ), as.is = TRUE )
	EmploymentGrowth <- TempInput..$value

	#Loop through the runtypes
	for( runtype in RunTypes$RunType ) {
		
		yr <- RunTypes$RunYear [ RunTypes$RunType == runtype ]
		print( paste(runtype, yr, sep= " ") )
		RunTypeDir <- paste( "outputs/Type", runtype, sep="" )
		
		#Just iterate through the synthetic population for the "E" and "ELESNP" runtypes 
		if( runtype %in% c("E","ELESNP") ){
			
			StartTime <- Sys.time()
			print( paste( "Synthesizing businesses for ", yr, sep = "" ) )
			#fix seed as synthesis involves sampling
			set.seed(1)
			
			if( yr == BaseYear ) {
				# Create businesses for the region
				SynBiz.IsEs <- createBiz( Cbp..CoIsEs[Cbp..CoIsEs$year == yr,-2] )[[1]]
				
				# Assign a specific employment amount to each business
				sizeminmax <- matrix(c(1,4,5,9,10,19,20,49,50,99,100,249,250,499,500,999,1000,1499,1500,2499,2500,4999,5000,9999),nrow=12,ncol=2,byrow=TRUE,
						dimnames = list(levels(SynBiz.IsEs$esizecat),c("sizemin","sizemax")))
				draws <- runif(nrow(SynBiz.IsEs))
				SynBiz.size <- sizeminmax[SynBiz.IsEs$esizecat,] 
				SynBiz.IsEs$emp <- SynBiz.size[,"sizemin"] + round(draws * (SynBiz.size[,"sizemax"] - SynBiz.size[,"sizemin"]),0)
			
			} else {
				# Load the base year business list
				Filename <- paste( "outputs/Biz", BaseYear, ".RData", sep="" )
				SynBiz.IsEs <- assignLoad( Filename )
				
				# Sample to either create extra businesses if there is growth or select businesses to keep if there is decline
				if(EmploymentGrowth > 1){	#there is growth
					newbiz <- nrow(SynBiz.IsEs) * (EmploymentGrowth - 1)
					SynBiz.new <- sample( row.names(SynBiz.IsEs), newbiz, replace = TRUE )
					SynBiz.IsEs <- rbind(SynBiz.IsEs,SynBiz.IsEs[SynBiz.new,])
					rm(SynBiz.new)
				} else { #there is decline
					remainingbiz <- round(nrow(SynBiz.IsEs) * EmploymentGrowth, 0)
					SynBiz.remaining <- sample( row.names(SynBiz.IsEs), remainingbiz, replace = FALSE )
					SynBiz.IsEs <- SynBiz.IsEs[SynBiz.remaining,]
					rm(SynBiz.remaining)
				}
			}
			
			# Save the results for the year
			SaveFileName <- paste( "outputs/Biz", yr, ".RData", sep="" )
			save( SynBiz.IsEs, file=SaveFileName, compress=TRUE )
			rm( SynBiz.IsEs, SaveFileName )
			
			# Copy the file to the runtype folder
			file.copy( paste( "outputs/Biz", yr, ".RData", sep="" ), paste(RunTypeDir,"/",runtype,"Emp.RData",sep=""), overwrite=TRUE )
				
			#print to screen
			print( StartTime )
			print( Sys.time() )
			
			#end of if
		}	
		#end of loop of run types	
	}

	#Add Household Structure Variables
	#=================================
	for( runtype in RunTypes$RunType ) {
		
		yr <- RunTypes$RunYear [ RunTypes$RunType == runtype ]
		print( paste(runtype, yr, sep= " ") )
		RunTypeDir <- paste( "outputs/Type", runtype, sep="" )
		
		#Just iterate through the synthetic population for the "E" and "ELESNP" runtypes 
		if( runtype %in% c("E","ELESNP") ){
			
			# Load the synthetic population file for the year
			PopFileName <- paste( "outputs/Hsld", yr, ".RData", sep="" )
			SynPop_ <- assignLoad( PopFileName )
			rm( PopFileName )
			
			StartTime <- Sys.time()          
			print( "Adding household structure variables" )
			print(ls())
			
			# Extract the synthetic population
			SynPop.. <- data.frame( SynPop_ )
						
			# Give each household a unique id
			SynPop..$Houseid <- 1:nrow( SynPop.. )
			
			# Calculate the household size
			SynPop..$Hhsize <- rowSums( SynPop..[ , Ap ] )
			
			# Calculate driving age population
			SynPop..$DrvAgePop <- rowSums( SynPop..[ , Ap[-1] ] )
			
			# Create a variable identifying driver population levels
			DrvLevels. <- c( 0, 1, 2, max( SynPop..$DrvAgePop ) )
			SynPop..$DrvLevels <- as.character( cut( SynPop..$DrvAgePop, breaks=DrvLevels.,
							labels=c( "Drv1", "Drv2", "Drv3Plus" ) ) )
			
			# Identify households having only elderly persons
			SynPop..$OnlyElderly <- as.numeric( SynPop..$DrvAgePop == SynPop..$Age65Plus )
			
			# Identify households having children
			SynPop..$Children <- as.numeric( SynPop..$Age0to14 > 0 )
			
			# Identify households having only one working age person
			SynPop..$Singleton <- as.numeric( SynPop..$Age0to14 == 0 & SynPop..$Age65Plus == 0 & SynPop..$Hhsize == 1 )
			
			# Identify households having two people, no kids <15
			SynPop..$CoupleNoKids <- as.numeric( SynPop..$Age0to14 == 0 & SynPop..$Age65Plus == 0 & SynPop..$Hhsize == 2 )
			
			# Save the outputs
			Filename <- paste( RunTypeDir, "/", runtype, ".RData", sep="" )
			save( SynPop.., file=Filename, compress=TRUE )
			rm( SynPop_, SynPop.., DrvLevels. )
			
			print( StartTime )
			print( Sys.time() )
			gc()
			
			#end of if
		}
		# end of loop on runtypes
	}
		  
	#Household Income Model
	#======================
	#Load the inputs
	#Load the regional income file
	TempInput.. <- read.csv( inputLocation( "regional_income.csv" ), as.is = TRUE )
	PerCapInc.Yr <- TempInput..$Income
	names( PerCapInc.Yr ) <- TempInput..$Year

	#Loop through the runtypes	
	for( runtype in RunTypes$RunType ) {
		
		yr <- RunTypes$RunYear [ RunTypes$RunType == runtype ]
		print( paste(runtype, yr, sep= " ") )
		RunTypeDir <- paste( "outputs/Type", runtype, sep="" )
		
		#Just iterate through the model for the "E" and "ELESNP" runtypes 
		if( runtype %in% c("E","ELESNP") ){
			
			StartTime <- Sys.time()
			print(paste( "Adding household income for state:", State) )
			print(ls())
			#fix seed as income model involves random error
			set.seed(1)
			
			# Load synpop file
			Filename <- paste( RunTypeDir, "/", runtype, ".RData", sep="" )
			SynPop.. <- assignLoad( Filename )		
			
			# Calculate and add the power-transformed per capita income for the forecast year
			PowPerCapInc <- PerCapInc.Yr[ yr ]^0.4
			SynPop..$PowPerCapInc <- PowPerCapInc
						
			#predictIncome model by state
			
			# Predict household income
			SynPop..$Hhincttl <- predictIncome( SynPop.., State)
			MinInc <- quantile( SynPop..$Hhincttl, prob=0.01 )
			SynPop..$Hhincttl[ SynPop..$Hhincttl < MinInc ] <- MinInc
			
			# Classify households according to income group
			MaxInc <- max( SynPop..$Hhincttl )
			IncBreaks. <- c( 0, 20000, 40000, 60000, 80000, 100000, MaxInc )
			SynPop..$IncGrp <- cut( SynPop..$Hhincttl, breaks=IncBreaks., labels=Ig, include.lowest=TRUE )
			
			# Save the outputs
			save( SynPop.., file=Filename, compress=TRUE )
			rm( SynPop.., PowPerCapInc, MinInc, MaxInc )
						
			print( StartTime )
			print( Sys.time() )
			gc()
			#end of if
		}
		# end of loop on runtypes
	}
#end of household functions
}

urban <- function() {
#=====================================
print("URBAN FORM MODELS")
#=====================================

#The model iterates through each run type. 

	for( runtype in RunTypes$RunType ) {
		
		yr <- RunTypes$RunYear [ RunTypes$RunType == runtype ]
		print( paste(runtype, yr, sep= " ") )
		RunTypeDir <- paste( "outputs/Type", runtype, sep="" )
	
		#Just iterate through the model for the "E" and "ELESNP" runtypes 
		if( runtype %in% c("E","ELESNP") ){
		
		StartTime <- Sys.time()
		print( "Allocating households and employment locations to place types" )
		print(ls())
		
		
		# Load required inputs
		if( yr == BaseYear ) {
			# Load existing place type distribution
			Filename <- inputLocation( "place_type_existing.csv" )
			TempInput.. <- read.csv( Filename, row.names=1 )
			PlaceTypeDist.Pt <- as.matrix( TempInput.. )
			rm( Filename, TempInput.. )	
		} else {
			# Load place type growth from inputs
			Filename <- inputLocation( "place_type_growth.csv" )
			TempInput.. <- read.csv( Filename, row.names=1 )
			PlaceTypeDist.Pt <- as.matrix( TempInput.. )
			rm( Filename, TempInput.. )		
			# Load the base year population and employment
			BasePop.Pt <- assignLoad( "outputs/TypeE/Pop.Pt.RData" )
			BaseEmp.Pt <- assignLoad( "outputs/TypeE/Emp.Pt.RData" )
		}
			
		# Load synpop file
		Filename <- paste( RunTypeDir, "/", runtype, ".RData", sep="" )
		SynPop.. <- assignLoad( Filename )
		
		# Load employment file
		Filename <- paste( RunTypeDir, "/", runtype, "Emp.RData", sep="" )
		SynBiz.IsEs <- assignLoad( Filename )
		
		#Calculate total population and households, and total employment
		Pop <- sum( colSums( SynPop..[ , 1:6 ] ) )
		Hhslds <- nrow( SynPop.. )
		Emp <- sum( SynBiz.IsEs$emp )
		
		# Calculate the place type population and employment proportions
		if( yr == BaseYear  ) {
			Pop.Pt <- PlaceTypeDist.Pt[,1] * Pop
			Emp.Pt <- PlaceTypeDist.Pt[,2] * Emp
		} else {
			PopGrowth <- Pop - sum( BasePop.Pt )
			EmpGrowth <- Emp - sum( BaseEmp.Pt )
			PopGrowth.Pt <- PopGrowth * PlaceTypeDist.Pt[,1]
			EmpGrowth.Pt <- EmpGrowth * PlaceTypeDist.Pt[,2]
			Pop.Pt <- BasePop.Pt + PopGrowth.Pt
			Emp.Pt <- BaseEmp.Pt + EmpGrowth.Pt
		}
		names(Pop.Pt) <- Pt
		names(Emp.Pt) <- Pt
		
		# Round population to whole number and correct rounding error
		Pop.Pt <- round( Pop.Pt )
		if( sum( Pop.Pt ) != Pop ) {
			PopDiff <- Pop - sum( Pop.Pt )
			Sign <- sign( PopDiff )
			Probs.Pt <- Pop.Pt / sum( Pop.Pt )
      Probs.Pt[Probs.Pt < 0] <- 0
			PopDiff. <- sample( Pt, abs( PopDiff ), replace = TRUE, prob = Probs.Pt )
			PopDiff.Pt <- Sign * table( PopDiff. )[ Pt ]
			names( PopDiff.Pt ) <- Pt
			PopDiff.Pt[ is.na( PopDiff.Pt ) ] <- 0
			Pop.Pt <- Pop.Pt + PopDiff.Pt
		}
		if( any( Pop.Pt < 0 ) ) {
			PopDiff <- sum( Pop.Pt[ Pop.Pt < 0 ] )
			Pop.Pt[ Pop.Pt < 0 ] <- 0
			Probs.Pt <- Pop.Pt / sum( Pop.Pt )
			Probs.Pt[Probs.Pt < 0] <- 0
			PopDiff. <- sample( Pt, abs( PopDiff ), replace = TRUE, prob = Probs.Pt )
			PopDiff.Pt <- table( PopDiff. )[ Pt ]
			names( PopDiff.Pt ) <- Pt
			PopDiff.Pt[ is.na( PopDiff.Pt ) ] <- 0
			Pop.Pt <- Pop.Pt - PopDiff.Pt
		}
	
		# Round employment to whole number and correct rounding error
		Emp.Pt <- round( Emp.Pt )
		if( sum( Emp.Pt ) != Emp ) {
			EmpDiff <- Emp - sum( Emp.Pt )
			Sign <- sign( EmpDiff )
			Probs.Pt <- Emp.Pt / sum( Emp.Pt )
			Probs.Pt[Probs.Pt < 0] <- 0
			EmpDiff. <- sample( Pt, abs( EmpDiff ), replace = TRUE, prob = Probs.Pt )
			EmpDiff.Pt <- Sign * table( EmpDiff. )[ Pt ]
			names( EmpDiff.Pt ) <- Pt
			EmpDiff.Pt[ is.na( EmpDiff.Pt ) ] <- 0
			Emp.Pt <- Emp.Pt + EmpDiff.Pt
		}
		if( any( Emp.Pt < 0 ) ) {
			EmpDiff <- sum( Emp.Pt[ Emp.Pt < 0 ] )
			Emp.Pt[ Emp.Pt < 0 ] <- 0
			Probs.Pt <- Emp.Pt / sum( Emp.Pt )
			Probs.Pt[Probs.Pt < 0] <- 0
			EmpDiff. <- sample( Pt, abs( EmpDiff ), replace = TRUE, prob = Probs.Pt )
			EmpDiff.Pt <- table( EmpDiff. )[ Pt ]
			names( EmpDiff.Pt ) <- Pt
			EmpDiff.Pt[ is.na( EmpDiff.Pt ) ] <- 0
			Emp.Pt <- Emp.Pt - EmpDiff.Pt
		}
		
		#Assign employment locations randomly to the place types
		#fix seed as allocation involves sampling
		set.seed(1)
		Prob.Pt <- Emp.Pt / sum( Emp.Pt )
		Prob.Pt[Prob.Pt < 0] <- 0
		SynBiz.IsEs$PlaceType <- sample( Pt, nrow( SynBiz.IsEs ), replace=TRUE,	prob = Prob.Pt )
		
			#Calculate area type targets for the hh allocation
			Target.At <- rowsum(as.vector(Pop.Pt),At)
			#put a small value in to replace any zeros
			Target.At <- ifelse(Target.At == 0,1,Target.At)
			#Initialize Pop.At
			Pop.At <- Target.At * 0
			AreaTypes <- row.names(Pop.At) 
			
			#Apply the HhAllocationModel
			#Creating all the variables that are required for simulating the choice
			SynPop..$ASC <- 1
			SynPop..$Hhinc1000 <- SynPop..$Hhincttl/1000
			
			ModelVar. <- c( "ASC", "Singleton", "Children", "CoupleNoKids", 
					"OnlyElderly", "Hhinc1000" )  
			######predict logit needs data.tabls package
			###if change SynPop.. to data.table this won't be necessary
			ModelData <- data.table(SynPop..[ , ModelVar. ])
			
			###could put the following code into a function to call
			HhAllocationModelCoeffs <- HhAllocationModel
			MaxIter <- 10
			QuitCriteria <- 4000
			
			for(i in 1:MaxIter){
				#apply the model
				#fix seed as model involves sampling
				set.seed(1)
				HhAllocation_ <- predictLogit( ModelData, HhAllocationModelCoeffs )
				#Calculate population by four area types
				#recode HhAllocation to match At (Rural, Suburban, CIC, Urban Core) and attach to SynPop..
				SynPop..$AreaType <- ifelse(HhAllocation_ == 1, "CIC", ifelse(HhAllocation_ == 3,"Sub", ifelse(HhAllocation_ == 5,"UC","Rur")))
				Pop.At[AreaTypes,] <- rowSums(rowsum(SynPop..[ , 1:6 ],SynPop..$AreaType))[AreaTypes]
				Pop.At[is.na(Pop.At)] <- 0
				#Check to see if close enough - break if it is 
				if (sum(abs(Target.At - Pop.At)) < QuitCriteria){ break }  
				######This does not necessarily converge!
				#Calculate adjustments to ASC's
				ASC.At <- log(Target.At/Pop.At)
				#Update the ASC's in the table model coefficients (add the adjustments)
				HhAllocationModelCoeffs$COEFF[HhAllocationModelCoeffs$VAR == "ASC"] <- ASC.At[c(1:3,2,4)] + HhAllocationModelCoeffs$COEFF[HhAllocationModelCoeffs$VAR == "ASC"]
			}
			
			#reassign any households assigned to an area where there should be zero
			Target.AtR <- rowsum(as.vector(Pop.Pt),At)
			SynPop..$AreaType[SynPop..$AreaType == names(Target.AtR[Target.AtR==0,])] <- names(Target.AtR[Target.AtR==max(Target.AtR),])[1]
			
			#Assign development types to households: draw randomly for each area type
			#Calculate development type targets for the hh allocation
			Target.Pt <- as.vector(c(Target.At[2], rep(Target.At[3],4), rep(Target.At[1],4), rep(Target.At[4],4))) 
			Prob.Pt <- (Pop.Pt/Target.Pt)
		  Prob.Pt[Prob.Pt < 0] <- 0
			
			if(Prob.Pt[1] > 0){
				#fix seed as allocation involves sampling
				set.seed(1)
				SynPop..$DevType[SynPop..$AreaType == "Rur"] <- sample( Dt[1], nrow( SynPop..[SynPop..$AreaType == "Rur",] ), replace=TRUE,
					prob = Prob.Pt[1] )
			}
			if(sum(Prob.Pt[2:5]) > 0){
				#fix seed as allocation involves sampling
				set.seed(1)
				SynPop..$DevType[SynPop..$AreaType == "Sub"] <- sample( Dt[2:5], nrow( SynPop..[SynPop..$AreaType == "Sub",] ), replace=TRUE,
					prob = Prob.Pt[2:5] )
			}
			if(sum(Prob.Pt[6:9]) > 0){
				#fix seed as allocation involves sampling
				set.seed(1)
				SynPop..$DevType[SynPop..$AreaType == "CIC"] <- sample( Dt[6:9], nrow( SynPop..[SynPop..$AreaType == "CIC",] ), replace=TRUE,
					prob = Prob.Pt[6:9] )
			}
			if(sum(Prob.Pt[10:13]) > 0){
				#fix seed as allocation involves sampling
				set.seed(1)
				SynPop..$DevType[SynPop..$AreaType == "UC"] <- sample( Dt[10:13], nrow( SynPop..[SynPop..$AreaType == "UC",] ), replace=TRUE,
					prob = Prob.Pt[10:13] )
			}
						
			#Assign place type based on area type and development type
			AtDt.Pt <- cbind(At, Dt, Pt)
			PlaceType <- merge(SynPop..[,c("Houseid", "AreaType","DevType")], AtDt.Pt, by.x = c("AreaType","DevType"), by.y = c("At","Dt"))
			PlaceType <- PlaceType[order(PlaceType$Houseid),]
			SynPop..$PlaceType <- PlaceType$Pt

			######CS Average Density
			###CS should this be a calculated average for the region?
			SynPop..$Htppopdn <- 500
			###CS should this be 0 for rural? Or are we just using an average for both density and this var and then adjusting using 5D values?
			SynPop..$Urban <- 1
			
			# Calculate the natural log of density
			SynPop..$LogDen <- log( SynPop..$Htppopdn )
			
			#Update Pop.Pt and Emp.Pt so they reflect actual assigned population and employment rather than input proportions.
			Pop.Pt[Pt] <- rowSums(rowsum(SynPop..[ , 1:6 ],SynPop..$PlaceType))[Pt]
			Emp.Pt[Pt] <- tapply( SynBiz.IsEs$emp , SynBiz.IsEs$PlaceType, sum )[Pt]
			Pop.Pt[is.na(Pop.Pt)] <- 0
			Emp.Pt[is.na(Emp.Pt)] <- 0
			
			# Sum income by development type
			Inc.Pt <- Pop.Pt * 0
			Inc.Pt[Pt] <- tapply( SynPop..$Hhincttl, SynPop..$PlaceType, sum )[Pt]
			Inc.Pt[is.na(Inc.Pt)] <- 0
			
			# Save the outputs
			Filename <- paste( RunTypeDir, "/", runtype, ".RData", sep="" )
			save( SynPop.., file=Filename, compress=TRUE )
			rm( SynPop.. )
			gc()
			
			Filename <- paste( RunTypeDir, "/", runtype, "Emp.RData", sep="" )
			save( SynBiz.IsEs, file=Filename, compress=TRUE )
			rm( SynBiz.IsEs )
			gc()
			
			# Save the income tabulation
			#===========================
			
			Filename <- paste( RunTypeDir, "/Inc.Pt.RData", sep="" )
			save( Inc.Pt, file=Filename )
			rm( Filename )
			rm( Inc.Pt )
			
			#Save Pop.Pt 
			#=============
			
			Filename <- paste( RunTypeDir, "/Pop.Pt.RData", sep="" )
			save( Pop.Pt, file=Filename )
			rm( Filename )
			rm( Pop.Pt )
			
			#Save Emp.Pt 
			#=============
			
			Filename <- paste( RunTypeDir, "/Emp.Pt.RData", sep="" )
			save( Emp.Pt, file=Filename )
			rm( Filename )
			rm( Emp.Pt )	  
			
			print( StartTime )
			print( Sys.time() )
	
		#end of if
		}
	# end of loop on runtypes
	}
		  
# end of urban function
}	
	
accessibility <- function(printthis = "ACCESSIBILITY MODELS", funcRunTypes = c("ELESNP")) {
#==========================================
print(printthis)
#==========================================
	#Load the inputs
	#Load the existing and future transportation supply
	TempInput.. <- read.csv( inputLocation( "transportation_supply.csv" ), row.names=1 )
	BaseSupply.Ma <- as.matrix( TempInput.. )
	row.names(BaseSupply.Ma) <- trim(row.names(BaseSupply.Ma))
	TempInput.. <- read.csv( inputLocation( "transportation_supply_growth.csv" ), row.names=1 )
	SupplyGrowth.Ma <- as.matrix( TempInput.. ) 
	row.names(SupplyGrowth.Ma) <- trim(row.names(SupplyGrowth.Ma))
	
	#The model iterates through each run type; when called with defualt args only "ELSNP".
	#uses the funcRunType rather than global RunType as called in two pieces, before and after adding supply
	for( runtype in funcRunTypes ) {
	
		yr <- RunTypes$RunYear [ RunTypes$RunType == runtype ]
		supply <- RunTypes$Supply [ RunTypes$RunType == runtype ]
		print( paste(runtype, yr, sep= " ") )
		RunTypeDir <- paste( "outputs/Type", runtype, sep="" )
	
		#Calculate freeway, arterial and transit supply by metropolitan area
		#===================================================================
		
		#This section and the following section (Add the freeway and transit supply data to the SynPop..)
		#are self contained and constitute the remainder of the vars needed for auto ownership
		
		StartTime <- Sys.time()
		print( "Calculating highway and transit supply" )
		print(ls())

		# Load population
		if( yr == BaseYear ) {
			Filename <- paste( RunTypeDir, "/Pop.Pt.RData", sep="" )
			Pop.Pt <- assignLoad( Filename )
			BasePop.Pt <- Pop.Pt
		}
		if( yr != BaseYear ) {
			Filename <- paste( RunTypeDir, "/Pop.Pt.RData", sep="" )
			Pop.Pt <- assignLoad( Filename )
			Filename <- "outputs/TypeE/Pop.Pt.RData"
			BasePop.Pt <- assignLoad( Filename )
		}  
		
                    # Calculate the metropolitan population growth proportion
                    if( yr == BaseYear ) {
                         MetroPop <- sum( BasePop.Pt )
                    } else {
                         MetroPop <- sum( Pop.Pt )
                         BaseMetroPop <- sum( BasePop.Pt )
                         MetroPopChange <- ( MetroPop / BaseMetroPop ) - 1
                         if( MetroPopChange < 0 ) MetroPopChange <- 0
                    }

                    # Calculate per capita freeway lane miles
                    if( yr == BaseYear | supply == "E" ) {
                         FwyLnMiCap.Ma <- 1000 * BaseSupply.Ma[ "Freeway", ] / MetroPop
                    } else {
                         FwyLnMiGrowth <- MetroPopChange * SupplyGrowth.Ma[ "Freeway", ] *
								 BaseSupply.Ma[ "Freeway", ]
                         FwyLnMi <- BaseSupply.Ma[ "Freeway", ] + FwyLnMiGrowth
						 FwyLnMiCap.Ma <- 1000 * FwyLnMi / MetroPop
                    }

				# Calculate per capita arterial lane miles
                    if( yr == BaseYear | supply == "E" ) {
                         ArtLnMiCap.Ma <- 1000 * BaseSupply.Ma[ "Arterial", ] / MetroPop
                    } else {
                         ArtLnMiGrowth <- MetroPopChange * SupplyGrowth.Ma[ "Arterial", ] *
								 BaseSupply.Ma[ "Arterial", ]
                         ArtLnMi <- BaseSupply.Ma[ "Arterial", ] + ArtLnMiGrowth
                         ArtLnMiCap.Ma <- 1000 * ArtLnMi / MetroPop
                    }

                    # Calculate per capita transit revenue miles
                    if( yr == BaseYear ) {
                         TranRevMiCap.Ma <- BaseSupply.Ma[ "Bus", ] + BaseSupply.Ma[ "Rail", ] 
                         BusRevMi.Ma <- BaseSupply.Ma[ "Bus", ] * MetroPop
                         RailRevMi.Ma <- BaseSupply.Ma[ "Rail", ] * MetroPop
                    } else {
                        #####this is a bit conufusing - should transit supply per capita be based on base year pop or future year pop? 
						if ( supply == "E" ) {
							 	TranRevMiCap.Ma <- BaseSupply.Ma[ "Bus", ] + BaseSupply.Ma[ "Rail", ]					 
								BusRevMi.Ma <- BaseSupply.Ma[ "Bus", ] * MetroPop
								RailRevMi.Ma <- BaseSupply.Ma[ "Rail", ] * MetroPop
							} else {
								BusRevMi.Ma <- BaseSupply.Ma[ "Bus", ] * MetroPop * SupplyGrowth.Ma[ "Bus", ]
								RailRevMi.Ma <- BaseSupply.Ma[ "Rail", ] * MetroPop * SupplyGrowth.Ma[ "Rail", ]
								TranRevMiCap.Ma <- (BusRevMi.Ma + RailRevMi.Ma)/MetroPop 
							}
                  	}
     
          # Save the results
          #-----------------
          Filename <- paste( RunTypeDir, "/FwyLnMiCap.Ma.RData", sep="" )
          save( FwyLnMiCap.Ma, file=Filename )
          rm( Filename )
          Filename <- paste( RunTypeDir, "/ArtLnMiCap.Ma.RData", sep="" )
          save( ArtLnMiCap.Ma, file=Filename )
          rm( Filename )
          Filename <- paste( RunTypeDir, "/TranRevMiCap.Ma.RData", sep="" )
          save( TranRevMiCap.Ma, file=Filename )
          rm( Filename )
          Filename <- paste( RunTypeDir, "/BusRevMi.Ma.RData", sep="" )
          save( BusRevMi.Ma, file=Filename )
          rm( Filename )
          Filename <- paste( RunTypeDir, "/RailRevMi.Ma.RData", sep="" )
          save( RailRevMi.Ma, file=Filename )
          rm( Filename )
		  
	#Add the freeway and transit supply data to the SynPop..
	#=======================================================
          
			# Load synpop..
			Filename <- paste( RunTypeDir, "/", runtype, ".RData", sep="" )
			SynPop.. <- assignLoad( Filename )
			
			# Make calculations
			SynPop..$Fwylnmicap <- 0
			SynPop..$Fwylnmicap <- FwyLnMiCap.Ma
			SynPop..$Tranmilescap <- 0
			SynPop..$Tranmilescap <- TranRevMiCap.Ma
			
			# Save results
			save( SynPop.., file=Filename, compress=TRUE )
			rm( SynPop.. )
			gc()

		  # Remove objects
		  #-----------------
		  rm( Pop.Pt, BasePop.Pt, FwyLnMiCap.Ma, ArtLnMiCap.Ma, TranRevMiCap.Ma, BusRevMi.Ma, RailRevMi.Ma )
			  
		  print( StartTime )
          print( Sys.time() )

	  # end of loop on runtypes
	  }
		  
#end of accessibility function
}
	  
vehicle <- function(printthis = "VEHICLE MODELS", funcRunTypes = c("ELESNP")) {
#======================
print(printthis)
#======================
#The model iterates through each run type; when called with defualt args only "ELESNP".
#uses the funcRunType rather than global RunType as called in two pieces, before and after adding supply
	
for( runtype in funcRunTypes ) {
	
	yr <- RunTypes$RunYear [ RunTypes$RunType == runtype ]
	print( paste(runtype, yr, sep= " ") )
	RunTypeDir <- paste( "outputs/Type", runtype, sep="" )
	
	StartTime <- Sys.time()
	print( "Simulating vehicle ownership" )
	print(ls())
	
		# Load synpop.. file
		Filename <- paste( RunTypeDir, "/", runtype, ".RData", sep="" )
		SynPop.. <- assignLoad( Filename )
		
		# Identify metropolitan area 
		IsMetro. <- SynPop..$Urban == 1
		  
		# Calculate vehicle ownership
		#============================
		
		# Initialize Hhvehcnt and VehPerDrvAgePop variables
		SynPop..$Hhvehcnt <- 0
		SynPop..$VehPerDrvAgePop <- 0
		# Predict ownership for metropolitan households if any exist
		if( any( IsMetro. ) ) {
			ModelVar. <- c( "Hhincttl", "Htppopdn", "Tranmilescap", "Urban", 
					"Fwylnmicap", "OnlyElderly", "DrvLevels", "DrvAgePop" )  
			MetroVehOwn_ <- predictVehOwn( SynPop..[ IsMetro., ModelVar. ],
					Model_=VehicleOwnModels_, Type="Metro" )
			rm( ModelVar. )
		}
		# Predict ownership for nonmetropolitan households if any exist
		if( any( !IsMetro. ) ) {
			ModelVar. <- c( "Hhincttl", "Htppopdn", "OnlyElderly", "DrvLevels", "DrvAgePop" )  
			NonMetroVehOwn_ <- predictVehOwn( SynPop..[ !IsMetro., ModelVar. ],
					Model_=VehicleOwnModels_, Type="NonMetro" )
			rm( ModelVar. )
		}
		# Assign values to SynPop.. and return the result
		if( any( IsMetro. ) ) {
			SynPop..$Hhvehcnt[ IsMetro. ] <- MetroVehOwn_$NumVeh
			SynPop..$VehPerDrvAgePop[ IsMetro. ] <- MetroVehOwn_$VehRatio
		}
		if( any( !IsMetro. ) ) {
			SynPop..$Hhvehcnt[ !IsMetro. ] <- NonMetroVehOwn_$NumVeh
			SynPop..$VehPerDrvAgePop[ !IsMetro. ] <- NonMetroVehOwn_$VehRatio
		}
		# Clean up
		if( exists( "MetroVehOwn_" ) ) rm( MetroVehOwn_ )
		if( exists( "NonMetroVehOwn_" ) ) rm( NonMetroVehOwn_ )
		
		# Calculate vehicle types, ages, and initial fuel economy
		#========================================================
		
		# Predict light truck ownership and vehicle ages
		#-----------------------------------------------
		# Apply vehicle type model
		ModelVar. <- c( "Hhincttl", "Htppopdn", "Urban", "Hhvehcnt", "Hhsize" ) 
		#light truck proportion is a single value in parameters_
		#fix seed as allocation involves sampling
		set.seed(1)
		SynPop..$VehType <- predictLtTruckOwn( SynPop..[ , ModelVar. ], Model_=LtTruckModels_,
				TruckProp=LtTruckProp )
		rm( ModelVar. )
		# Apply vehicle age model
		ModelVar. <- c( "IncGrp", "Hhvehcnt", "VehType" )
		#fix seed as allocation involves sampling
		set.seed(1)
		VehTypeAgeResults_ <- calcVehicleAges( SynPop..[ , ModelVar. ], VProp_=VehProp_ )
		rm( ModelVar. )
		# Add type and age model results
		SynPop..$VehType[ SynPop..$Hhvehcnt == 0 ] <- NA
		SynPop..$VehAge <- VehTypeAgeResults_$VehAge
		SynPop..$VehAge[ SynPop..$Hhvehcnt == 0 ] <- NA
		rm( VehTypeAgeResults_ )
		
		# Assign initial fuel economy
		#----------------------------
		# Assign fuel economy to vehicles
		HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
		SynPop..$VehMpg <- NA
		ModelVar. <- c( "VehType", "VehAge", "Hhvehcnt" )
		SynPop..$VehMpg[ HasVeh.Hh ] <- assignFuelEconomy( SynPop..[ HasVeh.Hh, ModelVar. ],
				VehicleMpg.Yr[,c("Auto","LtTruck")], CurrYear=yr )
		rm( ModelVar. )
		
		# Assign vehicle mileage proportions to household vehicles
		SynPop..$DvmtProp <- NA
		ModelVar. <- c( "Hhvehcnt", "Houseid" ) 
		#fix seed as allocation involves sampling
		set.seed(1)
		SynPop..$DvmtProp[ HasVeh.Hh ] <- apportionDvmt( SynPop..[ HasVeh.Hh, ],
				DP_=DvmtProp_ )
		rm( ModelVar. )
		
		# Save the outputs
		#=================
		save( SynPop.., file=Filename, compress=TRUE )
		rm( SynPop.. )
		gc()
		
	print( StartTime )
	print( Sys.time() )

	# end of loop on runtypes
	}
	
#end of vehicle models function
}

demand <- function (printthis = "TRAVEL DEMAND MODELS", funcRunTypes = c("ELESNP")) {
#============================
print(printthis) 
#============================

#The model iterates through each run type; when called with defualt args only "ELESNP".
#uses the funcRunType rather than global RunType as called in two pieces, before and after adding supply
	
for( runtype in funcRunTypes ) {
	
	yr <- RunTypes$RunYear [ RunTypes$RunType == runtype ]
	print( paste(runtype, yr, sep= " ") )
	RunTypeDir <- paste( "outputs/Type", runtype, sep="" )
	
	 StartTime <- Sys.time()
     print( "Simulating household travel" )
	 print(ls())

	 # Initialize a matrix to hold the DVMT sums by place type
     #--------------------------------------------------------------
		Filename <- paste( RunTypeDir, "/Pop.Pt.RData", sep="" )
		Pop.Pt <- assignLoad( Filename ) 
		Dvmt.Pt <- Pop.Pt * 0
		rm( Pop.Pt )

     # Simulate travel
     #=============================================
                      
		# Load SynPop.. file
		Filename <- paste( RunTypeDir, "/", runtype, ".RData", sep="" )
		SynPop.. <- assignLoad( Filename )
				
		# Identify metropolitan area
		IsMetro. <- SynPop..$Urban == 1

		  #1st DVMT calculation (no adjustment for costs)
		#==============================================

		# Calculate the average DVMT
		#---------------------------
          ModelVar. <- c( "Hhincttl", "Htppopdn", "Hhvehcnt", "Tranmilescap", "Fwylnmicap", "DrvAgePop", "Hhsize", 
               "Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64", "Age65Plus", "Urban", "BaseCostPerMi",
               "FutrCostPerMi" )
		# Assume a base and future cost of 4 cents per mile
		# so that budget constraints don't impinge on the amount of vehicle travel
     	SynPop..$BaseCostPerMi <- BaseCostPerMile
     	SynPop..$FutrCostPerMi <- BaseCostPerMile
     	SynPop..$Dvmt <- 0
     	if( any( IsMetro. ) ) {
               SynPop..$Dvmt[ IsMetro. ] <- calcAdjAveDvmt( SynPop..[ IsMetro., ModelVar. ],
                    DvmtLmModels_, "Metro", BudgetProp=BudgetProp, AnnVmtInflator=AnnVmtInflator,
                    TrnstnProp=1 )[[1]]
          }
		if( any( !IsMetro. ) ) {
               SynPop..$Dvmt[ !IsMetro. ] <- calcAdjAveDvmt( SynPop..[ !IsMetro., ModelVar. ],
                    DvmtLmModels_, "NonMetro", BudgetProp=BudgetProp, AnnVmtInflator=AnnVmtInflator,
                    TrnstnProp=1 )[[1]]
          }

		# Assign vehicle DVMT
		#====================
		
		# Assign vehicle mileage to household vehicles
		SynPop..$VehDvmt <- calcVehDvmt( SynPop..$Dvmt, SynPop..$DvmtProp )

		# Calculate fuel consumption and CO2e production
		#=============================================================

		# Calculate average fuel CO2e per gallon
     	#---------------------------------------
		AveFuelCo2e. <- calcAveFuelCo2e( yr, Fuels..=Fuel.Ty, Co2..=Fuel_Co2.Ft,
			MjPerGallon=121, OutputType="MetricTons" )

		# Calculate consumption and production at a household level
		#----------------------------------------------------------

		ModelVar. <- c( "Hhvehcnt", "VehMpg", "VehType", "VehDvmt", "Dvmt" ) 
		FuelCo2e_ <- calcVehFuelCo2( SynPop..[ , ModelVar. ], AveFuelCo2e.=AveFuelCo2e. )
		SynPop..$FuelGallons <- FuelCo2e_$FuelGallons
		SynPop..$FuelCo2e <- FuelCo2e_$FuelCo2e
		rm( AveFuelCo2e., FuelCo2e_ )
		rm( ModelVar. )
		gc()

		#Calculate household travel costs
		#================================

		# Calculate all household costs
		#------------------------------

		# assume zero parking cost at this point
		SynPop..$DailyPkgCost <- 0
		# gathers cost parameters into costs.
		Costs. <- c(FuelCost,GasTax,0,0)
		names(Costs.) <- c("FuelCost","GasTax","CarbonCost","VmtCost")
		ModelVar. <- c( "FuelGallons", "FuelCo2e", "Dvmt", "DailyPkgCost", "Hhvehcnt" ) 
		Costs_ <- calcCosts( SynPop..[ , ModelVar. ], Costs. )
		SynPop..$FutrCostPerMi <- Costs_$FutrCostPerMi
		rm( Costs_, ModelVar. )
		gc()

	      # Calculate DVMT with new costs and reallocate to vehicles
          #=========================================================
          
          # Recalculate DVMT
          #-----------------
          PrevDvmt.Hh <- SynPop..$Dvmt
          ModelVar. <- c( "Hhincttl", "Htppopdn", "Hhvehcnt", "Tranmilescap", "Fwylnmicap", "DrvAgePop", 
			"Hhsize", "Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64", "Age65Plus", 
			"Urban", "BaseCostPerMi", "FutrCostPerMi" )
          if( any( IsMetro. ) ) {
               SynPop..$Dvmt[ IsMetro. ] <- calcAdjAveDvmt( SynPop..[ IsMetro., ModelVar. ], DvmtLmModels_,
                    "Metro", BudgetProp=BudgetProp, AnnVmtInflator=AnnVmtInflator, TrnstnProp=1 )[[1]]
          }
          if( any( !IsMetro. ) ) {
               SynPop..$Dvmt[ !IsMetro. ] <- calcAdjAveDvmt( SynPop..[ !IsMetro., ModelVar. ], 
			DvmtLmModels_, "NonMetro", BudgetProp=BudgetProp, AnnVmtInflator=AnnVmtInflator, 
			TrnstnProp=1 )[[1]]
          }

          # Split adjusted DVMT among vehicles
          #-----------------------------------
          DvmtAdjFactor.Hh <- SynPop..$Dvmt / PrevDvmt.Hh
          HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
          SynPop..$VehDvmt[ HasVeh.Hh ] <- mapply( "*", SynPop..$VehDvmt[ HasVeh.Hh ], DvmtAdjFactor.Hh[ HasVeh.Hh ])
          rm( DvmtAdjFactor.Hh, HasVeh.Hh )
          gc()

          # Sum up DVMT by development type
          #================================

          Dvmt.Pt[Pt] <- tapply( SynPop..$Dvmt, SynPop..$PlaceType, sum )[Pt]
		  Dvmt.Pt[is.na(Dvmt.Pt)] <- 0
		  
          # Save the outputs
          #=================
          
          save( SynPop.., file=Filename, compress=TRUE )
          rm( SynPop.. )
          gc()
     
     # Save the DVMT tabulation by place type
     #========================================================
     Filename <- paste( RunTypeDir, "/", "Dvmt.Pt", ".RData", sep="" )
     save( Dvmt.Pt, file=Filename )
	 rm( Dvmt.Pt )
     
     print( StartTime )
     print( Sys.time() )

	 # end of loop on runtype
	}
	 

	#CALCULATE HEAVY TRUCK VMT 
	#=========================
	
	#Load the Base year DVMT parameters
	BaseDvmtParm.. <- read.csv( inputLocation( "base_vmt.csv" ), row.names=1 )
	BaseLtVehDvmt.Ma <- BaseDvmtParm..$LtVehDvmt * 1000
	#Load the bus and truck vmt proportions and functional class splits
	TruckBusDvmtParm.. <- read.csv( inputLocation( "truck_bus_vmt.csv" ), as.is = TRUE )
	PropVmt <- TruckBusDvmtParm..[ TruckBusDvmtParm..$Value == "TruckVmt","PropVmt" ]
	BaseTruckDvmt <- BaseLtVehDvmt.Ma / (1 - PropVmt) * PropVmt
	
	for( runtype in funcRunTypes ) {
		
		yr <- RunTypes$RunYear [ RunTypes$RunType == runtype ]
		print( paste(runtype, yr, sep= " ") )
		RunTypeDir <- paste( "outputs/Type", runtype, sep="" )
		
		StartTime <- Sys.time()
		print( "Calculating Heavy Truck VMT" )
		print(ls())
		
		# Load data summaries
		#--------------------
		# Load base year income
		if( yr == BaseYear ) {
			Filename <- paste( RunTypeDir, "/Inc.Pt.RData", sep="" )
			Inc.Pt <- assignLoad( Filename )
			BaseInc.Pt <- Inc.Pt
		}
		if( yr != BaseYear ) {
			Filename <- paste( RunTypeDir, "/Inc.Pt.RData", sep="" )
			Inc.Pt <- assignLoad( Filename )
			Filename <- "outputs/TypeE/Inc.Pt.RData"
			BaseInc.Pt <- assignLoad( Filename )
		}
		
		# Calculate truck VMT by metropolitan area
		#-----------------------------------------
		# Calculate growth in total percapita income from base year
		# Calculate change in income
		BaseInc <- sum( BaseInc.Pt )
		FutrInc <- sum( Inc.Pt )
		IncGrowth <- FutrInc / BaseInc
		# Calculate truck DVMT
		TruckDvmt.Ma <- IncGrowth * TruckVmtGrowthMultiplier * BaseTruckDvmt
		
		# Save the results
		#-----------------
		Filename <- paste( RunTypeDir, "/TruckDvmt.Ma.RData", sep="" )
		save( TruckDvmt.Ma, file=Filename )
		rm( Filename )
		
		#remove objects
		#--------------
		rm( Inc.Pt, BaseInc.Pt, TruckDvmt.Ma, BaseInc, FutrInc, IncGrowth )
		
		print( StartTime )
		print( Sys.time() )
		
		# end of loop on runtype
	}

#end of demand function
}

induced <- function () {
#==================================
print("INDUCED GROWTH AND TRAVEL") 
#==================================

	runtype <- "FLFSNP"
	yr <- RunTypes$RunYear [ RunTypes$RunType == runtype ]
	print( paste(runtype, yr, sep= " ") )
	RunTypeDir <- paste( "outputs/Type", runtype, sep="" )
	
	#Copy SynPop and then the Emp, Pop and Inc summary tabulations
	file.copy( "outputs/TypeELESNP/ELESNP.RData", paste(RunTypeDir,"/FLFSNP.RData",sep=""), overwrite=TRUE )
	file.copy( "outputs/TypeELESNP/Emp.Pt.RData", RunTypeDir, overwrite=TRUE )
	file.copy( "outputs/TypeELESNP/Inc.Pt.RData", RunTypeDir, overwrite=TRUE )
	file.copy( "outputs/TypeELESNP/Pop.Pt.RData", RunTypeDir, overwrite=TRUE )
			
	#Calculate effects of changed transportation supply
	accessibility(" ", "FLFSNP")
	vehicle(" ", "FLFSNP")
	demand(" ", "FLFSNP")
	
	#PLACE TYPE VMT ADJUSTMENT
	#==========================================
	#This component of the model calculates an adjustment to VMT per capita, vehicle trips, transit trips, and walking for population and employment based on the land use of the area the household/firm is located in
	
	# Load inputs
	# Load the base year population and employment
	BasePop.Pt <- assignLoad( "outputs/TypeE/Pop.Pt.RData" )
	BaseEmp.Pt <- assignLoad( "outputs/TypeE/Emp.Pt.RData" )
	# Load the future year population and employment
	Pop.Pt <- assignLoad( "outputs/TypeFLFSNP/Pop.Pt.RData" )
	Emp.Pt <- assignLoad( "outputs/TypeFLFSNP/Emp.Pt.RData" )
	#Load persons trips file
	TripsPerCapita <- read.csv( inputLocation( "trips_per_cap.csv" ), row.names=1 )
	
	# Calculate Pop and Emp growth
	PopGrowth.Pt <- Pop.Pt - BasePop.Pt
	EmpGrowth.Pt <- Emp.Pt - BaseEmp.Pt
	PlaceTypeGrowth.Pt <- rbind(PopGrowth.Pt,EmpGrowth.Pt)
	
	#Percentage Change in D Value Compared to Regional Average 
	#(Negative Value indicates D's are worse than regional average, Positive value means D is better than regional average)
	PlaceTypeValues.PtD <- PlaceTypeValues.PtD - 1	
	
	#Apply Elasticities to Determine Change in VMT Per Place Type
	PlaceTypeDDiscount.Pt <- matrix(0,nrow=4,ncol=13,dimnames=list(colnames(PlaceTypeElasticities.D),Pt))
	for(i in 1:4){
		PlaceTypePercentChange.PtD <- apply(PlaceTypeValues.PtD,1,"*",PlaceTypeElasticities.D[,i])
		PlaceTypePercentChange.PtD <- PlaceTypePercentChange.PtD + 1
		PlaceTypeDDiscount.Pt[i,] <- apply(PlaceTypePercentChange.PtD,2,prod)
	}
	
	# Calculate just regional accessibility percent change
	AccessiblityPercentChange.Pt <- -1 * PlaceTypeValues.PtD[,"Regional_Accessibility"] * PlaceTypeElasticities.D["Regional_Accessibility","VMT"]
	
	# Load SynPop.. file
	Filename <- paste( RunTypeDir, "/", runtype, ".RData", sep="" )
	SynPop.. <- assignLoad( Filename )
	
	#Calc Income by PlaceTpye and Group
	IncGrp.Pt <- table(SynPop..$IncGrp, SynPop..$PlaceType)

	# Determine VMT, Vehicle Trips and Transit Trips per Capita per total of people and jobs
	VMTPerCapita <- sum(SynPop..$Dvmt)/(sum(Pop.Pt) + sum(Emp.Pt))	
	TripsPerCapita <- TripsPerCapita * sum(Pop.Pt)/(sum(Pop.Pt) + sum(Emp.Pt))
	
	#Determine Growth in VMT, Vehicle Trips, Transit Trips, Job Access, and Walking Per Place Type
	#Growth in VMT Per Place Type For Population= (Population) X (VMT Per Capita) X Total D Discount
	#Growth in VMT Per Place Type For Employment= (Employment) X (VMT Per Capita) X Total D Discount
	PlaceTypeVMTGrowth.Pt <- rowSums(apply(PlaceTypeGrowth.Pt,1,"*",PlaceTypeDDiscount.Pt["VMT",] - 1) * VMTPerCapita)
  VehicleTrips.Pt <- rowSums(apply(PlaceTypeGrowth.Pt,1,"*",PlaceTypeDDiscount.Pt["VehicleTrips",]) * TripsPerCapita["Auto",])
	TransitTrips.Pt <- rowSums(apply(PlaceTypeGrowth.Pt,1,"*",PlaceTypeDDiscount.Pt["TransitTrips",]) * TripsPerCapita["Transit",])
	Walking.Ma <- sum(apply(PlaceTypeGrowth.Pt,1,"*",PlaceTypeDDiscount.Pt["Walking",] - 1))/sum(PlaceTypeGrowth.Pt)
	Access.Ma <- sum(apply(PlaceTypeGrowth.Pt,1,"*",AccessiblityPercentChange.Pt))/sum(PlaceTypeGrowth.Pt)
	Equity.Ig <- colSums(apply(IncGrp.Pt[,Pt],1,"*",AccessiblityPercentChange.Pt))/rowSums(IncGrp.Pt)	
	
	#Calculate factors to adjust hh Dvmt by Place Type
	Dvmt.Pt <- Pop.Pt * 0	
	Dvmt.Pt[Pt] <- tapply( SynPop..$Dvmt, SynPop..$PlaceType, sum )[Pt]
	DvmtAdj.Pt <- PlaceTypeVMTGrowth.Pt / Dvmt.Pt + 1
	DvmtAdj.Pt[is.na(DvmtAdj.Pt)] <- 1
  DvmtAdj.Pt[DvmtAdj.Pt<0.01] <- 0.01
		
	#Update Dvmt
	SynPop..$DvmtPtAdj <- DvmtAdj.Pt [ as.character(SynPop..$PlaceType) ]	 #need as.character else it uses factor levels to index into DvmtAdj.Pt
	SynPop..$Dvmt <- SynPop..$Dvmt * SynPop..$DvmtPtAdj 
	
	#Recalculate Dvmt tabulation with adjusted Dvmt
	Dvmt.Pt[Pt] <- tapply( SynPop..$Dvmt, SynPop..$PlaceType, sum )[Pt]
	Dvmt.Pt[is.na(Dvmt.Pt)] <- 0
	
	# Save the outputs
	#==================
	save( SynPop.., file=Filename, compress=TRUE )
	rm( SynPop.. )
	gc()
	
	# Save Dvmt
	Filename <- paste( RunTypeDir, "/", "Dvmt.Pt", ".RData", sep="" )
	save( Dvmt.Pt, file=Filename )
	rm( Dvmt.Pt )

	# Save other performance metrics - vehicle trips, transit trips, accissibility, and walking 
	#==========================================================================================
	#Policy Adjusted Travel Demand	VehicleTrips.Pt.RData	Daily Vehicle Trips	Direct Travel Impacts
	#Policy Adjusted Travel Demand	TransitTrips.Pt.RData	Daily Transit Trips	Direct Travel Impacts
	#Performance Metrics	Access.Pt.Rdata	Regional Accessibility	Location Impacts
	save( Access.Ma, file=paste(RunTypeDir,"/Access.Ma.RData",sep="" ))
	save( Equity.Ig, file=paste(RunTypeDir,"/Equity.Ig.RData",sep="" ))
	save( VehicleTrips.Pt, file=paste(RunTypeDir,"/VehicleTrips.Pt.RData",sep="" ))
	save( TransitTrips.Pt, file=paste(RunTypeDir,"/TransitTrips.Pt.RData",sep="" ))
	save( Walking.Ma, file=paste(RunTypeDir,"/Walking.Ma.RData",sep="" ))
	
	#Calculate congestion including all induced demand effects
	#=============================================================

	congestion(" ", "FLFSNP")

	#end of induced function
}	

policyvmt <- function () {
#=====================================
print("POLICY ADJUSTED TRAVEL DEMAND") 
#=====================================

	#POLICY ADJUSTMENTS TO VMT 
	#=========================	

	#This section adjusts the VMT for the policies entered for the scenario. 
	#Starts from the model status at this point, i.e. through the end of heavy truck VMT for the
	#RunType = FLFSNP
	#First copy the files from that runtype over to the FLFSYP folder
	#Copy SynPop.. and then the Pop and Inc summary tabulations
	runtype <- "FLFSYP"
	yr <- RunTypes$RunYear [ RunTypes$RunType == runtype ]
	print( paste(runtype, yr, sep= " ") )
	RunTypeDir <- paste( "outputs/Type", runtype, sep="" )
	
	file.copy( "outputs/TypeFLFSNP/FLFSNP.RData", paste(RunTypeDir,"/", runtype, ".RData",sep=""), overwrite=TRUE )
	file.copy( "outputs/TypeFLFSNP/Emp.Pt.RData", RunTypeDir, overwrite=TRUE )
	file.copy( "outputs/TypeFLFSNP/Inc.Pt.RData", RunTypeDir, overwrite=TRUE )
	file.copy( "outputs/TypeFLFSNP/Pop.Pt.RData", RunTypeDir, overwrite=TRUE )
	file.copy( "outputs/TypeFLFSNP/FwyLnMiCap.Ma.RData", RunTypeDir, overwrite=TRUE )
	file.copy( "outputs/TypeFLFSNP/ArtLnMiCap.Ma.RData", RunTypeDir, overwrite=TRUE )
	file.copy( "outputs/TypeFLFSNP/TranRevMiCap.Ma.RData", RunTypeDir, overwrite=TRUE )
	file.copy( "outputs/TypeFLFSNP/BusRevMi.Ma.RData", RunTypeDir, overwrite=TRUE )
	file.copy( "outputs/TypeFLFSNP/RailRevMi.Ma.RData", RunTypeDir, overwrite=TRUE )
	file.copy( "outputs/TypeFLFSNP/Dvmt.Pt.RData", RunTypeDir, overwrite=TRUE )
	file.copy( "outputs/TypeFLFSNP/TruckDvmt.Ma.RData", RunTypeDir, overwrite=TRUE )
	file.copy( "outputs/TypeFLFSNP/VehicleTrips.Pt.RData", RunTypeDir, overwrite=TRUE )
	file.copy( "outputs/TypeFLFSNP/TransitTrips.Pt.RData", RunTypeDir, overwrite=TRUE )
	file.copy( "outputs/TypeFLFSNP/Walking.Ma.RData", RunTypeDir, overwrite=TRUE )
	file.copy( "outputs/TypeFLFSNP/Access.Ma.RData", RunTypeDir, overwrite=TRUE )
	file.copy( "outputs/TypeFLFSNP/Equity.Ig.RData", RunTypeDir, overwrite=TRUE )
	
	#Now calculate and apply all of the policy adjustments to VMT
	# Load input data
	# Load vmt charge data
	TempInput.. <- read.csv( inputLocation( "vmt_charge.csv" ), as.is = TRUE )
	VmtCharge <- TempInput..$value
	
	# Load parking charge data
	ParkingCosts.Yr <- read.csv( inputLocation( "parking_growth.csv" ), row.names=1 )
	ParkingCosts.Yr <- as.matrix(ParkingCosts.Yr)
	
	#Load vehicle operating cost growth data
	TempInput.. <- read.csv( inputLocation( "auto_op_cost_growth.csv" ), as.is = TRUE )
	AutoCostGrowth <- TempInput..$value
	
	#Load commute options assumptions
	TDMPrograms <- read.csv( inputLocation( "commute_options.csv" ))
	TDMPrograms$TDMProgram <- trim(TDMPrograms$TDMProgram)
	TDMPrograms$DataItem <- trim(TDMPrograms$DataItem)
	
	#Load bicycling/light vehicle assumptions 
	LightVehicles.. <- read.csv( inputLocation( "light_vehicles.csv" ), row.names=1)
	row.names(LightVehicles..) <- trim(row.names(LightVehicles..))
	
	# Load SynPop.. file
	Filename <- paste( RunTypeDir, "/", runtype, ".RData", sep="" )
	SynPop.. <- assignLoad( Filename )	
	
	# Identify metropolitan area
	IsMetro. <- SynPop..$Urban == 1
	
	# Apply travel demand management policies
	#========================================
	#Evaluate effectiveness
	
	#Ridesharing
	TDMRidesharingEffect.At <- TDMRidesharing * TDMPrograms$DataValue[TDMPrograms$DataItem == "RidesharingParticipation"]
	
	#Transit
	if(length(TDMTransitLevels[TDMTransitLevels > TDMPrograms$DataValue[TDMPrograms$DataItem == "TransitSubsidyLevel"]]) > 0) {
		subsidylevel <- rownames(TDMTransitLevels)[TDMTransitLevels == min(TDMTransitLevels[TDMTransitLevels > TDMPrograms$DataValue[TDMPrograms$DataItem == "TransitSubsidyLevel"]])]
	} else {
		subsidylevel <- rownames(TDMTransitLevels)[TDMTransitLevels == max(TDMTransitLevels)]
	}
	
	subsidyhigh <- which(rownames(TDMTransitLevels) == subsidylevel)
	subsidylow <- subsidyhigh - 1
	subsidyratio <- (TDMTransitLevels[subsidyhigh,] - TDMPrograms$DataValue[TDMPrograms$DataItem == "TransitSubsidyLevel"])/(TDMTransitLevels[subsidyhigh,] - TDMTransitLevels[subsidylow,])
	TDMTransitEffect.At <- TDMTransit[,subsidyhigh] - subsidyratio * (TDMTransit[,subsidyhigh] - TDMTransit[,subsidylow]) 
	
	#Work Schedules
	#Schedule 980
	if(length(TDMWorkScheduleLevels[TDMWorkScheduleLevels > TDMPrograms$DataValue[TDMPrograms$DataItem == "Schedule980Participation"]]) > 0) {
		participationlevel <- rownames(TDMWorkScheduleLevels)[TDMWorkScheduleLevels == min(TDMWorkScheduleLevels[TDMWorkScheduleLevels > TDMPrograms$DataValue[TDMPrograms$DataItem == "Schedule980Participation"]])]
	} else {
		participationlevel <- rownames(TDMWorkScheduleLevels)[TDMWorkScheduleLevels == max(TDMWorkScheduleLevels)]
	}
	
	participationhigh <- which(rownames(TDMWorkScheduleLevels) == participationlevel)
	participationlow <- participationhigh - 1
	participationratio <- (TDMWorkScheduleLevels[participationhigh,] - TDMPrograms$DataValue[TDMPrograms$DataItem == "Schedule980Participation"])/(TDMWorkScheduleLevels[participationhigh,] - TDMWorkScheduleLevels[participationlow,])
	TDMSchedule980Effect <- TDMWorkSchedule["Schedule980",participationhigh] - participationratio * (TDMWorkSchedule["Schedule980",participationhigh] - TDMWorkSchedule["Schedule980",participationlow])
	
	#Schedule 440
	if(length(TDMWorkScheduleLevels[TDMWorkScheduleLevels > TDMPrograms$DataValue[TDMPrograms$DataItem == "Schedule440Participation"]]) > 0) {
		participationlevel <- rownames(TDMWorkScheduleLevels)[TDMWorkScheduleLevels == min(TDMWorkScheduleLevels[TDMWorkScheduleLevels > TDMPrograms$DataValue[TDMPrograms$DataItem == "Schedule440Participation"]])]
	} else {
		participationlevel <- rownames(TDMWorkScheduleLevels)[TDMWorkScheduleLevels == max(TDMWorkScheduleLevels)]
	}
	
	participationhigh <- which(rownames(TDMWorkScheduleLevels) == participationlevel)
	participationlow <- participationhigh - 1
	participationratio <- (TDMWorkScheduleLevels[participationhigh,] - TDMPrograms$DataValue[TDMPrograms$DataItem == "Schedule440Participation"])/(TDMWorkScheduleLevels[participationhigh,] - TDMWorkScheduleLevels[participationlow,])
	TDMSchedule440Effect <- TDMWorkSchedule["Schedule440",participationhigh] - participationratio * (TDMWorkSchedule["Schedule440",participationhigh] - TDMWorkSchedule["Schedule440",participationlow])
	
	#Telecommute 1.5 Days
	if(length(TDMWorkScheduleLevels[TDMWorkScheduleLevels > TDMPrograms$DataValue[TDMPrograms$DataItem == "Telecommute1.5DaysParticipation"]]) > 0) {
		participationlevel <- rownames(TDMWorkScheduleLevels)[TDMWorkScheduleLevels == min(TDMWorkScheduleLevels[TDMWorkScheduleLevels > TDMPrograms$DataValue[TDMPrograms$DataItem == "Telecommute1.5DaysParticipation"]])]
	} else {
		participationlevel <- rownames(TDMWorkScheduleLevels)[TDMWorkScheduleLevels == max(TDMWorkScheduleLevels)]
	}
	
	participationhigh <- which(rownames(TDMWorkScheduleLevels) == participationlevel)
	participationlow <- participationhigh - 1
	participationratio <- (TDMWorkScheduleLevels[participationhigh,] - TDMPrograms$DataValue[TDMPrograms$DataItem == "Telecommute1.5DaysParticipation"])/(TDMWorkScheduleLevels[participationhigh,] - TDMWorkScheduleLevels[participationlow,])
	TDMTelecommute1.5DaysEffect <- TDMWorkSchedule["Telecommute1.5Days",participationhigh] - participationratio * (TDMWorkSchedule["Telecommute1.5Days",participationhigh] - TDMWorkSchedule["Telecommute1.5Days",participationlow])
	
	#Total Effectiveness
	TDMScheduleEffect <- TDMSchedule980Effect + TDMSchedule440Effect + TDMTelecommute1.5DaysEffect
	
	#Vanpooling
	TDMVanpoolEffect <- sum(TDMVanpooling * TDMPrograms$DataValue[TDMPrograms$TDMProgram == "Vanpooling"])
	
	#Total effect by Area Type
	TDMEffect.At <- TDMRidesharingEffect.At + TDMTransitEffect.At + TDMScheduleEffect + TDMVanpoolEffect
	#Convert to effect on total VMT using proportion of VMT for work trips 
	TDMEffect.At <- TDMEffect.At * WorkVmtProp
	row.names(TDMEffect.At) <- c("Rur","Sub","CIC","UC")
	
	#Create a list of reductions for each household
	TdmAdjFactor.Hh <- 1 - TDMEffect.At[ SynPop..$AreaType, ]  
	
	# Calculate the light vehicle adjustment factor
	#==============================================
	# Predict light vehicle ownership
	LtVehOwn.Hh <- rep( 0, nrow( SynPop.. ) )
	###CS Note this is problematic as actual density not calculated - need to use 5D density adjustment from average by place type
	SynPop..$LogDen <- log( SynPop..$Htppopdn )                 
	ModelVar. <- c( "LogDen", "Hhsize", "Hhincttl", "Age15to19", "Age20to29", "Age30to54", 
			"Age55to64", "Age65Plus", "VehPerDrvAgePop", "DrvAgePop" ) 
	#fix seed as allocation involves sampling
	set.seed(1)
	if( any( IsMetro. ) ) {
		LtVehOwn.Hh[ IsMetro. ] <- predictLightVehicles( SynPop..[ IsMetro., ModelVar. ],
				LtVehOwnModels_=LtVehOwnModels_, Type="Metro",
				TargetProp=LightVehicles..["TargetProp",] )
	}
	if( any( !IsMetro. ) ) {
		LtVehOwn.Hh[ !IsMetro. ] <- predictLightVehicles( SynPop..[ !IsMetro., ModelVar. ],
				LtVehOwnModels_=LtVehOwnModels_, Type="NonMetro",
				TargetProp=LightVehicles..["TargetProp",] )
	}
	SynPop..$LtVehCnt <- LtVehOwn.Hh
	rm( LtVehOwn.Hh, ModelVar. )
	SynPop..$LogDen <- NULL
	
	# Predict light vehicle DVMT
	#---------------------------
	LtVehDvmt.Hh <- SynPop..$Dvmt
	###CS Note this is problematic as actual density not calculated - need to use 5D density adjustment from average by place type
	SynPop..$LogDen <- log( SynPop..$Htppopdn )
	SynPop..$LogSize <- log( SynPop..$Hhsize )
	SynPop..$LogDvmt <- log( SynPop..$Dvmt )
	ModelVar. <- c( "Hhincttl", "LogDen", "LogSize", "Urban", "LogDvmt", "Dvmt", "LtVehCnt",
			"DrvAgePop" )
	#fix seed as allocation involves sampling
	set.seed(1)
	if( any( IsMetro. ) ) {
		LtVehDvmt.Hh[ IsMetro. ] <- calcLtVehDvmt( SynPop..[ IsMetro., ModelVar. ], 
				AveSovPropModels_, Threshold=LightVehicles..["Threshold",],
				PropSuitable=LightVehicles..["PropSuitable",], Sharing=FALSE )
	}
	if( any( !IsMetro. ) ) {
		LtVehDvmt.Hh[ !IsMetro. ] <- calcLtVehDvmt( SynPop..[ !IsMetro., ModelVar. ], 
				AveSovPropModels_, Threshold=LightVehicles..["Threshold",],
				PropSuitable=LightVehicles..["PropSuitable",], Sharing=FALSE )
	}
	# Calculate adjustment factor
	LtVehAdjFactor.Hh <- ( SynPop..$Dvmt - LtVehDvmt.Hh ) / SynPop..$Dvmt
	LtVehAdjFactor.Hh[ SynPop..$Dvmt == 0 ] <- 1
	
	# Calculate overall adjustment factor and apply to adjust DVMT
	#-------------------------------------------------------------
	TdmLtVehAdjFactor.Hh <- TdmAdjFactor.Hh * LtVehAdjFactor.Hh
	SynPop..$TdmLtVehAdjFactor <- TdmLtVehAdjFactor.Hh
	SynPop..$TdmAdjFactor <- TdmAdjFactor.Hh
	SynPop..$LtVehAdjFactor.Hh <- LtVehAdjFactor.Hh
	SynPop..$Dvmt <- SynPop..$Dvmt * TdmLtVehAdjFactor.Hh
	rm( TdmAdjFactor.Hh, LtVehAdjFactor.Hh, TdmLtVehAdjFactor.Hh, ModelVar. )
	SynPop..$LogDen <- NULL
	SynPop..$LogSize <- NULL
	SynPop..$LogDvmt <- NULL

	#Apply parking model to identify parkers and calculate daily parking costs
	#=========================================================================
	SynPop..$DailyPkgCost <- 0
	SynPop..$CashOutIncAdj <- 0
	ModelVar. <- c( "DrvAgePop", "Houseid", "Dvmt", "Hhvehcnt" )
	#fix seed as model involves sampling
	set.seed(1)
	if( any( IsMetro. ) ) {
		Parkers_ <- idPayingParkers( SynPop..[ IsMetro., ModelVar. ],
				PropWrkPkg=ParkingCosts.Yr[yr,"PropWrkPkg"],
				PropWrkChrgd=ParkingCosts.Yr[yr,"PropWrkChrgd"],
				PropCashOut=ParkingCosts.Yr[yr,"PropCashOut"],
				PropOthChrgd=ParkingCosts.Yr[yr,"PropOthChrgd"],
				PkgCost=ParkingCosts.Yr[yr,"PkgCost"],
				PropWrkTrav=0.22, WrkDaysPerYear=260 )
		PkgCosts_ <- calcParkCostAdj( SynPop..[ IsMetro., ModelVar. ], Parkers_ )
		SynPop..$DailyPkgCost[ IsMetro. ] <- PkgCosts_$DailyPkgCost
		SynPop..$CashOutIncAdj[ IsMetro. ] <- PkgCosts_$CashOutIncAdj
		SynPop..$Hhincttl[ IsMetro. ] <- SynPop..$Hhincttl[ IsMetro. ] + PkgCosts_$CashOutIncAdj
		rm( Parkers_, PkgCosts_ )
	}
	rm( ModelVar. )
	gc()
	
	#Calculate household travel costs
	#================================
	
	#Gather cost parameters into costs.
	Costs. <- c(FuelCost,GasTax,0,VmtCharge)
	names(Costs.) <- c("FuelCost","GasTax","CarbonCost","VmtCost")
	
	#Apply auto operating cost growth
	#This applies to FuelCost and Gas Tax only
	#It does not apply to the separate policy inputs of VmtCost and Carbon Cost
	#Note that CarbonCost is not currently used in the SHRP2C16 model
	Costs.[c("FuelCost","GasTax")] <- Costs.[c("FuelCost","GasTax")] * AutoCostGrowth
	
	ModelVar. <- c( "FuelGallons", "FuelCo2e", "Dvmt", "DailyPkgCost", "Hhvehcnt" ) 
	Costs_ <- calcCosts( SynPop..[ , ModelVar. ], Costs. )
	SynPop..$FutrCostPerMi <- Costs_$FutrCostPerMi
	rm( Costs_, ModelVar. )
	gc()
	
	# Calculate DVMT with new costs 
	#==============================
	#Save starting Dvmt for application of adjustments to vehicles
	PrevDvmt.Hh <- SynPop..$Dvmt

	ModelVar. <- c( "Hhincttl", "Htppopdn", "Hhvehcnt", "Tranmilescap", "Fwylnmicap", "DrvAgePop", 
			"Hhsize", "Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64", "Age65Plus", 
			"Urban", "BaseCostPerMi", "FutrCostPerMi" )
	if( any( IsMetro. ) ) {
		SynPop..$Dvmt[ IsMetro. ] <- calcAdjAveDvmt( SynPop..[ IsMetro., ModelVar. ], DvmtLmModels_,
				"Metro", BudgetProp=BudgetProp, AnnVmtInflator=AnnVmtInflator, TrnstnProp=1 )[[1]]
	}
	if( any( !IsMetro. ) ) {
		SynPop..$Dvmt[ !IsMetro. ] <- calcAdjAveDvmt( SynPop..[ !IsMetro., ModelVar. ], 
				DvmtLmModels_, "NonMetro", BudgetProp=BudgetProp, AnnVmtInflator=AnnVmtInflator, 
				TrnstnProp=1 )[[1]]
	}
	
	# Adjust for urban form and TDM
	#===============================
	SynPop..$Dvmt <- SynPop..$Dvmt * SynPop..$DvmtPtAdj
	SynPop..$Dvmt <- SynPop..$Dvmt * SynPop..$TdmLtVehAdjFactor
	
	# Split adjusted DVMT among vehicles 
	#===================================
	DvmtAdjFactor.Hh <- SynPop..$Dvmt / PrevDvmt.Hh
	HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
	SynPop..$VehDvmt[ HasVeh.Hh ] <- mapply( "*", SynPop..$VehDvmt[ HasVeh.Hh ], DvmtAdjFactor.Hh[ HasVeh.Hh ])
	rm( DvmtAdjFactor.Hh, HasVeh.Hh )
	gc()
	
	# Sum up DVMT by place type
	#================================
	Dvmt.Pt <- assignLoad( "outputs/TypeFLFSYP/Dvmt.Pt.RData" )
	Dvmt.Pt[Pt] <- tapply( SynPop..$Dvmt, SynPop..$PlaceType, sum )[Pt]
	Dvmt.Pt[is.na(Dvmt.Pt)] <- 0
	
	# Save the outputs
	#=================
	Filename <- paste( RunTypeDir, "/", runtype, ".RData", sep="" )
	save( SynPop.., file=Filename, compress=TRUE )
	rm( SynPop.. )
	gc()
	
	# performance metrics
	# Policy Adjusted Travel Demand	Dvmt.Pt.RData	Daily Vehicle Miles Traveled	Direct Travel Impacts
	# Save the DVMT tabulation by place type
	#========================================================
	Filename <- paste( RunTypeDir, "/", "Dvmt.Pt", ".RData", sep="" )
	save( Dvmt.Pt, file=Filename )
	rm( Dvmt.Pt )
	
	#costs data for final cost calculations
	Filename <- paste( RunTypeDir, "/", "Costs", ".RData", sep="" )
	save( Costs., file=Filename )
	rm( Costs. )
	
	#Copy performance metrics to outputs folder
	file.copy( "outputs/TypeFLFSYP/Emp.Pt.RData", "outputs", overwrite=TRUE )
	file.copy( "outputs/TypeFLFSYP/Inc.Pt.RData", "outputs", overwrite=TRUE )
	file.copy( "outputs/TypeFLFSYP/Pop.Pt.RData", "outputs", overwrite=TRUE )
	file.copy( "outputs/TypeFLFSYP/Dvmt.Pt.RData", "outputs", overwrite=TRUE )
	file.copy( "outputs/TypeFLFSYP/VehicleTrips.Pt.RData", "outputs", overwrite=TRUE )
	file.copy( "outputs/TypeFLFSYP/TransitTrips.Pt.RData", "outputs", overwrite=TRUE )
	file.copy( "outputs/TypeFLFSYP/Walking.Ma.RData", "outputs", overwrite=TRUE )
	file.copy( "outputs/TypeFLFSYP/Access.Ma.RData", "outputs", overwrite=TRUE )
	file.copy( "outputs/TypeFLFSYP/Equity.Ig.RData", "outputs", overwrite=TRUE )
	
#end of policyvmt function
}


congestion <- function (printthis = "CONGESTION", funcRunTypes = c("ELESNP")) {
#======================
print(printthis) 
#======================
	#Load the inputs
	#Load the its file
	TempInput.. <- read.csv( inputLocation( "its.csv" ), as.is = TRUE )
	ITS.Yr <- TempInput..$ITS
	names( ITS.Yr ) <- TempInput..$Year
	
	#Load the Base year DVMT parameters
	BaseDvmtParm.. <- read.csv( inputLocation( "base_vmt.csv" ), row.names=1 )
	
	#Load the bus and truck vmt proportions and functional class splits
	TruckBusDvmtParm.. <- read.csv( inputLocation( "truck_bus_vmt.csv" ), as.is = TRUE )
	TruckBusDvmtParm..$Value <- trim(TruckBusDvmtParm..$Value)
	BusVmt.Fc <- TruckBusDvmtParm..[ TruckBusDvmtParm..$Value == "BusVmt",c("Fwy","Art","Other") ]
	#row.names(BusVmt.Fc) <- TruckBusDvmtParm..$Area[ TruckBusDvmtParm..$Value == "BusVmt"]
	TruckVmt.Fc <- TruckBusDvmtParm..[ TruckBusDvmtParm..$Value == "TruckVmt",c("Fwy","Art","Other") ]
	#row.names(TruckVmt.Fc) <- TruckBusDvmtParm..$Area[ TruckBusDvmtParm..$Value == "TruckVmt"]
	
#The model iterates through each run type; when called with defualt args just "ELESNP".
#uses the funcRunType rather than global RunType as called in three pieces, initailly, after induded demand added and after policy adjustments
for( runtype in funcRunTypes ) {
	
	yr <- RunTypes$RunYear [ RunTypes$RunType == runtype ]
	print( paste(runtype, yr, sep= " ") )
	RunTypeDir <- paste( "outputs/Type", runtype, sep="" )

	StartTime <- Sys.time()
	print( "Calculating the effects of congestion" )
	print(ls())


          # Load data summaries
          #--------------------

          # Load population
          if( yr == BaseYear ) {
          	Filename <- paste( RunTypeDir, "/Pop.Pt.RData", sep="" )
          	Pop.Pt <- assignLoad( Filename )
			BasePop.Pt <- Pop.Pt
         	}
          if( yr != BaseYear ) {
          	Filename <- paste( RunTypeDir, "/Pop.Pt.RData", sep="" )
          	Pop.Pt <- assignLoad( Filename )
               Filename <- "outputs/TypeE/Pop.Pt.RData"
          	BasePop.Pt <- assignLoad( Filename )
          }
          PopChangeRatio.Pt <- Pop.Pt / BasePop.Pt
          PopChangeRatio.Pt[ is.na( PopChangeRatio.Pt ) ] <- 0
          # Load metropolitan transportation summaries
          Filename <- paste( RunTypeDir, "/FwyLnMiCap.Ma.RData", sep="" )        
          FwyLnMiCap.Ma <- assignLoad( Filename )
          Filename <- paste( RunTypeDir, "/ArtLnMiCap.Ma.RData", sep="" )
          ArtLnMiCap.Ma <- assignLoad( Filename )
          Filename <- paste( RunTypeDir, "/TranRevMiCap.Ma.RData", sep="" )
          TranRevMiCap.Ma <- assignLoad( Filename )
          Filename <- paste( RunTypeDir, "/BusRevMi.Ma.RData", sep="" )
          BusRevMi.Ma <- assignLoad( Filename )
          Filename <- paste( RunTypeDir, "/RailRevMi.Ma.RData", sep="" )
          RailRevMi.Ma <- assignLoad( Filename )
          rm( Filename )
          # Load DVMT
          Filename <- paste( RunTypeDir, "/Dvmt.Pt.RData", sep="" )
          Dvmt.Pt <- assignLoad( Filename )
		  # Load Truck DVMT
		  Filename <- paste( RunTypeDir, "/TruckDvmt.Ma.RData", sep="" )
		  TruckDvmt.Ma <- assignLoad( Filename )
          
          # Calculate bus DVMT by metropolitan area
          #----------------------------------------
          # Calculate bus DVMT
          BusDvmt.Ma <- BusRevMi.Ma * TranAdjFactor / 365

          # Calculate light vehicle DVMT for the metropolitan area
          #-------------------------------------------------------
		  # Sum light vehicle DVMT
		  HhDvmt <- sum( Dvmt.Pt )
		  # If this is an ELESNP run, calculate factor to convert HhDvmt to metropolitan road light vehicle DVMT
          if( runtype == "ELESNP" ) {
               BaseLtVehDvmt.Ma <- BaseDvmtParm..$LtVehDvmt
               names( BaseLtVehDvmt.Ma ) <- rownames( BaseDvmtParm.. )
               LtVehDvmtFactor.Ma <- BaseLtVehDvmt.Ma * 1000 / HhDvmt
               # Save the result
               save( LtVehDvmtFactor.Ma, file= "outputs/LtVehDvmtFactor.Ma.RData" )
		  } else {
			  #load the LtVehDvmtFactor.Ma.RData
				LtVehDvmtFactor.Ma <- assignLoad( "outputs/LtVehDvmtFactor.Ma.RData" )
          }          
		# Factor household light vehicle DVMT to produce metropolitan road light vehicle DVMT
		LtVehDvmt.Ma <- HhDvmt * LtVehDvmtFactor.Ma
		# Clean up
		rm( HhDvmt, Dvmt.Pt )
		
		# Calculate total DVMT by metropolitan area
		#------------------------------------------
		Dvmt.MaTy <- cbind( LtVeh=LtVehDvmt.Ma, Truck=TruckDvmt.Ma, Bus=BusDvmt.Ma ) 
		rm( LtVehDvmt.Ma, TruckDvmt.Ma, BusDvmt.Ma )

		# Sum population by metropolitan area
		#------------------------------------
		Pop.Ma <- sum( Pop.Pt )
		rm( Pop.Pt, BasePop.Pt )
		
		# Calculate congestion effects
		#=============================
		
          # Initialize arrays to store results
          Ty <- c( "LtVeh", "Truck", "Bus" )
          MpgAdj.MaTy <- array( 0, dim=c(length(Ma),length(Ty)), dimnames=list(Ma,Ty) )
          VehHr.MaTy <- array( 0, dim=c(length(Ma),length(Ty)), dimnames=list(Ma,Ty) )
          AveSpeed.MaTy <- array( 0, dim=c(length(Ma),length(Ty)), dimnames=list(Ma,Ty) )
          FfVehHr.MaTy <- array( 0, dim=c(length(Ma),length(Ty)), dimnames=list(Ma,Ty) )
          DelayVehHr.MaTy <- array( 0, dim=c(length(Ma),length(Ty)), dimnames=list(Ma,Ty) )
         
		#####CS no need to loop by metro area - remove this looping  
		for( ma in Ma ) {

               # Get the DVMT by vehicle type
               Dvmt.Ty <- Dvmt.MaTy[ ma, ]
               		
			# Extract freeway and arterial supply for the metropolitan area
			PerCapFwyLnMi <- FwyLnMiCap.Ma
			PerCapArtLnMi <- ArtLnMiCap.Ma

			# Extract population and ITS factor for the metropolitan area
			Population <- Pop.Ma #just a single value
			ITS <- ITS.Yr[ yr ]

               # Calculate the MPG adjustment, travel time and travel delay
               CongResults_ <- calcCongestion( CongModel_, Dvmt.Ty=Dvmt.Ty, PerCapFwy=PerCapFwyLnMi,
                    PerCapArt=PerCapArtLnMi, Pop=Population, IncdReduc=ITS, 
                    FwyArtProp=BaseDvmtParm..[ ma, "FwyArtProp" ], 
                    BusVmtSplit.Fc=BusVmt.Fc, TruckVmtSplit.Fc=TruckVmt.Fc, UsePce=FALSE )
                    
               # Insert results in array
               MpgAdj.MaTy[ ma, ] <- CongResults_$MpgAdj.Ty
               VehHr.MaTy[ ma, ] <- CongResults_$VehHr.Ty
               AveSpeed.MaTy[ ma, ] <- CongResults_$AveSpeed.Ty
               FfVehHr.MaTy[ ma, ] <- CongResults_$FfVehHr.Ty
               DelayVehHr.MaTy[ ma, ] <- CongResults_$DelayVehHr.Ty                    

               # Clean up
               rm( Dvmt.Ty, PerCapFwyLnMi, PerCapArtLnMi, Population, ITS, CongResults_ )
               
          }

          # Calculate MPG adjustment on a household basis
          # Assuming the household VMT outside of metropolitan area is uncongested
          HhMpgAdj.Ma <- ( MpgAdj.MaTy[,"LtVeh"] * LtVehDvmtFactor.Ma ) + ( 1 - LtVehDvmtFactor.Ma )

          # Save the results
          #=================
          
		  Filename <- paste( RunTypeDir, "/", "Dvmt.MaTy", ".RData", sep="" )
		  save( Dvmt.MaTy, file=Filename )
          Filename <- paste( RunTypeDir, "/", "MpgAdj.MaTy", ".RData", sep="" )
          save( MpgAdj.MaTy, file=Filename )
          Filename <- paste( RunTypeDir, "/", "VehHr.MaTy", ".RData", sep="" )
          save( VehHr.MaTy, file=Filename )
          Filename <- paste( RunTypeDir, "/", "AveSpeed.MaTy", ".RData", sep="" )
          save( AveSpeed.MaTy, file=Filename )
          Filename <- paste( RunTypeDir, "/", "FfVehHr.MaTy", ".RData", sep="" )
          save( FfVehHr.MaTy, file=Filename )
          Filename <- paste( RunTypeDir, "/", "DelayVehHr.MaTy", ".RData", sep="" )
          save( DelayVehHr.MaTy, file=Filename )
          Filename <- paste( RunTypeDir, "/", "HhMpgAdj.Ma", ".RData", sep="" )
          save( HhMpgAdj.Ma, file=Filename )
          
		  #remove objects
		  #--------------
		  rm( Dvmt.MaTy, MpgAdj.MaTy, VehHr.MaTy, AveSpeed.MaTy, FfVehHr.MaTy, DelayVehHr.MaTy, HhMpgAdj.Ma )
		
	  print( StartTime )
	  print( Sys.time() )
		
	  # end of loop on runtypes
	  }
	  
#end of congestion function
} 

#Calls congestion function but just for "FLFSYP" runtype
policycongestion <- function () {
	congestion("POLICY ADJUSTED CONGESTION", "FLFSYP")	

	#Copy performance metrics to outputs folder
	#Policy Adjusted Congestion	AveSpeed.MaTy.Rdata	DataPeak Travel Speeds by Facility Class	Direct Travel Impacts
	#Policy Adjusted Congestion	VehHr.MaTy.RData	Vehicle Hours of Travel 	Direct Travel Impacts
	#Policy Adjusted Congestion	DelayVehHr.MaTy.RData	Vehicle Hours of Delay 	Direct Travel Impacts
	file.copy( "outputs/TypeFLFSYP/AveSpeed.MaTy.RData", "outputs", overwrite=TRUE )
	file.copy( "outputs/TypeFLFSYP/VehHr.MaTy.RData", "outputs", overwrite=TRUE )
	file.copy( "outputs/TypeFLFSYP/DelayVehHr.MaTy.RData", "outputs", overwrite=TRUE )
	
}

metrics <- function () {
#===========================
print("PERFORMANCE METRICS") 
#=========================== 
	#The model iterates through each run type. 
	for( runtype in RunTypes$RunType ) {
		
		#just for the final runtype
		if( runtype == "FLFSYP") {	
		
			yr <- RunTypes$RunYear [ RunTypes$RunType == runtype ]
			print( paste(runtype, yr, sep= " ") )
			RunTypeDir <- paste( "outputs/Type", runtype, sep="" )
			
			StartTime <- Sys.time()
			print( "Calculating performance metrics" )
			print(ls())
			
			#VEHICLE FUEL ECONOMY AND RECALCULATE CONSUMPTION AND COSTS
			#==========================================================	
			
			#Load the population
			Filename <- paste( RunTypeDir, "/Pop.Pt.RData", sep="" )
			Pop.Pt <- assignLoad( Filename )
			# Load results from previous model steps
			Filename <- paste( RunTypeDir, "/HhMpgAdj.Ma.RData", sep="" )        
			HhMpgAdj.Ma <- assignLoad( Filename )
			Filename <- paste( RunTypeDir, "/MpgAdj.MaTy.RData", sep="" )        
			MpgAdj.MaTy <- assignLoad( Filename )
			Filename <- paste( RunTypeDir, "/Dvmt.MaTy.RData", sep="" )        
			Dvmt.MaTy <- assignLoad( Filename )
			Filename <- paste( RunTypeDir, "/Costs.RData", sep="" )        
			Costs. <- assignLoad( Filename )
			
			# Load SynPop..
			Filename <- paste( RunTypeDir, "/", runtype, ".RData", sep="" )
			SynPop.. <- assignLoad( Filename )
			
			IsMetro. <- SynPop..$Urban == 1
			
			# Change the vehicle fuel economy due to congestion
			#--------------------------------------------------
			if( any( IsMetro. ) ) {
				MpgAdj <- HhMpgAdj.Ma
				SynPop..$VehMpg[ IsMetro. ] <- lapply( SynPop..$VehMpg[ IsMetro. ], function(x) {
							x * MpgAdj } )
			}
			
			# Calculate average fuel CO2e per gallon
			#---------------------------------------
			AveFuelCo2e. <- calcAveFuelCo2e( yr, Fuels..=Fuel.Ty, Co2..=Fuel_Co2.Ft,
					MjPerGallon=121, OutputType="MetricTons" )
			
			# Calculate consumption and production at a household level
			#----------------------------------------------------------
			ModelVar. <- c( "Hhvehcnt", "VehMpg", "VehType", "VehDvmt", "Dvmt" ) 
			FuelCo2e_ <- calcVehFuelCo2( SynPop..[ , ModelVar. ], AveFuelCo2e.=AveFuelCo2e.)
			SynPop..$FuelGallons <- FuelCo2e_$FuelGallons
			SynPop..$FuelCo2e <- FuelCo2e_$FuelCo2e
			rm( AveFuelCo2e., FuelCo2e_ )
			rm( ModelVar. )
			gc()
			
			#Performance Metrics	Emissions.Pt.Rdata	Greenhouse Gas and Criteria Emissions	Environment and Energy Impacts
			#Performance Metrics	Fuel.Pt.Rdata	Fuel Consumption	Environment and Energy Impacts
			
			#Tabulate Co2e
			Emissions.Pt <- Pop.Pt * 0
			Emissions.Pt[Pt] <- tapply( SynPop..$FuelCo2e, SynPop..$PlaceType, sum )[Pt]
			Emissions.Pt[is.na(Emissions.Pt)] <- 0
			save( Emissions.Pt, file=paste(RunTypeDir,"/Emissions.Pt.RData",sep="" ))
			
			#Tabulate Fuel
			Fuel.Pt <- Pop.Pt * 0
			Fuel.Pt[Pt] <- tapply( SynPop..$FuelGallons, SynPop..$PlaceType, sum )[Pt]
			Fuel.Pt[is.na(Fuel.Pt)] <- 0
			save( Fuel.Pt, file=paste(RunTypeDir,"/Fuel.Pt.RData",sep="" ))
			
			# Calculate household travel costs
			#---------------------------------
			
			ModelVar. <- c( "FuelGallons", "FuelCo2e", "Dvmt", "DailyPkgCost", "Hhvehcnt" ) 
			Costs_ <- calcCosts( SynPop.., Costs. )
			SynPop..$FutrCostPerMi <- Costs_$FutrCostPerMi
			SynPop..$TotCost <- Costs_$TotCost 
			rm( Costs_, ModelVar. )
			gc()
			
			#Performance Metrics	Costs.Pt.Rdata	Annual Traveler Cost (fuel and travel time)	Financial and Economic Impacts
			#Tabulate Costs
			Costs.Pt <- Pop.Pt * 0
			Costs.Pt[Pt] <- tapply( SynPop..$TotCost, SynPop..$PlaceType, sum )[Pt] * AnnVmtInflator
			Costs.Pt[is.na(Costs.Pt)] <- 0
			save( Costs.Pt, file=paste(RunTypeDir,"/Costs.Pt.RData",sep="" ))
			
			# Save the results
			#-----------------
			Filename <- paste( RunTypeDir, "/", runtype, ".RData", sep="" )
			save( SynPop.., file=Filename, compress=TRUE )
			rm( SynPop.. )
			gc()
			
			#Clean up
			rm ( HhMpgAdj.Ma )
			
			#CALCULATE METROPOLITAN AREA HEAVY VEHICLE CONSUMPTION AND EMISSIONS
			#===================================================================
			
			# Calculate truck and bus age distributions
			#------------------------------------------
			# Calculate the truck age distribution
			TruckAgProp.Ag <- adjustHvyVehAgeDistribution( TruckBusAgeDist.AgTy[ , "Truck" ], 
					AdjRatio=1 )
			# Calculate bus age distribution
			BusAgProp.Ag <- adjustHvyVehAgeDistribution( TruckBusAgeDist.AgTy[ , "Bus" ], 
					AdjRatio=1 )
			
			# Calculate truck and bus fuel economy
			#-------------------------------------
			# Calculate truck fuel economy
			TruckMpg <- assignHvyVehFuelEconomy( TruckAgProp.Ag, Mpg..Yr=VehicleMpg.Yr[,c("Truck","Bus")],
					Type="Truck", CurrYear=yr )
			# Calculate bus fuel economy
			BusMpg <- assignHvyVehFuelEconomy( BusAgProp.Ag, Mpg..Yr=VehicleMpg.Yr[,c("Truck","Bus")], Type="Bus", 
					CurrYear=yr )
			# Adjust truck and bus fuel economy to account for congestion
			TruckMpg.Ma <- TruckMpg * MpgAdj.MaTy[,"Truck"]
			BusMpg.Ma <- BusMpg * MpgAdj.MaTy[,"Bus"]
			# Clean up
			rm( TruckAgProp.Ag, BusAgProp.Ag, TruckMpg, BusMpg )
			
			# Calculate truck fuel consumption by fuel type
			#----------------------------------------------
			# Calculate overall fuel consumption
			TruckFuel.Ma <- Dvmt.MaTy[,"Truck"] / TruckMpg.Ma
			rm( TruckMpg.Ma )
			# Calculate fuel consumption by type
			TruckFuelProp.Ft <- numeric(5)
			names( TruckFuelProp.Ft ) <- Ft
			TruckFuelInput. <- Fuel.Ty["Truck",]
			PropGas <- 1 - TruckFuelInput.[ "PropDiesel" ] - TruckFuelInput.[ "PropCng" ]
			TruckFuelProp.Ft[ "ULSD" ] <- TruckFuelInput.[ "PropDiesel" ] * ( 1 - TruckFuelInput.[ "DieselPropBio" ] )
			TruckFuelProp.Ft[ "Biodiesel" ] <- TruckFuelInput.[ "PropDiesel" ] * ( TruckFuelInput.[ "DieselPropBio" ] )
			TruckFuelProp.Ft[ "Gasoline" ] <- PropGas *	( 1 - TruckFuelInput.[ "GasPropEth" ] )
			TruckFuelProp.Ft[ "Ethanol" ] <- PropGas * ( TruckFuelInput.[ "GasPropEth" ] )
			TruckFuelProp.Ft[ "CNG" ] <- ( TruckFuelInput.[ "PropCng" ] )
			TruckFuel.MaFt <- outer( TruckFuel.Ma, TruckFuelProp.Ft, "*" )
			rm( TruckFuelInput., TruckFuelProp.Ft )
			
			# Calculate Bus Fuel Consumption and Emissions
			#---------------------------------------------
			# Calculate overall fuel consumption
			BusFuel.Ma <- Dvmt.MaTy[,"Bus"] / BusMpg.Ma
			rm( BusMpg.Ma )
			# Calculate fuel consumption by type
			BusFuelProp.Ft <- numeric(5)
			names( BusFuelProp.Ft ) <- Ft
			BusFuelInput. <- Fuel.Ty["Bus",]
			PropGas <- 1 - BusFuelInput.[ "PropDiesel" ] - BusFuelInput.[ "PropCng" ]
			BusFuelProp.Ft[ "ULSD" ] <- BusFuelInput.[ "PropDiesel" ] * ( 1 - BusFuelInput.[ "DieselPropBio" ] )
			BusFuelProp.Ft[ "Biodiesel" ] <- BusFuelInput.[ "PropDiesel" ] * ( BusFuelInput.[ "DieselPropBio" ] )
			BusFuelProp.Ft[ "Gasoline" ] <- PropGas * ( 1 - BusFuelInput.[ "GasPropEth" ] )
			BusFuelProp.Ft[ "Ethanol" ] <- PropGas * ( BusFuelInput.[ "GasPropEth" ] )
			BusFuelProp.Ft[ "CNG" ] <- ( BusFuelInput.[ "PropCng" ] )
			BusFuel.MaFt <- outer( BusFuel.Ma, BusFuelProp.Ft, "*" )
			rm( BusFuelInput., BusFuelProp.Ft )
			
			# Calculate emissions per gallon of fuel consumed
			#------------------------------------------------
			FuelCo2.Ft <- numeric(5)
			names( FuelCo2.Ft ) <- Ft
			FuelCo2.Ft[ "ULSD" ] <- Fuel_Co2.Ft[ "ULSD" ]
			FuelCo2.Ft[ "Biodiesel" ] <- Fuel_Co2.Ft[ "Biodiesel" ]
			if( yr == "1990" ) {
				FuelCo2.Ft[ "Gasoline" ] <- Fuel_Co2.Ft[ "RFG" ]
			} else {
				FuelCo2.Ft[ "Gasoline" ] <- Fuel_Co2.Ft[ "CARBOB" ]
			}
			FuelCo2.Ft[ "Ethanol" ] <- Fuel_Co2.Ft[ "Ethanol" ]
			FuelCo2.Ft[ "CNG" ] <- Fuel_Co2.Ft[ "Cng" ]
			
			# Calculate truck and bus emissions
			#----------------------------------
			# Calculate truck emissions
			TruckMj.MaTy <- TruckFuel.MaFt * MjPerGallon
			TruckCo2e.MaTy <- sweep( TruckMj.MaTy, 2, FuelCo2.Ft, "*" ) / 1000000
			TruckCo2e.Ma <- rowSums( TruckCo2e.MaTy )
			rm( TruckMj.MaTy, TruckCo2e.MaTy )
			# Calculate bus emissions
			BusMj.MaTy <- BusFuel.MaFt * MjPerGallon
			BusCo2e.MaTy <- sweep( BusMj.MaTy, 2, FuelCo2.Ft, "*" ) / 1000000
			BusCo2e.Ma <- rowSums( BusCo2e.MaTy )
			rm( BusMj.MaTy, BusCo2e.MaTy, FuelCo2.Ft )
			
			# Calculate rail emissions
			#-------------------------
			# Calculate DVMT and power consumed
			Filename <- paste( RunTypeDir, "/RailRevMi.Ma.RData", sep="" )
			RailRevMi.Ma <- assignLoad( Filename )
			
			RailDvmt.Ma <- RailRevMi.Ma * TranAdjFactor / 365
			RailPower.Ma <- RailDvmt.Ma / VehicleMpg.Yr[yr,"Train"]
			# Calculate average emissions per kwh by metropolitan area
			#######hardcoded value - do we need to caclulate this or should we just stop at RailPower.Ma?
			PowerCo2.Ma <- 1.4
			# Calculate total emissions by metropolitan area
			RailCo2e.Ma <- RailPower.Ma * PowerCo2.Ma / 2204.62262
			rm( RailDvmt.Ma, PowerCo2.Ma )
			
			# Save the results
			#-----------------
			Filename <- paste( RunTypeDir, "/", "TruckFuel.MaFt", ".RData", sep="" )
			save( TruckFuel.MaFt, file=Filename )
			Filename <- paste( RunTypeDir, "/", "BusFuel.MaFt", ".RData", sep="" )
			save( BusFuel.MaFt, file=Filename )
			Filename <- paste( RunTypeDir, "/", "TruckCo2e.Ma", ".RData", sep="" )
			save( TruckCo2e.Ma, file=Filename )
			Filename <- paste( RunTypeDir, "/", "BusCo2e.Ma", ".RData", sep="" )
			save( BusCo2e.Ma, file=Filename )
			Filename <- paste( RunTypeDir, "/", "RailPower.Ma", ".RData", sep="" )
			save( RailPower.Ma, file=Filename )
			Filename <- paste( RunTypeDir, "/", "RailCo2e.Ma", ".RData", sep="" )
			save( RailCo2e.Ma, file=Filename )
			rm( TruckFuel.MaFt, BusFuel.MaFt, TruckCo2e.Ma, BusCo2e.Ma, RailPower.Ma, RailCo2e.Ma )
			
			#CALCULATE COST BASED PERFORMANCE METRICS
			#===================================================================

			#Performance Metrics	HighwayCost.Ma.Rdata	Regional Infrastructure Costs for Highway	Financial and Economic Impacts
			#Performance Metrics	TransitCapCost.Ma.Rdata	Regional Infrastructure Costs for Transit	Financial and Economic Impacts
			#Performance Metrics	TransitOpCost.Ma.Rdata	Annual Transit Operating Cost	Financial and Economic Impacts
			
			#Load the existing and future transportation supply
			#--------------------------------------------------
			#Existing supply - highway and transit
			TempInput.. <- read.csv( inputLocation( "transportation_supply.csv" ), row.names=1 )
			BaseSupply.Ma <- as.matrix( TempInput.. )
			#Future supply summaries - highway
			Filename <- paste( RunTypeDir, "/FwyLnMiCap.Ma.RData", sep="" )        
			FwyLnMiCap.Ma <- assignLoad( Filename )
			Filename <- paste( RunTypeDir, "/ArtLnMiCap.Ma.RData", sep="" )
			ArtLnMiCap.Ma <- assignLoad( Filename )
			#Future supply summaries - transit
			Filename <- paste( RunTypeDir, "/BusRevMi.Ma.RData", sep="" )
			BusRevMi.Ma <- assignLoad( Filename )
			Filename <- paste( RunTypeDir, "/RailRevMi.Ma.RData", sep="" )
			RailRevMi.Ma <- assignLoad( Filename )
			#Future transit trips
			Filename <- paste( RunTypeDir, "/TransitTrips.Pt.RData", sep="" )
			TransitTrips.Pt <- assignLoad( Filename )
			
			#Calculate the freeway capital costs
			#-----------------------------------
			BaseFwyLnMi <- BaseSupply.Ma["Freeway",]
			FutureFwyLnMi <- FwyLnMiCap.Ma * sum(Pop.Pt) / 1000
			FwyLnMiGrowth <- FutureFwyLnMi - BaseFwyLnMi
			FwyLnMiCost <- SupplyCosts.Ma ["Freeway","CapCosts"] * FwyLnMiGrowth
			
			#Calculate the arterial capital costs
			#-----------------------------------
			BaseArtLnMi <- BaseSupply.Ma["Arterial",]
			FutureArtLnMi <- ArtLnMiCap.Ma * sum(Pop.Pt) / 1000
			ArtLnMiGrowth <- FutureArtLnMi - BaseArtLnMi
			ArtLnMiCost <- SupplyCosts.Ma ["Arterial","CapCosts"] * ArtLnMiGrowth
			
			#Calculate total highway costs
			#-----------------------------
			HighwayCost.Ma <- FwyLnMiCost + ArtLnMiCost
			
			#Calculate transit capital and operating costs
			#---------------------------------------------
			#Calculation is per trip and there are costs for bus and rail trips
			#Assume that trips are in proportion to revenue miles operated
			#Costs are in terms of just additional trips due to growth
			BusShare <- BusRevMi.Ma / (BusRevMi.Ma + RailRevMi.Ma)
			RailShare <- 1 - BusShare 
			TransitCosts <- BusShare * SupplyCosts.Ma["Bus",] + RailShare * SupplyCosts.Ma["Rail",] 
			TransitTrips <- sum(TransitTrips.Pt)
			
			#Annual capital cost (converted from daily to annual using AnnVmtInflator)
			TransitCapCost.Ma <- TransitCosts["CapCosts"] * TransitTrips * AnnVmtInflator
			#Annual operating cost (converted from daily to annual using AnnVmtInflator)
			TransitOpCost.Ma <- TransitCosts["OpCosts"] * TransitTrips * AnnVmtInflator
			#Annual fare revenue (converted from daily to annual using AnnVmtInflator)
			TransitRevenue.Ma <- TransitCosts["Fare"] * TransitTrips * AnnVmtInflator 
			
			#Save the output files
			#---------------------
			save( HighwayCost.Ma, file=paste(RunTypeDir,"/HighwayCost.Ma.RData",sep="" ))
			save( TransitCapCost.Ma, file=paste(RunTypeDir,"/TransitCapCost.Ma.RData",sep="" ))
			save( TransitOpCost.Ma, file=paste(RunTypeDir,"/TransitOpCost.Ma.RData",sep="" ))
			save( TransitRevenue.Ma, file=paste(RunTypeDir,"/TransitRevenue.Ma.RData",sep="" ))
			
			#CALCULATE Livability and Health (Accidents) Performance Metrics
			#===================================================================
			
			#Performance Metrics	Live.Pt.Rdata	Livability (FTA Criteria)	Community Impacts
			#Performance Metrics	Health.Pt.Rdata	Public Health Impacts and Costs	Community Impacts
			
			#Livability
			#####Need algorithm for this metric

			#Calculate accidents
			Filename <- paste( RunTypeDir, "/Dvmt.Pt.RData", sep="" )
			Dvmt.Pt <- assignLoad( Filename )
			AnnVmtMillions.Ma <- sum(Dvmt.Pt) * AnnVmtInflator / 100e6 
			Accidents.As <- round(AnnVmtMillions.Ma * AccidentRates,0)  
			
			#Save the output files
			#---------------------
			#save( Live.Pt, file=paste(RunTypeDir,"/Live.Pt.RData",sep="" ))
			save( Accidents.As, file=paste(RunTypeDir,"/Accidents.As.RData",sep="" ))
			
			#Copy summary tabulations to outputs folder
			#==========================================
			file.copy( "outputs/TypeFLFSYP/Emissions.Pt.RData", "outputs", overwrite=TRUE )
			file.copy( "outputs/TypeFLFSYP/Fuel.Pt.RData", "outputs", overwrite=TRUE )
			file.copy( "outputs/TypeFLFSYP/HighwayCost.Ma.RData", "outputs", overwrite=TRUE )
			file.copy( "outputs/TypeFLFSYP/TransitCapCost.Ma.RData", "outputs", overwrite=TRUE )
			file.copy( "outputs/TypeFLFSYP/TransitOpCost.Ma.RData", "outputs", overwrite=TRUE )
			file.copy( "outputs/TypeFLFSYP/Costs.Pt.RData", "outputs", overwrite=TRUE )
			#file.copy( "outputs/TypeFLFSYP/Live.Pt.RData", "outputs", overwrite=TRUE )
			file.copy( "outputs/TypeFLFSYP/Accidents.As.RData", "outputs", overwrite=TRUE )
			
			
			# Clean up
			gc()
			
			print( StartTime )
			print( Sys.time() )

		#end if			
		}
	# end of loop on run types
	}
#end of metrics function
}

exportoutputs <- function() {
  #===========================
  print("EXPORTING OUTPUTS") 
  #=========================== 
  outputslist <- read.csv(file.path(ScriptDir,"outputs.csv"))
  loadwrite <- function(filename){
    load(file.path("outputs",filename))
    write.csv(get(sub(".RData","",filename)),file=file.path("outputs",sub(".RData",".csv",filename)))
  }
  lapply(outputslist$filename,loadwrite)
}

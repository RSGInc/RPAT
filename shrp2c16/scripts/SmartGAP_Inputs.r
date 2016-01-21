#SmartGAP_Inputs.r
#==================

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

#This scripts loads the model objects and parameters needed to run the Regional Evaluation for Growth Strategies (REGS) model.
#Note that each function in the Sim script loads the inputs it requires at the start of the function. This facilitates 

#Define naming vectors
#=====================

####CS
#This section needs some work
#Need to allow the geographic specification (state, county, metropolitan areas) to be read from an input file
#Need to handle cross state regions

     Abbr_ <- list()

	# Categories for the age of persons
	Abbr_$Ap <- c( "Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64", "Age65Plus" )

	# Metropolitan areas
	Abbr_$Ma <- c( "Metro" )

     # Vehicle type
     Abbr_$Vt <- c( "Auto", "LtTruck", "HvyTruck", "Bus" )
     
     # Income group
     Abbr_$Ig <- c( "0to20K", "20Kto40K", "40Kto60K", "60Kto80K", "80Kto100K", "100KPlus" )

	# Fuel Type
 	Abbr_$Ft <- c( "ULSD", "Biodiesel", "Gasoline", "Ethanol", "CNG" )
	
	# Place type
	Abbr_$Pt <- c("Rur","Sub R", "Sub E", "Sub M", "Sub T", "CIC R", "CIC E", "CIC M", "CIC T", "UC R", "UC E", "UC MU", "UC T")
	
	# Area type
	Abbr_$At <- c("Rur", rep("Sub",4), rep("CIC",4), rep("UC",4))
	
	# Development type
	Abbr_$Dt <- c("Rur", rep(c("Res","Emp","MU","TOD"),3))
	

     attach( Abbr_ )

#Load estimated models and functions
#===================================

#The submodels of SmartGAP are combinations of data and functions. For example, the submodel that adjusts household income distributions based on changes in the distribution of the household age structure consists of a matrix of probabilities and a function to calculate income adjustments based on the matrix of probabilities and other inputs. All of the submodels are contained in one .RData file that is composed of a list containing the data and functions for the submodels. This list is attached to the workspace so that each submodel can be called using it's individual name.

###currently this is in the script directory

     # Load the estimated model
	setwd( ScriptDir )
	load( "SmartGAP_.RData" )
	attach( GreenSTEP_ )
     setwd( RunDir )

#Load various files of parameters used in the model
#==================================================

	setwd( ParameterDir )

     Parameters_ <- list()
	
	 # parameters in fuel.csv
	 Filename <- "accident_rates.csv"
	 TempInput.. <- read.csv( Filename, row.names=1 )
	 Parameters_$AccidentRates <- as.matrix( TempInput.. )
	 rm( Filename, TempInput.. ) 
	 
	# parameters in fuel.csv
	Filename <- "fuel.csv"
	TempInput.. <- read.csv( Filename, row.names=1 )
	Parameters_$Fuel.Ty <- as.matrix( TempInput.. )
	rm( Filename, TempInput.. )

	# parameters in fuel_co2.csv
	Filename <- "fuel_co2.csv"
	TempInput.. <- read.csv( Filename, row.names=1 )
	Parameters_$Fuel_Co2.Ft <- as.matrix( TempInput.. )
	rm( Filename, TempInput.. )
	
	# parameters in keyvalue.csv
	Filename <- "keyvalue.csv"
	TempInput.. <- read.csv( Filename, as.is = TRUE )
	for(i in 1:nrow(TempInput..)){
		ParamName <- trim(paste(TempInput..$key[i],sep=""))
		
		#typecast
		if(ParamName=="State") {
		  assign(ParamName,TempInput..$value[i])
		} else {
		  assign(ParamName,as.double(TempInput..$value[i]))
		}
		
		Parameters_[[`ParamName`]] <- get(ParamName)
		rm(list=paste(ParamName,sep=""))
	}	
	rm( Filename, TempInput.. )
	
	# parameters in place_type_relative_values.csv
	Filename <- "place_type_relative_values.csv"
	TempInput.. <- read.csv( Filename, row.names=1 )
	Parameters_$PlaceTypeValues.PtD <- as.matrix( TempInput.. )
	rm( Filename, TempInput.. )
	
	# parameters in place_type_elasticities.csv
	Filename <- "place_type_elasticities.csv"
	TempInput.. <- read.csv( Filename, row.names=1 )
	Parameters_$PlaceTypeElasticities.D <- as.matrix( TempInput.. )
	rm( Filename, TempInput.. )
	
	# parameters in tdm_ridesharing.csv
	Filename <- "tdm_ridesharing.csv"
	TempInput.. <- read.csv( Filename, row.names=1 )
	Parameters_$TDMRidesharing <- as.matrix( TempInput.. )
	rm( Filename, TempInput.. )	
	
	# parameters in tdm_transit.csv
	Filename <- "tdm_transit.csv"
	TempInput.. <- read.csv( Filename, row.names=1 )
	Parameters_$TDMTransit <- as.matrix( TempInput.. )
	rm( Filename, TempInput.. )	
	
	# parameters in tdm_transitlevels.csv
	Filename <- "tdm_transitlevels.csv"
	TempInput.. <- read.csv( Filename, row.names=1 )
	Parameters_$TDMTransitLevels <- as.matrix( TempInput.. )
	rm( Filename, TempInput.. )	
	
	# parameters in tdm_workschedule.csv
	Filename <- "tdm_workschedule.csv"
	TempInput.. <- read.csv( Filename, row.names=1 )
	Parameters_$TDMWorkSchedule <- as.matrix( TempInput.. )
	rm( Filename, TempInput.. )	
	
	# parameters in tdm_workschedulelevels.csv
	Filename <- "tdm_workschedulelevels.csv"
	TempInput.. <- read.csv( Filename, row.names=1 )
	Parameters_$TDMWorkScheduleLevels <- as.matrix( TempInput.. )
	rm( Filename, TempInput.. )	
	
	# parameters in tdm_vanpooling.csv
	Filename <- "tdm_vanpooling.csv"
	TempInput.. <- read.csv( Filename, row.names=1 )
	Parameters_$TDMVanpooling <- as.matrix( TempInput.. )
	rm( Filename, TempInput.. )	
	
	# parameters in vehicle_mpg.csv
	Filename <- "transportation_costs.csv"
	TempInput.. <- read.csv( Filename, row.names=1 )
	Parameters_$SupplyCosts.Ma <- as.matrix( TempInput.. )
	rm( Filename, TempInput.. )
	
	# parameters in vehicle_mpg.csv
	Filename <- "vehicle_mpg.csv"
	TempInput.. <- read.csv( Filename, row.names=1 )
	Parameters_$VehicleMpg.Yr <- as.matrix( TempInput.. )
	rm( Filename, TempInput.. )
	
	# Define multiplier for cost difference to determine effect on household income
	# The multiplier adjusts for the fact that a percentage of gross household income
	# is accounted for by non-discretionary income reductions (taxes).
	Parameters_$CostMultiplier <- c( 1.19, 1.21, 1.23, 1.25, 1.26, 1.27 )
	names( Parameters_$CostMultiplier ) <- c( "0to20K", "20Kto40K", "40Kto60K", "60Kto80K", "80Kto100K", "100KPlus" )

	setwd( RunDir )
	
	attach( Parameters_ )



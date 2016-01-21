#===========
#SmartGAP.r
#===========

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

#This is the main script for running the Regional Evaluation for Growth Strategies (REGS) model. 
#The purpose of the REGS model is to forecast the impacts (such as peak hour traffic congestion) of policy assumptions about urban growth patterns, public transit and freeway supply, and demand management policies. 
#This script sets up run parameters and then calls the sim script which implements the REGS model.

#===================================================
# Library calls
#===================================================
#note these calls are required even though they are also in dependencies.r
#the calls in dependencies.r do check for the presence of the package and install it, so can just use library here
library(optparse)
library(reshape)
library(data.table)

#===================================================
# Command-line Parsing
#===================================================
option_list <- list(
		make_option(c("-p", "--process"), help="Process to run")
)

write_status <- function(status_name){
    fileConn<-file("../../stdout.txt")
    writeLines(c(status_name), fileConn)
    close(fileConn)
}

# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults,
opt <- parse_args(OptionParser(option_list=option_list))

#=================================================================
	print("RUN STARTED")
#=================================================================

#Identify directory locations for model, inputs, etc.
#====================================================

     # Make a list to store the directory references
	Dir_ <- list()

	# Directory references are made with respect to the run directory
	Dir_$RunDir <- getwd()

	# The inputs directory is in the directory where the scenario is run
	Dir_$InputDir <- "inputs"
	
	# Make directories for the outputs
	if( !file.exists("outputs") ) dir.create( "outputs" )
	Dir_$OutputDir <- "outputs"
	
	# Directories containing parameters and run scripts are common for all scenarios in the project
	Dir_$ParameterDir <- "parameters"
	Dir_$ScriptDir <- "../../../scripts"
	attach( Dir_ )
	
#Define function to load an RData object to an object name
#=========================================================

    assignLoad <- function(filename){
          load(filename)
          get(ls()[ls() != "filename"])
     }

#Define function to find the correct location of an input file
#================================================================
	 
	 #scenario inputs are either in inputs or if not should be in base/inputs
	 inputLocation <- function(filename){
		 if( file.exists( paste( InputDir,filename,sep="/" ))) {
			inputLocation <- paste( InputDir,filename,sep="/" )			 
		 } else {
			inputLocation <- paste( "../base",InputDir,filename,sep="/" ) 
		 } 
	 }
	
#Define function to trim space from both ends of a string
#========================================================
	 
	 # trim space from both ends of a string
	 trim <- function (str) gsub("(^ +)|( +$)", "", str)
	 
#Run the SmartGAP_Inputs.r script
#=================================

#The SmartGAP_Inputs.r script loads all of the data objects needed to run the model.

	source( paste( ScriptDir, "/SmartGAP_Inputs.r", sep="" ) )
	
#Run the SmartGAP_Sim.r script
#==============================

#The SmartGAP_Sim.r module contains a set of functions that perform all of the household microsimulation calculations for determining household income, vehicle ownership, household travel and vehicle characteristics and use. 

	source( paste( ScriptDir, "/SmartGAP_Sim.r", sep="" ) )

#Set up and execute either a single run step or the whole model
#==============================================================

#Variable to help with looping through those run types - columns specify the year being represented, the transportation supply to use, and whether to include policy variations to VMT in the calculations
RunTypes <- data.frame(RunType = c("E","ELESNP","FLFSNP","FLFSYP"), RunYear = c(as.character(BaseYear),as.character(FutureYear),as.character(FutureYear),as.character(FutureYear)), Landuse = c("E","E","F","F"), Supply = c("E","E","F","F"), Policy = c("N","N","N","Y"), stringsAsFactors = FALSE )

#See if we should execute a particular model step
stepToRun  <- opt$process

if(!is.null(stepToRun)) {
	# execute it
    write_status(stepToRun)
	get(stepToRun)()
} else {
	# execute all
    write_status('household')
	household()
    write_status('urban')
	urban()
    write_status('accessibility')
	accessibility()
    write_status('vehicle')
	vehicle()
    write_status('demand')
	demand()
    write_status('congestion')
	congestion()
    write_status('induced')
	induced()
    write_status('policyvmt')
	policyvmt()
    write_status('policycongestion')
	policycongestion() 
    write_status('metrics')
	metrics()
	exportoutputs()
}
write_status('done')

#=================================================================
print("RUN COMPLETED")
#=================================================================
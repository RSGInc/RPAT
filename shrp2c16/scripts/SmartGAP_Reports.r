#===========
#SmartGAP_Reports.r
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
# Updated 2015 Ben Stabler, ben.stabler@rsginc.com, 03/31/15

#Description
#===========

#This script compares multiple scenarios by producing a series of performance metric plots
#Example call: C:\SmartGAP\projects\Demo Project\base>Rscript ../../../scripts/SmartGap_Reports.R -s "test,base" -p "Accidents" -a "all" -m "comma"

#===================================================
# Library calls
#===================================================
#note these calls are required even though they are also in dependencies.r
#the calls in dependencies.r do check for the presence of the package and install it, so can just use library here
library(optparse)
library(reshape)
library(data.table)
library(ggplot2)
library(scales)

#=================================================================
	print("REPORTS STARTED")
#=================================================================

#Identify directory locations for model, inputs, etc.
#====================================================

  # Make a list to store the directory references
	Dir_ <- list()

	# Directory references are made with respect to the run directory
	Dir_$RunDir <- getwd()

	# The inputs directory is in the directory where the scenario is run
	Dir_$InputDir <- "inputs"
	Dir_$OutputDir <- "outputs"
	
	# Directories containing parameters and run scripts are common for all scenarios in the project
	Dir_$ParameterDir <- "../parameters"
	Dir_$ScriptDir <- "../../../scripts"
	Dir_$ReportsDir <- "../reports"
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

#Define function to get output files
#========================================================
  getOutputFiles <- function() {
    outputFiles <<- read.csv(paste(ScriptDir, "outputs.csv",sep="/"), header=T, sep=",", as.is=T)
    outputFiles
  }

#Define function to build the "Metrics" object which defines 
#the relationships between the metrics, the categorization used 
#for those metrics, and their number formats
#========================================================
buildMetrics <- function() {
	Metrics <<- subset(getOutputFiles(), reportable=1)
}

#Define function to check aggregation types
#========================================================
checkAggregationTypes <- function(metric, aggregation) {
	category <- Metrics$category[Metrics$metric == metric]
	if (category=="Pt") {
	  aggs = c("all","place","area","development")
	} else if (category=="Ma") {
	  aggs = c("all")
	}else if (category=="MaTy") {
	  aggs = c("all","vehtype")
	}else if (category=="As") {
	  aggs = c("all","accsev")
	}else if (category=="Ig") {
	  aggs = c("all","incgrp")
	}
	if(!(aggregation %in% aggs)) {
	  print("Aggregation type inconsistent with metric")
	  print(paste("Possible aggregation types for metric are: ", paste(aggs, collapse=" ")))
	  return(0)
	} else {
	  return(1)
	}
}

#function to make the charts
#1. supports Pt, MaTy, and Ma outputs
#2. supports number, percentage, index measures
makeChart <- function(scenarios, metric, aggregation, measure){
		
	#get scenarios
	scenarios = strsplit(scenarios,",")[[1]]
	
	#helper functons
	#load the .Rdata files
	assignLoad <- function(filename){
		load(filename)
		get(ls()[ls() != "filename"])
	}
	
	#importing and formatting the data varies by the five output categories (Pt, MaTy, As, Ig,Ma )
	if(Metrics$category[Metrics$metric == metric] == "Pt"){
		#get the files specified by the user inputs
		#initialize an object to hold the data and make the correspondences between place, area and development
		chartdata <- data.frame(place = c("Rur","Sub R", "Sub E", "Sub M", "Sub T", "CIC R", "CIC E", "CIC M", "CIC T", "UC R", "UC E", "UC MU", "UC T"),
				area = c("Rural", rep("Suburban",4),rep("Close Com",4),rep("Urban Core",4)),
				development = c("Greenfield",rep(c("Resident","Employ","Mix Use","TOD"),3)))
		
		#read in the scenario files one by one and add to chartdata
		#don't assume that they are sorrted correctly - merge on place
		for(scenario in scenarios){
		  scenarioOutputDir <- paste("..", scenario, OutputDir, sep="/")
			temp <- assignLoad(paste(paste(scenarioOutputDir,metric,sep="/"),".Pt.RData",sep=""))
			temp <- data.frame(place = names(temp),temp)
			names(temp)[2] <- scenario
			chartdata <- merge(chartdata,temp,by="place") 
		}
	} 
	
	if(Metrics$category[Metrics$metric == metric] == "MaTy"){
		#get the files specified by the user inputs
		chartdata <- data.frame(vehtype = c("LtVeh","Truck","Bus"))
		#read in the scenario files one by one and add to chartdata
		#don't assume that they are sorrted correctly - merge on vehtype
		for(scenario in scenarios){
		  scenarioOutputDir <- paste("..", scenario, OutputDir, sep="/")
			temp <- assignLoad(paste(paste(scenarioOutputDir,metric,sep="/"),".MaTy.RData",sep=""))
			temp <- data.frame(vehtype = colnames(temp),temp[1,])
			names(temp)[2] <- scenario
			chartdata <- merge(chartdata,temp, by="vehtype") 
		}
	}
	
	if(Metrics$category[Metrics$metric == metric] == "Ig"){
		#get the files specified by the user inputs
		chartdata <- data.frame(incgrp = c( "0to20K", "20Kto40K", "40Kto60K", "60Kto80K", "80Kto100K", "100KPlus" ))
		#read in the scenario files one by one and add to chartdata
		#don't assume that they are sorrted correctly - merge on incgrp
		for(scenario in scenarios){
		  scenarioOutputDir <- paste("..", scenario, OutputDir, sep="/")
			temp <- assignLoad(paste(paste(scenarioOutputDir,metric,sep="/"),".Ig.RData",sep=""))
			temp <- data.frame(incgrp = names(temp),temp)
			names(temp)[2] <- scenario
			chartdata <- merge(chartdata,temp, by="incgrp") 
		}
	}
	
	if(Metrics$category[Metrics$metric == metric] == "As"){
		#get the files specified by the user inputs
		chartdata <- data.frame(accsev = c("Fatal","Injury","Property"))
		#read in the scenario files one by one and add to chartdata
		#don't assume that they are sorrted correctly - merge on accsev
		for(scenario in scenarios){
		  scenarioOutputDir <- paste("..", scenario, OutputDir, sep="/")
			temp <- assignLoad(paste(paste(scenarioOutputDir,metric,sep="/"),".As.RData",sep=""))
			temp <- data.frame(accsev = rownames(temp),temp[,1])
			names(temp)[2] <- scenario
			chartdata <- merge(chartdata,temp, by="accsev") 
		}
	}
	
	if(Metrics$category[Metrics$metric == metric] == "Ma"){
		#get the files specified by the user inputs
		chartdata <- data.frame(variable = scenarios, value = NA)
		#read in the scenario files one by one and add to chartdata
		for(scenario in scenarios){
		  scenarioOutputDir <- paste("..", scenario, OutputDir, sep="/")
			temp <- assignLoad(paste(paste(scenarioOutputDir,metric,sep="/"),".Ma.RData",sep=""))
			chartdata$value[chartdata$variable == scenario] <- temp 
		}
		
	}
	
	#default labels is to use the value from Metrics
	formatter <- paste(Metrics$formatter[Metrics$metric == metric])
	#default axis unit is to use the units from Metrics
	unitsname <- Metrics$units[Metrics$metric == metric]
	#if measure is percentages, convert values to percentages
	if(measure == "percentage"){
		if(Metrics$category[Metrics$metric == metric] != "Ma"){
			chartdata[,scenarios] <- prop.table(as.matrix(chartdata[,scenarios]),margin=2)
		}else{
			chartdata$value <- 1 #within category percentages don't make sense for the overall regional values but need to do this to avoid error
		}	
		formatter <- "percent"
		unitsname <- "(percentage)"
	}
	#if measure is index, convert values to index relative to first scenario
	#index100 is based off 100, index0 is then centered on 0 to show small changes more clearly
	if(measure %in% c("index0","index100")){
		formatter <- "comma"
		unitsname <- "(index, base=100)"
		normalize <- 0
		if(measure == "index0") {
					normalize <- 100
					unitsname <- "(percent change)"
				}
		if(Metrics$category[Metrics$metric == metric] != "Ma"){	
			if(aggregation != "all"){
				chartdata <- aggregate(chartdata[,scenarios],by=list(chartdata[[aggregation]]),sum)
			}else{
				chartdata <- aggregate(chartdata[,scenarios],by=list(rep("all",nrow(chartdata))),sum)
			}
			if(length(scenarios == 1)){ names(chartdata)[2] <- scenarios[1] }
			chartdata[,scenarios] <- chartdata[,scenarios]/chartdata[,scenarios[1]]*100 - normalize
			names(chartdata)[1] <- aggregation
		}else{
			chartdata$value <- chartdata$value/chartdata$value[1]*100 - normalize
		}
	}
	
	#for data with categories (i.e. anything other than single regional values) need to reshape for charting
	if(Metrics$category[Metrics$metric == metric] != "Ma"){
		#Reshape the data so that it is ready for ggplot2
		chartdata <- melt(chartdata)
	}
	
	#build the metric name for the title and y axis name
  	metricname <- Metrics$metricname[Metrics$metric == metric]
	yname <- paste(metricname,unitsname,sep=" ")

	#chart type and labeling depends on aggregation
	if(aggregation=="all"){
		#make the chart with scenario as x axis
		chart <- qplot(variable,data=chartdata, geom="bar", weight = value, main=paste("Comparison of ",metricname," by Scenario",sep=""))
		chart <- chart + labs(x="Scenario",y=yname)
  	#remove the legend, uneccessary as the scenarios are labeled on the x axis
    chart <- chart + theme(legend.position = "none")
    #change the fill color to a more pleasant color that black
		chart <- chart + geom_bar(fill = "#9ECAE1")
	} else {
		if(aggregation=="place"){
			xname <- "Place Type"
			xorder <- c("Rur","Sub R", "Sub E", "Sub M", "Sub T", "CIC R", "CIC E", "CIC M", "CIC T", "UC R", "UC E", "UC MU", "UC T")
		}	
		if(aggregation=="area"){
			xname <- "Area Type"
			xorder <- c("Rural", "Suburban","Close Com","Urban Core")
		}
		if(aggregation=="development"){
			xname <- "Development Type"
			xorder <- c("Greenfield","Resident","Employ","Mix Use","TOD")
		}
		if(aggregation=="vehtype"){
			xname <- "Vehicle Type"
			xorder <- c("LtVeh","Truck", "Bus")
		}
		if(aggregation=="accsev"){
			xname <- "Accident Severity"
			xorder <- c("Fatal","Injury","Property")
		}
		if(aggregation=="incgrp"){
			xname <- "Income Group"
			xorder <- c( "0to20K", "20Kto40K", "40Kto60K", "60Kto80K", "80Kto100K", "100KPlus" )
		}
		#make chart and format x axis
		chart <- qplot(get(`aggregation`),data=chartdata, geom="bar", weight = value, group = variable, fill = variable, position="dodge", main=paste("Comparison of ",metricname," by ", xname,sep=""))
		chart <- chart + labs(x=xname,y=yname,fill="Scenario")
		chart <- chart + xlim(xorder)
		#color palette for charts with multiple series
		chart <- chart + scale_fill_brewer()
	}
	#format y axis so it is numbers with commas separating 1000's
	if(formatter=="comma"){chart <- chart + scale_y_continuous(labels = comma)}
	if(formatter=="dollar"){chart <- chart + scale_y_continuous(labels = dollar)}
	if(formatter=="percent"){chart <- chart + scale_y_continuous(labels = percent)}
	#font size - make it bigger
  	chart <- chart + theme(axis.text.x = element_text(size = 14, angle = 90),
                        axis.text.y = element_text(size = 14),
                        axis.title.x = element_text(size = 16),
                        axis.title.y = element_text(size = 16, angle = 90),
                        legend.text = element_text(size = 14),
                        legend.title = element_text(size = 16),
                        plot.title = element_text(size = 18))
  #color: white background, gray ticks, no grid lines
 	chart <- chart + theme(panel.background = element_rect(fill = NA, colour = "grey80"), 
                        axis.ticks = element_line(colour = "grey80"),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank())
}

# exports plot to user-specified location
exportPlot <- function (scenarios,metric,aggregation,measure) {
  fName = paste(gsub(",","-",scenarios), metric, aggregation, measure, sep="_")
	outPath = paste(ReportsDir, "/", fName, ".jpeg", sep="")
	makeChart(scenarios, metric, aggregation, measure)
	ggsave(file=outPath)
}

#=================================================================
	print("PARSE COMMAND-LINE OPTIONS")
#=================================================================

#Command-line Parsing
option_list <- list(
		make_option(c("-s", "--scenarios"), help="Scenarios"),
		make_option(c("-p", "--metric"), help="Performance Metric"),
		make_option(c("-a", "--aggregation"), help="Aggregation"),
		make_option(c("-m", "--measure"), help="Measure")
)
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults,
opt <- parse_args(OptionParser(option_list=option_list))
print(opt)

#create reports directory
dir.create(ReportsDir)

#build report data
buildMetrics()

#check command line options
ok = 1
if(!(opt$metric %in% Metrics$metric)) {
	  print("Metric not defined in outputs.csv")
	  ok = ok * 0
} 

measures = c("comma","percentage","index100","index0")
if(!(opt$measure %in% measures)) {
	  print(paste("Measure not one of: ", paste(measures, collapse=" ")))
	  ok = ok * 0
}
ok = ok * checkAggregationTypes(opt$metric,opt$aggregation)

#create report
if(ok==1) {
  exportPlot(opt$scenarios,opt$metric,opt$aggregation,opt$measure)
}

#=================================================================
print("REPORTS COMPLETED")
#=================================================================

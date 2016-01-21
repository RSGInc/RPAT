SHRP2 C16 Decision Software for Smart Growth

Model Component Descriptions

This document introduces three of the model components, describes their inputs, proposed calculations, and outputs, and discusses what each of the mode is and is not sensitive to:

1.	Household and Business Synthesis
2.	Urban Form, Demographics
3.	Auto Ownership

Figure 1 shows the location of the three model components (highlighted in green) in the model flow.

Figure 1: Model Flow Chart
 
Household and Business Synthesis
Introduction
This component of the model:
?	Synthesizes a household population, described along household size and person age dimensions
?	Applies a models of household income to predict income for the synthesized households
?	Synthesizes a population of businesses, described along number of employees and industrial section dimensions
The household synthesis component and household income models are taken from GreenSTEP. The business synthesis model is based on the household synthesis component of the GreenSTEP model, with alternative inputs developed to describe the distribution of businesses along the two control dimensions.
Household Synthesis
Introduction
A set of households is created that represents the likely household composition for each county given county-level forecast of persons by age. Each household is described in terms of the number of persons in each of six age categories residing in the household (0 – 14, 15 – 19, 20 – 29, 30 – 54, 55 – 64, 65+). The household synthesis process uses a combination of probabilities derived from Public User Micro Sample (PUMS) data and an iterative proportional fitting (IPF) process to create a balanced set of households. 
Inputs
POP_FORECASTS/POP_BY_AGE_XXXX.CSV
Format: This directory contains population estimates/forecasts by county and age cohort for each year (1990 to 2050 in 5-year intervals).  The estimates/forecasts for each year are text files having the name “pop_by_age_XXXX.csv” where XXXX is the 4 digit year. Figure 2 shows the file layout, with one row per county and a column for each of the six age categories. The values are numbers of people. 
Source: These file are user created. 
Figure 2: File layout for pop_by_age_XXXX.csv
 
GREENSTEP_.RDATA, GREENSTEP_$HTPROB.HTAP
Format: GreenSTEP_.RData is an R binary object that is a list containing all of the estimated models for GreenSTEP. GreenSTEP_$HtProb.HtAp is a table of household types by age category for population synthesis generated using PUMS data. Figure 3 shows the file layout, with one row per household type. The household type code is in the first column, with the code indicating the number of household members in each age category, i.e. 0-0-0-0-0-1 indicates zero household members in the first five age categories and one household member in the final age category, 65+. The following six columns show the probability that a person in that age group resides in a household of that type. The columns in the file sum to one.
Source: This file is user created. PUMS data for each State can be downloaded from the Census 2000 website (http://www.census.gov/main/www/cen2000.html).
The PUMS data is used to create the set of household types defined by the number of persons in each of the six age groups. The maximum number of persons in each age category is capped at values that account for 99 percent of all households. Using this criterion, the 0 to 14 age category is capped at four and all other age categories are capped at two. This puts the theoretical limit of the number of household types at 1215. In addition, households in the PUMS data that are composed of persons all under the age of 15 are removed. Since the PUMS data associates person information with household information, the probabilities that persons of each age group can be found in households of each type can be easily computed. 
Figure 3: File layout for GreenSTEP_$HtProb.HtAp
 
Proposed calculations
GREENSTEP CODE REFERENCE
This component of the model uses the GreenSTEP_Hh_Synthesis.r script which applies the createHhByAge process to create synthetic households for each county.
MODEL DESCRIPTION
Multiple estimates of households by type result from the application of the probabilities for each person age group. An IPF process is used to reconcile the household type estimates and create a consistent set of households. The first control for the IPF process is to matching the population forecasts by age category. The second control is to create a consistent forecast of the number of households of each type. Each iteration is comprised of the following steps:
1.	Persons of each age group are allocated to households by type by applying the calculated probabilities to the number of persons in each age category.
2.	The persons allocated by household type are converted to households by type by dividing persons in each age category and type by the corresponding persons by age for that household type. For example, 100 persons of age 0 – 14 allocated to household type 2-0-0-2-0-0, implies 50 households of that type.
3.	The result of step #2 will be several conflicting estimates of the number of households of each type. The method used to resolve the differences in the estimates is the "mean" method that chooses the average of the estimates.
4.	The resolved number of households for each type computed in step #3 is multiplied by the corresponding number of persons in each age group to yield an estimate of the number of persons by age group and household type.
5.	A new table of household type probabilities for each age group is computed from the step #4 tabulation.
6.	The sum of persons by age group is calculated from the results of step #4 and subtracted from the control totals of persons by age group to determine the difference to be reallocated.
7.	The person differences are allocated to household types using the probabilities calculated in step #5.
These steps are repeated until the difference between the maximum number of households and the resolved number of households computed for every household type is less than 0.1 per cent or until a maximum number of iterations (default 100).
Outputs
HSLDXXXX.RDATA
Format: HsldXXXX.RData (where XXXX is replaced with a year in the 5-year sequence starting at 1990 and ending at 2050) are R binary objects that contain the household level synthetic population for each county. Figure 4 shows the file layout for each county: the file contains a list with one row per household and each household is described in six columns, with each column indicating the number of persons in one of six age categories residing in the household (0 – 14, 15 – 19, 20 – 29, 30 – 54, 55 – 64, 65+).
Figure 4:  File layout for HsldXXXX.RData
 
Model Sensitivity
The synthesized population is sensitive to changes in population forecasts by age and to changes in household structure over time. To test changes in the evolution of the population over time, alternative version of either the population forecasts by age files or the household types by age category probability table can be created. 
Household Income
Introduction
A regression model predicts household income from the number and ages of persons in the household and the average per capita income for the region of the state in where the household resides. Census PUMS data for Oregon was used for estimating this model and so it might require re-estimation for a new application. 
Inputs
COUNTY_GROUPS.CSV
Format county_groups.csv is a table associating counties with regions and metropolitan areas. Figure 5 shows the file layout, with one row per county and adjacent columns defining the region and MSA that the county (or part of the county) falls in. The literal character string “NA” denotes that no portion of the county is in an MSA. 
Source: This file is user created. The analyst can develop this file using Census definitions of county, MSA and PUMA boundaries (http://www.census.gov/geo/www/cob/bdy_files.html). Counties are associated together in regions that correspond to PUMAs or aggregations of PUMAs. The regions should be designated to reflect areas in the State with consistent economic characteristics by grouping together adjacent counties with relatively similar average household incomes.
HSLDXXXX.RDATA
Source: This file is output from the population synthesis.
Figure 5: File layout for county_groups.csv
 
PER_CAP_INC.CSV
Format: per_cap_inc.csv contains information on statewide average per capita income by forecast year in year 2000 dollars. Figure 6 shows the file layout, with one row for each forecast year (1990 to 2050 in 5 year increments). 
Source: This file is user created. The data can be obtained from State sources such as Bureaus of Economic and Business Research. 
Figure 6: File layout for per_cap_inc.csv
 
regional_inc_prop.csv
Format: regional_inc_prop.csv identifies the ratios of average per capita income for each region of the state to the overall statewide average per capita income. Figure 7 shows the file layout, with one row for each region.
Source: This file is user created. The data are developed using PUMS data for each region 
Figure 7: File layout for regional_inc_prop.csv
 
Proposed calculations
GREENSTEP CODE REFERENCE
This component of the model uses the GreenSTEP_Sim.r script script.
Power-transformed per capita income by county
Applies calcCountyPowPerCapInc to calculate a power transformed per capita income by county, using the statewide per capita income for each forecast year, PerCapInc.Yr, from “per_cap_inc.csv”, the regional income proportion, IncomeProp.Rg, from “regional_inc_prop.csv”, and the region to county correspondence in “county_groups.csv”. The power used in the model, pow = 0.4. 
?	Regional per capita income ($/year)	PerCapInc.Rg = PerCapInc * IncomeProp.Rg
?	Per capita income by county ($/year)	PerCapInc.Co
?	Power transformed income		PowPerCapInc.Co = PerCapInc.Co ^ Pow
Household income
Applies the predictIncome calculation, which uses the IncomeModel linear regression model, to estimate household income. Classifies income into groups, IncGrp.Hh, by selecting the group that Inc.Hh falls in using income breaks at (0, 20000, 40000, 60000, 80000, 100000)
MODEL DESCRIPTION
Income data was found to follow a power distribution, so a power transform of income is used as the dependent variable in the model. The chosen exponent (0.4) minimizes the skewness of the income distribution. The model fit was found to be improved if the regional per capita income variable is similarly transformed. The model intercept is set to zero because household income should be zero when the average per capita income is zero or when the household has no persons. Table 1 shows a summary of the model.
The regional per capita income variable and all of the age variables are highly significant. The coefficients for all of the age terms have an appropriate relationship to one another. The contribution of persons to household income rises with age up to the 30 to 54 age group and decline with increasing age. This regression model does not reproduce the tails of the income distribution, as shown in Figure 8. This could cause several problems if left uncorrected. First, since the income distribution is skewed to the right, the mean predicted value for income will be lower than the mean observed value. Second, since the effects of income on household travel are not linear, improper representation of the tails of the income distribution could cause even greater deficiencies in representing the distribution of household travel and the sensitivity of that travel to changes in costs.
To achieve the proper dispersion of incomes, a procedure for adding random variability is added to the model. The variability in observed income was examined across all model income groups. This variability was found to be fairly uniform. A standard normal distribution is used to add variation to the model predictions of the power transform of income. The addition of this variation results in modeled household incomes nearly matching the observed distribution. This can be seen in Figure 9.
Table 1: Household Income Model (Oregon specific)
Coefficients	Estimate	Std. Error	t value	Pr(>|t|)
PowPerCapInc	0.792567	0.003147	251.818	< 2e-16 ***
Age0to14	-1.00861	0.077073	-13.086	2.99e-10 ***
Age15to19	0.93887	0.149014	6.301	< 2e-16 ***
Age20to29	7.740331	0.119585	64.727	< 2e-16 ***
Age30to54	15.19027	0.111942	135.698	< 2e-16 ***
Age55to64	13.14969	0.148598	88.492	< 2e-16 ***
Age65Plus	8.410674	0.138817	60.588	< 2e-16 ***
Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residuals
Min	1Q	Median	3Q	Max
-109.4430	-11.1195  	0.1774  	11.0499  	67.8462 

Residual standard error: 17.35 on 63502 degrees of freedom
Multiple R-squared: 0.9412,	Adjusted R-squared: 0.9412 
F-statistic: 1.451e+05 on 7 and 63502 DF, p-value: < 2.2e-16

Figure 8: Observed and Modeled Incomes
 
Figure 9: Distribution of Observed and Adjusted Modeled Household Incomes
 
Outputs
COUNTY.RDATA, COUNTY$SYNPOP..$POWPERCAPINC, COUNTY$SYNPOP..$HHINCTTL, COUNTY$SYNPOP..$INCGRP
In the output directory there are files containing the household simulation results for each county. These files are named with the name of the county and an “.RData” extension. Within each of the county files is a table called “SynPop..” which contains variables for each household in the county. The income model adds three variables to the table for each household:
?	PowPerCapInc: Per capita income raised to the power 0.4
?	Hhincttle: Household income in year 2000 dollars
?	IncGrp: The income category that the household income falls in
The SynPop.. table, following the application of the household income model, is shown in Figure 10.
Figure 10: File layout for SynPop.. following application of the household income model
 
Model Sensitivity
Income forecasts are contained in per_cap_inc.csv, and regional income values are in regional_inc_prop.csv. The first of the two files can be used to test the effects of different income growth in the State over time. The second file can be varied to test alternative regional income distributions.
Business Synthesis
Introduction
A set of businesses is created that represents the likely business composition for each county given county-level forecast of employment by industry. Each business is described in terms of the number of employees and it’s industrial sector using a 6 digit NAICS codes. The household synthesis process uses County Business Patterns data to describe the current distribution of businesses by sector and by employment category and enumerates those data to create a complete list of businesses. Growth in employment is used to control the addition of new businesses, assuming that the existing distribution of businesses by employment size and by detailed industrial sector is maintained. 
Inputs
COUNTY BUSINESS PATTERNS DATA
County-level employment data are used to synthesis businesses.  For each county, this dataset contains the number of firms in each category defined by industry (six-digit NAICS) and number of employees.  The primary source for these data is County Business Patterns (CBP) data (http://www.census.gov/econ/cbp/download/index.htm).  However, the CBP data contain limited or no agricultural or construction employment.  While agriculture employment is less important for urban applications such, construction employment is an important element of overall employment.
CBP data contains county level employment and firm data with the following fields; several rows are data are show in Figure 11. The employment field (EMP) is censored when there are small numbers of businesses (5 or less) in an NAICS sector and so these values need to be imputed based on the number of establishments and their employee size class categorization:
?	FIPSTATE: FIPS State Code
?	FIPSCTY: FIPS County Code
?	NAICS: Fields for employment by Industry Code - 6-digit NAICS code
?	EMP: Total Mid-March Employees (with noise – added to the data to avoid disclosure)
?	 QP1: Total First Quarter Payroll ($1,000) (with noise)
?	AP: Total Annual Payroll ($1,000) (with noise)
?	EST: Total Number of Establishments
?	N1_4: Number of Establishments: 1-4 Employee Size Class
?	N5_9: Number of Establishments: 5-9 Employee Size Class
?	N10_19: Number of Establishments: 10-19 Employee Size Class
?	N20_49: Number of Establishments: 20-49 Employee Size Class
?	N50_99: Number of Establishments: 50-99 Employee Size Class
?	N100_249: Number of Establishments: 100-249 Employee Size Class
?	N250_499: Number of Establishments: 250-499 Employee Size Class
?	N500_999: Number of Establishments: 500-999 Employee Size Class
?	N1000: Number of Establishments: 1,000 or More Employee Size Class
?	N1000_1: Number of Establishments: Employment Size Class: 1,000-1,499 Employees
?	N1000_2: Number of Establishments: Employment Size Class: 1,500-2,499 Employees
?	N1000_3: Number of Establishments: Employment Size Class: 2,500-4,999 Employees
?	N1000_4: Number of Establishments: Employment Size Class: 5,000 or More Employees
Figure 11: County Business Patterns data
 
CONSTRUCTION EMPLOYMENT DATA (POSSIBLY QCEW DATA)
For construction employment, the user could supply county employment for NAICS industry 23 if it is available, or it could be sourced from Quarterly Census of Employment and Wages (QCEW) data, but these are also incomplete as self employed workers are omitted and self employed workers form a large element of construction employment. 
EMPLOYMENT GROWTH DATA
Data describing the growth (or decline) in employment by top level NAICS categories is used to control the synthesis of businesses, with additional business synthesized to account for forecast growth or fewer business synthesized to account for forecast decline, in comparison with the base employment data obtained from CBP and QCEW.
These data could be formatted as either just growth/decline proportions for each top level NAICS category 
Proposed calculations
Outputs
Model Sensitivity

Urban Form, Demographics
Introduction
This component of the model:
?	Allocate each household to a land use type
?	Based on the land use type, estimate the population density for the area in which they are located
?	Estimate whether urban households reside in a mixed use area
?	Allocate each business to a land use type
The household allocation component, household density model, and mixed use model are taken from GreenSTEP. The business allocation model is based on the household allocation component of the GreenSTEP model, with alternative inputs developed to describe the distribution of businesses amongst land use types.
Household Allocation, Density and Mixed Use
Introduction
This component of the model develops population density and land use characteristics, which are important variables in the subsequent vehicle ownership model and other model components. Each household is assigned to a metropolitan, other urban, or rural development type in the county where it is located based on policy assumptions about what proportions of population growth will be of each type. The overall densities for metropolitan and other urban areas in each county are calculated based on policy assumptions for urban growth boundary expansions. Households assigned to metropolitan areas are assigned to population density drawn from a likely household density distribution corresponding to the overall metropolitan area density. Households assigned to other urban areas are assigned the overall population density for non-metropolitan areas in the county. Households assigned to rural areas are assigned a population density reflecting the predominant rural population density of the county where it is located. Households in urban areas are also assigned to an urban-mixed use setting or not, based on a model using population density. This can be overridden to simulate greater amounts of urban mixed-use development.
Inputs
AVE_RURAL_POP_DENSITY.CSV
Format: ave_rural_pop_density.csv is a table of base year rural population density by county. Figure 11 shows the file layout, with one row per county and the values in units of people per square mile.
Source: This file is user created. The analyst can develop this file using Census 2000 block group data (http://www.census.gov/main/www/cen2000.html), and State specific information such as urban growth boundaries.  
Figure 12: File layout for ave_rural_pop_density.csv
 
COUNTY.RDATA, COUNTY$SYNPOP..
Source: This file is output from the household income model.
METROPOLITAN_URBAN_TYPE_PROPORTIONS.CSV
Format: metropolitan_urban_type_proportions.csv contains a table of household proportions that are located in urban m ixed-use areas by metropolitan area and forecast year. The model will compute the proportion based on year 2000 household characteristics where the input value is “NA”. If a number (between 0 and 1) is input, the model will make adjustments to match the target value instead. Figure 12 shows the file layout, with one row for each forecast year (1990 to 2050 in 5 year increments) and one column for each metropolitan area.
Source: This file is user created.
Figure 13: File layout for metropolitan_urban_type_proportions.csv
 
UGB_AREAS.CSV
Format: ugb_areas.csv is a table of geographic areas contained within urban growth boundaries defining metropolitan areas and other urban areas by county. Figure 13 shows the file layout, with one row per county, and columns for metropolitan and town urban areas measured in square miles.  The urban areas are used in calculations of urban densities.
Source: This file is user created. The analyst can develop these data using Census definitions of MSA and urban area boundaries based on Census block group population densities (http://www.census.gov/geo/www/cob/bdy_files.html), and State GIS data defining urban growth boundaries and/or urban areas.
Figure 14: File layout for ugb_areas.csv
 
UGB_AREA_GROWTH_RATES.CSV
Format: ugb_area_growth_rates.csv contains a table of growth rates of urban growth boundary areas relative to urban population growth rates for metropolitan and other urban areas in each county. Values can be between 0 and any positive number where a value of 0 will result in no increase in the area within urban growth boundaries and a value of 1 will increase the area within urban growth boundaries at the same rate as population growth. Figure 14 shows the file layout, with one row per county and columns for metropolitan and town growth rates. This file is used in used to adjust the growth in urban land area over time.
Source: This file is user created.
Figure 15: File layout for ugb_area_growth_rates.csv
 
URBAN_RURAL_POP_SPLITS.CSV
Format: urban_rural_pop_splits.csv is a table of the proportions of population located in the metropolitan, town and rural portions of each county in the base years (2005 and earlier). Figure 15 shows the file layout, with one row per county, and columns for the proportion of the population in metropolitan, town and rural areas. This file is used to allocate the population from the county level to each of the three land use types.
Source: The file can be developed from Census 2000 block group data (http://www.census.gov/main/www/cen2000.html) along with the geographic definitions of urban and rural land use types discussed above under ugb_areas.csv.
Figure 16: File layout for urban_rural_pop_splits.csv
 
URBAN_RURAL_GROWTH_SPLITS.CSV
Format: urban_rural_growth_splits.csv contains a table of the proportions of population growth in each county that will occur in metropolitan, other urban and rural portions of the county. Figure 16 shows the file layout, with one row per county and proportions of population growth that occur in each of the area types. This file is used to allocate population growth by area type.
Source: This file is user created.
Figure 17: File layout for urban_rural_growth_splits.csv
 
Proposed calculations
GREENSTEP CODE REFERENCE
This component of the model uses the GreenSTEP_Sim.r script script.
?	Population by county			Pop.Co = sum(Hsld.HhAp)	
?	Households by county			Hsld.Co = count(Hsld.HhAp)	

Urban rural population proportions
Uses the urban rural population proportions in “urban_rural_pop_splits.csv”, and the urban rural growth splits in “urban_rural_growth_splits.csv”. For run years other that 2005 (the base year), the 2005 population is imported as BasePop.CoDt.
?	Population by dev. type (year <=2005)	UrbRurPop.CoDt = UrbRurPopProp.CoDt * Pop.Co	
?	Population growth				PopGrowth.Co = Pop.Co - sum(BasePop.CoDt)	
?	Population growth by dev. type		PopGrowth.CoDt = PopGrowth.Co * UrbRurGrowthSplit.CoDt                    
?	Population by dev. type (year > 2005)	UrbRurPop.CoDt = BasePop.CoDt + PopGrowth.CoDt	
Urban growth boundary area
Uses the base year urban growth boundary (UGB) areas in “ugb_areas.csv” and the urban growth boundary growth rates in “ugb_area_growth_rates.csv”.	
?	UGB area (year <= 2005)               		UgbAreas.CoUt = BaseUgbAreas.CoUt	
?	Population growth rate by dev. type	PopGrowthRate.CoDt = PopGrowth.CoDt / BasePop.CoDt	
?	UGB area (year > 2005)                    		UgbAreas.CoUt = (1 + PopGrowthRate.CoDt * UgbAreaGrowthRates.CoUt) * UgbAreas.CoUt
Urban density
?	Town density (people/sq mile)		TownDen.Co = UrbRurPop.CoDt(Town) / UgbAreas.CoUt(Town)
?	Metropolitan density (people/sq mile)	MetroDen.Co = UrbRurPop.CoDt(Metro)/UgbAreas.CoUt(Metro)	
Rural density	
Uses base year average rural densities by county in “ave_rural_pop_density.csv”.
?	Rural density (<= 2005)			AveRuralDen.Co
?	Rural population				RuralPop.Co = UrbRurPop.CoDt(Rural)	
?	Base year rural population			BaseRuralPop.Co = BasePop.CoDt(Rural)	
?	Rural population growth			RuralPopGrowth.Co = RuralPop.Co – BaseRuralPop.Co	
?	Rural density (>2005)			AveRuralDen.Co = AveRuralDen.Co * BaseRuralPop.Co / RuralPop.Co + 120 * RuralPopGrowth.Co / RuralPop.Co	
Assign development types to households	
Randomly sample households and assign to development types based on the probability of being in a particular development type, given by UrbRurPop.CoDt / sum(UrbRurPop.CoDt)
?	Development type				DevType.Hh
Density at household level	
Applies the predictDensityUrban calculation, which uses the UbzDenModel_ models to assign each metropolitan household a population density drawn from a likely population density distribution corresponding to the overall metropolitan area density. 
?	Density for metropolitan households	Den.Hh(Metro)
?	Density for households in towns		Den.Hh(Town) = TownDen.Co
?	Density for rural households		Den.Hh(Rural) = AveRuralDen.Co
?	Natural log of density			LogDen.Hh = log(Den.Hh)	
Income summaries	
?	Income by development type		Inc.CoDt = sum(Inc.Hh)
MODEL DESCRIPTION
Models were developed to estimate density and land use characteristics at a Census tract level given more aggregate policy assumptions about metropolitan and other urban area characteristics.  
Several land use characteristics must be predicted for households in order to estimate household vehicle ownership and vehicle travel. These include the type of area where the household resides (metropolitan, other urban, rural), the population density (persons per square mile) of the Census tract where the household resides, and the urban form characteristics of the Census tract where the household resides (urban mixed-use vs. other). Although the vehicle and travel models require Census tract level characteristics, this level of geography is not explicitly represented in the model because GreenSTEP was developed to model policies at a statewide level. Therefore, models and calculation methods were developed to estimate likely Census tract characteristics for urban areas based on larger scale characteristics. 
Land use characteristics are assigned to households in the following steps:
1.	Each household in each county is assigned to one of three development types - metropolitan, other urban, or rural.
2.	The geographic extent of urban growth in metropolitan and other urban areas in each county is calculated.
3.	Overall metropolitan, other urban and rural densities are calculated.
4.	Households are assigned a Census tract population density based on the overall metropolitan, urban or rural area where it is located.
5.	Households in metropolitan areas are designated as being in an urban mixed-use community/neighborhood or not, based on Census tract density and existing metropolitan amounts/future goals for urban mixed-use development.
Households in each county are assigned to metropolitan, other urban, and rural development types based on the base year distribution of population by development type and forecasts of the proportions of future population growth by type. The base year distribution is developed from Census data, using Census tract population density as an indicator. Forecasts of proportions of population growth of each type are developed from local sources. From these data, the total proportions of households to be assigned to each development type are computed. Households are then randomly assigned to each type using the calculated proportions as probabilities. Since the forecasts of population growth proportions are inputs to the model, they can be modified to test the effects of alternative land use policies on vehicle travel (e.g. what is the effect of a greater proportion of population growth occurring in rural areas). 
The geographic extent of metropolitan and other urban areas is calculated from base year measurements of urban growth boundary areas by type for each county and policy inputs which describe how rapidly urban growth boundaries grow relative to population. For example, a value of 0.5 for metropolitan development means that the urban growth area for the metropolitan portion of a county will grow at half the rate of metropolitan population growth.
Overall population densities for metropolitan and other urban areas in each county are computed from the number of households (and hence people), assigned to each development type, and the total urban area computed for each type. For non-metropolitan areas, it is assumed that Census tract densities are approximately equal to overall urban densities since small cities tend to be composed of few Census tracts and population densities in small cities tend to be fairly uniform.
The assumption of uniform density is not valid for metropolitan areas since Census tract densities can vary by orders of magnitude within a metropolitan area. This can be seen in Figure 17, which compares the Census tract population density distributions of a number of U.S. metropolitan areas.
Figure 18: Population Density Distributions for Selected Metropolitan Areas
 
Census tract densities are assigned to metropolitan areas by sampling from distributions such as those shown in Figure 17. Each distribution is associated with a different overall metropolitan density. Higher density metropolitan areas, like Los Angeles, have density distributions that are broader and the peak is shifted to the right. The model includes several prototype density distributions including Atlanta, Portland, San Francisco, Los Angeles, and several additional prototypes having densities less than Atlanta and greater than Los Angeles. The data on each prototype includes the overall average metropolitan density and the distribution of Census tract population densities. The range of densities represented by the prototypes is sufficient to address all land use policies that might be tested. The lower density prototypes are generated by shifting the Atlanta distribution to the left. The higher density prototypes are generated by shifting the Los Angeles distribution to the right. The corresponding overall metropolitan area densities for these created prototypes are calculated as the harmonic means of the distributions of population by Census tract density.
The procedure for applying the model to the metropolitan portion of a county is:
1.	The overall metropolitan density is calculated from the metropolitan population and the metropolitan area.
2.	The overall population density of the metropolitan area is compared to the overall densities of the prototype areas to identify the prototype metropolitan areas that have population densities that bound the subject metropolitan area density.
3.	The proportional difference between the overall metropolitan area density and the respective densities of the bounding prototypes is used to compute a weighted average of the population distributions by Census tract density of the two prototypes.
4.	The resulting population distribution by Census tract density is used as a sample distribution to assign densities to the households assigned to the metropolitan area.
Figure 18 shows the results of generating a Census tract density distribution for a metropolitan area having an overall density between those of San Francisco and Los Angeles. The model process produces a Census tract density distribution that is between the prototype density distributions.
Figure 19: Test Generation of Metropolitan Census Tract Density Distribution
 
For rural areas, a uniform population density is assumed for the rural portions of each county. Although densities in rural areas vary, the degree of variation is not large and the variation tends to be localized. For the base year, this density is the household weighted average of rural Census tract densities. For forecast years, it is assumed that additional rural population will be added at a density of one household per two acres. The new development is averaged with the base year density to arrive at the forecast rural density.
Density is one of several land use variables associated with the amount of vehicle travel that occurs. These are often referred to as the 4-Ds or 5-Ds. Following are the variables included in the Transportation Research Board (TRB) Special Report 298:
?	Density: Population and employment by geographic unit (e.g., per square mile, per developed acre).
?	Diversity: Mix of land uses, typically residential and commercial development, and the degree to which they are balanced in an area (e.g., jobs-housing balance). 
?	Design: Neighborhood layout and street characteristics, particularly connectivity, presence of sidewalks and other design features (e.g., shade, scenery, presence of attractive homes and stores) that enhance the pedestrian and bicycle friendliness of an area.
?	Destination accessibility: Ease or convenience of trip destinations from point of origin, often measured at the zonal level in terms of distance from the central business district or other major centers.
?	Distance to transit: Ease of access to transit from home or work (e.g., bus or rail stop within 1/4–1/2 mi of trip origin).
Several land-use related variables in the NHTS dataset were tested in the vehicle ownership and vehicle travel models to capture these effects. Census tract population density (HTPPOPDN) was found to be highly significant. Household and worker density measures are also available, but population density was found to have a stronger association. 
Density measures are available at the Census block group level and the Census tract level. The Census tract level measure is used because it is in keeping with the large scale nature of GreenSTEP and is more likely to provide a more consistent indicator of transportation effects related to population density. 
The other variable found to be highly significant in these models is the HTHUR variable. This variable is explained in Appendix Q of the NHTS User’s Guide, and was developed by Claritas, Inc., to represent the rural urban continuum.  Census tracts are identified as being rural, town, suburban, second city, or urban. Rural and town designations are based on the population density of the area where the Census tract is located. The suburban, second city and urban designations are based on the combination of the population density at their location and the population density at the location of the nearest population center. 
The urban classification, according to the classification system represented by the HTHUR variable, is likely to represent several land use characteristics on the TRB list. The urban classification is closely related to the older, more central portions of metropolitan areas. These areas typically have more neighborhood-level mixing of different land uses, a grid-based street system with greater connectivity, greater pedestrian accessibility and sidewalk orientation of land uses, and greater transit accessibility. Since the variable measures the relationship of the Census tract to the density of the nearest population center, it also has a relationship to the destination accessibility of the area. The urban classification is useful for capturing land use effects in the vehicle ownership and vehicle travel models that are not captured by population density alone.
Although the HTHUR variable is clearly related to population density, the relationship is not so strong as to create co-linearity problems in the models that use both variables. Table 2 shows that almost all of the households at densities of 30,000 persons per square mile are identified as being in an urban type area. Almost none of the households located in densities less than 3,000 persons per square mile are identified as being in an urban type area. However, in the middle range of densities, at which about two-thirds of the “urban mixed-use” households live, there is a substantial amount of variation in the percentage of households of this type. Table 3 shows that the residual deviance of a binary logit model to predict urban mixed-use type based on population density is relatively high. 
Table 2: Comparison of Population Density and “Urban” Type of Households
Population Density	Urban Type % of Households	% of Urban Type Households	Total Number of Households
50	0	0	9,653
300	0.1	0.2	10,079
750	0.3	0.3	4,971
1500	0.8	1.0	6,639
3000	3.9	6.6	9,754
7000	19.9	32.5	9,399
17000	67.8	33.0	2,809
30000	95.5	26.4	1,592
Although the “urban” type is not determined solely by the population density of the Census, it is important to account for the fact that Census tracts having higher population densities are more likely to be an “urban” type. Not accounting for this relationship will result in an underestimation of the effect on household vehicle travel of land use policies that result in higher densities. A simple binomial logit model was developed to predict the likelihood that a household is located in an “urban” type area based on Census tract population density. Table 3 shows a summary of the model.
Table 3: Urban Mixed-Use Development Type Model
Coefficients	Estimate	Std. Error	t value	Pr(>|t|)
(Intercept)	-3.16E+00	3.93E-02	-80.44	< 2e-16 ***
Htppopdn	2.80E-04	4.48E-06	62.5	< 2e-16 ***
Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Deviance Residuals
Min	1Q	Median	3Q	Max
-3.2416	-0.4326	-0.3533  	0.1024  	2.5263 

(Dispersion parameter for binomial family taken to be 1)
Null deviance: 21150 on 18428 degrees of freedom
Residual deviance: 11972 on 18427 degrees of freedom
AIC: 11976
Number of Fisher Scoring iterations: 5
Figure 20 compares observed and modeled values by metropolitan area for the NHTS survey dataset. The box and whiskers plots show the distributions of results for 30 model runs. The red dots show the survey results. 
The amount of urban mixed-use development, however, is not just a deterministic property of a metropolitan area. It is also affected by land use policies and the model needs to reflect the effects of policies aimed at changing the amount of this development. To accommodate testing of the effects of more or less urban mixed-use development, the user can input a target urban mixed-use proportion for each metropolitan area and the intercept for the model shown in Table 3 will be iteratively adjusted until the overall urban proportion is within 0.01 of the input proportion. Not all possible targets are achievable, however, given the form of the model equation. For example, it is unrealistic to expect a high percentage of urban mixed-use development in a metropolitan area having a low overall population density. The function that implements the iterative adjustment will stop when further adjustment to the mixed-use proportion is infeasible.
Figure 20: Observed and Estimated Number of Urban Mixed Use Households by Metropolitan Area
 
Outputs
COUNTY.RDATA, COUNTY$SYNPOP..$POWPERCAPINC, COUNTY$SYNPOP..$HHINCTTL, COUNTY$SYNPOP..$INCGRP
In the output directory there are files containing the household simulation results for each county. These files are named with the name of the county and an “.RData” extension. Within each of the county files is a table called “SynPop..” which contains variables for each household in the county. The urban form model adds three variables to the table for each household:
?	DevType: Whether the household is in a Metropolitan, Town, or Rural area
?	Htppopdn: The population density of the area that the household lives in
?	Urban: Whether the household is in a mixed use urban area or not
The SynPop.. table, following the application of the household income model, is shown in Figure 20.
Figure 21: File layout for SynPop.. following application of the urban form models
 
INC.CODT.RDATA, POP.CODT.RDATA
In addition, this component of the model produces two summaries that are saved into the output directory:
?	Inc.CoDt.Rdata: Income by county and by development type
?	Pop.CoDt.Rdata: Population by county and by development type
Model Sensitivity
The urban forms models are sensitive to assumptions describing various types of alternative land use scenarios. These scenarios include allocating development amongst metropolitan, urban and rural areas, policies that will affect growth in urban areas (e.g. allowing urban growth boundaries to grow) and policies that affect urban form in metropolitan areas. There are three key files that can be used to describe alternative land use scenarios:
?	urban_rural_growth_splits.csv is used to allocate population growth by area type. This file can be used, for example, to represent restrictions in rural population growth through land use policies.
?	ugb_area_growth_rates.csv is used in used to adjust the growth in urban land area over time. This allows the testing of policies such as hard limits on urban growth that will (in combination with assumptions in urban_rural_growth_splits.csv) increase urban density over time.
?	One type of policy that can be tested is to attempt to increase the proportion of the population living in mixed use areas with a well interconnected street and walkway system. metropolitan_urban_type_proportions.csv contains a table of household proportions that are located in urban mixed-use areas by metropolitan area and forecast year. The file can be used to test the effects of policies that facilitate and encourage mixed-use development.
Business Allocation, Density and Mixed Use
Introduction
Inputs
Proposed Calculations
Outputs
Model Sensitivity
Auto Ownership
Introduction
The vehicle ownership model predicts the number of vehicles each household is likely to own based on the number of persons of driving age in the household, whether only elderly persons live in the household, the income of the household, and the population density where the household lives. For metropolitan households, vehicle ownership also depends on the freeway supply, transit supply and whether the household is located in an urban mixed-use area. 
Inputs
COUNTY.RDATA, COUNTY$SYNPOP..
Source: This file is output from the urban form model.
FREEWAY_LANE_MILES.CSV
Format: freeway_lane_miles.csv is a table of base year freeway lane miles by metropolitan area. Figure 21 shows the file layout, with one row per metropolitan area and the values in units of miles. The freeway lane miles data are used to describe the supply of freeway capacity in the vehicle ownership model.
Source: This file is user created. The analyst can obtain these data from FHWA’s Highway Statistics data (http://www.fhwa.dot.gov/ohim/hs01/index.htm) and State DOT data.
Figure 22: File layout for freeway_lane_miles.csv
 
FWY_ART_GROWTH.CSV
Format: fwy_art_growth.csv contains information about rates of freeway lane-mile growth and arterial lane-mile growth relative to population for each metropolitan area. For example, a value of 0.5 for arterials means that arterial lane-miles are assumed to grow at half the rate of population growth. Figure 22 shows the file layout, with one row per metropolitan area and columns for freeways and arterials.
Source: This file is user created.
Figure 23: File layout for fwy_art_growth.csv
 
TRANSIT_REVENUE_MILES.CSV
Format: transit_revenue_miles.csv is a table of annual bus and rail revenue miles  per capita by metropolitan area. Figure 23 shows the file layout, with one row per metropolitan area and a column for each of bus, rail and total revenue miles. The values are per capita revenue miles, i.e. total revenue miles divided by the population of the metropolitan area. The transit revenue miles are used to represent transit supply in the vehicle ownership model.
Source: This file is user created. The analyst can obtain these data from the National Transit Database (http://www.ntdprogram.gov/ntdprogram/data.htm)
Figure 24: File layout for transit_revenue_miles.csv
 
TRANSIT_GROWTH.CSV
Format: transit_growth.csv contains information about rates of transit revenue-mile growth relative to population growth for each metropolitan area and the proportion of transit revenue-mile growth that is electrified rail transit. Figure 24 shows the file layout, with one row per metropolitan area for each of the following variables:
?	RevMiCapGrowth – rates of transit revenue-mile growth relative to population growth. A value of 1 indicates that revenue miles grow at the same rate as the population, less than 1 and population growth exceeds revenue mile growth, and more than 1 revenue mile growth is faster than population growth.
?	PctElectric – the proportion of transit revenue-mile growth that is electrified rail transit.
Source: This file is user created. 
Figure 25: File layout for transit_growth.csv
 
Proposed calculations
GREENSTEP CODE REFERENCE
This component of the model uses the GreenSTEP_Sim.r script script.
Driving age population, elderly households
?	Driving age population			DrvAgePop.Hh = Hsld.HhAp(-Age0to14)	
?	Households having only elderly (>65)	OnlyElderly.Hh
Vehicle ownership
Applies the predictVehOwn models to calculate vehicle ownership for each household. Binomial logit choice models are applied to estimate the probability that a household has zero, less than one, one, and more than one vehicle per driving age person; specific number of vehicles are then assigned to households in the less than one and more than categories by drawing from the observed distribution of vehicle ownership in such households.    
MODEL DESCRIPTION
The vehicle ownership model predicts the number of vehicles owned by each household. It is implemented in two stages. In the first stage, households are categorized by the ratio of vehicles per driving age person according to the following categories:
1.	Zero vehicles.
2.	Less than one vehicle per driving age person.
3.	One vehicle per driving age person.
4.	More than one vehicle per driving age person.
In the second stage, the number of vehicles for category 2 and category 4 households is determined.
The first stage is implemented using a set of binomial logit models. Separate sets of models are used for metropolitan and non-metropolitan areas. The metropolitan models include freeway supply, transit supply and urban type variables, while the non-metropolitan models do not.
The models are segmented into three groups defined by the number of persons of driving age in the household: one driving age person, two driving age persons, three or more driving age persons. Table 4 to Table 14 show the statistics for models for the metropolitan zero vehicle, less than one vehicle, one vehicle, and greater than one vehicle households, respectively. Colons between variable names indicate that the variables are interacted. The variables in the models have the following meanings: 
?	Hhincttl – total annual household income in dollars
?	Htppopdn – census tract population density in persons per square mile
?	Transmilescap – annual metropolitan transit revenue miles per person
?	Urban – dummy variable indicating whether household is in an urban mixed-use area
?	Fwylnmicap – metropolitan freeway lane miles per 1000 persons
?	OnlyElderly – dummy variable indicating whether all persons in the household are 65 years old or older
Table 4: Metropolitan Area Zero-Vehicle Household Models - One Driving Age Person in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	-6.83E-01	2.44E-01	-2.8	0.005108	**
Hhincttl	-1.10E-04	8.84E-06	-12.484	< 2e-16	***
Htppopdn	1.10E-04	4.00E-05	2.739	0.006159	**
Tranmilescap	-3.62E-02	1.11E-02	-3.273	0.001063	**
Urban	1.03E+00	2.17E-01	4.731	2.23E-06	***
Hhincttl:Htppopdn	9.06E-10	3.25E-10	2.791	0.005259	**
Hhincttl:Tranmilescap	9.50E-07	2.45E-07	3.886	0.000102	***
Hhincttl:Urban	1.97E-05	7.41E-06	2.662	0.007772	**
Htppopdn:Tranmilescap	9.63E-07	4.49E-07	2.144	0.032065	*
Htppopdn:Urban	-5.51E-05	1.53E-05	-3.602	0.000316	***
Htppopdn:Fwylnmicap	-1.19E-04	5.15E-05	-2.315	0.020601	*
Tranmilescap:Fwylnmicap	5.77E-02	2.06E-02	2.803	0.005059	**

Table 5: Metropolitan Area Zero-Vehicle Household Models - Two Driving Age Persons in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	-1.43E+00	1.48E-01	-9.634	< 2e-16	***
Hhincttl	-6.79E-05	5.00E-06	-13.589	< 2e-16	***
Hhincttl:Htppopdn	1.42E-09	1.98E-10	7.152	8.53E-13	***
Hhincttl:OnlyElderly	-3.55E-05	7.05E-06	-5.041	4.64E-07	***
Htppopdn:Tranmilescap	1.85E-06	1.66E-07	11.124	< 2e-16	***

Table 6: Metropolitan Area Zero-Vehicle Household Models - Three or More Driving Age Persons in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	-3.49E+00	4.86E-01	-7.188	6.57E-13	***
Hhincttl	-4.90E-05	8.12E-06	-6.04	1.54E-09	***
Htppopdn	9.72E-05	1.76E-05	5.513	3.53E-08	***
Hhincttl:Htppopdn	7.31E-10	3.58E-10	2.04	4.14E-02	*
Tranmilescap:Fwylnmicap	7.55E-02	2.28E-02	3.308	0.000938	***

Table 7: Metropolitan Area <1-Vehicle per Driving Age Person Household Models - Two Driving Age Persons in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	-2.63E-01	9.58E-02	-2.743	0.006095	**
Hhincttl	-4.59E-05	2.31E-06	-19.893	< 2e-16	***
Htppopdn	5.65E-05	1.33E-05	4.235	2.28E-05	***
OnlyElderly	1.74E+00	3.27E-01	5.302	1.15E-07	***
Hhincttl:Htppopdn	1.19E-09	9.59E-11	12.434	< 2e-16	***
Hhincttl:Tranmilescap	3.34E-07	5.09E-08	6.566	5.15E-11	***
Hhincttl:OnlyElderly	9.36E-06	2.66E-06	3.516	0.000438	***
Htppopdn:Tranmilescap	-1.43E-06	2.44E-07	-5.843	5.13E-09	***
Htppopdn:Urban	-4.75E-05	1.01E-05	-4.694	2.68E-06	***
Htppopdn:OnlyElderly	-2.71E-05	9.64E-06	-2.811	0.004933	**
Tranmilescap:Urban	2.95E-02	3.84E-03	7.677	1.63E-14	***
OnlyElderly:Tranmilescap	-1.29E-02	5.22E-03	-2.47	0.013501	*

Table 8: Metropolitan Area <1-Vehicle per Driving Age Person Household Models - Three or More Driving Age Persons in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	9.34E-01	1.03E-01	9.089	< 2e-16	***
Hhincttl	-1.83E-05	1.52E-06	-12.042	< 2e-16	***
OnlyElderly	5.21E+00	2.32E+00	2.24	0.0251	*
Hhincttl:Tranmilescap	1.66E-07	3.03E-08	5.475	4.38E-08	***
Hhincttl:Urban	1.31E-05	2.09E-06	6.283	3.32E-10	***
Hhincttl:OnlyElderly	-1.20E-04	5.43E-05	-2.216	0.0267	*
Urban:Htppopdn	-4.89E-05	9.92E-06	-4.935	8.01E-07	***
Htppopdn:Fwylnmicap	8.93E-05	1.91E-05	4.686	2.79E-06	***
Urban:Fwylnmicap	-6.89E-01	3.26E-01	-2.112	0.0347	*

Table 9: Metropolitan Area One-Vehicle per Driving Age Person Household Models - One Driving Age Person in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	6.22E-01	1.05E-01	5.951	2.67E-09	***
Tranmilescap	2.33E-02	4.73E-03	4.92	8.65E-07	***
Hhincttl:Htppopdn	1.13E-09	1.25E-10	9.09	< 2e-16	***
Tranmilescap:Hhincttl	-2.76E-07	5.17E-08	-5.342	9.17E-08	***
Hhincttl:OnlyElderly	7.20E-06	2.71E-06	2.654	0.00796	**
Tranmilescap:Htppopdn	-1.66E-06	2.24E-07	-7.436	1.04E-13	***
Htppopdn:Urban	-4.54E-05	7.85E-06	-5.779	7.50E-09	***
Htppopdn:Fwylnmicap	4.08E-05	1.64E-05	2.484	0.01298	*
Tranmilescap:OnlyElderly	-7.76E-03	3.34E-03	-2.326	0.02004	*

Table 10: Metropolitan Area One-Vehicle per Driving Age Person Household Models - Two Driving Age Persons in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	1.53E-01	6.95E-02	2.204	0.027558	*
Hhincttl	5.79E-06	8.57E-07	6.755	1.43E-11	***
Htppopdn	4.02E-05	1.16E-05	3.479	0.000503	***
Urban	-3.81E-01	1.62E-01	-2.349	0.018817	*
OnlyElderly	-5.54E-01	1.21E-01	-4.575	4.77E-06	***
Hhincttl:Htppopdn	2.41E-10	1.20E-10	2.008	0.044633	*
Hhincttl:Urban	8.18E-06	2.12E-06	3.864	0.000112	***
Hhincttl:OnlyElderly	7.11E-06	2.14E-06	3.322	0.000894	***
Htppopdn:Tranmilescap	-1.79E-06	2.10E-07	-8.519	< 2e-16	***
Htppopdn:Urban	-4.94E-05	8.97E-06	-5.509	3.61E-08	***

Table 11: Metropolitan Area One-Vehicle per Driving Age Person Household Models - Three or More Driving Age Persons in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	-1.28E+00	1.27E-01	-10.099	< 2e-16	***
Hhincttl	7.91E-06	1.42E-06	5.555	2.78E-08	***
Htppopdn	-5.76E-05	1.66E-05	-3.472	0.000517	***
Hhincttl:Htppopdn	5.38E-10	1.81E-10	2.975	0.002928	**
Tranmilescap:Urban	-2.04E-02	4.39E-03	-4.64	3.49E-06	***

Table 12: Metropolitan Area >1-Vehicle per Driving Age Person Household Models - One Driving Age Person in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	-1.75E+00	1.02E-01	-17.089	< 2e-16	***
Hhincttl	1.61E-05	1.32E-06	12.195	< 2e-16	***
Htppopdn	-5.67E-05	1.92E-05	-2.953	0.003152	**
OnlyElderly	-1.02E+00	2.70E-01	-3.782	0.000156	***
Htppopdn:Tranmilescap	-1.19E-06	4.23E-07	-2.801	0.005097	**
Htppopdn:Urban	4.53E-05	1.74E-05	2.61	0.009065	**
Urban:Fwylnmicap	-9.46E-01	2.83E-01	-3.336	0.000848	***
OnlyElderly:Fwylnmicap	1.11E+00	4.19E-01	2.643	0.008227	**

Table 13: Metropolitan Area >1-Vehicle per Driving Age Person Household Models - Two Driving Age Persons in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	-1.96E+00	1.21E-01	-16.164	< 2e-16	***
Hhincttl	7.57E-06	9.77E-07	7.75	9.19E-15	***
Fwylnmicap	7.64E-01	1.54E-01	4.944	7.65E-07	***
OnlyElderly	-6.65E-01	9.62E-02	-6.912	4.77E-12	***
Hhincttl:Htppopdn	5.78E-10	1.55E-10	3.725	0.000195	***
Htppopdn:Tranmilescap	-1.27E-06	3.29E-07	-3.841	0.000123	***
Htppopdn:Urban	2.87E-05	1.42E-05	2.012	0.04425	*
Fwylnmicap:Htppopdn	-1.56E-04	2.39E-05	-6.52	7.03E-11	***
Tranmilescap:Urban	-2.27E-02	5.74E-03	-3.961	7.47E-05	***
Table 14: Metropolitan Area >1-Vehicle per Driving Age Person Household Models – Three or More Driving Age Persons in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	-1.00E+00	1.14E-01	-8.751	< 2e-16	***
Htppopdn	-3.01E-04	3.55E-05	-8.478	< 2e-16	***
Tranmilescap	-1.29E-02	3.71E-03	-3.462	0.000537	***
Htppopdn:Hhincttl	2.21E-09	3.31E-10	6.659	2.76E-11	***

The estimated percentages of households by vehicle ownership category for each household income group match the observed values fairly well. However, for the $0 to $20,000 income group, the model underestimates the proportion of zero vehicle households and overestimates the proportion of households owning as many vehicles as driving age persons.
Figure 25 compares the observed and estimated average vehicle ownership ratios by income group. Except for the lowest income group, the observed means for the estimation dataset are within the range of average values produced by 50 simulations using the estimation dataset input values. The overestimation of vehicle ownership for the lower income households is consistent with the underestimation of zero-vehicle households. Since vehicle ownership affects vehicle travel, this overestimate can be expected to result in an overestimate of vehicle travel by lower income households as well. However, since these households travel less and are a small percentage of all households, the effect on total emissions will be small. 
Figure 26: Observed and Estimated Mean Vehicle Ownership Ratios for Metropolitan Households by Income Group
 
Table 15 to 
Table 23 show the statistics for the non-metropolitan area zero vehicle, less than one vehicle, one vehicle, and greater than one vehicle models, respectively. The variables in the models have the same meanings as for the metropolitan models. The non-metropolitan models are much simpler because they do not include the variables that are unique to the metropolitan models.
Table 15: Non-Metro Area Zero-Vehicle Household Models - One Driving Age Person in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	-7.65E-01	9.16E-02	-8.35	< 2e-16	***
Hhincttl	-9.49E-05	5.07E-06	-18.712	< 2e-16	***
Htppopdn	5.59E-05	1.26E-05	4.424	9.67E-06	***
Hhincttl:Htppopdn	1.55E-09	4.42E-10	3.509	0.00045	***
Htppopdn:OnlyElderly	3.45E-05	1.26E-05	2.746	0.00604	**

Table 16: Non-Metro Area Zero-Vehicle Household Models - Two Driving Age Persons in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	-1.97E+00	1.58E-01	-12.51	< 2e-16	***
Hhincttl	-8.50E-05	5.67E-06	-14.982	< 2e-16	***
Htppopdn	9.49E-05	1.24E-05	7.663	1.82E-14	***
OnlyElderly	-7.51E-01	2.32E-01	-3.237	0.00121	**
Htppopdn:OnlyElderly	6.91E-05	3.02E-05	2.287	0.02222	*

Table 17: Non-Metro Area Zero-Vehicle Household Models – Three of More Driving Age Persons in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	-3.18E+00	3.13E-01	-10.155	< 2e-16	***
Hhincttl	-5.00E-05	8.05E-06	-6.21	5.29E-10	***
Htppopdn	1.33E-04	2.14E-05	6.229	4.68E-10	***

Table 18: Non-Metro Area <1-Vehicle Per Driving Age Person Household Models - Two Driving Age Persons in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	-4.14E-01	6.27E-02	-6.596	4.22E-11	***
Hhincttl	-3.93E-05	1.43E-06	-27.452	< 2e-16	***
Htppopdn	4.71E-05	1.04E-05	4.548	5.43E-06	***
OnlyElderly	3.04E-01	9.55E-02	3.179	0.00148	**
Hhincttl:Htppopdn	9.68E-10	2.00E-10	4.832	1.35E-06	***
Hhincttl:OnlyElderly	1.54E-05	2.32E-06	6.624	3.51E-11	***

Table 19: Non-Metro Area <1-Vehicle Per Driving Age Person Household Models – Three or More Driving Age Persons in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	4.82E-01	5.98E-02	8.049	8.36E-16	***
Hhincttl	-1.26E-05	8.47E-07	-14.898	< 2e-16	***
Htppopdn	9.05E-05	9.44E-06	9.588	< 2e-16	***
OnlyElderly	1.83E+00	4.89E-01	3.745	0.000181	***

Table 20: Non-Metro Area One-Vehicle Per Driving Age Person Household Models - One Driving Age Person in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	9.73E-01	6.13E-02	15.887	< 2e-16	***
Hhincttl	-9.72E-06	1.44E-06	-6.768	1.30E-11	***
Htppopdn	-2.84E-05	1.11E-05	-2.559	0.0105	*
OnlyElderly	2.55E-01	9.35E-02	2.723	0.00647	**
Hhincttl:Htppopdn	1.49E-09	2.92E-10	5.123	3.01E-07	***
Hhincttl:OnlyElderly	6.46E-06	2.57E-06	2.519	0.01177	*
Htppopdn:OnlyElderly	-2.76E-05	1.30E-05	-2.115	0.03443	*

Table 21: Non-Metro Area One-Vehicle Per Driving Age Person Household Models - Two Driving Age Persons in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	2.44E-01	4.02E-02	6.082	1.19E-09	***
Hhincttl	2.21E-06	6.31E-07	3.508	0.000451	***
Htppopdn	-5.87E-05	1.01E-05	-5.821	5.85E-09	***
OnlyElderly	-3.62E-01	7.92E-02	-4.575	4.76E-06	***
Hhincttl:Htppopdn	1.29E-09	1.79E-10	7.184	6.77E-13	***
Hhincttl:OnlyElderly	7.84E-06	1.57E-06	4.98	6.36E-07	***
Htppopdn:OnlyElderly	-5.58E-05	1.45E-05	-3.854	0.000116	***

Table 22: Non-Metro Area One-Vehicle Per Driving Age Person Household Models – Three or More Driving Age Persons in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	-1.09E+00	6.34E-02	-17.167	< 2e-16	***
Hhincttl	7.32E-06	8.47E-07	8.632	< 2e-16	***
Htppopdn	-5.23E-05	1.00E-05	-5.225	1.74E-07	***

Table 23: Non-Metro Area >1-Vehicle Per Driving Age Person Household Models - One Driving Age Person in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	-1.51E+00	5.87E-02	-25.739	< 2e-16	***
Hhincttl	1.98E-05	1.17E-06	16.867	< 2e-16	***
Htppopdn	-1.01E-04	1.11E-05	-9.137	< 2e-16	***
OnlyElderly	-5.03E-01	8.01E-02	-6.273	3.54E-10	***
Htppopdn:OnlyElderly	-8.93E-05	2.96E-05	-3.019	0.00254	**

Table 24: Non-Metro Area >1-Vehicle Per Driving Age Person Household Models – Two Driving Age Persons in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	-1.29E+00	4.01E-02	-32.201	<2e-16	***
Hhincttl	9.13E-06	5.56E-07	16.409	<2e-16	***
Htppopdn	-1.28E-04	8.58E-06	-14.862	<2e-16	***
OnlyElderly	-5.89E-01	6.58E-02	-8.946	<2e-16	***
Htppopdn:OnlyElderly	-6.49E-05	3.09E-05	-2.101	0.0357	*

Table 25: Non-Metro Area >1-Vehicle Per Driving Age Person Household Models – Three or More Driving Age Persons in Household
	Estimate	Std. Error	t value	Pr(>|t|)	
(Intercept)	-1.89E+00	7.76E-02	-24.413	<2e-16	***
Hhincttl	1.03E-05	9.97E-07	10.364	<2e-16	***
Htppopdn	-1.28E-04	1.58E-05	-8.089	6.01E-16	***

The estimated percentages of households by vehicle ownership category for each household income group match the observed values fairly well. The differences in the observed and estimated proportions for the $0 to $20,000 income group are not as great as was the case with the metropolitan household comparison. However, greater differences exist in the less than one and greater than one vehicle per driving age person households in the $40,000 - $60,000 income group and the $60,000 - $80,000 income group.
Figure 26 compares the observed and estimated average vehicle ownership ratios by income group. As with the metropolitan household data, the model overestimates vehicle ownership for the lowest income group. The model also underestimates vehicle ownership in the $40,000 - $60,000 and $60,000 - $80,000 income groups. This latter difference, however, is quite small. The overestimation of lower income household vehicle ownership will have a very small effect on emissions calculations because these households are a small percentage of the total and they travel less than households in higher income groups.
Figure 27: Observed and Estimated Mean Vehicle Ownership Ratios for Non-Metropolitan Households by Income Group
 
The number of vehicles assigned to each household is computed by vehicle ownership category. Obviously, the number is zero for the first ownership category and is equal to the number of driving age persons for the third ownership category. For the other two categories, tabulations of numbers of households by number of vehicles owned were made from the estimation dataset. These tabulations were converted into proportions that are used as probabilities in a Monte Carlo process to assign the number of vehicles to the household.
Outputs
COUNTY.RDATA, COUNTY$SYNPOP..$DRVAGEPOP, COUNTY$SYNPOP..$DRVLEVELS, COUNTY$SYNPOP..$ONLYELDERLY, COUNTY$SYNPOP..$FWYLNMICAP, COUNTY$SYNPOP..$TRANMILESCAP, COUNTY$SYNPOP..$HHVEHCNT, COUNTY$SYNPOP..$VEHPERDRVAGEPOP, 
In the output directory there are files containing the household simulation results for each county. These files are named with the name of the county and an “.RData” extension. Within each of the county files is a table called “SynPop..” which contains variables for each household in the county. The urban form model adds seven variables to the table for each household:
?	DrvAgePop: The number of driving age members of the household
?	DrvLevels: Categorized version of DrvAgePop, with top category 3+ driving age members
?	OnlyElderly: dummy variable indicating whether all persons in the household are 65 years old or older
?	Fwylnmicap: metropolitan freeway lane miles per 1000 persons
?	Transmilescap: annual metropolitan transit revenue miles per person
?	Hhvehcnt: the number of vehicles that the household owns
?	VehPerDrvAgePop: ratio of the number households vehicles and the number of driving age members of the household
The SynPop.. table, following the application of the vehicle ownership model, is shown in Figure 20.
Figure 28: File layout for SynPop.. following application of the vehicle ownership models
 
Model Sensitivity
The logit models that are applied during the vehicle ownership models are sensitive to the following inputs:
?	Hhincttl – total annual household income in dollars
?	Htppopdn – census tract population density in persons per square mile
?	Transmilescap – annual metropolitan transit revenue miles per person
?	Urban – dummy variable indicating whether household is in an urban mixed-use area
?	Fwylnmicap – metropolitan freeway lane miles per 1000 persons
?	OnlyElderly – dummy variable indicating whether all persons in the household are 65 years old or older
Changes to any of these inputs will affect the vehicle ownership model results.

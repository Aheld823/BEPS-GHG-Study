library(scales); library(data.table); library(purrr); library(openxlsx); library("readxl"); library (ggpubr); library(ggplot2); library(data.table); library (tidyverse) ; library (httr) ; library(jsonlite); library (plyr); library (dplyr);library(tidyr); library (broom)

getwd()
setwd("~/")
setwd("C:/Users/andrew.held/OneDrive - Government of The District of Columbia/Documents/04_Data Analysis/GHG Study")


#Uncomment if Open data is up

Disclosure_path = "https://opendata.arcgis.com/datasets/961d83512e8b44c893d866081840b2bf_45.geojson"
request = GET(url = Disclosure_path)
response = content(request, as = "text", encoding = "UTF-8")
Disclosure = fromJSON (response, flatten = TRUE) %>%
	data.frame()
for (col in 1:ncol(Disclosure)){
	colnames(Disclosure)[col] = sub("features.properties.","",colnames(Disclosure)[col])
}

openxlsx::write.xlsx(as.data.frame(Disclosure), 'Output/Disclosure_FILE.xlsx', sheetName = "Sheet1",
										 col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)


# Pull Property Type Mappings

PROPERTYTYPEMAPPING = read_excel("Input Data/Property Type Mappings.xlsx")
Disclosure = join(Disclosure, PROPERTYTYPEMAPPING, by = c("PRIMARYPROPERTYTYPE_EPACALC"))


# Pull GHG factors Cambium

EMISSIONSFACTORS = read_excel("Input Data/Emissions Factors_Carbon Free DC.xlsx")

Disclosure = cbind(Disclosure, EMISSIONSFACTORS)

# First Filters
print (paste0("Starting Observations: ", nrow(Disclosure)))
print (paste0("Filter 1 (CY 2019 Only) Removed: ", nrow(Disclosure) - nrow(Disclosure[Disclosure$REPORTINGYEAR == 2019,])))
Disclosure = Disclosure[Disclosure$REPORTINGYEAR == 2019,]

print (paste0("Filter 2 (Compliance Filter) Removed: ", nrow(Disclosure) - nrow(Disclosure[Disclosure$REPORTSTATUS == 'In Compliance',])))
Disclosure = Disclosure[Disclosure$REPORTSTATUS == 'In Compliance',]



#Data Cleaning
Disclosure$REPORTEDBUILDINGGROSSFLOORAREA =  as.numeric(as.character(Disclosure$REPORTEDBUILDINGGROSSFLOORAREA))
Disclosure$YEARBUILT =  as.numeric(as.character(Disclosure$YEARBUILT))
Disclosure$ELECTRICITYUSE_GRID_KWH =  as.numeric(as.character(Disclosure$ELECTRICITYUSE_GRID_KWH))
Disclosure$ELECTRICITYUSE_RENEWABLE_KWH =  as.numeric(as.character(Disclosure$ELECTRICITYUSE_RENEWABLE_KWH))
Disclosure$NATURALGASUSE_THERMS =  as.numeric(as.character(Disclosure$NATURALGASUSE_THERMS))
Disclosure$FUELOILANDDIESELFUELUSEKBTU =  as.numeric(as.character(Disclosure$FUELOILANDDIESELFUELUSEKBTU))
Disclosure$DISTRSTEAM_KBTU =  as.numeric(as.character(Disclosure$DISTRSTEAM_KBTU))
Disclosure$DISTRHOTWATER_KBTU =  as.numeric(as.character(Disclosure$DISTRHOTWATER_KBTU))
Disclosure$DISTRCHILLEDWATER_KBTU =  as.numeric(as.character(Disclosure$DISTRCHILLEDWATER_KBTU))
Disclosure$TOTGHGEMISSINTENSITY_KGCO2EFT =  as.numeric(as.character(Disclosure$TOTGHGEMISSINTENSITY_KGCO2EFT))
Disclosure$TOTGHGEMISSIONS_METRICTONSCO2E =  as.numeric(as.character(Disclosure$TOTGHGEMISSIONS_METRICTONSCO2E))
Disclosure$ENERGYSTARSCORE =  as.numeric(as.character(Disclosure$ENERGYSTARSCORE))
Disclosure$SITEEUI_KBTU_FT =  as.numeric(as.character(Disclosure$SITEEUI_KBTU_FT))
Disclosure$SOURCEEUI_KBTU_FT =  as.numeric(as.character(Disclosure$SOURCEEUI_KBTU_FT))
Disclosure$WEATHERNORMALZEDSITEEUI_KBTUFT =  as.numeric(as.character(Disclosure$WEATHERNORMALZEDSITEEUI_KBTUFT))

Disclosure$REPORTEDBUILDINGGROSSFLOORAREA[ is.na(Disclosure$REPORTEDBUILDINGGROSSFLOORAREA)] = 0
Disclosure$YEARBUILT[ is.na(Disclosure$YEARBUILT)] = 0
Disclosure$ELECTRICITYUSE_GRID_KWH[ is.na(Disclosure$ELECTRICITYUSE_GRID_KWH)] = 0
Disclosure$ELECTRICITYUSE_RENEWABLE_KWH[ is.na(Disclosure$ELECTRICITYUSE_RENEWABLE_KWH)] = 0
Disclosure$NATURALGASUSE_THERMS[ is.na(Disclosure$NATURALGASUSE_THERMS)] = 0
Disclosure$FUELOILANDDIESELFUELUSEKBTU[ is.na(Disclosure$FUELOILANDDIESELFUELUSEKBTU)] = 0
Disclosure$DISTRSTEAM_KBTU[ is.na(Disclosure$DISTRSTEAM_KBTU)] = 0
Disclosure$DISTRHOTWATER_KBTU[ is.na(Disclosure$DISTRHOTWATER_KBTU)] = 0
Disclosure$DISTRCHILLEDWATER_KBTU[ is.na(Disclosure$DISTRCHILLEDWATER_KBTU)] = 0
Disclosure$TOTGHGEMISSINTENSITY_KGCO2EFT[ is.na(Disclosure$TOTGHGEMISSINTENSITY_KGCO2EFT)] = 0
Disclosure$TOTGHGEMISSIONS_METRICTONSCO2E[ is.na(Disclosure$TOTGHGEMISSIONS_METRICTONSCO2E)] = 0
Disclosure$ENERGYSTARSCORE[ is.na(Disclosure$ENERGYSTARSCORE)] = 0
Disclosure$SITEEUI_KBTU_FT[ is.na(Disclosure$SITEEUI_KBTU_FT)] = 0
Disclosure$SOURCEEUI_KBTU_FT[ is.na(Disclosure$SOURCEEUI_KBTU_FT)] = 0
Disclosure$WEATHERNORMALZEDSITEEUI_KBTUFT[ is.na(Disclosure$WEATHERNORMALZEDSITEEUI_KBTUFT)] = 0


# Recalculate Source EUI/ SITE EUI
setDT(Disclosure)[,SOURCEEUI_KBTU_FT:= round(((ELECTRICITYUSE_GRID_KWH*3.412*2.8)+
																							 (ELECTRICITYUSE_RENEWABLE_KWH*3.412)+
																							 (NATURALGASUSE_THERMS*100*1.05)+
																							 (FUELOILANDDIESELFUELUSEKBTU*1.01)+
																							 (DISTRSTEAM_KBTU*1.2)+
																							 (DISTRHOTWATER_KBTU*1.2)+
																							 (DISTRCHILLEDWATER_KBTU*.91))/REPORTEDBUILDINGGROSSFLOORAREA,1)]


setDT(Disclosure)[,SITEEUI_KBTU_FT:= round(((ELECTRICITYUSE_GRID_KWH*3.412)+
																					(ELECTRICITYUSE_RENEWABLE_KWH*3.412)+
																					(NATURALGASUSE_THERMS*100)+
																					(FUELOILANDDIESELFUELUSEKBTU)+
																					(DISTRSTEAM_KBTU)+
																					(DISTRHOTWATER_KBTU)+
																					(DISTRCHILLEDWATER_KBTU))/REPORTEDBUILDINGGROSSFLOORAREA,1)]

setDT(Disclosure)[,TOTGHGEMISSINTENSITY_KGCO2EFT:= round(((ELECTRICITYUSE_GRID_KWH*ELECTRICITYUSE_GRID_KWH_GHG_2020)+
																					(ELECTRICITYUSE_RENEWABLE_KWH*0)+
																					(NATURALGASUSE_THERMS*NATURALGASUSE_THERMS_GHG)+
																					(FUELOILANDDIESELFUELUSEKBTU*FUELOILANDDIESELFUELUSEKBTU_GHG)+
																					(DISTRSTEAM_KBTU *DISTRSTEAM_KBTU_GHG)+
																					(DISTRHOTWATER_KBTU* DISTRHOTWATER_KBTU_GHG)+
																					(DISTRCHILLEDWATER_KBTU*DISTRCHILLEDWATER_KBTU_GHG))/REPORTEDBUILDINGGROSSFLOORAREA,1)]


Disclosure$TOTAL_SOURCE_ENERGY = Disclosure$SOURCEEUI_KBTU_FT * Disclosure$REPORTEDBUILDINGGROSSFLOORAREA
Disclosure$TOTAL_SITE_ENERGY = Disclosure$SITEEUI_KBTU_FT * Disclosure$REPORTEDBUILDINGGROSSFLOORAREA
Disclosure$TOTGHGEMISSIONS_METRICTONSCO2E = Disclosure$TOTGHGEMISSINTENSITY_KGCO2EFT * Disclosure$REPORTEDBUILDINGGROSSFLOORAREA / 1000

# Filters
print (paste0("Filter 3 (Electricity Consumption): ", nrow(Disclosure) - nrow(Disclosure[Disclosure$ELECTRICITYUSE_GRID_KWH > 0,])))
Disclosure = Disclosure[Disclosure$ELECTRICITYUSE_GRID_KWH > 0,]

print (paste0("Filter 4 (Site EUI Present): ", nrow(Disclosure) - nrow(Disclosure[Disclosure$WEATHERNORMALZEDSITEEUI_KBTUFT != 0,])))
Disclosure = Disclosure[Disclosure$WEATHERNORMALZEDSITEEUI_KBTUFT != 0,]

print (paste0("Filter 5 (Parking Removal): ", nrow(Disclosure) - nrow(Disclosure[Disclosure$PRIMARYPROPERTYTYPE_EPACALC != 'Parking',])))
Disclosure = Disclosure[Disclosure$PRIMARYPROPERTYTYPE_EPACALC != 'Parking',]


Disclosure$LN_SITEUI = log(Disclosure$WEATHERNORMALZEDSITEEUI_KBTUFT)
setDT(Disclosure)[,AVERAGE_LN_SITEEUI := mean(LN_SITEUI), by = BEPSPROPERTYTYPE]
setDT(Disclosure)[,STD_LN_SITEEUI := sd(LN_SITEUI), by = BEPSPROPERTYTYPE]
print (paste0("Filter 6 (Outlier Fillter): ", nrow(Disclosure) - nrow(Disclosure[Disclosure$LN_SITEUI <= AVERAGE_LN_SITEEUI+(2*STD_LN_SITEEUI) & Disclosure$LN_SITEUI >= AVERAGE_LN_SITEEUI-(2*STD_LN_SITEEUI),])))
Disclosure = Disclosure[Disclosure$LN_SITEUI <= AVERAGE_LN_SITEEUI+(2*STD_LN_SITEEUI) & Disclosure$LN_SITEUI >= AVERAGE_LN_SITEEUI-(2*STD_LN_SITEEUI),]

print (paste0("Final Analysis Set: ", nrow(Disclosure)))



#New Buildings
#Adding new buildings based on build rates from OP. 
# We are assuming an average growth rate across sections and roudning up. So .62% every year X 6 = 3.72% growth rate each cycle (pull from Carbon Free DC)
# 3.72 is multiplied by the number of buildings in each property type and then rounded up
# Each build comes online with a very low EUI/GHG emission intensity such that it complies
# We don't need GFA or other information becuase we won't be looking at the emissions/energy consumption from these buildings
# we will just be looking at their impact on the standards

New_buildings = function(data, i, eui_variable_name, ghg_variable_name, br, nb_EUI_level, nb_GHG_level ){
	
	building_counts = ddply(data, .(BEPSPROPERTYTYPE), summarize, count = length(PID), 
		count_orginal = sum(PID != 'DUMMY NEW BUILDING'),
		EUI <- quantile(get(eui_variable_name), probs = nb_EUI_level, na.rm = TRUE),
		GHG <- quantile(get(ghg_variable_name), probs = .5, na.rm = TRUE)*nb_GHG_level, #This just 0s this value out. Could set it up to actually be a percentile if necessary
		PID = 'DUMMY NEW BUILDING')

	#print(paste("Round #",i,sept=""))	
	building_counts$growth_count_unrounded = building_counts$count_orginal * br
	building_counts$growth_count_rounded = round(building_counts$growth_count_unrounded,0)
	building_counts$growth_count_total = building_counts$count_orginal + building_counts$growth_count_rounded
	#print(building_counts[building_counts$BEPSPROPERTYTYPE=="College/University",]$growth_count_unrounded)
	z=0
	while (z  < i){
		building_counts$growth_count_unrounded = building_counts$growth_count_total* br + building_counts$growth_count_unrounded - building_counts$growth_count_rounded
		building_counts$growth_count_rounded = round(building_counts$growth_count_unrounded,0)
		building_counts$growth_count_total = building_counts$growth_count_total + building_counts$growth_count_rounded
		#print(building_counts[building_counts$BEPSPROPERTYTYPE=="College/University",]$growth_count_unrounded)

		z = z+1
	}

	colnames(building_counts)[4] <- eui_variable_name
	colnames(building_counts)[5] <- ghg_variable_name

	new_build_list = list() 
	n=1
	for (x in 1:nrow(building_counts)){
		row <-  building_counts[x,]
		for (y in 1:row$growth_count_rounded){
			if (row$growth_count_rounded > 0){
				new_build_list[[n]] = row
				n = n +1
				} else {
				}	
	}}
	new_builds = do.call(rbind,new_build_list)
	data = rbind.fill(data, new_builds)
	return(data)
}


# Simulation Frameworks

BEPS_standard_target_simuliation = function(data, iterations, standard_level, reduction, br, nb_EUI_level,nb_GHG_level){
	i = 0
	setDT(data)[,paste("BEPS_",2020+i*6, sep = "") := quantile(SITEEUI_KBTU_FT, na.rm = TRUE, c(standard_level)), by = BEPSPROPERTYTYPE]
	setDT(data)[,paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "") := ifelse(SITEEUI_KBTU_FT >  get(paste("BEPS_",2020+i*6, sep = "")),ifelse(STANDARDTARGET == "Yes",
																															 pmax(get(paste("BEPS_",2020+i*6, sep = "")),(1- reduction)*SITEEUI_KBTU_FT),(1- reduction)*SITEEUI_KBTU_FT),
																															 SITEEUI_KBTU_FT)]


	# Site EUI Savings
	setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := round((get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) - SITEEUI_KBTU_FT)/SITEEUI_KBTU_FT,2)]
	setDT(data)[,paste("Total_site_energy_",2026+i*6, sep = "") := get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) * REPORTEDBUILDINGGROSSFLOORAREA]
	setDT(data)[,paste("Annual_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]
	setDT(data)[,paste("Aggregate_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]  
	
	# Fuel level savings
	setDT(data)[,paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = ""):=ELECTRICITYUSE_GRID_KWH * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT) ]
	setDT(data)[,paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = ""):=ELECTRICITYUSE_RENEWABLE_KWH * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""):=NATURALGASUSE_THERMS * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT) ]
	setDT(data)[,paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = ""):=FUELOILANDDIESELFUELUSEKBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRSTEAM_KBTU_",2026+i*6,sep = ""):=DISTRSTEAM_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = ""):=DISTRHOTWATER_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""):=DISTRCHILLEDWATER_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]

	# GHG Savings
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2020+i*6,sep=""))/1000)) + 
				((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
				((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
				((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
				((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
				((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
				((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
				((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
				((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
				((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
				((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
				((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
				((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
	
	setDT(data)[,paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6,sep = ""):= ((get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))*1000)/REPORTEDBUILDINGGROSSFLOORAREA)]
	setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "")) - TOTGHGEMISSINTENSITY_KGCO2EFT)/TOTGHGEMISSINTENSITY_KGCO2EFT]
		
	setDT(data)[,paste("Emissions_Cumulative_Baseline_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6,sep = ""):= (((ELECTRICITYUSE_GRID_KWH * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
				((ELECTRICITYUSE_RENEWABLE_KWH * 0)/1000)+
				((NATURALGASUSE_THERMS *NATURALGASUSE_THERMS_GHG)/1000)+
				((FUELOILANDDIESELFUELUSEKBTU * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
				((DISTRSTEAM_KBTU * DISTRSTEAM_KBTU_GHG)/1000)+
				((DISTRHOTWATER_KBTU * DISTRHOTWATER_KBTU_GHG)/1000)+
				((DISTRCHILLEDWATER_KBTU * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average

	setDT(data)[,paste("Emissions_Cumulative_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) +TOTGHGEMISSIONS_METRICTONSCO2E*5 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6, sep = ""))]	
	setDT(data)[,paste("Emissions_Cumulative_Policy_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]
	setDT(data)[,paste("Emissions_Cumulative_Policy_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = "")) + TOTGHGEMISSIONS_METRICTONSCO2E*4 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""))+ get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))] 
	setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2020+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = ""))]
	setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2026+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2026+i*6, sep = ""))]
	setDT(data)[,paste("Annual_ghg_savings_",2026+i*6, sep = "") := TOTGHGEMISSIONS_METRICTONSCO2E- get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]
	
	assign('nb_EUI_level',nb_EUI_level, env=.GlobalEnv)
	assign('nb_GHG_level',nb_GHG_level, env=.GlobalEnv)
	assign('i',i, env=.GlobalEnv)
	assign('eui_variable_name',  paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""), env=.GlobalEnv)
	assign('ghg_variable_name',  paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = ""), env=.GlobalEnv)
	data = New_buildings(data = data,
	i = i,
	eui_variable_name = eui_variable_name,
	ghg_variable_name = ghg_variable_name,
	br = br,
	nb_EUI_level = nb_EUI_level,
	nb_GHG_level=nb_GHG_level)

	
	i = i +1 
	while (i < iterations){

		setDT(data)[,paste("BEPS_",2020+i*6, sep = "") := quantile(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")), na.rm = TRUE, c(standard_level)), by = BEPSPROPERTYTYPE]
		setDT(data)[,paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "") := ifelse(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")) >  get(paste("BEPS_",2020+i*6, sep = "")),
																																 pmax(get(paste("BEPS_",2020+i*6, sep = "")),(1- reduction)*get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = ""))),
																																 get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]        
		# Site EUI Savings
		setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := round((get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) - get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")),2)]
		setDT(data)[,paste("Total_site_energy_",2026+i*6, sep = "") := get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) * REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Annual_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - get(paste("Total_site_energy_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("Aggregate_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]  
		
		# Fuel level savings
		setDT(data)[,paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_GRID_KWH_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""):=get(paste("NATURALGASUSE_THERMS_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = ""):=get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRSTEAM_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRSTEAM_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRHOTWATER_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRCHILLEDWATER_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		
		# GHG Savings 
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2020+i*6,sep=""))/1000)) + 
					((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
					((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
					((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
					((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
					((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
					((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",pmin(2026+i*6,2050),sep=""))/1000)) + 
								((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
								((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
								((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
								((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
								((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
								((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))*1000) / REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "")) - get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = "")))/get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6,sep = ""):= (((ELECTRICITYUSE_GRID_KWH * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
			((ELECTRICITYUSE_RENEWABLE_KWH * 0)/1000)+
			((NATURALGASUSE_THERMS *NATURALGASUSE_THERMS_GHG)/1000)+
			((FUELOILANDDIESELFUELUSEKBTU * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
			((DISTRSTEAM_KBTU * DISTRSTEAM_KBTU_GHG)/1000)+
			((DISTRHOTWATER_KBTU * DISTRHOTWATER_KBTU_GHG)/1000)+
			((DISTRCHILLEDWATER_KBTU * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average


		setDT(data)[,paste("Emissions_Cumulative_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2020+i*6, sep = ""))*5 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6, sep = "")) ]
		setDT(data)[,paste("Emissions_Cumulative_Policy_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = ""))*4 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""))+ get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))] 
		setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2026+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2026+i*6, sep = ""))]
		setDT(data)[,paste("Annual_ghg_savings_",2026+i*6, sep = "") := get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = "")) - get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]
		#setDT(data)[,paste("Avoided_ghg_emissions_",2026+i*6, sep = "") := get(paste("Annual_ghg_savings_",2020+i*6, sep = ""))*5 +get(paste("Annual_ghg_savings_",2026+i*6, sep = ""))+get(paste("Avoided_ghg_emissions_",2020+i*6, sep = ""))] 


		
		assign('eui_variable_name',  paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""), env=.GlobalEnv)
		assign('ghg_variable_name',  paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = ""), env=.GlobalEnv)
		data = New_buildings(data = data,
		i = i,
		eui_variable_name = eui_variable_name,
		ghg_variable_name = ghg_variable_name,
		br = br,
		nb_EUI_level = nb_EUI_level,
		nb_GHG_level=nb_GHG_level)
    		
		i  = i +1
	}
	return(data)
}

BEPS_standard_target_increasing_standard_simuliation = function(data, iterations, standard_level, reduction, br, nb_EUI_level,nb_GHG_level, policy_switch, standard_modifier){
	i = 0
	setDT(data)[,paste("BEPS_",2020+i*6, sep = "") := quantile(SITEEUI_KBTU_FT, na.rm = TRUE, c(standard_level)), by = BEPSPROPERTYTYPE]
	setDT(data)[,paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "") := ifelse(SITEEUI_KBTU_FT >  get(paste("BEPS_",2020+i*6, sep = "")),ifelse(STANDARDTARGET == "Yes",
																															 pmax(get(paste("BEPS_",2020+i*6, sep = "")),(1- reduction)*SITEEUI_KBTU_FT),(1- reduction)*SITEEUI_KBTU_FT),
																															 SITEEUI_KBTU_FT)]
	
	
	# Site EUI Savings
	setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := round((get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) - SITEEUI_KBTU_FT)/SITEEUI_KBTU_FT,2)]
	setDT(data)[,paste("Total_site_energy_",2026+i*6, sep = "") := get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) * REPORTEDBUILDINGGROSSFLOORAREA]
	setDT(data)[,paste("Annual_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]
	setDT(data)[,paste("Aggregate_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]  
	
	# Fuel level savings
	setDT(data)[,paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = ""):=ELECTRICITYUSE_GRID_KWH * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = ""):=ELECTRICITYUSE_RENEWABLE_KWH * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""):=NATURALGASUSE_THERMS * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = ""):=FUELOILANDDIESELFUELUSEKBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRSTEAM_KBTU_",2026+i*6,sep = ""):=DISTRSTEAM_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = ""):=DISTRHOTWATER_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""):=DISTRCHILLEDWATER_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	
	# GHG Savings
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2020+i*6,sep=""))/1000)) + 
				((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
				((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
				((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
				((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
				((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
				((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
	
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
																																				((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
																																				((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
																																				((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
																																				((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
																																				((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
																																				((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
	setDT(data)[,paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6,sep = ""):= ((get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))*1000)/REPORTEDBUILDINGGROSSFLOORAREA)]
	setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "")) - TOTGHGEMISSINTENSITY_KGCO2EFT)/TOTGHGEMISSINTENSITY_KGCO2EFT]
		
	setDT(data)[,paste("Emissions_Cumulative_Baseline_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]

	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6,sep = ""):= (((ELECTRICITYUSE_GRID_KWH * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
																																				((ELECTRICITYUSE_RENEWABLE_KWH * 0)/1000)+
																																				((NATURALGASUSE_THERMS *NATURALGASUSE_THERMS_GHG)/1000)+
																																				((FUELOILANDDIESELFUELUSEKBTU * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
																																				((DISTRSTEAM_KBTU * DISTRSTEAM_KBTU_GHG)/1000)+
																																				((DISTRHOTWATER_KBTU * DISTRHOTWATER_KBTU_GHG)/1000)+
																																				((DISTRCHILLEDWATER_KBTU * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average

	setDT(data)[,paste("Emissions_Cumulative_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) +TOTGHGEMISSIONS_METRICTONSCO2E*5 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6, sep = ""))]	
	setDT(data)[,paste("Emissions_Cumulative_Policy_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]
	setDT(data)[,paste("Emissions_Cumulative_Policy_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = "")) + TOTGHGEMISSIONS_METRICTONSCO2E*4 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""))+ get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]
	setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2020+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = ""))]
	setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2026+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2026+i*6, sep = ""))]
	setDT(data)[,paste("Annual_ghg_savings_",2026+i*6, sep = "") := TOTGHGEMISSIONS_METRICTONSCO2E- get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]

	assign('nb_EUI_level',nb_EUI_level, env=.GlobalEnv)
	assign('nb_GHG_level',nb_GHG_level, env=.GlobalEnv)
	assign('i',i, env=.GlobalEnv)
	assign('eui_variable_name',  paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""), env=.GlobalEnv)
	assign('ghg_variable_name',  paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = ""), env=.GlobalEnv)
	data = New_buildings(data = data,
	i = i,
	eui_variable_name = eui_variable_name,
	ghg_variable_name = ghg_variable_name,
	br = br,
	nb_EUI_level = nb_EUI_level,
	nb_GHG_level=nb_GHG_level)
	
	i = i +1 
	while (i < policy_switch){
		
		setDT(data)[,paste("BEPS_",2020+i*6, sep = "") := quantile(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")), na.rm = TRUE, c(standard_level)), by = BEPSPROPERTYTYPE]
		setDT(data)[,paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "") := ifelse(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")) >  get(paste("BEPS_",2020+i*6, sep = "")),
																																 pmax(get(paste("BEPS_",2020+i*6, sep = "")),(1- reduction)*get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = ""))),
																																 get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]        
		# Site EUI Savings
		setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := round((get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) - get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")),2)]
		setDT(data)[,paste("Total_site_energy_",2026+i*6, sep = "") := get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) * REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Annual_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - get(paste("Total_site_energy_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("Aggregate_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]  
		
		# Fuel level savings
		setDT(data)[,paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_GRID_KWH_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""):=get(paste("NATURALGASUSE_THERMS_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = ""):=get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRSTEAM_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRSTEAM_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRHOTWATER_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRCHILLEDWATER_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		
		# GHG Savings 
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2020+i*6,sep=""))/1000)) + 
					((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
					((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
					((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
					((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
					((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
					((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",pmin(2026+i*6,2050),sep=""))/1000)) + 
								((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
								((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
								((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
								((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
								((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
								((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))*1000) / REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "")) - get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = "")))/get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6,sep = ""):= (((ELECTRICITYUSE_GRID_KWH * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
			((ELECTRICITYUSE_RENEWABLE_KWH * 0)/1000)+
			((NATURALGASUSE_THERMS *NATURALGASUSE_THERMS_GHG)/1000)+
			((FUELOILANDDIESELFUELUSEKBTU * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
			((DISTRSTEAM_KBTU * DISTRSTEAM_KBTU_GHG)/1000)+
			((DISTRHOTWATER_KBTU * DISTRHOTWATER_KBTU_GHG)/1000)+
			((DISTRCHILLEDWATER_KBTU * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average


		setDT(data)[,paste("Emissions_Cumulative_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2020+i*6, sep = ""))*5 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6, sep = "")) ]
		setDT(data)[,paste("Emissions_Cumulative_Policy_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = ""))*4 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""))+ get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))] 
		setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2026+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2026+i*6, sep = ""))]
		setDT(data)[,paste("Annual_ghg_savings_",2026+i*6, sep = "") := get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = "")) - get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]
		#setDT(data)[,paste("Avoided_ghg_emissions_",2026+i*6, sep = "") := get(paste("Annual_ghg_savings_",2020+i*6, sep = ""))*5 +get(paste("Annual_ghg_savings_",2026+i*6, sep = ""))+get(paste("Avoided_ghg_emissions_",2020+i*6, sep = ""))] 


		assign('eui_variable_name',  paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""), env=.GlobalEnv)
		assign('ghg_variable_name',  paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = ""), env=.GlobalEnv)
		data = New_buildings(data = data,
		i = i,
		eui_variable_name = eui_variable_name,
		ghg_variable_name = ghg_variable_name,
		br = br,
		nb_EUI_level = nb_EUI_level,
		nb_GHG_level=nb_GHG_level)

		i  = i +1
	}
	while (i < iterations){
		standard_level = standard_level - standard_modifier 
		
		setDT(data)[,paste("BEPS_",2020+i*6, sep = "") := quantile(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")), na.rm = TRUE, c(standard_level)), by = BEPSPROPERTYTYPE]
		setDT(data)[,paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "") := ifelse(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")) >  get(paste("BEPS_",2020+i*6, sep = "")),
																																 pmax(get(paste("BEPS_",2020+i*6, sep = "")),(1- reduction)*get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = ""))),
																																 get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]      
		# Site EUI Savings
		setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := round((get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) - get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")),2)]
		setDT(data)[,paste("Total_site_energy_",2026+i*6, sep = "") := get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) * REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Annual_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - get(paste("Total_site_energy_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("Aggregate_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]  
		
		# Fuel level savings
		setDT(data)[,paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_GRID_KWH_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""):=get(paste("NATURALGASUSE_THERMS_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = ""):=get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRSTEAM_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRSTEAM_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRHOTWATER_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRCHILLEDWATER_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		
		# GHG Savings 
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2020+i*6,sep=""))/1000)) + 
					((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
					((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
					((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
					((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
					((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
					((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",pmin(2026+i*6,2050),sep=""))/1000)) + 
								((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
								((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
								((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
								((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
								((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
								((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))*1000) / REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "")) - get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = "")))/get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6,sep = ""):= (((ELECTRICITYUSE_GRID_KWH * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
			((ELECTRICITYUSE_RENEWABLE_KWH * 0)/1000)+
			((NATURALGASUSE_THERMS *NATURALGASUSE_THERMS_GHG)/1000)+
			((FUELOILANDDIESELFUELUSEKBTU * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
			((DISTRSTEAM_KBTU * DISTRSTEAM_KBTU_GHG)/1000)+
			((DISTRHOTWATER_KBTU * DISTRHOTWATER_KBTU_GHG)/1000)+
			((DISTRCHILLEDWATER_KBTU * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average


		setDT(data)[,paste("Emissions_Cumulative_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2020+i*6, sep = ""))*5 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6, sep = "")) ]
		setDT(data)[,paste("Emissions_Cumulative_Policy_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = ""))*4 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""))+ get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))] 
		setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2026+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2026+i*6, sep = ""))]
		setDT(data)[,paste("Annual_ghg_savings_",2026+i*6, sep = "") := get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = "")) - get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]

		

		assign('eui_variable_name',  paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""), env=.GlobalEnv)
		assign('ghg_variable_name',  paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = ""), env=.GlobalEnv)
		data = New_buildings(data = data,
		i = i,
		eui_variable_name = eui_variable_name,
		ghg_variable_name = ghg_variable_name,
		br = br,
		nb_EUI_level = nb_EUI_level,
		nb_GHG_level=nb_GHG_level)
		
		i  = i +1
	}
	return(data)
}


BEPS_no_standard_target_simuliation = function(data, iterations, standard_level, reduction, br, nb_EUI_level,nb_GHG_level, policy_switch){
	i = 0
	setDT(data)[,paste("BEPS_",2020+i*6, sep = "") := quantile(SITEEUI_KBTU_FT, na.rm = TRUE, c(standard_level)), by = BEPSPROPERTYTYPE]
	setDT(data)[,paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "") := ifelse(SITEEUI_KBTU_FT >  get(paste("BEPS_",2020+i*6, sep = "")),ifelse(STANDARDTARGET == "Yes",
																															pmax(get(paste("BEPS_",2020+i*6, sep = "")),(1- reduction)*SITEEUI_KBTU_FT),(1- reduction)*SITEEUI_KBTU_FT),
																															 SITEEUI_KBTU_FT)]
	
	
	# Site EUI Savings
	setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := round((get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) - SITEEUI_KBTU_FT)/SITEEUI_KBTU_FT,2)]
	setDT(data)[,paste("Total_site_energy_",2026+i*6, sep = "") := get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) * REPORTEDBUILDINGGROSSFLOORAREA]
	setDT(data)[,paste("Annual_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]
	setDT(data)[,paste("Aggregate_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]  
	
	# Fuel level savings
	setDT(data)[,paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = ""):=ELECTRICITYUSE_GRID_KWH * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = ""):=ELECTRICITYUSE_RENEWABLE_KWH * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""):=NATURALGASUSE_THERMS * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = ""):=FUELOILANDDIESELFUELUSEKBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRSTEAM_KBTU_",2026+i*6,sep = ""):=DISTRSTEAM_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = ""):=DISTRHOTWATER_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""):=DISTRCHILLEDWATER_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	
	# GHG Savings
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2020+i*6,sep=""))/1000)) + 
				((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
				((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
				((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
				((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
				((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
				((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
																																				((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
																																				((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
																																				((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
																																				((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
																																				((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
																																				((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
	setDT(data)[,paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6,sep = ""):= ((get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))*1000)/REPORTEDBUILDINGGROSSFLOORAREA)]
	setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "")) - TOTGHGEMISSINTENSITY_KGCO2EFT)/TOTGHGEMISSINTENSITY_KGCO2EFT]
		
	setDT(data)[,paste("Emissions_Cumulative_Baseline_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]

	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6,sep = ""):= (((ELECTRICITYUSE_GRID_KWH * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
																																				((ELECTRICITYUSE_RENEWABLE_KWH * 0)/1000)+
																																				((NATURALGASUSE_THERMS *NATURALGASUSE_THERMS_GHG)/1000)+
																																				((FUELOILANDDIESELFUELUSEKBTU * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
																																				((DISTRSTEAM_KBTU * DISTRSTEAM_KBTU_GHG)/1000)+
																																				((DISTRHOTWATER_KBTU * DISTRHOTWATER_KBTU_GHG)/1000)+
																																				((DISTRCHILLEDWATER_KBTU * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average

	setDT(data)[,paste("Emissions_Cumulative_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) +TOTGHGEMISSIONS_METRICTONSCO2E*5 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6, sep = ""))]	
	setDT(data)[,paste("Emissions_Cumulative_Policy_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]
	setDT(data)[,paste("Emissions_Cumulative_Policy_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = "")) + TOTGHGEMISSIONS_METRICTONSCO2E*4 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""))+ get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]
	setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2020+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = ""))]
	setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2026+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2026+i*6, sep = ""))]
	setDT(data)[,paste("Annual_ghg_savings_",2026+i*6, sep = "") := TOTGHGEMISSIONS_METRICTONSCO2E- get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]

	#New Buildings
	assign('nb_EUI_level',nb_EUI_level, env=.GlobalEnv)
	assign('nb_GHG_level',nb_GHG_level, env=.GlobalEnv)
	assign('i',i, env=.GlobalEnv)
	assign('eui_variable_name',  paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""), env=.GlobalEnv)
	assign('ghg_variable_name',  paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = ""), env=.GlobalEnv)
	data = New_buildings(data = data,
	i = i,
	eui_variable_name = eui_variable_name,
	ghg_variable_name = ghg_variable_name,
	br = br,
	nb_EUI_level = nb_EUI_level,
	nb_GHG_level=nb_GHG_level)
	
	i = i +1 
	while (i < policy_switch){

		setDT(data)[,paste("BEPS_",2020+i*6, sep = "") := quantile(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")), na.rm = TRUE, c(standard_level)), by = BEPSPROPERTYTYPE]
		setDT(data)[,paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "") := ifelse(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")) >  get(paste("BEPS_",2020+i*6, sep = "")),
																																 pmax(get(paste("BEPS_",2020+i*6, sep = "")),(1- reduction)*get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = ""))),
																																 get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]        
		# Site EUI Savings
		setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := round((get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) - get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")),2)]
		setDT(data)[,paste("Total_site_energy_",2026+i*6, sep = "") := get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) * REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Annual_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - get(paste("Total_site_energy_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("Aggregate_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]  
		
		# Fuel level savings
		setDT(data)[,paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_GRID_KWH_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""):=get(paste("NATURALGASUSE_THERMS_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = ""):=get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRSTEAM_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRSTEAM_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRHOTWATER_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRCHILLEDWATER_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		
		# GHG Savings 
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2020+i*6,sep=""))/1000)) + 
					((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
					((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
					((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
					((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
					((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
					((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",pmin(2026+i*6,2050),sep=""))/1000)) + 
								((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
								((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
								((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
								((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
								((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
								((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))*1000) / REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "")) - get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = "")))/get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6,sep = ""):= (((ELECTRICITYUSE_GRID_KWH * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
			((ELECTRICITYUSE_RENEWABLE_KWH * 0)/1000)+
			((NATURALGASUSE_THERMS *NATURALGASUSE_THERMS_GHG)/1000)+
			((FUELOILANDDIESELFUELUSEKBTU * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
			((DISTRSTEAM_KBTU * DISTRSTEAM_KBTU_GHG)/1000)+
			((DISTRHOTWATER_KBTU * DISTRHOTWATER_KBTU_GHG)/1000)+
			((DISTRCHILLEDWATER_KBTU * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average


		setDT(data)[,paste("Emissions_Cumulative_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2020+i*6, sep = ""))*5 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6, sep = "")) ]
		setDT(data)[,paste("Emissions_Cumulative_Policy_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = ""))*4 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""))+ get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))] 
		setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2026+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2026+i*6, sep = ""))]
		setDT(data)[,paste("Annual_ghg_savings_",2026+i*6, sep = "") := get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = "")) - get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]

		#New Buildings
		assign('eui_variable_name',  paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""), env=.GlobalEnv)
		assign('ghg_variable_name',  paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = ""), env=.GlobalEnv)
		data = New_buildings(data = data,
		i = i,
		eui_variable_name = eui_variable_name,
		ghg_variable_name = ghg_variable_name,
		br = br,
		nb_EUI_level = nb_EUI_level,
		nb_GHG_level=nb_GHG_level)
		i  = i +1
	}
	while (i < iterations){

			setDT(data)[,paste("BEPS_",2020+i*6, sep = "") := quantile(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")), na.rm = TRUE, c(standard_level)), by = BEPSPROPERTYTYPE]
			setDT(data)[,paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "") := ifelse(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")) >  get(paste("BEPS_",2020+i*6, sep = "")),
																																	 (1- reduction)*get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")), get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]        
			# Site EUI Savings
			setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := round((get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) - get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")),2)]
			setDT(data)[,paste("Total_site_energy_",2026+i*6, sep = "") := get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) * REPORTEDBUILDINGGROSSFLOORAREA]
			setDT(data)[,paste("Annual_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - get(paste("Total_site_energy_",2026+(i-1)*6, sep = ""))]
			setDT(data)[,paste("Aggregate_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]  
			
			# Fuel level savings
			setDT(data)[,paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_GRID_KWH_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
			setDT(data)[,paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
			setDT(data)[,paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""):=get(paste("NATURALGASUSE_THERMS_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
			setDT(data)[,paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = ""):=get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
			setDT(data)[,paste("DISTRSTEAM_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRSTEAM_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
			setDT(data)[,paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRHOTWATER_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
			setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRCHILLEDWATER_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
			
			# GHG Savings 
			setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2020+i*6,sep=""))/1000)) + 
						((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
						((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
						((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
						((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
						((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
						((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
			setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",pmin(2026+i*6,2050),sep=""))/1000)) + 
									((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
									((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
									((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
									((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
									((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
									((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
			setDT(data)[,paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))*1000) / REPORTEDBUILDINGGROSSFLOORAREA]
			setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "")) - get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = "")))/get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = ""))]
			setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6,sep = ""):= (((ELECTRICITYUSE_GRID_KWH * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
				((ELECTRICITYUSE_RENEWABLE_KWH * 0)/1000)+
				((NATURALGASUSE_THERMS *NATURALGASUSE_THERMS_GHG)/1000)+
				((FUELOILANDDIESELFUELUSEKBTU * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
				((DISTRSTEAM_KBTU * DISTRSTEAM_KBTU_GHG)/1000)+
				((DISTRHOTWATER_KBTU * DISTRHOTWATER_KBTU_GHG)/1000)+
				((DISTRCHILLEDWATER_KBTU * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average


			setDT(data)[,paste("Emissions_Cumulative_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2020+i*6, sep = ""))*5 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6, sep = "")) ]
			setDT(data)[,paste("Emissions_Cumulative_Policy_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = ""))*4 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""))+ get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))] 
			setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2026+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2026+i*6, sep = ""))]
			setDT(data)[,paste("Annual_ghg_savings_",2026+i*6, sep = "") := get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = "")) - get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]
			
			#New Buildings
			assign('eui_variable_name',  paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""), env=.GlobalEnv)
			assign('ghg_variable_name',  paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = ""), env=.GlobalEnv)
			data = New_buildings(data = data,
			i = i,
			eui_variable_name = eui_variable_name,
			ghg_variable_name = ghg_variable_name,
			br = br,
			nb_EUI_level = nb_EUI_level,
			nb_GHG_level=nb_GHG_level)
			i  = i +1
	}
	return(data)
}

BEPS_GHG_intensity_simuliation = function(data, iterations, standard_level, ghg_standard_level, reduction, br, nb_EUI_level,nb_GHG_level, policy_switch){
	i = 0

	setDT(data)[,paste("BEPS_",2020+i*6, sep = "") := quantile(SITEEUI_KBTU_FT, na.rm = TRUE, c(standard_level)), by = BEPSPROPERTYTYPE]
	setDT(data)[,paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "") := ifelse(SITEEUI_KBTU_FT >  get(paste("BEPS_",2020+i*6, sep = "")),ifelse(STANDARDTARGET == "Yes",
																															pmax(get(paste("BEPS_",2020+i*6, sep = "")),(1- reduction)*SITEEUI_KBTU_FT),(1- reduction)*SITEEUI_KBTU_FT),
																															 SITEEUI_KBTU_FT)]
	
	
	# Site EUI Savings
	setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := round((get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) - SITEEUI_KBTU_FT)/SITEEUI_KBTU_FT,2)]
	setDT(data)[,paste("Total_site_energy_",2026+i*6, sep = "") := get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) * REPORTEDBUILDINGGROSSFLOORAREA]
	setDT(data)[,paste("Annual_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]
	setDT(data)[,paste("Aggregate_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]  
	
	# Fuel level savings
	setDT(data)[,paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = ""):=ELECTRICITYUSE_GRID_KWH * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = ""):=ELECTRICITYUSE_RENEWABLE_KWH * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""):=NATURALGASUSE_THERMS * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = ""):=FUELOILANDDIESELFUELUSEKBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRSTEAM_KBTU_",2026+i*6,sep = ""):=DISTRSTEAM_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = ""):=DISTRHOTWATER_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""):=DISTRCHILLEDWATER_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]

	#GHG Savings	
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2020+i*6,sep=""))/1000)) + 
				((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
				((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
				((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
				((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
				((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
				((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
																																				((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
																																				((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
																																				((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
																																				((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
																																				((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
																																				((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
	setDT(data)[,paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6,sep = ""):= ((get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))*1000)/REPORTEDBUILDINGGROSSFLOORAREA)]
	setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "")) - TOTGHGEMISSINTENSITY_KGCO2EFT)/TOTGHGEMISSINTENSITY_KGCO2EFT]
		
	setDT(data)[,paste("Emissions_Cumulative_Baseline_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]

	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6,sep = ""):= (((ELECTRICITYUSE_GRID_KWH * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
																																				((ELECTRICITYUSE_RENEWABLE_KWH * 0)/1000)+
																																				((NATURALGASUSE_THERMS *NATURALGASUSE_THERMS_GHG)/1000)+
																																				((FUELOILANDDIESELFUELUSEKBTU * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
																																				((DISTRSTEAM_KBTU * DISTRSTEAM_KBTU_GHG)/1000)+
																																				((DISTRHOTWATER_KBTU * DISTRHOTWATER_KBTU_GHG)/1000)+
																																				((DISTRCHILLEDWATER_KBTU * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average

	setDT(data)[,paste("Emissions_Cumulative_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) +TOTGHGEMISSIONS_METRICTONSCO2E*5 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6, sep = ""))]	
	setDT(data)[,paste("Emissions_Cumulative_Policy_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]
	setDT(data)[,paste("Emissions_Cumulative_Policy_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = "")) + TOTGHGEMISSIONS_METRICTONSCO2E*4 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""))+ get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]
	setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2020+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = ""))]
	setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2026+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2026+i*6, sep = ""))]
	setDT(data)[,paste("Annual_ghg_savings_",2026+i*6, sep = "") := TOTGHGEMISSIONS_METRICTONSCO2E- get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]

	#New Buildings
	assign('nb_EUI_level',nb_EUI_level, env=.GlobalEnv)
	assign('nb_GHG_level',nb_GHG_level, env=.GlobalEnv)
	assign('i',i, env=.GlobalEnv)
	assign('eui_variable_name',  paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""), env=.GlobalEnv)
	assign('ghg_variable_name',  paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = ""), env=.GlobalEnv)
	data = New_buildings(data = data,
	i = i,
	eui_variable_name = eui_variable_name,
	ghg_variable_name = ghg_variable_name,
	br = br,
	nb_EUI_level = nb_EUI_level,
	nb_GHG_level=nb_GHG_level)
	
	i = i +1 
	while (i < policy_switch){

		setDT(data)[,paste("BEPS_",2020+i*6, sep = "") := quantile(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")), na.rm = TRUE, c(standard_level)), by = BEPSPROPERTYTYPE]
		setDT(data)[,paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "") := ifelse(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")) >  get(paste("BEPS_",2020+i*6, sep = "")),
																																 pmax(get(paste("BEPS_",2020+i*6, sep = "")),(1- reduction)*get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = ""))),
																																 get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]        
		# Site EUI Savings
		setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := round((get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) - get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")),2)]
		setDT(data)[,paste("Total_site_energy_",2026+i*6, sep = "") := get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) * REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Annual_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - get(paste("Total_site_energy_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("Aggregate_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]  
		
		# Fuel level savings
		setDT(data)[,paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_GRID_KWH_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""):=get(paste("NATURALGASUSE_THERMS_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = ""):=get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRSTEAM_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRSTEAM_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRHOTWATER_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRCHILLEDWATER_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		
		# GHG Savings 
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2020+i*6,sep=""))/1000)) + 
				((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
				((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
				((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
				((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
				((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
				((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",pmin(2026+i*6,2050),sep=""))/1000)) + 
								((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
								((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
								((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
								((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
								((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
								((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))*1000) / REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "")) - get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = "")))/get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6,sep = ""):= (((ELECTRICITYUSE_GRID_KWH * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
			((ELECTRICITYUSE_RENEWABLE_KWH * 0)/1000)+
			((NATURALGASUSE_THERMS *NATURALGASUSE_THERMS_GHG)/1000)+
			((FUELOILANDDIESELFUELUSEKBTU * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
			((DISTRSTEAM_KBTU * DISTRSTEAM_KBTU_GHG)/1000)+
			((DISTRHOTWATER_KBTU * DISTRHOTWATER_KBTU_GHG)/1000)+
			((DISTRCHILLEDWATER_KBTU * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average


		setDT(data)[,paste("Emissions_Cumulative_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2020+i*6, sep = ""))*5 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6, sep = "")) ]
		setDT(data)[,paste("Emissions_Cumulative_Policy_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = ""))*4 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""))+ get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))] 
		setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2026+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2026+i*6, sep = ""))]
		setDT(data)[,paste("Annual_ghg_savings_",2026+i*6, sep = "") := get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = "")) - get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]
		
		#New Buildings
		assign('eui_variable_name',  paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""), env=.GlobalEnv)
		assign('ghg_variable_name',  paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = ""), env=.GlobalEnv)
		data = New_buildings(data = data,
		i = i,
		eui_variable_name = eui_variable_name,
		ghg_variable_name = ghg_variable_name,
		br = br,
		nb_EUI_level = nb_EUI_level,
		nb_GHG_level=nb_GHG_level)
		i  = i +1
	}
	while (i < iterations){

		# AM I CALCULATING THIS RIGHT? I THINK I MIGH TNEED TO FIND THE MEDIAN BAU AND THEN FIND THE BEPS FROM THERE?
		setDT(data)[,paste("BAU_TOTGHGEMISSINTENSITY_KGCO2EFT_",2020+i*6, sep = "") := (((get(paste("ELECTRICITYUSE_GRID_KWH_",2020+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",pmin(2026+i*6,2050),sep="")))) + 
																																								 ((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2020+i*6,sep = "")) * 0))+
																																								 ((get(paste("NATURALGASUSE_THERMS_",2020+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG))+
																																								 ((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2020+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG))+
																																								 ((get(paste("DISTRSTEAM_KBTU_",2020+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG))+
																																								 ((get(paste("DISTRHOTWATER_KBTU_",2020+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG))+
																																								 ((get(paste("DISTRCHILLEDWATER_KBTU_",2020+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)))/REPORTEDBUILDINGGROSSFLOORAREA]
		
		setDT(data)[,paste("BEPS_",2020+i*6, sep = "") := quantile(get(paste("BAU_TOTGHGEMISSINTENSITY_KGCO2EFT_",2020+i*6, sep = "")), na.rm = TRUE, c(ghg_standard_level)), by = BEPSPROPERTYTYPE]
		setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := ifelse(get(paste("BAU_TOTGHGEMISSINTENSITY_KGCO2EFT_",2020+i*6, sep = "")) >  get(paste("BEPS_",2020+i*6, sep = "")),
																																					pmax(1-reduction,get(paste("BEPS_",2020+i*6, sep = ""))/ get(paste("BAU_TOTGHGEMISSINTENSITY_KGCO2EFT_",2020+i*6, sep = ""))),
																																							 1)]
	
		# Fuel level savings
		setDT(data)[,paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_GRID_KWH_",2026+(i-1)*6,sep = "")) * get(paste("Ghg_intensity_reduction_",2026+i*6, sep = ""))]
		setDT(data)[,paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+(i-1)*6,sep = "")) * get(paste("Ghg_intensity_reduction_",2026+i*6, sep = ""))]
		setDT(data)[,paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""):=get(paste("NATURALGASUSE_THERMS_",2026+(i-1)*6,sep = "")) * get(paste("Ghg_intensity_reduction_",2026+i*6, sep = ""))]
		setDT(data)[,paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = ""):=get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+(i-1)*6,sep = "")) * get(paste("Ghg_intensity_reduction_",2026+i*6, sep = ""))]
		setDT(data)[,paste("DISTRSTEAM_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRSTEAM_KBTU_",2026+(i-1)*6,sep = "")) * get(paste("Ghg_intensity_reduction_",2026+i*6, sep = ""))]
		setDT(data)[,paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRHOTWATER_KBTU_",2026+(i-1)*6,sep = "")) * get(paste("Ghg_intensity_reduction_",2026+i*6, sep = ""))]
		setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRCHILLEDWATER_KBTU_",2026+(i-1)*6,sep = "")) * get(paste("Ghg_intensity_reduction_",2026+i*6, sep = ""))]
		
		# Site EUI Savings
		setDT(data)[,paste("Total_site_energy_",2026+i*6, sep = "") := (get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) *3.412) + 
									(get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) *3.412)+
									(get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) * 100) +
									get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) + 
									get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) +
									get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) +
									get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""))]
		setDT(data)[,paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) / REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := round((get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) - get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")),2)]
		setDT(data)[,paste("Annual_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - get(paste("Total_site_energy_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("Aggregate_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]  
		
		# GHG Savings 
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2020+i*6,sep=""))/1000)) + 
				((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
				((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
				((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
				((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
				((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
				((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",pmin(2026+i*6,2050),sep=""))/1000)) + 
								((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
								((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
								((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
								((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
								((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
								((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))*1000) / REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "")) - get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = "")))/get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6,sep = ""):= (((ELECTRICITYUSE_GRID_KWH * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
			((ELECTRICITYUSE_RENEWABLE_KWH * 0)/1000)+
			((NATURALGASUSE_THERMS *NATURALGASUSE_THERMS_GHG)/1000)+
			((FUELOILANDDIESELFUELUSEKBTU * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
			((DISTRSTEAM_KBTU * DISTRSTEAM_KBTU_GHG)/1000)+
			((DISTRHOTWATER_KBTU * DISTRHOTWATER_KBTU_GHG)/1000)+
			((DISTRCHILLEDWATER_KBTU * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average


		setDT(data)[,paste("Emissions_Cumulative_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2020+i*6, sep = ""))*5 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6, sep = "")) ]
		setDT(data)[,paste("Emissions_Cumulative_Policy_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = ""))  + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = ""))*4 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))] 
		setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2026+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2026+i*6, sep = ""))]
		setDT(data)[,paste("Annual_ghg_savings_",2026+i*6, sep = "") := get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = "")) - get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]

		#New Buildings
		assign('eui_variable_name',  paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""), env=.GlobalEnv)
		assign('ghg_variable_name',  paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = ""), env=.GlobalEnv)
		data = New_buildings(data = data,
		i = i,
		eui_variable_name = eui_variable_name,
		ghg_variable_name = ghg_variable_name,
		br = br,
		nb_EUI_level = nb_EUI_level,
		nb_GHG_level=nb_GHG_level)
		
		i  = i +1
	}
	return(data)
}


## TRAJECTORY
BEPS_trajectory_simuliation = function(data, iterations, standard_level, reduction, br, nb_EUI_level,nb_GHG_level, policy_switch, targets){
	i = 0
	
	setDT(data)[,paste("BEPS_",2020+i*6, sep = "") := quantile(SITEEUI_KBTU_FT, na.rm = TRUE, c(standard_level)), by = BEPSPROPERTYTYPE]
	setDT(data)[,paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "") := ifelse(SITEEUI_KBTU_FT >  get(paste("BEPS_",2020+i*6, sep = "")),ifelse(STANDARDTARGET == "Yes",
																															 pmax(get(paste("BEPS_",2020+i*6, sep = "")),(1- reduction)*SITEEUI_KBTU_FT),(1- reduction)*SITEEUI_KBTU_FT),
																															 SITEEUI_KBTU_FT)]
	
	
	# Site EUI Savings
	setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := round((get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) - SITEEUI_KBTU_FT)/SITEEUI_KBTU_FT,2)]
	setDT(data)[,paste("Total_site_energy_",2026+i*6, sep = "") := get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) * REPORTEDBUILDINGGROSSFLOORAREA]
	setDT(data)[,paste("Annual_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]
	setDT(data)[,paste("Aggregate_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]  
	
	# Fuel level savings
	setDT(data)[,paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = ""):=ELECTRICITYUSE_GRID_KWH * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = ""):=ELECTRICITYUSE_RENEWABLE_KWH * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""):=NATURALGASUSE_THERMS * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = ""):=FUELOILANDDIESELFUELUSEKBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRSTEAM_KBTU_",2026+i*6,sep = ""):=DISTRSTEAM_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = ""):=DISTRHOTWATER_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""):=DISTRCHILLEDWATER_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	
	#GHG Savings
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2020+i*6,sep=""))/1000)) + 
				((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
				((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
				((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
				((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
				((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
				((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
																																				((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
																																				((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
																																				((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
																																				((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
																																				((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
																																				((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
	setDT(data)[,paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6,sep = ""):= ((get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))*1000)/REPORTEDBUILDINGGROSSFLOORAREA)]
	setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "")) - TOTGHGEMISSINTENSITY_KGCO2EFT)/TOTGHGEMISSINTENSITY_KGCO2EFT]
		
	setDT(data)[,paste("Emissions_Cumulative_Baseline_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]

	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6,sep = ""):= (((ELECTRICITYUSE_GRID_KWH * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
																																				((ELECTRICITYUSE_RENEWABLE_KWH * 0)/1000)+
																																				((NATURALGASUSE_THERMS *NATURALGASUSE_THERMS_GHG)/1000)+
																																				((FUELOILANDDIESELFUELUSEKBTU * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
																																				((DISTRSTEAM_KBTU * DISTRSTEAM_KBTU_GHG)/1000)+
																																				((DISTRHOTWATER_KBTU * DISTRHOTWATER_KBTU_GHG)/1000)+
																																				((DISTRCHILLEDWATER_KBTU * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average

	setDT(data)[,paste("Emissions_Cumulative_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) +TOTGHGEMISSIONS_METRICTONSCO2E*5 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6, sep = ""))]	
	setDT(data)[,paste("Emissions_Cumulative_Policy_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]
	setDT(data)[,paste("Emissions_Cumulative_Policy_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = "")) + TOTGHGEMISSIONS_METRICTONSCO2E*4 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""))+ get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]
	setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2020+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = ""))]
	setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2026+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2026+i*6, sep = ""))]
	#setDT(data)[,paste("Avoided_ghg_emissions_",2026+i*6, sep = "") := get(paste("Annual_ghg_savings_",2026+i*6, sep = ""))] 

	#New Buildings
	assign('nb_EUI_level',nb_EUI_level, env=.GlobalEnv)
	assign('nb_GHG_level',nb_GHG_level, env=.GlobalEnv)
	assign('i',i, env=.GlobalEnv)
	assign('eui_variable_name',  paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""), env=.GlobalEnv)
	assign('ghg_variable_name',  paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = ""), env=.GlobalEnv)
	data = New_buildings(data = data,
	i = i,
	eui_variable_name = eui_variable_name,
	ghg_variable_name = ghg_variable_name,
	br = br,
	nb_EUI_level = nb_EUI_level,
	nb_GHG_level=nb_GHG_level)	
	i = i +1 
	while (i < policy_switch){

		setDT(data)[,paste("BEPS_",2020+i*6, sep = "") := quantile(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")), na.rm = TRUE, c(standard_level)), by = BEPSPROPERTYTYPE]
		setDT(data)[,paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "") := ifelse(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")) >  get(paste("BEPS_",2020+i*6, sep = "")),
																																 pmax(get(paste("BEPS_",2020+i*6, sep = "")),(1- reduction)*get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = ""))),
																																 get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]        
		# Site EUI Savings
		setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := round((get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) - get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")),2)]
		setDT(data)[,paste("Total_site_energy_",2026+i*6, sep = "") := get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) * REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Annual_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - get(paste("Total_site_energy_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("Aggregate_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]  
		
		# Fuel level savings
		setDT(data)[,paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_GRID_KWH_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""):=get(paste("NATURALGASUSE_THERMS_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = ""):=get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRSTEAM_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRSTEAM_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRHOTWATER_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRCHILLEDWATER_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		
		# GHG Savings 
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2020+i*6,sep=""))/1000)) + 
					((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
					((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
					((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
					((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
					((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
					((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",pmin(2026+i*6,2050),sep=""))/1000)) + 
								((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
								((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
								((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
								((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
								((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
								((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))*1000) / REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "")) - get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = "")))/get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6,sep = ""):= (((ELECTRICITYUSE_GRID_KWH * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
			((ELECTRICITYUSE_RENEWABLE_KWH * 0)/1000)+
			((NATURALGASUSE_THERMS *NATURALGASUSE_THERMS_GHG)/1000)+
			((FUELOILANDDIESELFUELUSEKBTU * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
			((DISTRSTEAM_KBTU * DISTRSTEAM_KBTU_GHG)/1000)+
			((DISTRHOTWATER_KBTU * DISTRHOTWATER_KBTU_GHG)/1000)+
			((DISTRCHILLEDWATER_KBTU * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average


		setDT(data)[,paste("Emissions_Cumulative_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2020+i*6, sep = ""))*5 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6, sep = "")) ]
		setDT(data)[,paste("Emissions_Cumulative_Policy_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = ""))*4 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""))+ get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))] 
		setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2026+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2026+i*6, sep = ""))]
		setDT(data)[,paste("Annual_ghg_savings_",2026+i*6, sep = "") := get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = "")) - get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]
		#setDT(data)[,paste("Avoided_ghg_emissions_",2026+i*6, sep = "") := get(paste("Annual_ghg_savings_",2020+i*6, sep = ""))*5 +get(paste("Annual_ghg_savings_",2026+i*6, sep = ""))+get(paste("Avoided_ghg_emissions_",2020+i*6, sep = ""))] 
		
		#New Buildings
		assign('eui_variable_name',  paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""), env=.GlobalEnv)
		assign('ghg_variable_name',  paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = ""), env=.GlobalEnv)
		data = New_buildings(data = data,
		i = i,
		eui_variable_name = eui_variable_name,
		ghg_variable_name = ghg_variable_name,
		br = br,
		nb_EUI_level = nb_EUI_level,
		nb_GHG_level=nb_GHG_level)
		i  = i +1
	}
	while (i < iterations){

		
		setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := pmin(1-((get(paste("SITEEUI_KBTU_FT_",2026+(policy_switch-1)*6, sep = "")) - final_target_eui)/(iterations-policy_switch)/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = ""))),1)]
		setDT(data)[,paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "") := ifelse(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")) > final_target_eui, get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")) * get(paste("Site_eui_reduction_",2026+i*6, sep = "")), get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]  
		# Site EUI Savings
		setDT(data)[,paste("Total_site_energy_",2026+i*6, sep = "") := get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) * REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Annual_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - get(paste("Total_site_energy_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("Aggregate_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]  
		
		setDT(data)[,paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_GRID_KWH_",2026+(i-1)*6,sep = "")) * get(paste("Site_eui_reduction_",2026+i*6, sep = ""))]
		setDT(data)[,paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+(i-1)*6,sep = "")) * get(paste("Site_eui_reduction_",2026+i*6, sep = ""))]
		setDT(data)[,paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""):=get(paste("NATURALGASUSE_THERMS_",2026+(i-1)*6,sep = "")) * get(paste("Site_eui_reduction_",2026+i*6, sep = ""))]
		setDT(data)[,paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = ""):=get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+(i-1)*6,sep = "")) * get(paste("Site_eui_reduction_",2026+i*6, sep = ""))]
		setDT(data)[,paste("DISTRSTEAM_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRSTEAM_KBTU_",2026+(i-1)*6,sep = "")) * get(paste("Site_eui_reduction_",2026+i*6, sep = ""))]
		setDT(data)[,paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRHOTWATER_KBTU_",2026+(i-1)*6,sep = "")) * get(paste("Site_eui_reduction_",2026+i*6, sep = ""))]
		setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRCHILLEDWATER_KBTU_",2026+(i-1)*6,sep = "")) * get(paste("Site_eui_reduction_",2026+i*6, sep = ""))]
		
		#GHG Savings
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2020+i*6,sep=""))/1000)) + 
				((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
				((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
				((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
				((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
				((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
				((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",pmin(2026+i*6,2050),sep=""))/1000)) + 
								((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
								((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
								((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
								((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
								((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
								((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))*1000) / REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "")) - get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = "")))/get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6,sep = ""):= (((ELECTRICITYUSE_GRID_KWH * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
			((ELECTRICITYUSE_RENEWABLE_KWH * 0)/1000)+
			((NATURALGASUSE_THERMS *NATURALGASUSE_THERMS_GHG)/1000)+
			((FUELOILANDDIESELFUELUSEKBTU * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
			((DISTRSTEAM_KBTU * DISTRSTEAM_KBTU_GHG)/1000)+
			((DISTRHOTWATER_KBTU * DISTRHOTWATER_KBTU_GHG)/1000)+
			((DISTRCHILLEDWATER_KBTU * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average


		setDT(data)[,paste("Emissions_Cumulative_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2020+i*6, sep = ""))*5 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6, sep = "")) ]
		setDT(data)[,paste("Emissions_Cumulative_Policy_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = ""))*4 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""))+ get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))] 
		setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2026+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2026+i*6, sep = ""))]
		setDT(data)[,paste("Annual_ghg_savings_",2026+i*6, sep = "") := get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = "")) - get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]

		setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := get(paste("Site_eui_reduction_",2026+i*6, sep = ""))-1]
		#New Buildings
		assign('eui_variable_name',  paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""), env=.GlobalEnv)
		assign('ghg_variable_name',  paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = ""), env=.GlobalEnv)
		data = New_buildings(data = data,
		i = i,
		eui_variable_name = eui_variable_name,
		ghg_variable_name = ghg_variable_name,
		br = br,
		nb_EUI_level = nb_EUI_level,
		nb_GHG_level=nb_GHG_level)
		i  = i +1
	}
	return(data)
}



# TRAJECTORY W GHG
BEPS_trajectory_w_thermal_GHG_simuliation = function(data, iterations, standard_level, reduction, br, nb_EUI_level,nb_GHG_level, policy_switch, targets, ghg_target){
	i = 0

	setDT(data)[,paste("BEPS_",2020+i*6, sep = "") := quantile(SITEEUI_KBTU_FT, na.rm = TRUE, c(standard_level)), by = BEPSPROPERTYTYPE]
	setDT(data)[,paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "") := ifelse(SITEEUI_KBTU_FT >  get(paste("BEPS_",2020+i*6, sep = "")),ifelse(STANDARDTARGET == "Yes",
																															pmax(get(paste("BEPS_",2020+i*6, sep = "")),(1- reduction)*SITEEUI_KBTU_FT),(1- reduction)*SITEEUI_KBTU_FT),
																															 SITEEUI_KBTU_FT)]
	
	
	# Site EUI Savings
	setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := round((get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) - SITEEUI_KBTU_FT)/SITEEUI_KBTU_FT,2)]
	setDT(data)[,paste("Total_site_energy_",2026+i*6, sep = "") := get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) * REPORTEDBUILDINGGROSSFLOORAREA]
	setDT(data)[,paste("Annual_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]
	setDT(data)[,paste("Aggregate_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]  
	
	# Fuel level savings
	setDT(data)[,paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = ""):=ELECTRICITYUSE_GRID_KWH * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = ""):=ELECTRICITYUSE_RENEWABLE_KWH * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""):=NATURALGASUSE_THERMS * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = ""):=FUELOILANDDIESELFUELUSEKBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRSTEAM_KBTU_",2026+i*6,sep = ""):=DISTRSTEAM_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = ""):=DISTRHOTWATER_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""):=DISTRCHILLEDWATER_KBTU * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/SITEEUI_KBTU_FT)]
	
	#GHG Savings
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2020+i*6,sep=""))/1000)) + 
				((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
				((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
				((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
				((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
				((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
				((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
																																				((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
																																				((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
																																				((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
																																				((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
																																				((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
																																				((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
	setDT(data)[,paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6,sep = ""):= ((get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))*1000)/REPORTEDBUILDINGGROSSFLOORAREA)]
	setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "")) - TOTGHGEMISSINTENSITY_KGCO2EFT)/TOTGHGEMISSINTENSITY_KGCO2EFT]
		
	setDT(data)[,paste("Emissions_Cumulative_Baseline_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]
	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]

	setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6,sep = ""):= (((ELECTRICITYUSE_GRID_KWH * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
																																				((ELECTRICITYUSE_RENEWABLE_KWH * 0)/1000)+
																																				((NATURALGASUSE_THERMS *NATURALGASUSE_THERMS_GHG)/1000)+
																																				((FUELOILANDDIESELFUELUSEKBTU * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
																																				((DISTRSTEAM_KBTU * DISTRSTEAM_KBTU_GHG)/1000)+
																																				((DISTRHOTWATER_KBTU * DISTRHOTWATER_KBTU_GHG)/1000)+
																																				((DISTRCHILLEDWATER_KBTU * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average

	setDT(data)[,paste("Emissions_Cumulative_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) +TOTGHGEMISSIONS_METRICTONSCO2E*5 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6, sep = ""))]	
	setDT(data)[,paste("Emissions_Cumulative_Policy_",2020+i*6,sep = ""):= TOTGHGEMISSIONS_METRICTONSCO2E]
	setDT(data)[,paste("Emissions_Cumulative_Policy_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = "")) + TOTGHGEMISSIONS_METRICTONSCO2E*4 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""))+ get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]
	setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2020+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = ""))]
	setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2026+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2026+i*6, sep = ""))]
	setDT(data)[,paste("Annual_ghg_savings_",2026+i*6, sep = "") := TOTGHGEMISSIONS_METRICTONSCO2E- get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]


	
	setDT(data)[,paste("Onsite_Thermal_GHG_total_",2026+i*6, sep = "") := (((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
																												((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
																												((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
																												((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
																												((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
	setDT(data)[,paste("Onsite_Thermal_GHG_Intensity_",2026+i*6,sep = ""):= ((get(paste("Onsite_Thermal_GHG_total_",2026+i*6, sep = ""))*1000)/REPORTEDBUILDINGGROSSFLOORAREA)]
	
	setDT(data)[,paste("DISTRSTEAM_KBTU_GHG_",pmin(2026+i*6,2050),sep=""):= DISTRSTEAM_KBTU_GHG]
	setDT(data)[,paste("DISTRHOTWATER_KBTU_GHG_",pmin(2026+i*6,2050),sep=""):= DISTRHOTWATER_KBTU_GHG]
	setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_GHG_",pmin(2026+i*6,2050),sep=""):= DISTRCHILLEDWATER_KBTU_GHG]
	
	#New Buildings
	assign('nb_EUI_level',nb_EUI_level, env=.GlobalEnv)
	assign('nb_GHG_level',nb_GHG_level, env=.GlobalEnv)
	assign('i',i, env=.GlobalEnv)
	assign('eui_variable_name',  paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""), env=.GlobalEnv)
	assign('ghg_variable_name',  paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = ""), env=.GlobalEnv)
	data = New_buildings(data = data,
	i = i,
	eui_variable_name = eui_variable_name,
	ghg_variable_name = ghg_variable_name,
	br = br,
	nb_EUI_level = nb_EUI_level,
	nb_GHG_level=nb_GHG_level)	
	i = i +1 
	while (i < policy_switch){
		
		setDT(data)[,paste("BEPS_",2020+i*6, sep = "") := quantile(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")), na.rm = TRUE, c(standard_level)), by = BEPSPROPERTYTYPE]
		setDT(data)[,paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "") := ifelse(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")) >  get(paste("BEPS_",2020+i*6, sep = "")),
																																 pmax(get(paste("BEPS_",2020+i*6, sep = "")),(1- reduction)*get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = ""))),
																																 get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]        
		# Site EUI Savings
		setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := round((get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) - get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")),2)]
		setDT(data)[,paste("Total_site_energy_",2026+i*6, sep = "") := get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) * REPORTEDBUILDINGGROSSFLOORAREA]


		setDT(data)[,paste("Annual_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - get(paste("Total_site_energy_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("Aggregate_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]  
		
		# Fuel level savings
		setDT(data)[,paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_GRID_KWH_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""):=get(paste("NATURALGASUSE_THERMS_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = ""):=get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRSTEAM_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRSTEAM_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRHOTWATER_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRCHILLEDWATER_KBTU_",2026+(i-1)*6,sep = "")) * (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]
		
		# GHG Savings 
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2020+i*6,sep=""))/1000)) + 
				((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
				((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
				((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
				((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
				((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
				((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",pmin(2026+i*6,2050),sep=""))/1000)) + 
								((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
								((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
								((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
								((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
								((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
								((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))*1000) / REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "")) - get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = "")))/get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6,sep = ""):= (((ELECTRICITYUSE_GRID_KWH * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
			((ELECTRICITYUSE_RENEWABLE_KWH * 0)/1000)+
			((NATURALGASUSE_THERMS *NATURALGASUSE_THERMS_GHG)/1000)+
			((FUELOILANDDIESELFUELUSEKBTU * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
			((DISTRSTEAM_KBTU * DISTRSTEAM_KBTU_GHG)/1000)+
			((DISTRHOTWATER_KBTU * DISTRHOTWATER_KBTU_GHG)/1000)+
			((DISTRCHILLEDWATER_KBTU * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average


		setDT(data)[,paste("Emissions_Cumulative_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2020+i*6, sep = ""))*5 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6, sep = "")) ]
		setDT(data)[,paste("Emissions_Cumulative_Policy_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = ""))*4 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""))+ get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))] 
		setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2026+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2026+i*6, sep = ""))]
		setDT(data)[,paste("Annual_ghg_savings_",2026+i*6, sep = "") := get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = "")) - get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]
		
		setDT(data)[,paste("Onsite_Thermal_GHG_total_",2026+i*6, sep = "") := (((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
																																		 ((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
																																		 ((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
																																		 ((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
																																		 ((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("Onsite_Thermal_GHG_Intensity_",2026+i*6,sep = ""):= ((get(paste("Onsite_Thermal_GHG_total_",2026+i*6, sep = ""))*1000)/REPORTEDBUILDINGGROSSFLOORAREA)]
		
		
		setDT(data)[,paste("DISTRSTEAM_KBTU_GHG_",pmin(2026+i*6,2050),sep=""):= DISTRSTEAM_KBTU_GHG]
		setDT(data)[,paste("DISTRHOTWATER_KBTU_GHG_",pmin(2026+i*6,2050),sep=""):= DISTRHOTWATER_KBTU_GHG]
		setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_GHG_",pmin(2026+i*6,2050),sep=""):= DISTRCHILLEDWATER_KBTU_GHG] 

		#New Buildings
		assign('eui_variable_name',  paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""), env=.GlobalEnv)
		assign('ghg_variable_name',  paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = ""), env=.GlobalEnv)
		data = New_buildings(data = data,
		i = i,
		eui_variable_name = eui_variable_name,
		ghg_variable_name = ghg_variable_name,
		br = br,
		nb_EUI_level = nb_EUI_level,
		nb_GHG_level=nb_GHG_level)
		
		
		
		i  = i +1
	}
	while (i < iterations){

		
		
		#On-site Thermal GHG Reduction
		setDT(data)[,paste("thermal_GHG_reduction_",2026+i*6, sep = "") := ifelse(get(paste("Onsite_Thermal_GHG_Intensity_",2026+(i-1)*6, sep = "")) == 0, 0, pmax(pmin(1-((get(paste("Onsite_Thermal_GHG_Intensity_",2026+(policy_switch-1)*6, sep = "")) - ghg_target)/(iterations-policy_switch)/get(paste("Onsite_Thermal_GHG_Intensity_",2026+(i-1)*6, sep = ""))),1),0))]
		setDT(data)[,paste("Onsite_Thermal_GHG_Intensity_",2026+i*6, sep = "") := ifelse(get(paste("Onsite_Thermal_GHG_Intensity_",2026+(i-1)*6, sep = "")) > ghg_target, get(paste("Onsite_Thermal_GHG_Intensity_",2026+(i-1)*6, sep = "")) * get(paste("thermal_GHG_reduction_",2026+i*6, sep = "")), get(paste("Onsite_Thermal_GHG_Intensity_",2026+(i-1)*6, sep = "")))]  
		setDT(data)[,paste("Onsite_Thermal_GHG_total_",2026+i*6, sep = "") := ((get(paste("Onsite_Thermal_GHG_Intensity_",2026+i*6, sep = ""))/1000) * REPORTEDBUILDINGGROSSFLOORAREA)]
		
		# EUI Reduction
		setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := pmin(1-((get(paste("SITEEUI_KBTU_FT_",2026+(policy_switch-1)*6, sep = "")) - final_target_eui)/(iterations-policy_switch)/get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = ""))),1)]
		setDT(data)[,paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "") := ifelse(get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")) > final_target_eui, get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")) * get(paste("Site_eui_reduction_",2026+i*6, sep = "")), get(paste("SITEEUI_KBTU_FT_",2026+(i-1)*6, sep = "")))]  

		# Site EUI Savings
		setDT(data)[,paste("Total_site_energy_",2026+i*6, sep = "") := get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = "")) * REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Annual_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - get(paste("Total_site_energy_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("Aggregate_site_energy_savings_",2026+i*6, sep = "") := get(paste("Total_site_energy_",2026+i*6, sep = "")) - TOTAL_SITE_ENERGY]  
		
		
		
		
		setDT(data)[,paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""):=get(paste("NATURALGASUSE_THERMS_",2026+(i-1)*6,sep = "")) * get(paste("thermal_GHG_reduction_",2026+i*6, sep = ""))]
		setDT(data)[,paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = ""):=get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+(i-1)*6,sep = "")) * get(paste("thermal_GHG_reduction_",2026+i*6, sep = ""))]
		
		setDT(data)[,paste("non_onsite_thermal_eui_reduction_",2026+i*6,sep = ""):= (get(paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""))- (((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = ""))*100) +get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")))/ REPORTEDBUILDINGGROSSFLOORAREA))/ 
									(((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+(i-1)*6,sep = ""))*3.412) + (get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+(i-1)*6,sep = ""))*3.412) +get(paste("DISTRSTEAM_KBTU_",2026+(i-1)*6,sep = "")) +get(paste("DISTRHOTWATER_KBTU_",2026+(i-1)*6,sep = ""))+get(paste("DISTRCHILLEDWATER_KBTU_",2026+(i-1)*6,sep = "")))/REPORTEDBUILDINGGROSSFLOORAREA)]
		
		
		setDT(data)[,paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_GRID_KWH_",2026+(i-1)*6,sep = "")) * get(paste("non_onsite_thermal_eui_reduction_",2026+i*6, sep = ""))]
		setDT(data)[,paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = ""):=get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+(i-1)*6,sep = "")) * get(paste("non_onsite_thermal_eui_reduction_",2026+i*6, sep = ""))]
		setDT(data)[,paste("DISTRSTEAM_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRSTEAM_KBTU_",2026+(i-1)*6,sep = "")) * get(paste("non_onsite_thermal_eui_reduction_",2026+i*6, sep = ""))]
		setDT(data)[,paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRHOTWATER_KBTU_",2026+(i-1)*6,sep = "")) * get(paste("non_onsite_thermal_eui_reduction_",2026+i*6, sep = ""))]
		setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = ""):=get(paste("DISTRCHILLEDWATER_KBTU_",2026+(i-1)*6,sep = "")) * get(paste("non_onsite_thermal_eui_reduction_",2026+i*6, sep = ""))]
		
		setDT(data)[,paste("DISTRSTEAM_KBTU_GHG_",pmin(2026+i*6,2050),sep=""):= ifelse(get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) == 0,0, (get(paste("DISTRSTEAM_KBTU_",2026+(i-1)*6,sep = ""))* get(paste("non_onsite_thermal_eui_reduction_",2026+i*6, sep = ""))*get(paste("DISTRSTEAM_KBTU_GHG_",pmin(2026+(i-1)*6,2050),sep="")))/get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")))]
		setDT(data)[,paste("DISTRHOTWATER_KBTU_GHG_",pmin(2026+i*6,2050),sep=""):= ifelse(get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) == 0,0, (get(paste("DISTRHOTWATER_KBTU_",2026+(i-1)*6,sep = ""))* get(paste("non_onsite_thermal_eui_reduction_",2026+i*6, sep = ""))*get(paste("DISTRHOTWATER_KBTU_GHG_",pmin(2026+(i-1)*6,2050),sep="")))/get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")))]
		setDT(data)[,paste("DISTRCHILLEDWATER_KBTU_GHG_",pmin(2026+i*6,2050),sep=""):= ifelse(get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) == 0,0, (get(paste("DISTRCHILLEDWATER_KBTU_",2026+(i-1)*6,sep = ""))* get(paste("non_onsite_thermal_eui_reduction_",2026+i*6, sep = ""))*get(paste("DISTRCHILLEDWATER_KBTU_GHG_",pmin(2026+(i-1)*6,2050),sep="")))/get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")))] 
		
		#GHG Savings
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2020+i*6,sep=""))/1000)) + 
				((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
				((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
				((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
				((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
				((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
				((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6,sep = ""):= (((get(paste("ELECTRICITYUSE_GRID_KWH_",2026+i*6,sep = "")) * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",pmin(2026+i*6,2050),sep=""))/1000)) + 
								((get(paste("ELECTRICITYUSE_RENEWABLE_KWH_",2026+i*6,sep = "")) * 0)/1000)+
								((get(paste("NATURALGASUSE_THERMS_",2026+i*6,sep = "")) *NATURALGASUSE_THERMS_GHG)/1000)+
								((get(paste("FUELOILANDDIESELFUELUSEKBTU_",2026+i*6,sep = "")) * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
								((get(paste("DISTRSTEAM_KBTU_",2026+i*6,sep = "")) * DISTRSTEAM_KBTU_GHG)/1000)+
								((get(paste("DISTRHOTWATER_KBTU_",2026+i*6,sep = "")) * DISTRHOTWATER_KBTU_GHG)/1000)+
								((get(paste("DISTRCHILLEDWATER_KBTU_",2026+i*6,sep = "")) * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average
		setDT(data)[,paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))*1000) / REPORTEDBUILDINGGROSSFLOORAREA]
		setDT(data)[,paste("Ghg_intensity_reduction_",2026+i*6, sep = "") := (get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = "")) - get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = "")))/get(paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+(i-1)*6, sep = ""))]
		setDT(data)[,paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6,sep = ""):= (((ELECTRICITYUSE_GRID_KWH * get(paste("ELECTRICITYUSE_GRID_KWH_GHG_",2026+i*6,sep=""))/1000)) + 
			((ELECTRICITYUSE_RENEWABLE_KWH * 0)/1000)+
			((NATURALGASUSE_THERMS *NATURALGASUSE_THERMS_GHG)/1000)+
			((FUELOILANDDIESELFUELUSEKBTU * FUELOILANDDIESELFUELUSEKBTU_GHG)/1000)+ #Average
			((DISTRSTEAM_KBTU * DISTRSTEAM_KBTU_GHG)/1000)+
			((DISTRHOTWATER_KBTU * DISTRHOTWATER_KBTU_GHG)/1000)+
			((DISTRCHILLEDWATER_KBTU * DISTRCHILLEDWATER_KBTU_GHG)/1000))]  #Average


		setDT(data)[,paste("Emissions_Cumulative_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2020+i*6, sep = ""))*5 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE_",2026+i*6, sep = "")) ]
		setDT(data)[,paste("Emissions_Cumulative_Policy_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Policy_",2020+i*6, sep = "")) + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = ""))*4 + get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_temp_",2026+i*6,sep = ""))+ get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))] 
		setDT(data)[,paste("Emissions_Avoided_Policy_v_Baseline_",2026+i*6,sep = ""):= get(paste("Emissions_Cumulative_Baseline_",2026+i*6, sep = "")) - get(paste("Emissions_Cumulative_Policy_",2026+i*6, sep = ""))]
		setDT(data)[,paste("Annual_ghg_savings_",2026+i*6, sep = "") := get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2020+i*6, sep = "")) - get(paste("TOTGHGEMISSIONS_METRICTONSCO2E_",2026+i*6, sep = ""))]
		setDT(data)[,paste("Site_eui_reduction_",2026+i*6, sep = "") := get(paste("Site_eui_reduction_",2026+i*6, sep = ""))-1]		
		#New Buildings

		assign('eui_variable_name',  paste("SITEEUI_KBTU_FT_",2026+i*6, sep = ""), env=.GlobalEnv)
		assign('ghg_variable_name',  paste("TOTGHGEMISSINTENSITY_KGCO2EFT_",2026+i*6, sep = ""), env=.GlobalEnv)
		data = New_buildings(data = data,
		i = i,
		eui_variable_name = eui_variable_name,
		ghg_variable_name = ghg_variable_name,
		br = br,
		nb_EUI_level = nb_EUI_level,
		nb_GHG_level=nb_GHG_level)
		
		i  = i +1
	}
	return(data)
}





### Standard Target Output
STP_DATA = data.frame(Disclosure)
STP_Output = BEPS_standard_target_simuliation(data = STP_DATA, iterations = 5, standard_level = .5, reduction = .2, br = .0437,nb_EUI_level = .05, nb_GHG_level = 0)

STP_Output_2020 = data.frame(STP_Output %>% dplyr::select(c(1:96) | contains("_2020",ignore.case = TRUE)))
STP_Output_2020$CycleEnd = 2019
names(STP_Output_2020) = sub("_2020$", "", names(STP_Output_2020))

STP_Output_2026 = data.frame(STP_Output %>% dplyr::select(c(1:26) | contains("_2026",ignore.case = TRUE)))
STP_Output_2026$CycleEnd = 2026
names(STP_Output_2026) = sub("_2026$", "", names(STP_Output_2026))

STP_Output_2032 = data.frame(STP_Output %>% dplyr::select(c(1:26) | contains("_2032",ignore.case = TRUE)))
STP_Output_2032$CycleEnd = 2032
names(STP_Output_2032) = sub("_2032$", "", names(STP_Output_2032))

STP_Output_2038 = data.frame(STP_Output %>% dplyr::select(c(1:26) | contains("_2038",ignore.case = TRUE)))
STP_Output_2038$CycleEnd = 2038
names(STP_Output_2038) = sub('_2038$', "", names(STP_Output_2038))

STP_Output_2044 = data.frame(STP_Output %>% dplyr::select(c(1:26) | contains("_2044",ignore.case = TRUE)))
STP_Output_2044$CycleEnd = 2044
names(STP_Output_2044) = sub("_2044$", "", names(STP_Output_2044))

STP_Output_2050 = data.frame(STP_Output %>% dplyr::select(c(1:26) | contains("_2050",ignore.case = TRUE)))
STP_Output_2050$CycleEnd = 2050
names(STP_Output_2050) = sub("_2050$", "", names(STP_Output_2050))

STP_Output_FINAL = bind_rows(STP_Output_2020, STP_Output_2026, STP_Output_2032, STP_Output_2038, STP_Output_2044, STP_Output_2050)
STP_Output_FINAL$Scenario = "Base Package"



openxlsx::write.xlsx(as.data.frame(STP_Output), 'Output/STP_Output.xlsx', sheetName = "Sheet1",
										 col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)

openxlsx::write.xlsx(as.data.frame(STP_Output_FINAL), 'Output/STP_Output_FINAL.xlsx', sheetName = "Sheet1",
										 col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)

rm(STP_Output, STP_Output_2020, STP_Output_2026, STP_Output_2032, STP_Output_2038, STP_Output_2044, STP_Output_2050)
gc()

#Current policy plus inscreasing standard after 1 cycle
STP_Increase_DATA = data.frame(Disclosure)
STP_Inscrease_ps1_Output = BEPS_standard_target_increasing_standard_simuliation(data = STP_Increase_DATA, iterations = 5, standard_level = .5, reduction = .2, policy_switch = 1, br = .0437,nb_EUI_level = .05, nb_GHG_level = 0,  standard_modifier = .05)


STP_Inscrease_ps1_Output_2020 = data.frame(STP_Inscrease_ps1_Output %>% dplyr::select(c(1:96) | contains("_2020",ignore.case = TRUE)))
STP_Inscrease_ps1_Output_2020$CycleEnd = 2019
names(STP_Inscrease_ps1_Output_2020) = sub("_2020$", "", names(STP_Inscrease_ps1_Output_2020))

STP_Inscrease_ps1_Output_2026 = data.frame(STP_Inscrease_ps1_Output %>% dplyr::select(c(1:26) | contains("_2026",ignore.case = TRUE)))
STP_Inscrease_ps1_Output_2026$CycleEnd = 2026
names(STP_Inscrease_ps1_Output_2026) = sub("_2026$", "", names(STP_Inscrease_ps1_Output_2026))

STP_Inscrease_ps1_Output_2032 = data.frame(STP_Inscrease_ps1_Output %>% dplyr::select(c(1:26) | contains("_2032",ignore.case = TRUE)))
STP_Inscrease_ps1_Output_2032$CycleEnd = 2032
names(STP_Inscrease_ps1_Output_2032) = sub("_2032$", "", names(STP_Inscrease_ps1_Output_2032))

STP_Inscrease_ps1_Output_2038 = data.frame(STP_Inscrease_ps1_Output %>% dplyr::select(c(1:26) | contains("_2038",ignore.case = TRUE)))
STP_Inscrease_ps1_Output_2038$CycleEnd = 2038
names(STP_Inscrease_ps1_Output_2038) = sub('_2038$', "", names(STP_Inscrease_ps1_Output_2038))

STP_Inscrease_ps1_Output_2044 = data.frame(STP_Inscrease_ps1_Output %>% dplyr::select(c(1:26) | contains("_2044",ignore.case = TRUE)))
STP_Inscrease_ps1_Output_2044$CycleEnd = 2044
names(STP_Inscrease_ps1_Output_2044) = sub("_2044$", "", names(STP_Inscrease_ps1_Output_2044))

STP_Inscrease_ps1_Output_2050 = data.frame(STP_Inscrease_ps1_Output %>% dplyr::select(c(1:26) | contains("_2050",ignore.case = TRUE)))
STP_Inscrease_ps1_Output_2050$CycleEnd = 2050
names(STP_Inscrease_ps1_Output_2050) = sub("_2050$", "", names(STP_Inscrease_ps1_Output_2050))

STP_Inscrease_ps1_Output_FINAL = bind_rows(STP_Inscrease_ps1_Output_2020, STP_Inscrease_ps1_Output_2026, STP_Inscrease_ps1_Output_2032, STP_Inscrease_ps1_Output_2038, STP_Inscrease_ps1_Output_2044, STP_Inscrease_ps1_Output_2050)
STP_Inscrease_ps1_Output_FINAL$Scenario = "Alternative Base Package B: Effective 2026"
STP_Inscrease_ps1_Output_FINAL$Effective_Date = 2026

openxlsx::write.xlsx(as.data.frame(STP_Inscrease_ps1_Output), 'Output/STP_Inscrease_ps1_Output.xlsx', sheetName = "Sheet1",
										 col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)


rm(STP_Inscrease_ps1_Output, STP_Inscrease_ps1_Output_2020, STP_Inscrease_ps1_Output_2026, STP_Inscrease_ps1_Output_2032, STP_Inscrease_ps1_Output_2038, STP_Inscrease_ps1_Output_2044, STP_Inscrease_ps1_Output_2050)
gc()

### No Standard Target After 1 cycle
NoSTP_data = data.frame(Disclosure)
NoSTP_ps1_Output = BEPS_no_standard_target_simuliation(data = NoSTP_data, iterations = 5, policy_switch = 1, standard_level = .5, reduction = .2, br = .0437,nb_EUI_level = .05, nb_GHG_level = 0)

NoSTP_ps1_Output_2020 = data.frame(NoSTP_ps1_Output %>% dplyr::select(c(1:96) | contains("_2020",ignore.case = TRUE)))
NoSTP_ps1_Output_2020$CycleEnd = 2019
names(NoSTP_ps1_Output_2020) = sub("_2020", "", names(NoSTP_ps1_Output_2020))

NoSTP_ps1_Output_2026 = data.frame(NoSTP_ps1_Output %>% dplyr::select(c(1:26) | contains("_2026",ignore.case = TRUE)))
NoSTP_ps1_Output_2026$CycleEnd = 2026
names(NoSTP_ps1_Output_2026) = sub("_2026$", "", names(NoSTP_ps1_Output_2026))

NoSTP_ps1_Output_2032 = data.frame(NoSTP_ps1_Output %>% dplyr::select(c(1:26) | contains("_2032",ignore.case = TRUE)))
NoSTP_ps1_Output_2032$CycleEnd = 2032
names(NoSTP_ps1_Output_2032) = sub("_2032$", "", names(NoSTP_ps1_Output_2032))

NoSTP_ps1_Output_2038 = data.frame(NoSTP_ps1_Output %>% dplyr::select(c(1:26) | contains("_2038",ignore.case = TRUE)))
NoSTP_ps1_Output_2038$CycleEnd = 2038
names(NoSTP_ps1_Output_2038) = sub('_2038$', "", names(NoSTP_ps1_Output_2038))

NoSTP_ps1_Output_2044 = data.frame(NoSTP_ps1_Output %>% dplyr::select(c(1:26) | contains("_2044",ignore.case = TRUE)))
NoSTP_ps1_Output_2044$CycleEnd = 2044
names(NoSTP_ps1_Output_2044) = sub("_2044$", "", names(NoSTP_ps1_Output_2044))

NoSTP_ps1_Output_2050 = data.frame(NoSTP_ps1_Output %>% dplyr::select(c(1:26) | contains("_2050",ignore.case = TRUE)))
NoSTP_ps1_Output_2050$CycleEnd = 2050
names(NoSTP_ps1_Output_2050) = sub("_2050$", "", names(NoSTP_ps1_Output_2050))

NoSTP_ps1_Output_FINAL = bind_rows(NoSTP_ps1_Output_2020, NoSTP_ps1_Output_2026, NoSTP_ps1_Output_2032, NoSTP_ps1_Output_2038, NoSTP_ps1_Output_2044, NoSTP_ps1_Output_2050)
NoSTP_ps1_Output_FINAL$Scenario = "Alternative Base Package A: Effective 2026"
NoSTP_ps1_Output_FINAL$Effective_Date = 2026

openxlsx::write.xlsx(as.data.frame(NoSTP_ps1_Output), 'Output/NoSTP_ps1_Output.xlsx', sheetName = "Sheet1",
										 col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)


rm(NoSTP_ps1_Output, NoSTP_ps1_Output_2020, NoSTP_ps1_Output_2026, NoSTP_ps1_Output_2032, NoSTP_ps1_Output_2038, NoSTP_ps1_Output_2044, NoSTP_ps1_Output_2050)
gc()

### Switch to Trajectory after 1 cycle
Trajectory_Data = data.frame(Disclosure)
Trajectory_ps1_Output = BEPS_trajectory_simuliation(data = Trajectory_Data, iterations = 5, policy_switch = 1, standard_level = .5, reduction = .2, br = .0437,nb_EUI_level = .05, nb_GHG_level = 0, targets = PROPERTYTYPEMAPPING[ , c("BEPSPROPERTYTYPE", "final_target_eui")])

Trajectory_ps1_Output_2020 = data.frame(Trajectory_ps1_Output %>% dplyr::select(c(1:96) | contains("_2020",ignore.case = TRUE)))
Trajectory_ps1_Output_2020$CycleEnd = 2019
names(Trajectory_ps1_Output_2020) = sub("_2020$", "", names(Trajectory_ps1_Output_2020))

Trajectory_ps1_Output_2026 = data.frame(Trajectory_ps1_Output %>% dplyr::select(c(1:26) | contains("_2026",ignore.case = TRUE)))
Trajectory_ps1_Output_2026$CycleEnd = 2026
names(Trajectory_ps1_Output_2026) = sub("_2026$", "", names(Trajectory_ps1_Output_2026))

Trajectory_ps1_Output_2032 = data.frame(Trajectory_ps1_Output %>% dplyr::select(c(1:26) | contains("_2032",ignore.case = TRUE)))
Trajectory_ps1_Output_2032$CycleEnd = 2032
names(Trajectory_ps1_Output_2032) = sub("_2032$", "", names(Trajectory_ps1_Output_2032))

Trajectory_ps1_Output_2038 = data.frame(Trajectory_ps1_Output %>% dplyr::select(c(1:26) | contains("_2038",ignore.case = TRUE)))
Trajectory_ps1_Output_2038$CycleEnd = 2038
names(Trajectory_ps1_Output_2038) = sub('_2038$', "", names(Trajectory_ps1_Output_2038))

Trajectory_ps1_Output_2044 = data.frame(Trajectory_ps1_Output %>% dplyr::select(c(1:26) | contains("_2044",ignore.case = TRUE)))
Trajectory_ps1_Output_2044$CycleEnd = 2044
names(Trajectory_ps1_Output_2044) = sub("_2044$", "", names(Trajectory_ps1_Output_2044))

Trajectory_ps1_Output_2050 = data.frame(Trajectory_ps1_Output %>% dplyr::select(c(1:26) | contains("_2050",ignore.case = TRUE)))
Trajectory_ps1_Output_2050$CycleEnd = 2050
names(Trajectory_ps1_Output_2050) = sub("_2050$", "", names(Trajectory_ps1_Output_2050))

Trajectory_ps1_Output_FINAL = bind_rows(Trajectory_ps1_Output_2020, Trajectory_ps1_Output_2026, Trajectory_ps1_Output_2032, Trajectory_ps1_Output_2038, Trajectory_ps1_Output_2044, Trajectory_ps1_Output_2050)
Trajectory_ps1_Output_FINAL$Scenario = "Trajectory Package A: Effective 2026"
Trajectory_ps1_Output_FINAL$Effective_Date = 2026

openxlsx::write.xlsx(as.data.frame(Trajectory_ps1_Output), 'Output/Trajectory_ps1_Output.xlsx', sheetName = "Sheet1",
										 col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)
									 col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)

rm(Trajectory_ps1_Output, Trajectory_ps1_Output_2020, Trajectory_ps1_Output_2026, Trajectory_ps1_Output_2032, Trajectory_ps1_Output_2038, Trajectory_ps1_Output_2044, Trajectory_ps1_Output_2050)
gc()


### Switch to Trajectory with GHG intensity phase out after x cycle

Trajectory_GHG_Data = data.frame(Disclosure)
Trajectory_GHG_ps1_Output = BEPS_trajectory_w_thermal_GHG_simuliation(data = Trajectory_GHG_Data, iterations = 5, policy_switch = 1, standard_level = .5, reduction = .2, br = .0437,nb_EUI_level = .05, nb_GHG_level = 0, targets = PROPERTYTYPEMAPPING[ , c("BEPSPROPERTYTYPE", "final_target_eui")], ghg_target = 0)

Trajectory_GHG_ps1_Output_2020 = data.frame(Trajectory_GHG_ps1_Output %>% dplyr::select(c(1:96) | contains("_2020",ignore.case = TRUE)))
Trajectory_GHG_ps1_Output_2020$CycleEnd = 2019
names(Trajectory_GHG_ps1_Output_2020) = sub("_2020$", "", names(Trajectory_GHG_ps1_Output_2020))

Trajectory_GHG_ps1_Output_2026 = data.frame(Trajectory_GHG_ps1_Output %>% dplyr::select(c(1:26) | contains("_2026",ignore.case = TRUE)))
Trajectory_GHG_ps1_Output_2026$CycleEnd = 2026
names(Trajectory_GHG_ps1_Output_2026) = sub("_2026$", "", names(Trajectory_GHG_ps1_Output_2026))

Trajectory_GHG_ps1_Output_2032 = data.frame(Trajectory_GHG_ps1_Output %>% dplyr::select(c(1:26) | contains("_2032",ignore.case = TRUE)))
Trajectory_GHG_ps1_Output_2032$CycleEnd = 2032
names(Trajectory_GHG_ps1_Output_2032) = sub("_2032$", "", names(Trajectory_GHG_ps1_Output_2032))

Trajectory_GHG_ps1_Output_2038 = data.frame(Trajectory_GHG_ps1_Output %>% dplyr::select(c(1:26) | contains("_2038",ignore.case = TRUE)))
Trajectory_GHG_ps1_Output_2038$CycleEnd = 2038
names(Trajectory_GHG_ps1_Output_2038) = sub('_2038$', "", names(Trajectory_GHG_ps1_Output_2038))

Trajectory_GHG_ps1_Output_2044 = data.frame(Trajectory_GHG_ps1_Output %>% dplyr::select(c(1:26) | contains("_2044",ignore.case = TRUE)))
Trajectory_GHG_ps1_Output_2044$CycleEnd = 2044
names(Trajectory_GHG_ps1_Output_2044) = sub("_2044$", "", names(Trajectory_GHG_ps1_Output_2044))

Trajectory_GHG_ps1_Output_2050 = data.frame(Trajectory_GHG_ps1_Output %>% dplyr::select(c(1:26) | contains("_2050",ignore.case = TRUE)))
Trajectory_GHG_ps1_Output_2050$CycleEnd = 2050
names(Trajectory_GHG_ps1_Output_2050) = sub("_2050$", "", names(Trajectory_GHG_ps1_Output_2050))

Trajectory_GHG_ps1_Output_FINAL = bind_rows(Trajectory_GHG_ps1_Output_2020, Trajectory_GHG_ps1_Output_2026, Trajectory_GHG_ps1_Output_2032, Trajectory_GHG_ps1_Output_2038, Trajectory_GHG_ps1_Output_2044, Trajectory_GHG_ps1_Output_2050)
Trajectory_GHG_ps1_Output_FINAL$Scenario = "Trajectory Package B: Effective 2026"
Trajectory_GHG_ps1_Output_FINAL$Effective_Date = 2026

openxlsx::write.xlsx(as.data.frame(Trajectory_GHG_ps1_Output), 'Output/Trajectory_GHG_ps1_Output.xlsx', sheetName = "Sheet1",
										 col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)

rm(Trajectory_GHG_ps1_Output, Trajectory_GHG_ps1_Output_2020, Trajectory_GHG_ps1_Output_2026, Trajectory_GHG_ps1_Output_2032, Trajectory_GHG_ps1_Output_2038, Trajectory_GHG_ps1_Output_2044, Trajectory_GHG_ps1_Output_2050)
gc()

### Switch to GHG Intensity after 1 cycle

GHG_Limits_Data = data.frame(Disclosure)
GHG_Limits_ps1_Output = BEPS_GHG_intensity_simuliation(data = GHG_Limits_Data, iterations = 5, policy_switch = 1, standard_level = .5, ghg_standard_level = .25, reduction = .2, br = .0437,nb_EUI_level = .05, nb_GHG_level = 0)

GHG_Limits_ps1_Output_2020 = data.frame(GHG_Limits_ps1_Output %>% dplyr::select(c(1:96) | contains("_2020",ignore.case = TRUE)))
GHG_Limits_ps1_Output_2020$CycleEnd = 2019
names(GHG_Limits_ps1_Output_2020) = sub("_2020$", "", names(GHG_Limits_ps1_Output_2020))

GHG_Limits_ps1_Output_2026 = data.frame(GHG_Limits_ps1_Output %>% dplyr::select(c(1:26) | contains("_2026",ignore.case = TRUE)))
GHG_Limits_ps1_Output_2026$CycleEnd = 2026
names(GHG_Limits_ps1_Output_2026) = sub("_2026$", "", names(GHG_Limits_ps1_Output_2026))

GHG_Limits_ps1_Output_2032 = data.frame(GHG_Limits_ps1_Output %>% dplyr::select(c(1:26) | contains("_2032",ignore.case = TRUE)))
GHG_Limits_ps1_Output_2032$CycleEnd = 2032
names(GHG_Limits_ps1_Output_2032) = sub("_2032$", "", names(GHG_Limits_ps1_Output_2032))

GHG_Limits_ps1_Output_2038 = data.frame(GHG_Limits_ps1_Output %>% dplyr::select(c(1:26) | contains("_2038",ignore.case = TRUE)))
GHG_Limits_ps1_Output_2038$CycleEnd = 2038
names(GHG_Limits_ps1_Output_2038) = sub('_2038$', "", names(GHG_Limits_ps1_Output_2038))

GHG_Limits_ps1_Output_2044 = data.frame(GHG_Limits_ps1_Output %>% dplyr::select(c(1:26) | contains("_2044",ignore.case = TRUE)))
GHG_Limits_ps1_Output_2044$CycleEnd = 2044
names(GHG_Limits_ps1_Output_2044) = sub("_2044$", "", names(GHG_Limits_ps1_Output_2044))

GHG_Limits_ps1_Output_2050 = data.frame(GHG_Limits_ps1_Output %>% dplyr::select(c(1:26) | contains("_2050",ignore.case = TRUE)))
GHG_Limits_ps1_Output_2050$CycleEnd = 2050
names(GHG_Limits_ps1_Output_2050) = sub("_2050$", "", names(GHG_Limits_ps1_Output_2050))

GHG_Limits_ps1_Output_FINAL = bind_rows(GHG_Limits_ps1_Output_2020, GHG_Limits_ps1_Output_2026, GHG_Limits_ps1_Output_2032, GHG_Limits_ps1_Output_2038, GHG_Limits_ps1_Output_2044, GHG_Limits_ps1_Output_2050)
GHG_Limits_ps1_Output_FINAL$Scenario = "GHG Limits Package: Effective 2026"
GHG_Limits_ps1_Output_FINAL$Effective_Date = 2026

openxlsx::write.xlsx(as.data.frame(GHG_Limits_ps1_Output), 'Output/GHG_Limits_ps1_Output.xlsx', sheetName = "Sheet1",
										 col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)


rm(GHG_Limits_ps1_Output_2020, GHG_Limits_ps1_Output_2026, GHG_Limits_ps1_Output_2032, GHG_Limits_ps1_Output_2038, GHG_Limits_ps1_Output_2044, GHG_Limits_ps1_Output_2050)
gc()

# Current Policy + increasing standard level after 2 cycles
STP_Increase_DATA = data.frame(Disclosure)
STP_Inscrease_ps2_Output = BEPS_standard_target_increasing_standard_simuliation(data = STP_Increase_DATA, iterations = 5, standard_level = .5, reduction = .2, policy_switch = 2, br = .0437,nb_EUI_level = .05, nb_GHG_level = 0,  standard_modifier = .05)

STP_Inscrease_ps2_Output_2020 = data.frame(STP_Inscrease_ps2_Output %>% dplyr::select(c(1:96) | contains("_2020",ignore.case = TRUE)))
STP_Inscrease_ps2_Output_2020$CycleEnd = 2019
names(STP_Inscrease_ps2_Output_2020) = sub("_2020$", "", names(STP_Inscrease_ps2_Output_2020))

STP_Inscrease_ps2_Output_2026 = data.frame(STP_Inscrease_ps2_Output %>% dplyr::select(c(1:26) | contains("_2026",ignore.case = TRUE)))
STP_Inscrease_ps2_Output_2026$CycleEnd = 2026
names(STP_Inscrease_ps2_Output_2026) = sub("_2026$", "", names(STP_Inscrease_ps2_Output_2026))

STP_Inscrease_ps2_Output_2032 = data.frame(STP_Inscrease_ps2_Output %>% dplyr::select(c(1:26) | contains("_2032",ignore.case = TRUE)))
STP_Inscrease_ps2_Output_2032$CycleEnd = 2032
names(STP_Inscrease_ps2_Output_2032) = sub("_2032$", "", names(STP_Inscrease_ps2_Output_2032))


STP_Inscrease_ps2_Output_2038 = data.frame(STP_Inscrease_ps2_Output %>% dplyr::select(c(1:26) | contains("_2038",ignore.case = TRUE)))
STP_Inscrease_ps2_Output_2038$CycleEnd = 2038
names(STP_Inscrease_ps2_Output_2038) = sub('_2038$', "", names(STP_Inscrease_ps2_Output_2038))

STP_Inscrease_ps2_Output_2044 = data.frame(STP_Inscrease_ps2_Output %>% dplyr::select(c(1:26) | contains("_2044",ignore.case = TRUE)))
STP_Inscrease_ps2_Output_2044$CycleEnd = 2044
names(STP_Inscrease_ps2_Output_2044) = sub("_2044$", "", names(STP_Inscrease_ps2_Output_2044))

STP_Inscrease_ps2_Output_2050 = data.frame(STP_Inscrease_ps2_Output %>% dplyr::select(c(1:26) | contains("_2050",ignore.case = TRUE)))
STP_Inscrease_ps2_Output_2050$CycleEnd = 2050
names(STP_Inscrease_ps2_Output_2050) = sub("_2050$", "", names(STP_Inscrease_ps2_Output_2050))

STP_Inscrease_ps2_Output_FINAL = bind_rows(STP_Inscrease_ps2_Output_2020, STP_Inscrease_ps2_Output_2026, STP_Inscrease_ps2_Output_2032, STP_Inscrease_ps2_Output_2038, STP_Inscrease_ps2_Output_2044, STP_Inscrease_ps2_Output_2050)
STP_Inscrease_ps2_Output_FINAL$Scenario = "Alternative Base Package B: Effective 2032"
STP_Inscrease_ps2_Output_FINAL$Effective_Date = 2032



openxlsx::write.xlsx(as.data.frame(STP_Inscrease_ps2_Output), 'Output/STP_Inscrease_ps2_Output.xlsx', sheetName = "Sheet1",
										 col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)


rm(STP_Inscrease_ps2_Output, STP_Inscrease_ps2_Output_2020, STP_Inscrease_ps2_Output_2026, STP_Inscrease_ps2_Output_2032, STP_Inscrease_ps2_Output_2038, STP_Inscrease_ps2_Output_2044, STP_Inscrease_ps2_Output_2050)
gc()




### No Standard Target After 2 cycle
NoSTP_data = data.frame(Disclosure)
NoSTP_ps2_Output = BEPS_no_standard_target_simuliation(data = NoSTP_data, iterations = 5, policy_switch = 2, standard_level = .5, reduction = .2, br = .0437,nb_EUI_level = .05, nb_GHG_level = 0)

NoSTP_ps2_Output_2020 = data.frame(NoSTP_ps2_Output %>% dplyr::select(c(1:96) | contains("_2020",ignore.case = TRUE)))
NoSTP_ps2_Output_2020$CycleEnd = 2019
names(NoSTP_ps2_Output_2020) = sub("_2020$", "", names(NoSTP_ps2_Output_2020))

NoSTP_ps2_Output_2026 = data.frame(NoSTP_ps2_Output %>% dplyr::select(c(1:26) | contains("_2026",ignore.case = TRUE)))
NoSTP_ps2_Output_2026$CycleEnd = 2026
names(NoSTP_ps2_Output_2026) = sub("_2026$", "", names(NoSTP_ps2_Output_2026))

NoSTP_ps2_Output_2032 = data.frame(NoSTP_ps2_Output %>% dplyr::select(c(1:26) | contains("_2032",ignore.case = TRUE)))
NoSTP_ps2_Output_2032$CycleEnd = 2032
names(NoSTP_ps2_Output_2032) = sub("_2032$", "", names(NoSTP_ps2_Output_2032))

NoSTP_ps2_Output_2038 = data.frame(NoSTP_ps2_Output %>% dplyr::select(c(1:26) | contains("_2038",ignore.case = TRUE)))
NoSTP_ps2_Output_2038$CycleEnd = 2038
names(NoSTP_ps2_Output_2038) = sub('_2038$', "", names(NoSTP_ps2_Output_2038))

NoSTP_ps2_Output_2044 = data.frame(NoSTP_ps2_Output %>% dplyr::select(c(1:26) | contains("_2044",ignore.case = TRUE)))
NoSTP_ps2_Output_2044$CycleEnd = 2044
names(NoSTP_ps2_Output_2044) = sub("_2044$", "", names(NoSTP_ps2_Output_2044))

NoSTP_ps2_Output_2050 = data.frame(NoSTP_ps2_Output %>% dplyr::select(c(1:26) | contains("_2050",ignore.case = TRUE)))
NoSTP_ps2_Output_2050$CycleEnd = 2050
names(NoSTP_ps2_Output_2050) = sub("_2050$", "", names(NoSTP_ps2_Output_2050))

NoSTP_ps2_Output_FINAL = bind_rows(NoSTP_ps2_Output_2020, NoSTP_ps2_Output_2026, NoSTP_ps2_Output_2032, NoSTP_ps2_Output_2038, NoSTP_ps2_Output_2044, NoSTP_ps2_Output_2050)
NoSTP_ps2_Output_FINAL$Scenario = "Alternative Base Package A: Effective 2032"
NoSTP_ps2_Output_FINAL$Effective_Date = 2032




openxlsx::write.xlsx(as.data.frame(NoSTP_ps2_Output), 'Output/NoSTP_ps2_Output.xlsx', sheetName = "Sheet1",
										 col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)


rm(NoSTP_ps2_Output, NoSTP_ps2_Output_2020, NoSTP_ps2_Output_2026, NoSTP_ps2_Output_2032, NoSTP_ps2_Output_2038, NoSTP_ps2_Output_2044, NoSTP_ps2_Output_2050)
gc()


### Switch to Trajectory after 2 cycle
Trajectory_Data = data.frame(Disclosure)
Trajectory_ps2_Output = BEPS_trajectory_simuliation(data = Trajectory_Data, iterations = 5, policy_switch = 2, standard_level = .5, reduction = .2, br = .0437,nb_EUI_level = .05, nb_GHG_level = 0, targets = PROPERTYTYPEMAPPING[ , c("BEPSPROPERTYTYPE", "final_target_eui")])

Trajectory_ps2_Output_2020 = data.frame(Trajectory_ps2_Output %>% dplyr::select(c(1:96) | contains("_2020",ignore.case = TRUE)))
Trajectory_ps2_Output_2020$CycleEnd = 2019
names(Trajectory_ps2_Output_2020) = sub("_2020$", "", names(Trajectory_ps2_Output_2020))

Trajectory_ps2_Output_2026 = data.frame(Trajectory_ps2_Output %>% dplyr::select(c(1:26) | contains("_2026",ignore.case = TRUE)))
Trajectory_ps2_Output_2026$CycleEnd = 2026
names(Trajectory_ps2_Output_2026) = sub("_2026$", "", names(Trajectory_ps2_Output_2026))

Trajectory_ps2_Output_2032 = data.frame(Trajectory_ps2_Output %>% dplyr::select(c(1:26) | contains("_2032",ignore.case = TRUE)))
Trajectory_ps2_Output_2032$CycleEnd = 2032
names(Trajectory_ps2_Output_2032) = sub("_2032$", "", names(Trajectory_ps2_Output_2032))

Trajectory_ps2_Output_2038 = data.frame(Trajectory_ps2_Output %>% dplyr::select(c(1:26) | contains("_2038",ignore.case = TRUE)))
Trajectory_ps2_Output_2038$CycleEnd = 2038
names(Trajectory_ps2_Output_2038) = sub('_2038$', "", names(Trajectory_ps2_Output_2038))

Trajectory_ps2_Output_2044 = data.frame(Trajectory_ps2_Output %>% dplyr::select(c(1:26) | contains("_2044",ignore.case = TRUE)))
Trajectory_ps2_Output_2044$CycleEnd = 2044
names(Trajectory_ps2_Output_2044) = sub("_2044$", "", names(Trajectory_ps2_Output_2044))

Trajectory_ps2_Output_2050 = data.frame(Trajectory_ps2_Output %>% dplyr::select(c(1:26) | contains("_2050",ignore.case = TRUE)))
Trajectory_ps2_Output_2050$CycleEnd = 2050
names(Trajectory_ps2_Output_2050) = sub("_2050$", "", names(Trajectory_ps2_Output_2050))

Trajectory_ps2_Output_FINAL = bind_rows(Trajectory_ps2_Output_2020, Trajectory_ps2_Output_2026, Trajectory_ps2_Output_2032, Trajectory_ps2_Output_2038, Trajectory_ps2_Output_2044, Trajectory_ps2_Output_2050)
Trajectory_ps2_Output_FINAL$Scenario = "Trajectory Package A: Effective 2032"
Trajectory_ps2_Output_FINAL$Effective_Date = 2032

openxlsx::write.xlsx(as.data.frame(Trajectory_ps2_Output), 'Output/Trajectory_ps2_Output.xlsx', sheetName = "Sheet1",
										 col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)


rm(Trajectory_ps2_Output_2020, Trajectory_ps2_Output_2026, Trajectory_ps2_Output_2032, Trajectory_ps2_Output_2038, Trajectory_ps2_Output_2044, Trajectory_ps2_Output_2050)
gc()

### Switch to Trajectory with GHG intensity phase out after 2 cycle

Trajectory_GHG_Data = data.frame(Disclosure)
Trajectory_GHG_ps2_Output = BEPS_trajectory_w_thermal_GHG_simuliation(data = Trajectory_GHG_Data, iterations = 5, policy_switch = 2, standard_level = .5, reduction = .2, br = .0437,nb_EUI_level = .05, nb_GHG_level = 0, targets = PROPERTYTYPEMAPPING[ , c("BEPSPROPERTYTYPE", "final_target_eui")], ghg_target = 0)

Trajectory_GHG_ps2_Output_2020 = data.frame(Trajectory_GHG_ps2_Output %>% dplyr::select(c(1:96) | contains("_2020",ignore.case = TRUE)))
Trajectory_GHG_ps2_Output_2020$CycleEnd = 2019
names(Trajectory_GHG_ps2_Output_2020) = sub("_2020$", "", names(Trajectory_GHG_ps2_Output_2020))

Trajectory_GHG_ps2_Output_2026 = data.frame(Trajectory_GHG_ps2_Output %>% dplyr::select(c(1:26) | contains("_2026",ignore.case = TRUE)))
Trajectory_GHG_ps2_Output_2026$CycleEnd = 2026
names(Trajectory_GHG_ps2_Output_2026) = sub("_2026$", "", names(Trajectory_GHG_ps2_Output_2026))

Trajectory_GHG_ps2_Output_2032 = data.frame(Trajectory_GHG_ps2_Output %>% dplyr::select(c(1:26) | contains("_2032",ignore.case = TRUE)))
Trajectory_GHG_ps2_Output_2032$CycleEnd = 2032
names(Trajectory_GHG_ps2_Output_2032) = sub("_2032$", "", names(Trajectory_GHG_ps2_Output_2032))

Trajectory_GHG_ps2_Output_2038 = data.frame(Trajectory_GHG_ps2_Output %>% dplyr::select(c(1:26) | contains("_2038",ignore.case = TRUE)))
Trajectory_GHG_ps2_Output_2038$CycleEnd = 2038
names(Trajectory_GHG_ps2_Output_2038) = sub('_2038$', "", names(Trajectory_GHG_ps2_Output_2038))

Trajectory_GHG_ps2_Output_2044 = data.frame(Trajectory_GHG_ps2_Output %>% dplyr::select(c(1:26) | contains("_2044",ignore.case = TRUE)))
Trajectory_GHG_ps2_Output_2044$CycleEnd = 2044
names(Trajectory_GHG_ps2_Output_2044) = sub("_2044$", "", names(Trajectory_GHG_ps2_Output_2044))

Trajectory_GHG_ps2_Output_2050 = data.frame(Trajectory_GHG_ps2_Output %>% dplyr::select(c(1:26) | contains("_2050",ignore.case = TRUE)))
Trajectory_GHG_ps2_Output_2050$CycleEnd = 2050
names(Trajectory_GHG_ps2_Output_2050) = sub("_2050$", "", names(Trajectory_GHG_ps2_Output_2050))

Trajectory_GHG_ps2_Output_FINAL = bind_rows(Trajectory_GHG_ps2_Output_2020, Trajectory_GHG_ps2_Output_2026, Trajectory_GHG_ps2_Output_2032, Trajectory_GHG_ps2_Output_2038, Trajectory_GHG_ps2_Output_2044, Trajectory_GHG_ps2_Output_2050)
Trajectory_GHG_ps2_Output_FINAL$Scenario = "Trajectory Package B: Effective 2032"
Trajectory_GHG_ps2_Output_FINAL$Effective_Date = 2032

openxlsx::write.xlsx(as.data.frame(Trajectory_GHG_ps2_Output), 'Output/Trajectory_GHG_ps2_Output.xlsx', sheetName = "Sheet1",
										 col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)

rm(Trajectory_GHG_ps2_Output_2020, Trajectory_GHG_ps2_Output_2026, Trajectory_GHG_ps2_Output_2032, Trajectory_GHG_ps2_Output_2038, Trajectory_GHG_ps2_Output_2044, Trajectory_GHG_ps2_Output_2050)
gc()

### Switch to GHG Intensity after 2 cycle

GHG_Limits_Data = data.frame(Disclosure)
GHG_Limits_ps2_Output = BEPS_GHG_intensity_simuliation(data = GHG_Limits_Data, iterations = 5, policy_switch = 2, standard_level = .5, ghg_standard_level = .25,reduction = .2, br = .0437,nb_EUI_level = .05, nb_GHG_level = 0)

GHG_Limits_ps2_Output_2020 = data.frame(GHG_Limits_ps2_Output %>% dplyr::select(c(1:96) | contains("_2020",ignore.case = TRUE)))
GHG_Limits_ps2_Output_2020$CycleEnd = 2019
names(GHG_Limits_ps2_Output_2020) = sub("_2020$", "", names(GHG_Limits_ps2_Output_2020))

GHG_Limits_ps2_Output_2026 = data.frame(GHG_Limits_ps2_Output %>% dplyr::select(c(1:26) | contains("_2026",ignore.case = TRUE)))
GHG_Limits_ps2_Output_2026$CycleEnd = 2026
names(GHG_Limits_ps2_Output_2026) = sub("_2026$", "", names(GHG_Limits_ps2_Output_2026))

GHG_Limits_ps2_Output_2032 = data.frame(GHG_Limits_ps2_Output %>% dplyr::select(c(1:26) | contains("_2032",ignore.case = TRUE)))
GHG_Limits_ps2_Output_2032$CycleEnd = 2032
names(GHG_Limits_ps2_Output_2032) = sub("_2032$", "", names(GHG_Limits_ps2_Output_2032))


GHG_Limits_ps2_Output_2038 = data.frame(GHG_Limits_ps2_Output %>% dplyr::select(c(1:26) | contains("_2038",ignore.case = TRUE)))
GHG_Limits_ps2_Output_2038$CycleEnd = 2038
names(GHG_Limits_ps2_Output_2038) = sub('_2038$', "", names(GHG_Limits_ps2_Output_2038))

GHG_Limits_ps2_Output_2044 = data.frame(GHG_Limits_ps2_Output %>% dplyr::select(c(1:26) | contains("_2044",ignore.case = TRUE)))
GHG_Limits_ps2_Output_2044$CycleEnd = 2044
names(GHG_Limits_ps2_Output_2044) = sub("_2044$", "", names(GHG_Limits_ps2_Output_2044))

GHG_Limits_ps2_Output_2050 = data.frame(GHG_Limits_ps2_Output %>% dplyr::select(c(1:26) | contains("_2050",ignore.case = TRUE)))
GHG_Limits_ps2_Output_2050$CycleEnd = 2050
names(GHG_Limits_ps2_Output_2050) = sub("_2050$", "", names(GHG_Limits_ps2_Output_2050))

GHG_Limits_ps2_Output_FINAL = bind_rows(GHG_Limits_ps2_Output_2020, GHG_Limits_ps2_Output_2026, GHG_Limits_ps2_Output_2032, GHG_Limits_ps2_Output_2038, GHG_Limits_ps2_Output_2044, GHG_Limits_ps2_Output_2050)
GHG_Limits_ps2_Output_FINAL$Scenario = "GHG Limits Package: Effective 2032"
GHG_Limits_ps2_Output_FINAL$Effective_Date = 2032
openxlsx::write.xlsx(as.data.frame(GHG_Limits_ps2_Output_2020), 'Output/GHG_Limits_ps2_Output_2020.xlsx', sheetName = "Sheet1",
										 col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)


openxlsx::write.xlsx(as.data.frame(GHG_Limits_ps2_Output), 'Output/GHG_Limits_ps2_Output.xlsx', sheetName = "Sheet1",
										 col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = TRUE)

rm(GHG_Limits_ps2_Output, GHG_Limits_ps2_Output_2020, GHG_Limits_ps2_Output_2026, GHG_Limits_ps2_Output_2032, GHG_Limits_ps2_Output_2038, GHG_Limits_ps2_Output_2044, GHG_Limits_ps2_Output_2050)
gc()


#Combine everything

Output_data_FINAL = bind_rows(STP_Output_FINAL, STP_Inscrease_ps1_Output_FINAL, NoSTP_ps1_Output_FINAL, Trajectory_ps1_Output_FINAL, Trajectory_GHG_ps1_Output_FINAL, GHG_Limits_ps1_Output_FINAL , STP_Inscrease_ps2_Output_FINAL, NoSTP_ps2_Output_FINAL, Trajectory_ps2_Output_FINAL, Trajectory_GHG_ps2_Output_FINAL, GHG_Limits_ps2_Output_FINAL)

Base_Case = STP_Output_FINAL[STP_Output_FINAL$PID !="DUMMY NEW BUILDING",c("PID","CycleEnd","SITEEUI_KBTU_FT","Total_site_energy","TOTGHGEMISSIONS_METRICTONSCO2E","Annual_site_energy_savings","Emissions_Cumulative_Policy")]

Output_data_FINAL = merge(Output_data_FINAL[Output_data_FINAL$PID !="DUMMY NEW BUILDING",], Base_Case, by = c('PID','CycleEnd'), suffixes = c("","_STP"))

Output_data_FINAL$Emissions_Avoided_Policy_v_STP = Output_data_FINAL$Emissions_Cumulative_Policy_STP - Output_data_FINAL$Emissions_Cumulative_Policy

Output_data_FINAL$Emissions_Avoided_Policy_v_Baseline = Output_data_FINAL$Emissions_Cumulative_Baseline - Output_data_FINAL$Emissions_Cumulative_Policy



#Convert things to MMT
Output_data_FINAL$TOTGHGEMISSIONS_MMTCO2E = Output_data_FINAL$TOTGHGEMISSIONS_METRICTONSCO2E / 1000000
Output_data_FINAL$TOTGHGEMISSIONS_MMTCO2E_BASELINE = Output_data_FINAL$TOTGHGEMISSIONS_METRICTONSCO2E_BASELINE / 1000000
Output_data_FINAL$TOTGHGEMISSIONS_MMTCO2E_STP = Output_data_FINAL$TOTGHGEMISSIONS_METRICTONSCO2E_STP / 1000000
Output_data_FINAL$Emissions_Cumulative_Policy_MMTCO2E = Output_data_FINAL$Emissions_Cumulative_Policy / 1000000
Output_data_FINAL$Emissions_Cumulative_Policy_STP_MMTCO2E = Output_data_FINAL$Emissions_Cumulative_Policy_STP / 1000000
Output_data_FINAL$Emissions_Avoided_Policy_v_STP_MMTCO2E = Output_data_FINAL$Emissions_Avoided_Policy_v_STP / 1000000
Output_data_FINAL$Emissions_Avoided_Policy_v_Baseline_MMTCO2E = Output_data_FINAL$Emissions_Avoided_Policy_v_Baseline / 1000000

rm(Effective_2026_packages, Effective_2032_packages,Base_Case)
gc()

openxlsx::write.xlsx(as.data.frame(Output_data_FINAL), 'Output/Combined_Output_FINAL.xlsx', sheetName = "Sheet1",
                     colNames = TRUE, rowNames = FALSE, append = FALSE, overwrite = TRUE)

Effective_2026_packages = Output_data_FINAL[(Output_data_FINAL$PID !="DUMMY NEW BUILDING" & !is.na(Output_data_FINAL$PID) & Output_data_FINAL$Effective_Date == 2026),c("PID","CycleEnd","TOTGHGEMISSIONS_METRICTONSCO2E", "Scenario", "Emissions_Cumulative_Policy_MMTCO2E", "Emissions_Cumulative_Policy_STP_MMTCO2E")]
Effective_2032_packages = Output_data_FINAL[(Output_data_FINAL$PID !="DUMMY NEW BUILDING" & !is.na(Output_data_FINAL$PID) & Output_data_FINAL$Effective_Date == 2032),c("PID","CycleEnd","TOTGHGEMISSIONS_METRICTONSCO2E", "Scenario","Emissions_Cumulative_Policy_MMTCO2E", "Emissions_Cumulative_Policy_STP_MMTCO2E")]
Effective_2026_packages = Effective_2026_packages[complete.cases(Effective_2026_packages),c("PID","CycleEnd","TOTGHGEMISSIONS_METRICTONSCO2E", "Scenario","Emissions_Cumulative_Policy_MMTCO2E", "Emissions_Cumulative_Policy_STP_MMTCO2E")]
Effective_2032_packages = Effective_2032_packages[complete.cases(Effective_2032_packages),c("PID","CycleEnd","TOTGHGEMISSIONS_METRICTONSCO2E", "Scenario","Emissions_Cumulative_Policy_MMTCO2E", "Emissions_Cumulative_Policy_STP_MMTCO2E")]

Effective_2026_packages$TOTGHGEMISSIONS_MMTCO2E_e2026 = Effective_2026_packages$TOTGHGEMISSIONS_METRICTONSCO2E/1000000

Effective_2032_packages$TOTGHGEMISSIONS_MMTCO2E_e2032 = Effective_2032_packages$TOTGHGEMISSIONS_METRICTONSCO2E/1000000

openxlsx::write.xlsx(as.data.frame(Effective_2026_packages), 'Output/Effective_2026_packages.xlsx', sheetName = "Sheet1",
                     colNames = TRUE, rowNames = FALSE, append = FALSE, overwrite = TRUE)
openxlsx::write.xlsx(as.data.frame(Effective_2032_packages), 'Output/Effective_2032_packages.xlsx', sheetName = "Sheet1",
                     colNames = TRUE, rowNames = FALSE, append = FALSE, overwrite = TRUE)
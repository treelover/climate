PAR_correction_eben = function(datain,D9,D10,D11,D12){

# Description: R script coded by Eben Broadbent from equations provided by Jacob Bingham, Application Engineer, of Apogee Instruments (www.apogeeinstruments.com).

# Instructions for Calculating Quantum Sensor Accuracy:
#  1 - Comparison must be made on a clear, non-polluted, summer day within two hours of solar noon.
#  2 - Sensor must be level and perfectly clean. Enter your measured solar radiation in the blue cell below.
#  3 - Enter input parameters in green cells below.
#  4 - Difference between the model and your sensor is shown in the yellow cell below.
#  5 - If the measured value is more than 5 % different than the estimated value on replicate days, contact Apogee for recalibration.

#Definitions:
#  Latitude =	latitude of the measurement site [degrees] (positive for Northern hemisphere, negative for Southern hemisphere)		
#	Longitude =	longitude of the measurement site [degrees]				
#	Longitudetz =	longitude of the center of the local time zone [degrees] 		
#		(expressed as positive degrees west of the standard meridian in Greenwich, England)
#		Examples: 75, 90, 105, and 120 for Eastern, Central, Rocky Mountain, and Pacific time zones in the US					
#			 0 for Greenwich, England; 345 for Paris, France; 255 for Bangkok, Thailand
#	Elevation =	elevation of the measurement site [meters]				
#	Day of Year =	numeric day of the year (0-365)					
#	Time of Day =	numeric time of the day in tenths of hours (0-24.0)			
#	Daylight Savings =	correction to account for daylight savings time (enter 1 if on daylight savings time, 0 if not)					
#	TA =	air temperature at the time of measurement [C] (if air temperature is not available, leave cell blank)					
#	RH =	relative humidity at the time of measurement [%] (if relative humidity is not available, leave cell blank)					
#	Measured PPF =	measured value of photosynthetic photon flux [mmol m-2 s-1] from the sensor being tested					
#	Model Estimated PPF =	estimated photosynthetic photon flux [mmol m-2 s-1] incident on a horizontal plane for clear sky conditions					
#	Difference from Model =	difference in percent between the measured and estimated values of radiation					

# Definitions:				
#	Solar Constant = solar constant for the mean distance between the Earth and sun		
#	Energy in PAR =	average energy content of photosynthetically active radiation (PAR)	
#	PAR / Solar =	ratio of photosynthetically active radiation (PAR) to incoming shortwave radiation (SWi)		
#	Kt =	atmospheric turbidity coefficient (1 for clean air)		
#	dr =	inverse relative distance factor for the distance between the Earth and sun	
#	d =	solar declination		
#	eqt =	equation of time		
#	Solar N =	time of solar noon		
#	Solar Z =	solar zenith angle		
#	Kb =	clearness index for direct beam radiation		
#	Kd =	transmissivity index for diffuse radiation		
#	PB =	barometric pressure of the measurement site (kPa)		
#	eA =	air vapor pressure (kPa)		
#	w =	precipitable water in the atmosphere (mm)		
#	SWa =	extraterrestrial radiation (W m-2)		

# Reference:
#  The ASCE Standardized Reference Evapotranspiration Equation. 2005. American Society of Civil Engineers. Reston, Virginia, USA.

# Coding notes:
#  Run on .csv sheet containing over 800,000 rows and 20 columns of met data. will run in < 1 minute.
# Set working directory (where input and output files are stored/written to)
##setwd("")
# Open .csv table (must have column names as defined below, including columns created for output of modeled variables)
##datain <- read.csv('Input Met Data.csv', header=TRUE, colClasses = "character")
##colnames(datain)

# User defined variables
#D9  = 41.7                                        # Latitude =	41.7  (from http://itouchmap.com/latlong.html)
#D10 = 111.8                                     # Longitude =	111.8 (from http://itouchmap.com/latlong.html)
#D11 = 105                                        # Longitudetz =	105  (150 = Hawaii time zone) (http://clearskycalculator.com/longitudeTZ.htm)
#D12 = 1400                                      # Elevation =	1400  (from Google Earth)
D13 = as.numeric(datain$day_of_year)            # Day of Year =	172  (input from 10 minute measurements)
D14 = round(as.numeric(datain$day_hours),1)     # Time of Day =	13.5 (input from 10 minute measurements) (HI correction applied, 3 hours difference)
D15 = 0                                         # Daylight Savings = 0 (Hawaii does not observe daylight savings time (thus = 0): http://www.timetemperature.com/tzus/hawaii_time_zone.shtml)
D16 = as.numeric(datain$mid_Ta)                 # TA =	25 (input from 10 minute measurements)
D17 = as.numeric(datain$mid_rH)                 # RH =	30  (input from 10 minute measurements)

# test values of user defined variables (for copy / paste into R cmd line)
# D9=41.7;D10=111.8;D11=105;D12=1400;D13=172;D14=13.5;D15=1;D16=25;D17=30 # Apogee test
# D9=19.95;D10=-155.28;D11=150;D12=1155;D13=172;D14=12;D15=0;D16=25;D17=60 # Hawaii test

# Constants:			
Q2 =	1367.8 # Solar Constant (W m-2)
Q3 =	218000 # Energy in PAR (J mol-1)
Q4 =	0.45	 # PAR / Solar (J J-1)
Q5 =	1.0	   # Kt

# Calculated Parameters:			
Q8 = 1+0.033*cos(((2*pi)/365)*D13) # dr
Q9 = asin(0.39785*sin((278.97+0.9856*D13+1.9165*sin((356.6+0.9856*D13)*(pi/180)))*(pi/180)))*180/pi # delta
Q10 = (5.0323-430.847*cos(((2*pi*D13)/366)+4.8718)+12.5024*cos(2*(((2*pi*D13)/366)+4.8718))+18.25*cos(3*(((2*pi*D13)/366)+4.8718))-100.976*sin(((2*pi*D13)/366)+4.8718)+595.275*sin(2*(((2*pi*D13)/366)+4.8718))+3.6858*sin(3*(((2*pi*D13)/366)+4.8718))-12.47*sin(4*(((2*pi*D13)/366)+4.8718)))/60 # eqt
Q11 = 12+D15-(Q10/60)-((D11-D10)/15) # Solar N
Q12 = acos(sin(D9*(pi/180))*sin(Q9*(pi/180))+cos(D9*(pi/180))*cos(Q9*(pi/180))*cos((D14-Q11)*(pi/12)))*(180/pi) # Solar Z

T8 = 101.325*((288-0.0065*(D12-0))/288)^(9.80665/(0.0065*287)) # Pb
T9 = 0.61121*exp((17.502*D16)/(240.97+D16))*(D17/100.0) # ea
T10 = 0.14*T9*T8+2.1 # w
T11 = 0.98*exp(((-0.00146*T8)/(Q5*sin((90-Q12)*(pi/180))))-0.075*(T10/sin((90-Q12)*(pi/180)))^0.4) # kb

T12 = 0.18+0.82*T11 # kd, assumes <= 0.15 kb value

T11g = which(T11 > 0.15)
if (is.na(T11g[1]) == FALSE) {T12[T11g]  = 0.35-0.36*T11[T11g]}  # kd, adjusts for those having kb not <= 0.15	

T13 = Q2*Q8*cos(Q12*(pi/180)) # Swa

Model_Estimated_SW  = (T11+T12)*T13
Model_Estimated_PPF = (Q4/(0.000001*Q3))*(T11+T12)*T13 # umol m-2 s-1

# Write modeled PAR data into the data table			
datain$Clearsky_PAR_modeled <- Model_Estimated_PPF
datain$Clearsky_SW_modeled  <- Model_Estimated_SW

# Create modeled parameter table
model_vals <- cbind(datain$timestamp_1min,Q8,Q9,Q10,Q11,Q12,T8,T9,T10,T11,T12,T13,Model_Estimated_SW,Model_Estimated_PPF)

#List of outputs
output = list(datain,model_vals)

#Old method of output
#write.csv(datain, file = "Output MET with modeled.csv", row.names = FALSE)
#write.csv(model_vals, file = "Model_parameters.csv", row.names = FALSE)
return(output)
}




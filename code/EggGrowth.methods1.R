
# ===== Egg Growth Modeling runs ==== 
# This is an introduction to some scripting methods for generating results
# It IS NOT intended to be comprehensive
# It IS intended to give you some ideas.
# Happy Modeling :: Nick

# 1 April 2024
# W. Nicholas Beer CBR/SAFS/UW nickbeer@uw.edu
#
# Survivals are generated for user's parameters and data using the methods selected.
# https://cbr.washington.edu/sacramento/grow/index.html
# Suitability for any particular purpose is NOT guaranteed.
# These methods are subject to change.

# Two "modes" are possible with Egg Growth modeling: 
# --- Historical mode: modeling for American River Chinook based on the carcass survey data
#     in the CBR data base. Historical 'hindcasts' are made by pairing temperature 
#     and redd timeseries by year.
#     Hindcasts use token=value pairs to retrieve data from the CBR database.
# --- Scenario mode: This allows for effectively unlimited runs with 
#     user-provided temperatures, redd distributions, model selection 
#     and parameterization. 

# Most examples are of historical-mode runs. 
# The methods are also the basis of scenario mode runs which are introduced below.
# Parameter values for the egg-development and incubation-survival formulations are the defaults currently on: 
# version 2.1.5 


#==== 1. Generate query strings ==== 
#  Familiarity with the Egg Growth page GUI is very helpful.
# Using a browser, navigate to 
# https://cbr.washington.edu/sacramento/grow/index.html
# Select the "Generate Query Strings" in the "Analysis and Results Display" area.
# Click one of the "Run Emergence ..." buttons.

#==== 2.  Recover a 'raw' result ====
# Copy and paste one of the query strings from the previous step into a browser window

#==== 3. Modify query to customize it ====
# Return to the Egg Growth page in the browser window.
# Make a simple change. 
# Ensure "Generate Query Strings" is selected.
# Click one of the "Run Emergence ..." buttons.
# Review and test the new query strings.
# Compare to Step 2. 

#==== 4. Prepare for bulk processing with R ====
# assemble parts of a query string
# Familiarity with the programming language: "R" is required.
# R 'tidyverse' library is used here 
# Note that R is not required for using query strings to obtain results.

# install.packages("tidyverse")

library(tidyverse)

#==== 5. Run the model and capture the output ====
# Use R functionality to control modeling and post-processing
# Establish a 'base' string 

base <- "https://cbr.washington.edu/sac-bin/grow/emergecontrols.pl"

      # Describe the output you want. Here: the details of the cohort in a csv file
string5 <- paste0(base,"?raw=all&rtnCSV=1")
      # Run a specific year:
string5 <- paste0(string5,"&reddyear=2021&tempyear=2021")
results5 <- read_csv(string5,show_col_types = FALSE)
print(results5,n=51)

#==== 6. compare mortality models ====
# Next are the 3 default configurations of the exponential group of equations.
# They have been extracted from 'complete' query strings 
# There is a checkbox for this purpose on the GUI interface.
# The exponential models use the same 6 parameters. The defaults are set when the formula are chosen,
# and can be changed individually.
# The "paramdefault" token is for your benefit, to help you identify the group. 
# Regardless of the model name, the EXPONENTIAL MODEL  will run whatever the 6 parameters are assigned.

salmoddefault <- paste0(  "&alevinSurvPar1=2.521",
                          "&alevinSurvPar2=1.461",
                          "&alevinSurvParexp=12",
                          "&eggSurvPar1=1.475",
                          "&eggSurvPar2=1.392",
                          "&eggSurvParexp=11")
zeugdefault <- paste0(  "&alevinSurvPar1=1.35",
                        "&alevinSurvPar2=0.9054",
                        "&alevinSurvParexp=8",
                        "&eggSurvPar1=1.35",
                        "&eggSurvPar2=0.9054",
                        "&eggSurvParexp=8")
waterforumdefault <- paste0("&alevinSurvPar1=1.017554",
                            "&alevinSurvPar2=1.24092",
                            "&alevinSurvParexp=10",
                            "&eggSurvPar1=3.408488",
                            "&eggSurvPar2=1.21122",
                            "&eggSurvParexp=11")

# Build some different ways to access these values and names
mortmodels <- c("waterforumdefault","zeugdefault","salmoddefault")
mortwf <- eval(as.name(mortmodels[1]))   # this stringifys the content of the named models
mortze <- eval(as.name(mortmodels[2]))
mortsm <- eval(as.name(mortmodels[3]))

print(waterforumdefault)
print(mortwf)

string6 <- paste0(base,"?raw=cohort&rtnCSV=1")  # important token=value pairs
string6 <- paste0(string6,"&reddyear=2021&tempyear=2021")
  # ensure it is the exponential group 
string6 <- paste0(string6,"&mortality=exp")
  # identify the parameter groups
mortmodels <- c("waterforumdefault","zeugdefault","salmoddefault")
  # run and cache results
results6LIST <- list()
for(i in 1:length(mortmodels)){
  print(mortmodels[i])
  print(paste0(string6,eval(as.name(mortmodels[i]))))
  results6LIST[[mortmodels[i]]] <-  read_csv(paste0(string6,eval(as.name(mortmodels[i]))),show_col_types = FALSE)
}
print(results6LIST[["salmoddefault"]])

#==== 7: More compact and simpler (final survival of population only):
# NOTE: rtnCSV not needed nor wanted for single value (survival) return
# thus, when the result is returned, we use 'scan' function to obtain the single value

year <- 2021
results7 <- cbind.data.frame(model=mortmodels,survival=NA)
for(i in 1:length(mortmodels)){
  parz <- eval(as.name(mortmodels[i]))
  string7 <- paste0(base,"?raw=survival")
  string7<- paste0(string7,"&mortality=exp")
  string7 <- paste0(string7,"&rtnCSV=1&reddyear=",year,"&tempyear=",year,parz)
  # print(what)
  results7[i,2] <-  scan(string7)
}
print(string7)
print(year)
print(results7)

#==== 8. Multi-model runs  across parameterizations and years ====
# Also, keep track of how long this takes.
string8 <- paste0(base,"?rtnCSV=1&raw=survival")
        # ensure it is the exponential group 
string8 <- paste0(string8,"&mortality=exp")
        # identify the parameter groups
results8 <- NULL
# For all: 
# years <- 2011:2022
# For demo:
years <- 2019:2022
starttime <- Sys.time()
# A base run using db query
for(year in years){ 
  string8.0 <- paste0(string8,"&reddyear=",year,"&tempyear=",year,"&tempprojpoint=AWB:DailyAvg")
  out <- scan(paste0(string8.0,mortwf))
  out2 <- scan(paste0(string8.0,mortze))
  out3 <- scan(paste0(string8.0,mortsm))
  results8 <- rbind.data.frame(results8,cbind.data.frame(year=year,"waterforum"=out,"zeug"=out2,"salmod"=out3))
}
print(results8)
x <- Sys.time()-starttime
print(x)

#==== 9. Use cached files for scenario comparison ====
# A scenario run using cached files
# Files are placed on the CBR servr upon request.
# files are placed in a directory with a uniquename that begins with "userdir"
# You may request multiple directories in order to keep your files organized
# Please assure that files meet formating guidelines.
# AWB is a legacy site for temperatures
# American Rivers carcasses available back to 2011
# AFO online starting 2020. previous results use mean temperatures
# For all: 
# years <- 2011:2022
# For demo:
years <- 2019:2022
results9 <- NULL
for(year in years){ 
  string9 <- paste0(base,"?rtnCSV=1&raw=survival","&reddyear=",year)
  string9 <- paste0(string9,"&units=farenheit&tempsource=userdirN1&temfile=",paste0("AWB.",year,".csv"))
  out <-     scan(paste0(string9,mortwf))
  out2 <-    scan(paste0(string9,mortze))
  out3 <-    scan(paste0(string9,mortsm))
  results9 <- rbind.data.frame(results9,cbind.data.frame(year=year,"waterforum"=out,"zeug"=out2,"salmod"=out3))
}
print(results9)

#==== 10. Recover names of files in the directory ====
string10 <- paste0(base,"?tempsource=userdirN1&temfile=dirlist")
results10 <- read_csv(string10,show_col_types=TRUE)
print(results10)


#==== 11. redds and temperatures are cached ====
# redds: a single cohort on day 310
# temps: cached copies of the AWB temp profiles
results11 <- NULL
# For demo:
years <- 2019:2022
for(year in years){ 
  string12 <- paste0(base,"?raw=survival")
  string12 <- paste0(string12,"&units=farenheit&tempsource=userdirN1&temfile=",paste0("AWB.",year,".csv"))
  string12 <- paste0(string12,"&spawnsource=userdirN1&reddfile=redd.10.Day310.csv")
  out <-     scan(paste0(string12,mortwf))
  out2 <-    scan(paste0(string12,mortze))
  out3 <-    scan(paste0(string12,mortsm))
  results11 <- rbind.data.frame(results11,cbind.data.frame(year=year,"waterforum"=out,"zeug"=out2,"salmod"=out3))
}
print(results11)

#==== 12. temperatures are cached , shift a redd spawning day freely====
# single release on changing days
#ONLY look at waterforum results
results12 <- NULL
years <- 2019:2020
trydays <- seq(200,400,by=10)
for(year in years){ 
  tryvalues <- NULL
  for(tryday in trydays){
    string12 <- paste0(base,"?raw=survival")
    string12 <- paste0(string12,"&spawnsource=spawntext&spawntextstring=Day,Redds%0A",tryday,",10")
    string12 <- paste0(string12,"&units=farenheit&tempsource=userdirN1&temfile=",paste0("AWB.",year,".csv"))
    out <-     scan(paste0(string12,mortwf))
    tryvalues <- c(tryvalues,out)
  }
  results12 <- rbind.data.frame(results12,tryvalues)
}
results12WF <- cbind.data.frame(years,results12)
names(results12WF) <- c("year",as.character(trydays))
print(results12WF)


#==== 13.  Grab the temperature file and examine
# NOTE: reddyear is a dummy value and could be any legitimate year
# Setting raw=temperature will recover this file
string13 <- paste0(base,"?raw=temperature&reddyear=2020&tempsource=userdirN1&temfile=sine.temp.sample.csv")
result13 <- as.data.frame(read_csv(string13))
print(head(result13))
plot(result13$Day,result13$Tem10,type="line",xlab="Day of Year",ylab="Temperature (C)")
lines(result13$Day,result13$Tem11,col=2)
lines(result13$Day,result13$Tem12,col=3)
lines(result13$Day,result13$Tem13,col=4)


#==== 14. Use this file to generate results 
results14 <- NULL
columns <- 2:5   # Data columns in the sine data frame
trydays <-  seq(250,350,by=20)
for(column in columns){ 
  tryvalues <- NULL
  for(tryday in trydays){
    string14 <- paste0(base,"?raw=survival")
    string14 <- paste0(string14,"&spawnsource=spawntext&spawntextstring=Day,Redds%0A",tryday,",10")
    string14 <- paste0(string14,"&units=centigrade&tempsource=userdirN1&temfile=sine.temp.sample.csv")
    string14 <- paste0(string14,"&tempdatacolumn=",column)
    out <-     scan(paste0(string14,mortwf))
    tryvalues <- c(tryvalues,out)
  }
  results14 <- rbind.data.frame(results14,tryvalues)
}
results14WF <- cbind.data.frame(columns,results14)
names(results14WF) <- c("Column",as.character(trydays))
print(results14WF)

#====== 15. GENERAL  token=value pairs ====
# Select model combinations freely and run qith qerystrings box checked.
# Capture differences as text snippets
# retain or regenerate these as needed

#===== 16. TIP: ======
# Check any scripted run against an identical run using the GUI.
# This will ensure that you are creating the scenario that you intend.

#=======================



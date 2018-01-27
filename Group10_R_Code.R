################################################## GROUP 10 - TEAM MEMBERS ########################################################
# Florence SAVARYDEBEAUREGARD
# Yongsheng LOH
# Chenpei TAN

# Note: Click File > Reopen with Encoding > select UTF-8

################################################## Libraries ######################################################################


install.packages("dplyr") 
install.packages("textclean") 
install.packages("tidyr")

library(dplyr) 
library(textclean) 
library(tidyr) 

################################################## Load all the datasets into R ###################################################

# Set the directory to the folder that contains all the raw datasets
directory <- "//psf/Home/Desktop/IESEG_R/R"
setwd(directory)

# Ensure that the directory is now reflecting the folder that contains all the raw datasets
getwd()

# Load Info and Results Datasets
files <- list.files(pattern = ".RData")
lapply(files, load, .GlobalEnv)

# Load Zipcode Excel file
zipcode <- read.csv(file.path(directory, "zipcodes_BE3.csv"), stringsAsFactors=FALSE)

################################################## DEALING WITH ACCENTS ###########################################################

# To replace all accent into alphabets
accent = c("Ã¨","Ãˆ","Ã©","Ã‰","Ã«","Ã¢","Ã»","Ãª")
accent_repl = c("e","e","e","e","e","a","u","e")



################################################## DATASET : Info #################################################################

# Purpose: 
#   1 - Create unique key to join Info and Results Datasets together later
#   2 - Unique key used is the (Race) and (RaceDate) contained in the names of the datasets

# Place names Of all Info datasets into a list
listnames_info = ls(pattern="^info_")

i_file = data.frame() # Create Empty Data Frame for appending below

for (i in 1:length(listnames_info)) {
  # Extract filename information to create columns with unique key for joining
  i_label = read.table(text=gsub("^info_", "", listnames_info[i] ),
                       sep="_", col.names = c("Race", "RaceDate", "Result_NR", "Age_NR"))
  
  # Merge the original Info Dataset with "Race" as unique key
  i_set = cbind(get(listnames_info[i]), UKey_Race = i_label$Race)
  
  # Stack all the i_set for 24 races
  i_file = rbind(i_file, i_set)
}

i_file$date = as.Date(i_file$date, "%d-%m-%Y")

# Purpose: Use Function to change format of Longitude & Latitude
#   Converting coordinates from DMS or DdM formats to decimal degrees
#   DMS = "degrees minutes seconds"; DdM = "degrees decimal minutes"  

dg2dec <- function(varb, Dg=NA, Min=NA, Sec=NA, SW.Hemisphere="S|W") { 
  
  # Dg=decimal, Min=minutes and Sec=seconds;  
  # NOTE 1 - if the format is "degrees decimal minutes - DdM" (e.g. 40° 26.767′ N) and not  
  # "degrees minutes seconds - DMS" (e.g. 40° 26′ 46″ N), then call the function only with  
  # Dg and Min arguments, like dg2dec(varb, Dg="°", Min="′N").  
  # Same applies when there is no seconds symbol (e.g. 45°12'7.38). 
  # Note that one should not use blank spaces in Dg, Min or Sec arguments (will return NA). 
  # therefore, strsplit() will split string "varb" by what symbols you give to Dg, Min, Sec 
  
  DMS <- sapply(strsplit(varb, paste0('[', Dg, Min, Sec, ']')), as.numeric) 
  
  # DMS is a matrix; first row contains degrees; second - minutes; third - seconds. 
  # If the format is "degrees decimal minutes" (e.g. 40° 26.767′ N) and not  
  # "degrees minutes seconds" (e.g. 40° 26′ 46″ N), then the matrix has only two valid rows:  
  # first row contains degrees; the second - minutes; 
  # therefore, compute conversion for seconds only if there are more than 2 rows in DMS  
  # and Sec is different from NA (if there are seconds in the DMS format) 
  
  decdg <- abs(DMS[1, ]) + DMS[2, ]/60 + ifelse(dim(DMS)[1] > 2  & !is.na(Sec), DMS[3, ]/3600, 0) 
  
  # all cordinates from Southern or Western Hemispheres become negative in their decimal format 
  
  SW <- grepl(pattern = SW.Hemisphere, x = varb, ignore.case = TRUE) 
  
  return(ifelse(SW, -1, 1) * decdg) 
  
} 

# Change special characters in Latitude and Longitude
i_file$lat <- mgsub(i_file$lat, "Â°", "°") 
i_file$long <- mgsub(i_file$long, "Â°", "°") 

# Apply function to the Latitude and Longitude columns and transform the value into decimal degrees
i_file$lat <- dg2dec(varb=i_file$lat, Dg='°', Min="'", Sec='\\\\"N') 
i_file$long  <- dg2dec(varb=i_file$long , Dg='°', Min="'", Sec='\\\\"E|W') 

############################################### DATASET : RESULTS ###############################################################

# Purpose: 
#   1 - Create unique key to join Info and Results Datasets together later
#   2 - Unique key used is the (Race) and (RaceDate) contained in the names of the datasets

# Place names Of all Results datasets into a list
listnames_results = ls(pattern="results_")

r_file = data.frame() # Create Empty Data Frame for appending below

for (i in 1:length(listnames_results)) {
  # Extract filename information to create columns with unique key for joining
  r_label = read.table(text=gsub("^results_", "", listnames_results[i] ),
                       sep="_", col.names = c("Race", "RaceDate", "Result_NR", "Age_NR"))
  
  # Merge the original Results Dataset with "Race" as unique key
  r_set = cbind(get(listnames_results[i]), UKey_Race = r_label$Race)
  
  # Stack all the r_set for 24 races
  r_file = rbind(r_file, r_set)
}

################################################## JOIN INFO & RESULTS DATASETS TO CREATE FULL_TABLE #############################

# Join tables Info and Results into one table & delete unnecessary columns
full_table = left_join(r_file,i_file,  by = "UKey_Race") %>% # Left Join i_file to r_file  
             filter(!is.na(position)) %>% # Remove positions with "NA" to keep only results that came in top 25%
             select (-c(race, result, age, diff, coeficient)) %>%  # Remove unwanted columns
             separate(town, c("town", "province"), '\\(') %>% # To split values of towns that contain province
             group_by(loft_id,UKey_Race) %>% 
             mutate(Max_distance = max(distance)) %>% # To replace those pseudo distance with the actual distance of that race per loft id
             mutate(position_within_loft_race = ifelse(distance == Max_distance, 1 , distance)) # Create arriving position per race per loft for below variable creation

full_table$loft = mgsub(full_table$loft,accent,accent_repl)
full_table$town = mgsub(full_table$town,accent,accent_repl)

# Data Cleansing for Town
full_table$town = gsub('\\.', '',full_table$town) # Replace town "Wasmes-Audemez-Briff." & remove the period
full_table$town = gsub('St.-Truiden', 'Sint-Truiden',full_table$town) # Replace the short form 
full_table$town = trimws(full_table$town) # Remove trailing blanks after splitting towns by \\(

# Remove ending parenthesis for Province
full_table$province = gsub(")","",full_table$province) 

# To replace short forms for Province and Town
#   Note: For Nl, town is Baarle-Nassau. It is located in a borderline between Netherland and Belgium. Postalcode not available in Zipcode dataset

province_values = unique(full_table$province)  # Unique list of Province

shortform = c("Ht.","Bt.","Nam.","Antw.","Comines","Limb.","O.-Vl","O.-Vl.","O.Vl.","Wortegem-Peteg.","Lg.", "Leie","Lux.","W.-Vl.")
longform = c("Hainaut","Brabant wallon", "Namur","Antwerpen","Comines-Warneton","Limburg","Oost-Vlaanderen","Oost-Vlaanderen","Oost-Vlaanderen", "Wortegem-Petegem","Liege","Waregem","Luxembourg","West-Vlaanderen")

full_table$province = mgsub(full_table$province,shortform,longform) # Change short form to better match values in Zipcode dataset

# Change all values to lowercase
full_tablecase <- c("town","province") # Specify the variables into a vector
full_table[full_tablecase]= sapply(full_table[full_tablecase],tolower) # Cast them to lower case

################################################## DATASET : ZipCode #############################################################  

# Purpose: Data Cleansing for ZipCode

#   1.There are duplicates where name of town in French appears in both "Town" and "Translation" & vice versa for Dutch
#     - To keep only the observation with name of Town in French and its Dutch equivalent in Translation (where Sub_town is empty and N_inhab is not NA)
#   2.In addition, keep observations that have different values for Town and Sub_town (where Sub_town is not empty)
zipcode = subset(zipcode, ((zipcode$Sub_town == "" & zipcode$N_inhab != "NA") | zipcode$Town != zipcode$Sub_town & zipcode$Sub_town != ""))   

# Create New column to contain the values of town that has empty subtown for merging purpose below
zipcode$Pure_town <- ifelse(zipcode$Sub_town == "", zipcode$Town, "")   

# Replace all accents into alphabets  
zipcode = zipcode %>%    
          mutate_all(funs(mgsub(.,accent,accent_repl)))  

# Change all values to lowercase
zipcodecase <- c("Town","Sub_town","Province","translation","Pure_town") # Specify the variables into a vector
zipcode[zipcodecase]= sapply(zipcode[zipcodecase],tolower) # Cast them to lower case


#################################################### MERGE ZIPCODE INTO FULL_TABLE ###############################################

# Purpose: Add Postalcode from Zipcode Dataset into base table

# Create a listnames of full_table dataset and only 'Postalcode' column from zipcode dataset 
#     To aid selection of columns in below 4 merge
listnames_full_table = colnames(full_table)
listnames_zipcode = colnames(zipcode)
listnames_zipcode = listnames_zipcode[1]

# Obtained using below 4 merge to cater to the different combinations of merging the 2 datasets to obtain the postalcode
#     1st Merge 
#     Eg. For loft_id 188933-74, initial value "Kieldrecht (Beveren)", breakdown into town "Kieldrecht" & province "Beveren".
full_table_merge1 = left_join(full_table,zipcode, by = c("province" = "Town", "town" = "Sub_town")) %>%
                    select(c(listnames_full_table,listnames_zipcode)) # Keep only relevant columns

#     2nd Merge
#     Eg. For loft_id 204856-89, initial value "Tongerlo (Antw.)", breakdown into town "Tongerlo" & province "Antw." which is replaced with "Antwerpen". 
#     Tongerlo appears in both antwerpen and limburg hence this method will merge the correct postalcode of 2260 instead of 3960
full_table_merge2 = full_table_merge1 %>% 
                    filter(is.na(Postalcode)) %>% # Keep only rows with no match for Postalcode in 1st Merge
                    select(c(listnames_full_table)) %>% # Remove 'Postalcode' column from zipcode dataset joined from 1st Merge
                    left_join(.,zipcode, by = c("province" = "Province", "town" = "Sub_town")) %>%
                    select(c(listnames_full_table,listnames_zipcode)) # Keep only relevant columns

#     3rd Merge
#     Eg. For loft_id 117671-10, town "elene" is Sub_town in zipcode dataset.
full_table_merge3 = full_table_merge2 %>% 
                    filter(is.na(Postalcode)) %>% # Keep only rows with no match for Postalcode in 2nd Merge
                    select(c(listnames_full_table)) %>% # Remove 'Postalcode' column from zipcode dataset joined from 2nd Merge
                    left_join(.,zipcode, by = c("town" = "Sub_town")) %>%  
                    select(c(listnames_full_table,listnames_zipcode)) # Keep only relevant columns

#     4th Merge
#     Eg. For loft_id 149415-35, town "la hulpe" is merged with Pure_town in zipcode that has empty sub_town to obtain postalcode 1310.
full_table_merge4 = full_table_merge3 %>% 
                    filter(is.na(Postalcode)) %>% # Keep only rows with no match for Postalcode in 3rd Merge
                    select(c(listnames_full_table)) %>% # Remove 'Postalcode' column from zipcode dataset joined from 3rd Merge
                    left_join(.,zipcode, by = c("town" = "Pure_town")) %>%  
                    select(c(listnames_full_table,listnames_zipcode)) # Keep only relevant columns

#     5th Merge
#     Eg. For loft_id 308782-31, town "Bruxelles (Uccle)", breakdown into town "Bruxelles" & province "Uccle" which is in translation column in zipcode 
full_table_merge5 = full_table_merge4 %>% 
                    filter(is.na(Postalcode)) %>% # Keep only rows with no match for Postalcode in 4th Merge
                    select(c(listnames_full_table)) %>% # Remove 'Postalcode' column from zipcode dataset joined from 4th Merge
                    left_join(.,zipcode, by = c("province" = "translation")) %>%   
                    select(c(listnames_full_table,listnames_zipcode)) # Keep only relevant columns

# Keep the first 4 datasets that had a successful match with value for Postalcode & 5th dataset will contain both matched and NA value for Postalcode
full_table_merge1 = full_table_merge1 %>% filter(!is.na(Postalcode)) 
full_table_merge2 = full_table_merge2 %>% filter(!is.na(Postalcode)) 
full_table_merge3 = full_table_merge3 %>% filter(!is.na(Postalcode))
#     Remove duplicated rows & keep the first match for Postalcode as there is insufficient information to determine which is the correct postal code
#     For example loft_id 308173-04, results dataset has town with "Meldert" and no province information
#     In zipcode dataset, there are 3 Postalcodes with Sub_town "Meldert" in 3 different province
full_table_merge3 = full_table_merge3[!duplicated(full_table_merge3[c(listnames_full_table)]), ] 
full_table_merge4 = full_table_merge4 %>% filter(!is.na(Postalcode)) 

# Merge all 5 datasets together 
full_table_postal = do.call(rbind, mget(ls(pattern="^full_table_merge")))

# Try to find possible match to zipcode for the 201 loft_id that are still missing postalcode 
missing_postal  = full_table_postal %>% 
                  filter(is.na(Postalcode))%>%
                  group_by(loft_id, town, province) %>%
                  distinct(.,loft_id)

missing_postal_1 = missing_postal %>%
                   separate(town, c("town1", "town2","town3"), '-') %>% 
                   separate(town1, c("town1", "town4"), '/') 

missing_postal_1$town1 = trimws(missing_postal_1$town1) # Trim blank space

# Create a list of column names to keep after merging
listnames_missingpostal = colnames(missing_postal_1)

#   Eg. For loft_id 308729-75, initial value "Nonceveux (Aywaille)", 
#   breakdown into town "Nonceveux" and province "Aywaille" which is a town in
missing_postal_merge1 = missing_postal_1 %>% 
                        left_join(.,zipcode, by = c("province" = "Pure_town")) %>%  
                        select(c(listnames_missingpostal,listnames_zipcode)) # Keep only relevant columns

#   Eg. For "-", loft_id 165874-04, initial value "Putte-Beerzel", only required "putte" to get a match.
#   Eg. For "/", loft_id 305085-20, town "Ronse/Renaix" contains both Dutch and French version, only required "Ronse" to get a match.
missing_postal_merge2 = missing_postal_merge1 %>% 
                        filter(is.na(Postalcode)) %>%
                        select(-c(Postalcode)) %>%
                        left_join(.,zipcode, by = c("town1" = "Pure_town")) %>%  
                        select(c(listnames_missingpostal,listnames_zipcode)) # Keep only relevant columns

#   Eg. For "-", loft_id 307902-24, town "Booischot-Pijpelheide, only required "booischot" to get a match.
#   Eg. For "/", loft_id 176755-21, town "Helkijn/Helchin" contains both Dutch and French version, only required "Helkijn" to get a match.
missing_postal_merge3 = missing_postal_merge2 %>% 
                        filter(is.na(Postalcode)) %>%
                        select(-c(Postalcode)) %>%
                        left_join(.,zipcode, by = c("town1" = "Sub_town")) %>%  
                        select(c(listnames_missingpostal,listnames_zipcode)) # Keep only relevant columns

# Keep the datasets that had a successful match with value for Postalcode
missing_postal_merge1 = missing_postal_merge1 %>% filter(!is.na(Postalcode)) 
missing_postal_merge2  = missing_postal_merge2  %>% filter(!is.na(Postalcode)) 
missing_postal_merge3 = missing_postal_merge3 %>% filter(!is.na(Postalcode))

# Merge  datasets together 
additional_postal = do.call(rbind, mget(ls(pattern="^missing_postal_merge"))) 
additional_postal = additional_postal[,6:7]
additional_postal = additional_postal %>%
                    group_by(loft_id) %>%
                    slice(1:1) %>%
                    rename(loft_id = loft_id, addn_postal = Postalcode)

data_all = left_join(full_table_postal,additional_postal,by="loft_id") %>%
           mutate(Postalcode = ifelse(!is.na(Postalcode), Postalcode, addn_postal)) %>%
           select(-addn_postal)

data_all$date = as.Date(data_all$date, "%d-%m-%Y")
           
# Out of 4300 unique loft_id, only 65 loft_id could not be matched with a Postalcode (NA)
# Achieved 98% matching postalcode (4235 loft_id)



#################################################### FIND LOCATION OF LOFT #######################################################

calccoord <- function(x.lat,x.lon,x.dist,y.lat,y.lon,y.dist){
  
  # Put input in radians
  x.lat <- x.lat*pi/180
  x.lon <- x.lon*pi/180
  y.lat <- y.lat*pi/180
  y.lon <- y.lon*pi/180
  
  # Compute coordinates in (x,y,z) relative to center of earth
  coord.x <- c(cos(x.lon)*cos(x.lat),sin(x.lon)*cos(x.lat),sin(x.lat))
  coord.y <- c(cos(y.lon)*cos(y.lat),sin(y.lon)*cos(y.lat),sin(y.lat))
  
  # Compute distances as radians
  #r.x <- pi/180/60*x.dist # radians!
  #r.y <- pi/180/60*y.dist # radians!
  r.x <- x.dist/6.371E6#6.3781E6
  r.y <- y.dist/6.371E6#6.3781E6
  
  # Compute unit vector x0
  q <- coord.x%*%coord.y
  q2 <- q^2
  
  if (q2==1){
    # No solution: circle centers are either the same or antipodal
    return(NULL)
  }
  a <- as.vector((cos(r.x)-cos(r.y)*q)/(1-q2))
  b <- as.vector((cos(r.y)-cos(r.x)*q)/(1-q2))
  
  x0 <- a*coord.x + b*coord.y
  
  # we only have a solution if x0 is within the sphere - if not,
  # the circles are not touching
  x2 <- x0%*%x0
  if (x2>1){
    return(NULL)
  }
  
  # Get the normal vector
  # Cross product (not crossprod function in R!)
  n <- rep(0,3)
  n[1] <- coord.x[2]*coord.y[3] - coord.y[2]*coord.x[3]
  n[2] <- coord.y[1]*coord.x[3] - coord.x[1]*coord.y[3];
  n[3] <- coord.x[1]*coord.y[2] - coord.y[1]*coord.x[2];
  
  ## no solution: circle centers are either the same or antipodal
  n2 <- n%*%n
  if (n2==0){
    return(NULL)
  }
  
  # Find intersections
  t <- as.vector(sqrt((1 - x2)/n2))
  
  # if t > 0, two solutions, else only one
  if (t>0){
    solution <- list()
    solution[[1]] <- x0+t*n
    solution[[2]] <- x0-t*n
    solution[[3]] <- c(asin(solution[[1]][3])*180/pi,
                       atan2(solution[[1]][2],solution[[1]][1])*180/pi)
    solution[[3]][2] <- ((solution[[3]][2]+180)%%360)-180
    solution[[4]] <- c(asin(solution[[2]][3])*180/pi,
                       atan2(solution[[2]][2],solution[[2]][1])*180/pi)
    solution[[4]][2] <- ((solution[[4]][2]+180)%%360)-180
    return(solution)
  }else{
    solution <- list()
    solution[[1]] <- x0+t*n
    solution[[2]] <- c(asin(solution[[1]][3])*180/pi,
                       atan2(solution[[1]][2],solution[[1]][1])*180/pi)
    solution[[2]][2] <- ((solution[[3]][2]+180)%%360)-180
    return(solution)
  }
}


# Select distance, lat and long for loft_id that have participated in 2 or more different race location
coordinate_data = data_all %>% 
                  distinct(.,loft_id,UKey_Race,Max_distance,lat,long) %>% # Keep only 1 row per race for each loft_id
                  group_by(loft_id,Max_distance) %>%
                  distinct(.,loft_id,Max_distance,.keep_all = TRUE) %>% # Keep only 1 row per race location for each loft_id 
                  group_by(loft_id) %>%
                  mutate(race_count = n_distinct(loft_id, UKey_Race)) %>%
                  filter(race_count >= 2) %>%
                  arrange(.,loft_id,Max_distance) %>%   
                  slice(1:3)   # Select up to 3 obs for each loft_id to determine the long and lat using below function

########## PREPARE COORDINATES DATA FOR LOFT ID WITH ONLY 2 DIFFERENT RACE LOCATION
two_race = coordinate_data %>%
           filter(race_count == 2) %>%
           select(-race_count)

two_race_data_xy = reshape(transform(two_race, i = 1:2),
                           idvar = "loft_id", timevar = "i", direction = "wide")

two_race_data_coord = two_race_data_xy %>%
                      rename(loft_id = loft_id, x.lat = lat.1, x.lon = long.1, x.dist = Max_distance.1,
                             y.lat = lat.2, y.lon = long.2, y.dist = Max_distance.2) %>%
                      select(-c(UKey_Race.1,UKey_Race.2))

########## PREPARE COORDINATES DATA FOR LOFT ID WITH MORE THAN 2 DIFFERENT RACE LOCATION BUT WE KEEP ONLY 3 RACE LOCATION
mttwo_race = coordinate_data %>%
             filter(race_count > 2) %>%
             select(-race_count)

# Transposed 3 different race location for each loft id from 3 rows to 1 column
mttwo_race_data = reshape(transform(mttwo_race, i = 1:3),
                             idvar = "loft_id", timevar = "i", direction = "wide")

# Create 3 different x,y coordinates combination from these 3 different race location
mttwo_race_data_xy = mttwo_race_data %>%
                     select(loft_id, lat.1, long.1, Max_distance.1, lat.2, long.2, Max_distance.2) %>%
                     rename(loft_id  = loft_id, x.lat = lat.1, x.lon = long.1, x.dist = Max_distance.1,
                            y.lat = lat.2, y.lon = long.2, y.dist = Max_distance.2)

mttwo_race_data_xz = mttwo_race_data %>%
                     select(loft_id, lat.1, long.1, Max_distance.1, lat.3, long.3, Max_distance.3) %>%
                     rename(loft_id  = loft_id, x.lat = lat.1, x.lon = long.1, x.dist = Max_distance.1,
                            y.lat = lat.3, y.lon = long.3, y.dist = Max_distance.3)

mttwo_race_data_yz = mttwo_race_data %>%
                     select(loft_id, lat.2, long.2, Max_distance.2, lat.3, long.3, Max_distance.3) %>%
                     rename(loft_id  = loft_id, x.lat = lat.2, x.lon = long.2, x.dist = Max_distance.2,
                            y.lat = lat.3, y.lon = long.3, y.dist = Max_distance.3)

# Bind more than 2 different races data together
mttwo_race_data_coord = rbind(mttwo_race_data_xy,mttwo_race_data_xz,mttwo_race_data_yz)

# Bind with 2 races data together
data_coord = rbind(two_race_data_coord,mttwo_race_data_coord)

# Loft_id information for identifying the loft id in which the solution belongs to later
transposed_data_loftid = data_coord %>%
                         select(loft_id)

# Data ready to be applied with calccoord function
transposed_data = data_coord[,-1]

# Apply the calccoord function to multiple list arguments from the transposed_data table
transposed_data_test = mapply(calccoord,
                              x.lat = transposed_data$x.lat,
                              x.lon = transposed_data$x.lon,
                              x.dist = transposed_data$x.dist,
                              y.lat = transposed_data$y.lat,
                              y.lon = transposed_data$y.lon,
                              y.dist = transposed_data$y.dist)


# Extracts the lat and long for each solution stored in sub-element 3 and 4 of each sub list generated from transpoed-data_set
# Since the calccoord function will generate possible longitude and latitude solutions in sub-list 3 and 4
# Using as.numeric & as.character, we coerced the NULLS into NA
# Convert the list to a data frame
transposed_data_test_loc1_lat = data.frame(as.numeric(as.character(sapply(sapply(transposed_data_test, "[[", 3), "[[", 1))))
transposed_data_test_loc1_lon = data.frame(as.numeric(as.character(sapply(sapply(transposed_data_test, "[[", 3), "[[", 2))))
transposed_data_test_loc2_lat = data.frame(as.numeric(as.character(sapply(sapply(transposed_data_test, "[[", 4), "[[", 1))))
transposed_data_test_loc2_lon = data.frame(as.numeric(as.character(sapply(sapply(transposed_data_test, "[[", 4), "[[", 2))))


# Combine all columns together
coordinate_solutions = do.call(cbind, mget(ls(pattern="^transposed_data_test_loc"))) 
names(coordinate_solutions) = c("lat1","lon1","lat2","lon2") # Name the columns



# Loops through each value of lat1 & lon1 to check if the values are within the latitude and longitude ranges of belgium
# if it is within the range, take that as the actual latitude and longitude, 
# else loops through lat2 & lon2 to check if the values are within the latitude and longitude ranges of belgium
# Latitude and Longitude of belgium can be found in http://latitudelongitude.org/be/

select_coord <- function(solutions){
  lat_belgium = c(49.56652, 51.46791)
  lon_begium = c(2.59368, 6.25749)
  for(i in 1:nrow(solutions)) {
    
    if(is.na(solutions$lat1[i])) {
      solutions$lat_final[i] = solutions$lat1[i]
      solutions$lon_final[i] = solutions$lon1[i]
      
    } else if (solutions$lat1[i] >= min(lat_belgium) & 
               solutions$lat1[i] <= max(lat_belgium) &
               solutions$lon1[i] >= min(lon_begium) &
               solutions$lon1[i] <= max(lon_begium)) {
      solutions$lat_final[i] = solutions$lat1[i]
      solutions$lon_final[i] = solutions$lon1[i]
    } else if (solutions$lat2[i] >= min(lat_belgium) & 
               solutions$lat2[i] <= max(lat_belgium) &
               solutions$lon2[i] >= min(lon_begium) &
               solutions$lon2[i] <= max(lon_begium)) {
      solutions$lat_final[i] = solutions$lat2[i]
      solutions$lon_final[i] = solutions$lon2[i]
    } else {
      solutions$lat_final[i] = NA
      solutions$lon_final[i] = NA
    }
  }
  return(solutions)
}

# Apply select_coord function to dataset
coordinate_solutions = select_coord(coordinate_solutions)

# Bind the final lat and lot back to their respective loft ids, cast matrix as dataframe
loft_location = tbl_df(cbind(loft_id = transposed_data_loftid$loft_id, loft_latitude = coordinate_solutions$lat_final, loft_longitude = coordinate_solutions$lon_final))


# Cast latitude and longitude as numeric
loft_location$loft_latitude = as.numeric(loft_location$loft_latitude)
loft_location$loft_longitude = as.numeric(loft_location$loft_longitude)

# In cases where > 1 coordinate solutions fall within the coordinate range of Belgium,
# we need to approximate the actual coordinate of the loft by looking at the closeness of these solutions 
# The actual location of the loft is most likely to fall within the range of coordinate solutions that are close to each other
# We picked the coordinates with the least absolute difference to be the loft’s coordinates.

# For example loft_id 100042-35, given the below 3 solutions, we have calculated the mean.
# the two smallest total_diff (solution1 and solution2) will always be closer to each other than solution3
# and hence the actual coordinate of the loft is likely to fall within the range of solution1 and solution2 
# 
# As it is difficult to get a precise loft coordinate given that the calccoord function has a +/-5 km error,
# we will not be able to know for sure if solution1 or solution2 is better 
# hence we have selected solution1 with the least absolute difference to the mean
# Note: total_diff = lat_mean_diff + lon_mean_diff

# solution1 (51.04911, 3.889879) - total_diff = 0.807498525 (Selected coordinates as it has least absolute difference to the mean)
# solution2 (51.04987, 3.885178) - total_diff = 0.812961488
# solution3 (50.42368, 5.692403) - total_diff = 1.620460013 

loft_location_solution = loft_location %>%
                         arrange(loft_id, loft_latitude, loft_longitude) %>% # sort data by loft_id, latitude and longitude
                         group_by(loft_id) %>%
                         summarise(mean_lat = mean(loft_latitude, na.rm = TRUE), mean_lon = mean(loft_longitude, na.rm = TRUE)) %>% # calculate mean lat and mean lon for each loft id
                         left_join(loft_location, by = "loft_id") %>% # joins back to loft_location to introduce the 2 new vars created
                         mutate(lat_mean_diff = abs(loft_latitude - mean_lat), lon_mean_diff = abs(loft_longitude - mean_lon)) %>% # calculate absolute difference between mean and actual lat/lon values
                         mutate(total_diff = lat_mean_diff + lon_mean_diff) %>% # calculate the total absolute difference for lat and long
                         # the larger the total_diff of a particular solution indicates that it is further away from the other solutions
                         group_by(loft_id) %>%
                         filter(total_diff == min(total_diff,na.rm = TRUE)) %>% # selects the solution with minimum difference to approximate the actual coordinates of the loft
                         select(loft_id, loft_latitude, loft_longitude) %>%
                         slice(1:1) # Since the standard deviation is the same between 2 solutions, choose the first one


########## Deal with loft_id (having participated in 2 or more races) that has no intersection point 

# Purpose: These cases were derived by the function to have no intersection point, 
#          hence we are trying to increase the radius of the smaller circle by a distance error allowance 
#          to increase the chance of intersection between the two circles.
#          We have tried adding 5KM and other values but eventually decided to add only 400M 
#          as it has given us the highest number of lofts' coordinates that are within Belgium.

# Filter out the list of loft_id that have intersection point
loft_intp = loft_location %>%
            filter(!is.na(loft_latitude)) %>%
            distinct(.,loft_id,.KEEP_ALL=FALSE) 

# Prepare data for the loft_id that has no intersection point by adding distance error allowance to the smaller circle (shorter distance)
loft_nointp = anti_join(loft_location,loft_intp, by = "loft_id") %>% # Filter loft_id that have not appeared in loft_intp 
              distinct(.,loft_id,.KEEP_ALL=FALSE) %>% # Keep distinct loft_id
              inner_join(.,data_coord, by = "loft_id") %>% # Merge in the combinations of coordinates %>%
              mutate(x.add = ifelse(x.dist<y.dist, x.dist+400, x.dist)) %>% # Add distance error allowance to x distance if x is a smaller circle (shorter distance) than y
              mutate(y.add = ifelse(y.dist<x.dist, y.dist+400, y.dist)) %>% # Add distance error allowance to y distance if y is a smaller circle (shorter distance)
              mutate(x.dist = ifelse(x.dist==x.add, x.dist, x.add)) %>%  # Replace original x distance with the revised x distance + distance error allowance
              mutate(y.dist = ifelse(y.dist==y.add, y.dist, y.add)) %>%  # Replace original y distance with the revised y distance + distance error allowance
              select(-c(x.add,y.add)) # Drop unnecessary variables

# Apply the calccoord function 
loft_nointp_test = mapply(calccoord,
                          x.lat = loft_nointp$x.lat,
                          x.lon = loft_nointp$x.lon,
                          x.dist = loft_nointp$x.dist,
                          y.lat = loft_nointp$y.lat,
                          y.lon = loft_nointp$y.lon,
                          y.dist = loft_nointp$y.dist, SIMPLIFY = FALSE)

# Extracts the lat and long for each solution stored in sub-element 3 and 4 of each sub list generated from transpoed-data_set
# Since the calccoord function will generate possible longitude and latitude solutions in sub-list 3 and 4
# Using as.numeric & as.character, we coerced the NULLS into NA
# Convert the list to a data frame
loft_nointp_test_loc1_lat = data.frame(as.numeric(as.character(sapply(sapply(loft_nointp_test, "[[", 3,simplify = FALSE), "[[", 1,simplify = FALSE))))
loft_nointp_test_loc1_lon = data.frame(as.numeric(as.character(sapply(sapply(loft_nointp_test, "[[", 3,simplify = FALSE), "[[", 2,simplify = FALSE))))
loft_nointp_test_loc2_lat = data.frame(as.numeric(as.character(sapply(sapply(loft_nointp_test, "[[", 4,simplify = FALSE), "[[", 1,simplify = FALSE))))
loft_nointp_test_loc2_lon = data.frame(as.numeric(as.character(sapply(sapply(loft_nointp_test, "[[", 4,simplify = FALSE), "[[", 2,simplify = FALSE))))

# Combine all columns together
loft_nointp_coordsoln = do.call(cbind, mget(ls(pattern="^loft_nointp_test_loc")))
names(loft_nointp_coordsoln) = c("lat1","lon1","lat2","lon2")

# Loops through each value of lat1 & lon1 to check if the values are within the latitude and longitude ranges of belgium
# if it is within the range, take that as the actual latitude and longitude, 
# else loops through lat2 & lon2 to check if the values are within the latitude and longitude ranges of belgium

# Apply select_coord function to dataset
loft_nointp_coordsoln = select_coord(loft_nointp_coordsoln)


# Bind the final lat and lot back to their respective loft ids
loft_nointp_location = data.frame(cbind(loft_id = loft_nointp$loft_id, 
                                        loft_latitude = loft_nointp_coordsoln$lat_final, 
                                        loft_longitude = loft_nointp_coordsoln$lon_final))

loft_nointp_location$loft_id = as.character(loft_nointp_location$loft_id)
loft_nointp_location$loft_latitude = as.numeric(as.character(loft_nointp_location$loft_latitude))
loft_nointp_location$loft_longitude = as.numeric(as.character(loft_nointp_location$loft_longitude))

loft_nointp_location_solution = loft_nointp_location %>%
                                arrange(loft_id, loft_latitude, loft_longitude) %>% # sort data by loft_id, latitude and longitude
                                group_by(loft_id) %>%
                                summarise(mean_lat = mean(loft_latitude, na.rm = TRUE), mean_lon = mean(loft_longitude, na.rm = TRUE)) %>% # calculate mean lat and mean lon for each loft id
                                left_join(loft_nointp_location, by = "loft_id") %>% # joins back to loft_nointp_location to introduce the 2 new vars created
                                mutate(lat_mean_diff = abs(loft_latitude - mean_lat), lon_mean_diff = abs(loft_longitude - mean_lon)) %>% # calculate absolute difference between mean and actual lat/lon values
                                mutate(total_diff = lat_mean_diff + lon_mean_diff) %>% # calculate the total absolute difference for lat and long
                                # the larger the total_diff of a particular solution indicates that it is further away from the other solutions
                                group_by(loft_id) %>%
                                filter(total_diff == min(total_diff,na.rm = TRUE)) %>% # selects the solution with minimum difference to approximate the actual coordinates of the loft
                                select(loft_id, loft_latitude, loft_longitude) %>%
                                slice(1:1) # Since the standard deviation is the same between 2 solutions, choose the first one

# Final location of lofts that have been derived using calccoord function
loft_final_location = rbind(loft_location_solution,loft_nointp_location_solution)

########################################### CREATE NEW VARIABLES #################################################################

# Cast variables as numeric when relevant
sapply(data_all, class) # Check if there are any variables that need to be changed to numeric

cols.num <- c("position","basket","basket_nr") # Specify the variables into a vector
data_all[cols.num] = sapply(data_all[cols.num],as.numeric) # Cast them as numeric
sapply(data_all, class) # Check that all the necessary variables have been changed to numeric

# Variable 1: Average velocity per loft
avg_velocity_data_all = data_all %>% 
                        group_by(loft_id)%>%
                        summarise(v1_avg_velocity = mean(velocity))

# Variable 2: Maximum velocity per loft
max_velocity_data_all = data_all %>% 
                        group_by(loft_id) %>%
                        summarise(v2_max_velocity = max(velocity))

# Variable 3: Number of pigeons per loft that have arrived in top 25% 
number_pigeon_data_all = data_all %>% 
                         group_by(loft_id) %>%
                         summarise(v3_number_pigeon = n_distinct(ring))

# Variable 4: Among all the pigeons each loft has, find the maximum number of races a pigeon has participated in 
max_races_data_all = data_all %>% 
                     group_by(loft_id, ring) %>%
                     summarise(total_races = n()) %>% # Count the number of races each bird has participated 
                     select(loft_id, total_races) %>%
                     group_by(loft_id) %>%
                     slice(which.max(total_races)) %>% # Return only the maximum number of races among all the birds in each loft
                     rename(v4_max_race = total_races)

# Variable 5: Best position the pigeon has achieved for each loft among all races participated
best_position_data_all = data_all %>% 
                         group_by(loft_id) %>%
                         summarise(v5_best_position = min(position, na.rm = TRUE))

# Variable 6: Average position for each loft among all races participated 
avg_position_data_all = data_all %>% 
                        group_by(loft_id) %>%
                        summarise(v6_avg_position = round(mean(position, na.rm = TRUE)))

# Variable 7: Average basket size 
avg_basket_data_all = data_all %>% 
                      group_by(loft_id) %>%
                      summarise(v7_avg_basket = round(mean(basket, na.rm = TRUE)))

# Variable 8: Total number of pigeons participated in all races per loft
#     sum of all baskets (all pigeons that participated, even if they didn't arrive)
sum_basket_data_all = data_all %>% 
                      group_by(loft_id) %>%
                      summarise(v8_sum_basket = sum(basket, na.rm = TRUE)) 

# Variable 9: Total number of race each loft has participated
race_count_data_all = data_all %>% 
                      group_by(loft_id) %>%
                      summarise(v9_race_count = n_distinct(UKey_Race))

# Variable 10: Average distance of all races each loft has participated in
avg_distance_data_all = data_all %>% 
                        group_by(loft_id) %>%
                        distinct(loft_id,UKey_Race,.keep_all = TRUE) %>% # Keep distinct rows for each race per loft to prevent bias calculation towards race with more than 1 birds arriving in top 25%
                        summarise(v10_avg_distance = round(mean(Max_distance, na.rm = TRUE)))

# Variable 11: Accuracy divided by number of pigeons in top 25%. 
#   Accuracy: Assigned value of 1 if basket_nr = actual arriving position and 0 if differs. Sum the values for each loft_id. 
accuracy_percent_data_all = data_all %>% 
                               group_by(loft_id) %>%
                               mutate(right_position = ifelse(position_within_loft_race == basket_nr, 1, 0)) %>% # If ranking in basket_nr equals to actual arriving position within loft per race, assign 1 else 0
                               summarise(v11_accuracy_percent = round(sum(right_position, na.rm = TRUE)/n(),2)) # Denominator is total number of birds in given dataset for each loft for all races

# Variable 12: Percentage of birds arrived in top 25% compared to number per loft
number_top_25_data_all = data_all %>% 
                         group_by(loft_id) %>%
                         summarise(v12_number_top_25 = round(sum(!is.na(position))/ sum(basket, na.rm = TRUE),2)) # Don't count those rows with NA values in positions as they are not top 25%

# Variable 13: Best pigeon in each loft that has lowest average position
# Variable 14: Average position of the best pigeon in each loft
best_pigeon_data_all = data_all %>% 
                     group_by(loft_id,ring) %>%
                     summarise(v14_avg_best_pigeon_position = round(mean(position, na.rm = TRUE))) %>%
                     group_by(loft_id) %>%
                     slice(which.min(v14_avg_best_pigeon_position)) %>%
                     rename(v13_best_pigeon = ring)

# Variable 15: Measures standard error between owner’s prediction (basket_nr) and their actual arriving position. 
std_error_data_all = data_all %>% 
                     mutate(delta_sq = (position_within_loft_race - basket_nr)**2) %>% 
                     group_by(loft_id) %>% 
                     summarise(v15_std_error_pred = round((sum(delta_sq, na.rm = TRUE)/n())**0.5,2))


# Variable 16: Most consistent pigeon in terms of its velocity
#   It has the least standard deviation in term of its velocity across all races participated in each loft. 
#   filter out pigeons that has raced more > 1 since velocity consistency
#   is only relevant when the pigeon has raced > 1  
pigeons_multi_race = data_all %>% 
                     group_by(ring) %>%
                     mutate(total_races = n()) %>%
                     filter(total_races>1)

#   Calculates consistency in terms of standard deviation of pigeon velocity   
most_consistent_pigeon_data_all = data_all %>% 
                                  filter(ring %in% pigeons_multi_race$ring) %>%
                                  group_by(loft_id, ring) %>%
                                  summarise(stdev_velocity = sd(velocity)) %>%
                                  filter(stdev_velocity == min(stdev_velocity)) %>%
                                  group_by(loft_id, ring) %>%
                                  rename(v16_most_consistent_pigeon = ring) %>%
                                  select(-c(stdev_velocity))
#       Note: Results only available for 2773 lofts out of 4300 lofts. 986 lofts took part in
#       only 1 race while 541 lofts had different pigeons taking part in different races.
#       Hence it does not make sense to measure consistency of each pigeon in these 1527 (986+541) lofts


# Variable 17: Most inconsistent pigeon in terms of its velocity
#   It has the highest standard deviation in term of its velocity across all races participated in each loft. 
most_inconsistent_pigeon_data_all = data_all %>% 
                                    filter(ring %in% pigeons_multi_race$ring) %>%
                                    group_by(loft_id, ring) %>%
                                    summarise(stdev_velocity = sd(velocity)) %>%
                                    filter(stdev_velocity == max(stdev_velocity)) %>%
                                    group_by(loft_id, ring) %>%
                                    rename(v17_most_inconsistent_pigeon = ring) %>%
                                    select(-c(stdev_velocity))
#Note: Results only available for 2773 lofts out of 4300 lofts. 986 lofts took part in
#only 1 race while 541 lofts had different pigeons taking part in different races.
# Hence it does not make sense to measure consistency of each pigeon in these 1527 (986+541) lofts 


# Variable 18: Variability of all pigeon performance (velocity) for each loft across all races 
sd_velocity_data_all = data_all %>% 
                       group_by(loft_id)%>%
                       summarise(v18_sd_velocity = sd(velocity))

# Variable 19: Average number of days between races for each loft
avg_race_interval_data_all = data_all %>% 
                             group_by(loft_id) %>%
                             distinct(loft_id, UKey_Race, date = as.Date(date, "%d-%m-%Y")) %>%
                             arrange(loft_id, date, UKey_Race) %>%
                             group_by(loft_id) %>%
                             mutate(race_interval = c(NA,diff(date))) %>% #calculates the race interval for each loft, setting the interval for the first race of each loft as NA
                             summarise(v19_daysbetweenraces = round(mean(race_interval, na.rm = TRUE)))
#NOTE: There are 986 NaNs for the mean_race_interval as these lofts only participated in 1 race


# Variable 20: Average race interval (number of days) for each pigeon within each loft
avg_race_interval_pigeon_data_all = data_all %>% 
                                    group_by(loft_id, ring) %>%
                                    distinct(ring, UKey_Race, date = as.Date(date, "%d-%m-%Y")) %>%
                                    arrange(ring, date, UKey_Race) %>%
                                    group_by(loft_id, ring) %>%
                                    mutate(race_interval = c(NA,diff(date))) %>% #calculates the race interval for each pigeon, setting the interval for the first race of each loft as NA
                                    group_by(loft_id) %>% #aggregates the data by loft_id
                                    summarise(v20_avg_raceinterval_pigeon = round(mean(race_interval, na.rm = TRUE))) #calculates the mean race interval
                                
                          


########################################### MERGING VARIABLES TO EACH LOFT ########################################################

# Obtain unique loft_id and keep only relevant information 
data_loft = data_all %>% 
            group_by(loft_id, loft, town, province, Postalcode) %>%
            distinct(loft_id)

# Merge variables to each loft
data_aggregate = data_loft %>%
                 left_join(loft_final_location                , by = "loft_id") %>%
                 left_join(avg_velocity_data_all              , by = "loft_id") %>%
                 left_join(max_velocity_data_all              , by = "loft_id") %>%
                 left_join(number_pigeon_data_all             , by = "loft_id") %>%
                 left_join(max_races_data_all                 , by = "loft_id") %>%
                 left_join(best_position_data_all             , by = "loft_id") %>%
                 left_join(avg_position_data_all              , by = "loft_id") %>%
                 left_join(avg_basket_data_all                , by = "loft_id") %>%
                 left_join(sum_basket_data_all                , by = "loft_id") %>%
                 left_join(race_count_data_all                , by = "loft_id") %>%
                 left_join(avg_distance_data_all              , by = "loft_id") %>%
                 left_join(accuracy_percent_data_all          , by = "loft_id") %>%
                 left_join(number_top_25_data_all             , by = "loft_id") %>%
                 left_join(best_pigeon_data_all               , by = "loft_id") %>%
                 left_join(std_error_data_all                 , by = "loft_id") %>%
                 left_join(most_consistent_pigeon_data_all    , by = "loft_id") %>%
                 left_join(most_inconsistent_pigeon_data_all  , by = "loft_id") %>%
                 left_join(sd_velocity_data_all               , by = "loft_id") %>%
                 left_join(avg_race_interval_data_all         , by = "loft_id") %>%
                 left_join(avg_race_interval_pigeon_data_all  , by = "loft_id") %>%
                 select(loft_id, loft_latitude, loft_longitude, everything())

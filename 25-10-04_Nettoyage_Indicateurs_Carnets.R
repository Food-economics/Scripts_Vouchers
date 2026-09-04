# LOADING THE WORKING ENVIRONMENT  --------------
## Importing packages -------------------
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx); library(readxl);library(dplyr);
library(broom);library(scales);library(modelsummary);library(ggplot2);library(effsize);library(lfe);
library(ggpubr);library(vtable);library;library("openxlsx");
library("dplyr"); library("tidyr");library("ggplot2");library("gridExtra");library(lubridate);
library("RColorBrewer");library(reshape2);library(Metrics);library(questionr);library(zoo)

### Enter the campaign date ---------------------
campaign<-"23-11" #"22-11" #23-02 #"23-11" #"24-03" 
Nj <- 28 #"Number of entry days: 28 if 23-11/24-03 #29 otherwise 

### Importing Nov_2022 data ------------------
Carnet_nov_22 <- read.xlsx("22-11_Carnets.xlsx")
Metadata_nov_22 <- read_excel("FFQ_Tableaux_nov_22.xlsx", sheet = "Metadata")
### Importing Mars_2023 data ------------------
Carnet_mars_23 <- read_excel("23-02_Carnets.xlsx")
Metadata_mars_23 <- read_excel("FFQ_Tableaux_mars_23.xlsx", sheet = "Metadata")
Metadata_mars_23 <- Metadata_mars_23 %>%
  mutate(Identifiant = gsub("-CCAS \\(inclus Pôle emploi et SPF\\)", "-CCAS", Identifiant))

### Importing Nov_2023 data -------------------
Carnet_nov_23 <- read_excel("23-11_Carnets.xlsx")
Metadata_nov_23 <- read_excel("FFQ_Tableaux_nov_23.xlsx", sheet = "Metadata")

### Importing Mars_2024 data --------------------
Saisie_a <- read_excel("24-03_Carnets_a.xlsx")
Saisie_b <- read_excel("24-03_Carnets_b.xlsx")
Carnet_mars_24 <- full_join(Saisie_a,Saisie_b,by=c("Code","TicketCode","Lieu","Date","CodeCIQUAL","LibelleCIQUAL","Categorie1","Categorie2",
                                                   "Nb","Unite","Prix","Appreciation","Labels","Menu","PrixMenu","LibelleCustom","MontantChequeAlimentaire",
                                                   "DateSaisie","DateMAJ","Photo"))
Metadata_mars_24 <- read_excel("FFQ_Tableaux_mars_24.xlsx", sheet = "Metadata")

### Importing the appendix tables data ----------------
CALNUT<- read_excel("Alim_CALNUT_CODAPPRO_CARNET.xlsx")
magasins <- read_excel("Reclassement_magasins.xlsx")
RHD_COL <- read_excel("RHD_COL.xlsx")
RHD_COM <- read_excel("RHD_COM.xlsx") 
Correction_lieu_nov_22 <- read_excel("22_11_correction_lieu.xlsx")
Correction_date_nov_22 <- read_excel("22-11_Correction_date.xlsx")
Correction_lieu_mars_23 <- read_excel("23_02_correction_lieu.xlsx")
Correction_date_mars_23 <- read_excel("23-02_Correction_date.xlsx")
Correction_lieu_nov_23 <- read_excel("23_11_correction_lieu.xlsx")
Correction_date_nov_23 <- read_excel("23-11_Correction-date.xlsx")
Correction_date_mars_24 <- read_excel("24-03_Correction-date.xlsx")
Correction_lieu_mars_24 <- read_excel("24_03_correction_lieu.xlsx")
Reclassement_Libelle_Custom <- read_excel("Reclassement_Libelle_Custom.xlsx")
Reclassement_Groupe_TI <- read_excel("Reclassement_groupe_TI.xlsx")
resultats_pondérés <- read_excel("moyennes_pondérées.xlsx")
Poids_unitaires_manquants <- read_excel("poids_unitaire_manquants.xlsx")
Recap_envoi_cheques <- read_excel("Recap_envoi_cheque.xlsx")
resultats_pondérés <- read_excel("resultats_pondérés.xlsx")

# CLEANING THE COD_ACHATS FILE: LOCATIONS / DATES / LIBELLE_CUSTOM / LIBELLE_CIQUAL-----------------
#Selecting the datasets and assigning them to the resultats_codachats and metadonnees variables based on the value of the campaign variable.
if (campaign == "22-11") { 
  resultats_codachats <- Carnet_nov_22
  metadata <- Metadata_nov_22 } else { 
    if (campaign == "23-02") { 
      resultats_codachats <- Carnet_mars_23
      metadata <- Metadata_mars_23 
    } else { 
      if (campaign == "23-11") { 
        resultats_codachats <- Carnet_nov_23
        metadata <- Metadata_nov_23 
      } else {  
        if (campaign == "24-03")
          resultats_codachats <- Carnet_mars_24
        metadata <- Metadata_mars_24 }}}


## Correcting the identifiers ---------------
names(resultats_codachats)[1] = "Identifiant"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="8447-CCAS") ] = "8747-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="PE19-CCAS") ] = "PE019-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="1564-Epimut") ] = "1654-Epimut"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="P E013-CCAS") ] = "PE013-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="SP032-CCAS") ] = "SP040-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="SP-052-CCAS") ] = "SP052-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="SP-017-CCAS") ] = "SP017-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="pe003-CCAS") ] = "PE003-CCAS"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="PS004") ] = "LE255"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="PS197") ] = "PS161"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="LE148") ] = "PS284"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="LE195") ] = "PS285"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="LE088") ] = "PS286"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="LE207") ] = "PS287"
resultats_codachats$Identifiant[ (resultats_codachats$Identifiant=="LE093") ] = "PS288"

print(unique(resultats_codachats$Identifiant))
## Extracting the household composition and the number of CU TI from the Nov 22 metadata to March 23  -----------------
#For the 24-03 campaign, the questions about family composition were not asked again, so the number of CU is not included in
#metadata_mars_23: for each identifier, the information is extracted from the November 2022 campaign and merged into the 2023 metadata file.
if (campaign == "24-03") {
  Ajout <- Metadata_nov_23[, c("Identifiant", "UC_TI", "Combien.de.personnes.vivent.dans.votre.foyer")]
  metadata <- metadata[, !names(metadata) %in% c("UC_TI", "Combien.de.personnes.vivent.dans.votre.foyer" )]
  metadata <- inner_join(Ajout, metadata,  by = "Identifiant")}

#if (campaign == "23-02") {
#  Ajout <- Metadata_nov_22[, c("Identifiant", "UC_TI", "Combien.de.personnes.vivent.dans.votre.foyer")]
#  metadata <- metadata[, !names(metadata) %in% c("UC_TI", "Combien.de.personnes.vivent.dans.votre.foyer" )]
#  metadata <- inner_join(Ajout, metadata,  by = "Identifiant")}

#Joining the amounts of vouchers sent to metadata
metadata <- left_join (metadata, Recap_envoi_cheques, by="Identifiant")

## Adding the CU and household size variables to Resultats_codachats ------------
#The identifier, the entry date, the household size, and the Combien.de.personnes.vivent.dans.votre.foyer count are extracted from metadata
#A join is then performed with resultats_codachats. 
temp <- metadata[, c("Identifiant", "Date.de.saisie", "UC_TI", "Combien.de.personnes.vivent.dans.votre.foyer", "Budget.mensuel.alimentation.", "Budget.hebdomadaire.alimentation.")]
temp$budget_alim <- ifelse(is.na(temp$Budget.mensuel.alimentation.), (4*as.numeric(temp$Budget.hebdomadaire.alimentation.)), (temp$Budget.mensuel.alimentation.))
lignes_vide <- temp[is.na(temp $Date.de.saisie), ]


#Here, we disregard empty entries for which a response was expected but nothing was recorded: e.g. November 2023 / March 2024. 
#Check to see whether the linking with the identifiers works correctly
#resultats_codachats <- resultats_codachats %>%
#  anti_join(temp,resultats_codachats , by = c("Identifiant"))
# Find the identifiers present in metadata but not in resultats_codachats
#identifiants_seulement_metadata <- setdiff(metadata$Identifiant, resultats_codachats$Identifiant)
#print("Identifiants présents seulement dans metadata:")
#print(identifiants_seulement_metadata)
# Find the identifiers present in resultats_codachats but not in metadata
#identifiants_seulement_resultats_codachats <- setdiff(resultats_codachats$Identifiant, metadata$Identifiant)
#print("Identifiants présents seulement dans resultats_codachats:")
#print(identifiants_seulement_resultats_codachats)
#print(unique(valeurs_non_jointes$Identifiant))
resultats_codachats <- left_join(resultats_codachats,temp, by = "Identifiant") 
print(unique(resultats_codachats$Identifiant))





lignes_vide <- resultats_codachats[is.na(resultats_codachats$Date.de.saisie), ]
unique(lignes_vide$Identifiant)
## Correcting the locations --------------
### Joining resultats_codachats with the corrected sourcing locations from the tables reviewed by Pascal -----------------------------
# This step performs a join between the precise sourcing-location correction tables and the locations
# recorded in resultats_codachats. 
#the code modifies and merges the resultats_codachats data with different correction sources
#depending on the campaign, while standardizing the date format and assigning values to a new Lieu_vf column.
if (campaign == "22-11") {
  resultats_codachats$Date <- gsub("/", "-", resultats_codachats$Date)
  resultats_codachats$Date <- as.Date(resultats_codachats$Date, format = "%Y-%m-%d")
  #There are two date formats in the correction file. One of the dates is given as a character string
  #When converting to an R date, the month and day get swapped. The following steps correct this so the merge works
  Correction_lieu_nov_22$Date1 <-  as.Date(as.numeric(Correction_lieu_nov_22$Date), origin = "1899-12-30")
  #Converting the dates to character strings
  dates_char <- format(Correction_lieu_nov_22$Date1, "%Y-%d-%m")
  #Splitting the character strings into components
  date_parts <- strsplit(dates_char, "-")
  #Reassembling the components in the right order
  corrected_dates_char <- sapply(date_parts, function(x) {
    paste(x[1], x[2], x[3], sep = "-")
  })
  #Converting the new character strings into Date objects
  Correction_lieu_nov_22$Date1 <- as.Date(corrected_dates_char, format = "%Y-%m-%d")
  Correction_lieu_nov_22$Date2 <- as.Date(Correction_lieu_nov_22$Date, format = "%m/%d/%Y")
  Correction_lieu_nov_22$Date <-  coalesce(Correction_lieu_nov_22$Date1, Correction_lieu_nov_22$Date2)
  Correction_lieu_nov_22 <- Correction_lieu_nov_22[, !names(Correction_lieu_nov_22) %in% c("Date1", "Date2")]
  #Removing extra whitespace
  resultats_codachats$Lieu <- trimws(resultats_codachats$Lieu)
  Correction_lieu_nov_22$Lieu <- trimws(Correction_lieu_nov_22$Lieu)
  #Checking for case differences
  resultats_codachats$Lieu <- tolower(resultats_codachats$Lieu)
  Correction_lieu_nov_22$Lieu <- tolower(Correction_lieu_nov_22$Lieu)
  #Removing invisible characters
  resultats_codachats$Lieu <- iconv(resultats_codachats$Lieu, to = "ASCII//TRANSLIT")
  Correction_lieu_nov_22$Lieu <- iconv(Correction_lieu_nov_22$Lieu, to = "ASCII//TRANSLIT")
  #Check that the correction values are applied to the table 
  valeurs_non_jointes <- Correction_lieu_nov_22 %>%
    anti_join(resultats_codachats, by = c("Lieu", "Date","Identifiant"))
  print(valeurs_non_jointes)
  resultats_codachats <- left_join(resultats_codachats, Correction_lieu_nov_22, by=c("Lieu", "Date","Identifiant"))
  resultats_codachats$Lieu_vf <- coalesce(resultats_codachats$Lieu_cor, resultats_codachats$Lieu)
  #Check that the correction values are indeed found in lieu cor: if so: NA 
  temp<- setdiff(Correction_lieu_nov_22$Lieu_cor, resultats_codachats$Lieu_vf)
  print(unique((resultats_codachats$Lieu_cor)))
}else{ 
  if (campaign == "23-02") {
    resultats_codachats$Date <- gsub("/", "-", resultats_codachats$Date)
    resultats_codachats$Date <- as.Date(resultats_codachats$Date, format = "%Y-%m-%d")
    Correction_lieu_mars_23$Date1 <-  as.Date(as.numeric(Correction_lieu_mars_23$Date), origin = "1899-12-30")
    dates_char <- format(Correction_lieu_mars_23$Date1, "%Y-%d-%m")
    date_parts <- strsplit(dates_char, "-")
    corrected_dates_char <- sapply(date_parts, function(x) {
      paste(x[1], x[2], x[3], sep = "-")
    })
    Correction_lieu_mars_23$Date1 <- as.Date(corrected_dates_char, format = "%Y-%m-%d")
    Correction_lieu_mars_23$Date2 <- as.Date(Correction_lieu_mars_23$Date, format = "%m/%d/%Y")
    Correction_lieu_mars_23$Date <-  coalesce(Correction_lieu_mars_23$Date1, Correction_lieu_mars_23$Date2)
    Correction_lieu_mars_23 <- Correction_lieu_mars_23[, !names(Correction_lieu_mars_23) %in% c("Date1", "Date2")]
    #Removing extra whitespace
    resultats_codachats$Lieu <- trimws(resultats_codachats$Lieu)
    Correction_lieu_mars_23$Lieu <- trimws(Correction_lieu_mars_23$Lieu)
    #Checking for case differences
    resultats_codachats$Lieu <- tolower(resultats_codachats$Lieu)
    Correction_lieu_mars_23$Lieu <- tolower(Correction_lieu_mars_23$Lieu)
    #Removing invisible characters
    resultats_codachats$Lieu <- iconv(resultats_codachats$Lieu, to = "ASCII//TRANSLIT")
    Correction_lieu_mars_23$Lieu <- iconv(Correction_lieu_mars_23$Lieu, to = "ASCII//TRANSLIT")
    #Check that the correction values are applied to the table 
    valeurs_non_jointes <- Correction_lieu_mars_23 %>%
      anti_join(resultats_codachats, by = c("Lieu", "Date","Identifiant"))
    print(valeurs_non_jointes)
    resultats_codachats <- left_join(resultats_codachats, Correction_lieu_mars_23, by=c("Lieu", "Date","Identifiant"))
    resultats_codachats$Lieu_vf <- coalesce(resultats_codachats$Lieu_cor, resultats_codachats$Lieu)
    #Check that the correction values are indeed found in lieu cor: if so: NA 
    temp<- setdiff(Correction_lieu_mars_23$Lieu_cor, resultats_codachats$Lieu_vf)
    print(temp)
    print(unique((resultats_codachats$Lieu_cor)))
  } else {
    if (campaign == "23-11") {
      resultats_codachats$Date <- gsub("/", "-", resultats_codachats$Date)
      resultats_codachats$Date <- as.Date(resultats_codachats$Date , format="%Y-%m-%d")
      Correction_lieu_nov_23$Date <- as.Date(Correction_lieu_nov_23$Date, format="%Y-%m-%d")
      #Removing extra whitespace
      resultats_codachats$Lieu <- trimws(resultats_codachats$Lieu)
      Correction_lieu_nov_23$Lieu <- trimws(Correction_lieu_nov_23$Lieu)
      #Checking for case differences
      resultats_codachats$Lieu <- tolower(resultats_codachats$Lieu)
      Correction_lieu_nov_23$Lieu <- tolower(Correction_lieu_nov_23$Lieu)
      #Removing invisible characters
      resultats_codachats$Lieu <- iconv(resultats_codachats$Lieu, to = "ASCII//TRANSLIT")
      Correction_lieu_nov_23$Lieu <- iconv(Correction_lieu_nov_23$Lieu, to = "ASCII//TRANSLIT")
      #Check that the correction values are applied to the table 
      valeurs_non_jointes <- Correction_lieu_nov_23 %>%
        anti_join(resultats_codachats,  Correction_lieu_nov_23,by = c("Lieu", "Date","Identifiant"))
      print(valeurs_non_jointes)
      resultats_codachats <- left_join(resultats_codachats, Correction_lieu_nov_23, by=c("Lieu", "Date","Identifiant"))
      resultats_codachats$Lieu_vf <- coalesce(resultats_codachats$Lieu_cor, resultats_codachats$Lieu)
      #Check that the correction values are indeed found in lieu cor: if so: NA 
      temp<- setdiff(Correction_lieu_nov_23$Lieu_cor, resultats_codachats$Lieu_vf)
      print(temp)
      print(unique((resultats_codachats$Lieu_cor)))
    } else {
      if (campaign == "24-03") {
        resultats_codachats$Date <- gsub("/", "-", resultats_codachats$Date)
        resultats_codachats$Date <- as.Date(resultats_codachats$Date , format="%Y-%m-%d")
        Correction_lieu_mars_24$Date <- as.Date(Correction_lieu_mars_24$Date, format="%Y-%m-%d")
        #Removing extra whitespace
        resultats_codachats$Lieu <- trimws(resultats_codachats$Lieu)
        Correction_lieu_mars_24$Lieu <- trimws(Correction_lieu_mars_24$Lieu)
        #Checking for case differences
        resultats_codachats$Lieu <- tolower(resultats_codachats$Lieu)
        Correction_lieu_mars_24$Lieu <- tolower(Correction_lieu_mars_24$Lieu)
        #Removing invisible characters
        resultats_codachats$Lieu <- iconv(resultats_codachats$Lieu, to = "ASCII//TRANSLIT")
        Correction_lieu_mars_24$Lieu <- iconv(Correction_lieu_mars_24$Lieu, to = "ASCII//TRANSLIT")
        #Check that the correction values are applied to the table 
        valeurs_non_jointes <- Correction_lieu_mars_24 %>%
          anti_join(resultats_codachats,  Correction_lieu_mars_24,by = c("Lieu", "Date","Identifiant"))
        print(valeurs_non_jointes)
        resultats_codachats <- left_join(resultats_codachats, Correction_lieu_mars_24, by=c("Lieu", "Date","Identifiant"))
        resultats_codachats$Lieu_vf <- coalesce(resultats_codachats$Lieu_cor, resultats_codachats$Lieu)
        #Check that the correction values are indeed found in lieu cor: if so: NA 
        temp<- setdiff(Correction_lieu_mars_24$Lieu_cor, resultats_codachats$Lieu_vf)
        print(temp)
        print(unique((resultats_codachats$Lieu_cor)))
      }}}}

#print(unique(resultats_codachats$Identifiant))

### Joining the sourcing locations and the classification by store type "Lieu 1" / "Lieu2"----------
#Removing extra whitespace
resultats_codachats$Lieu_vf <- trimws(resultats_codachats$Lieu_vf)
magasins$Lieu_vf <- trimws(magasins$Lieu_vf)
#Checking for case differences
resultats_codachats$Lieu_vf <- tolower(resultats_codachats$Lieu_vf)
magasins$Lieu_vf <- tolower(magasins$Lieu_vf)
#Removing invisible characters
resultats_codachats$Lieu_vf<- iconv(resultats_codachats$Lieu_vf, to = "ASCII//TRANSLIT")
magasins$Lieu_vf<- iconv(magasins$Lieu_vf, to = "ASCII//TRANSLIT")
#valeurs_non_jointes <- resultats_codachats  %>%
#  anti_join(magasins, by = c("Lieu_vf"))

resultats_codachats <- inner_join(resultats_codachats, magasins, by=c("Lieu_vf")) 
describe(is.na(resultats_codachats$Lieu2))

#Removing unnecessary columns
if (campaign == "22-11" |campaign == "23-02"|campaign == "23-11") {
  resultats_codachats <- subset(resultats_codachats, select = -c(Photo, Nom_photo, Lieu_cor, Observation))}

## Correcting the dates ---------------------------------
### Initializing the start and end date bounds 
#the code adjusts the start and end dates of the periods in resultats_codachats
#based on the campaign, after converting the entry date into date format. The periods differ slightly depending on the campaign specified.
# For the 22-23 campaign: people started entering data on the day they received the supply booklet.
# For the 23-24 campaign: people started entering data the day after they answered the FFQ.
#For the 22-23 campaign, entries span 29 days, and 28 days for 23-24.

# Converting the dates into the correct format
formats <- c("%d/%m/%Y", "%Y-%m-%d", "%m/%d/%Y")
resultats_codachats$Date <- parse_date_time(resultats_codachats$Date, orders = formats)
resultats_codachats$Date <- as.Date(resultats_codachats$Date, format = "%Y-%m-%d")

#Defining the start dates 
resultats_codachats$date_starting <- as.Date(resultats_codachats$Date.de.saisie, format = "%d/%m/%Y")
if (campaign == "22-11" |campaign == "23-02") {
  #29 days of entries for the 22-23 campaign
  resultats_codachats$Date_début <- resultats_codachats$date_starting 
  resultats_codachats$Date_fin <- resultats_codachats$date_starting +28
} else if (campaign == "23-11" | campaign == "24-03") {
  #28 days of entries for the 23-24 campaign
  resultats_codachats$Date_début <- resultats_codachats$date_starting+1
  resultats_codachats$Date_fin <- resultats_codachats$date_starting +28}
lignes_vide <- resultats_codachats[is.na(resultats_codachats$Date_début), ]


### Removing data entered under the wrong link for November 22 and November 23---------------------
if (campaign == "22-11") {
  start_date <- as.Date('2023-09-12')
  end_date <- as.Date('2023-10-25')
  donnees_a_supprimer <- resultats_codachats %>%
    filter(DateSaisie >= start_date & DateSaisie <= end_date)
  #3580 observations to remove: 24399 observations should remain
  resultats_codachats <- resultats_codachats %>%
    filter(is.na(DateSaisie) | !(DateSaisie >= start_date & DateSaisie <= end_date))}

if (campaign == "23-11") {
  start_date <- as.Date('2024-03-01')
  end_date <- as.Date('2024-03-31')
  # Remove the March dates: 31190 observations should remain
  dates_mars <- resultats_codachats %>%
    filter(Date >= start_date & Date < end_date)
  resultats_codachats <- resultats_codachats %>%
    filter(is.na(Date) | Date < start_date | Date > end_date) } 

### Adjusting the end-of-entry dates for people who went on vacation-----------------
resultats_codachats <- resultats_codachats %>%
  mutate(Date_fin = case_when(
    campaign == "24-03" & Identifiant %in% c("LE041", "LE043", "PS192", "PS194", "PS208", "PS229", "PS244") ~ date_starting + 36, #29 days + Adding 7 days 
    campaign == "24-03" & Identifiant == "PS267" ~ date_starting + 43, #Adding two weeks: 29 days + Adding 14 days
    TRUE ~ Date_fin
  ))

#Check on a few ids to see whether the filter works correctly 
temp <- resultats_codachats %>% filter (resultats_codachats$Identifiant == "PS267") 

### Assigning a random date within the entry window for each ticket entered more than 56 days before the start of entries and after their end-----------
#### Function to generate a random date between Date_début and Date_fin---------------
#The Date_début and Date_fin arguments are converted to Date-class objects to ensure the following operations run on valid dates.
generate_random_date <- function(Date_début, Date_fin) { 
  Date_début <- as.Date(Date_début)
  Date_fin <- as.Date(Date_fin)
  #If one of the dates is NA, the function returns NA, which avoids calculation errors on undefined values.
  if (is.na(Date_début) || is.na(Date_fin)) { return(NA) }
  #The difference in days between Date_fin and Date_début is calculated using the difftime function, then converted to a numeric value.
  diff_days <- as.numeric(difftime(Date_fin, Date_début, units = "days"))
  #A random number of days is generated between 0 and diff_days using the sample function.
  random_days <- sample(0:diff_days, 1)
  #The random date is obtained by adding the random number of days to Date_début.
  Date_aléatoire <- Date_début + random_days
  return(Date_aléatoire)
}

#### Applying the function to each row of the dataframe------------

resultats_codachats <- resultats_codachats %>%
  rowwise() %>%
  mutate(Date_aléatoire = generate_random_date(Date_début, Date_fin))




#### Condition to replace dates outside the 56-day range--------------
#In summary, this transformation checks for each row of resultats_codachats whether the Date column is outside the range
#between 56 days before Date_début and 56 days after Date_fin. If so, the Date value is replaced by Date_aléatoire. Otherwise, Date keeps its original value.
resultats_codachats <- resultats_codachats %>%
  mutate(Date = case_when(
    !is.na(Date) & (Date < Date_début - 56 | Date > Date_fin + 56) ~ Date_aléatoire,
    TRUE ~ Date
  ))

resultats_codachats$Date <- as.Date(resultats_codachats$Date, origin = "1970-01-01")

### Checking whether there are still tickets outside the bounds ----------
#A_verifier <-resultats_codachats %>% filter(Date < Date_début| Date > Date_fin)
#tickets_a_verifier <- A_verifier %>%
#  select(Identifiant, Lieu, Date) %>%
#  distinct()
# Defining the path to the directory where your photos are stored
#repertoire_photos <- "E:/TdC_novembre_2023"

# Listing all the files in the directory
#photos <- list.files(path = repertoire_photos)
# Converting the list of file names into a data frame
#df_photos <- data.frame(Noms_de_Photos = photos)
# Extracting the identifier and date from each file name
#df_photos$Identifiant <- sub("^(.*)_.*$", "\\1", df_photos$Noms_de_Photos)
#df_photos$Date <- sub("^.*_(\\d{8}).*$", "\\1", df_photos$Noms_de_Photos)
# Transforming the date to the "YYYY-MM-DD" format
#df_photos$Date <- as.Date(df_photos$Date, format = "%Y%m%d")
# Removing the Noms_de_Photos column and duplicate rows
#df_photos <- df_photos %>%
#  select(-Noms_de_Photos) %>%
#  distinct()
#df_photos$Message <- "La_photo_existe" 

# Performing a join to check the matches
#result <- left_join(tickets_a_verifier, df_photos, by = c("Identifiant", "Date"))
#write.xlsx(result,(paste0(campaign,"_carnet_appro/verif2.xlsx")))
#write.xlsx(tickets_a_verifier,(paste0(campaign,"_carnet_appro/verif.xlsx")))

### Applying manual corrections: all existing tickets outside the bounds have been checked: the date is corrected / removed, or left pending if the ticket is illegible. --------------
# Converting the Date columns to Date format
# For all values outside the bounds and entered up to 56 days before and after the entry date, the date was checked against the photo if available. 
#resultats_codachats$Date <- as.Date(resultats_codachats$Date, format="%Y-%m-%d")

# Logic based on the campaign
if (campaign == "22-11") {
  Correction_date <- Correction_date_nov_22
} else if (campaign == "23-02") {
  Correction_date <- Correction_date_mars_23
} else if (campaign == "23-11") {
  Correction_date <- Correction_date_nov_23
} else if (campaign == "24-03") {
  Correction_date <- Correction_date_mars_24
}

# Converting the Date_corrigée columns to Date format if they exist
Correction_date$Date_corrigée <- as.Date(Correction_date$Date_corrigée, format="%Y-%m-%d")
Correction_date$Date <- as.Date(Correction_date$Date, format="%Y-%m-%d")
Correction_date$Lieu <- trimws(Correction_date$Lieu)
#Checking for case differences
Correction_date$Lieu <- tolower(Correction_date$Lieu)
#Removing invisible characters
Correction_date$Lieu<- iconv(Correction_date$Lieu, to = "ASCII//TRANSLIT")
# Left join between resultats_codachats and Correction_date
valeurs_non_jointes <- Correction_date %>%
  anti_join(resultats_codachats, by = c("Lieu", "Date", "Identifiant"))

resultats_codachats <- left_join(resultats_codachats, Correction_date, by=c("Lieu", "Date", "Identifiant"))
dup_rows <- Correction_date[duplicated(Correction_date[, c("Identifiant", "Date", "Lieu")]), ]
print(dup_rows)
print(unique(resultats_codachats$Date_corrigée))
print(unique(resultats_codachats$Identifiant))

# Using coalesce to create a new Date_vf column
resultats_codachats$Date_vf <- coalesce(resultats_codachats$Date_corrigée, resultats_codachats$Date)

# Converting Date_vf to Date format
resultats_codachats$Date_vf <- as.Date(resultats_codachats$Date_vf, format="%Y-%m-%d")

# Filtering the rows where A supprimer is not NA
resultats_codachats <- resultats_codachats %>%
  filter(is.na(`A supprimer`)) 
# Selecting the columns to keep in resultats_codachats
print(unique(resultats_codachats$`A supprimer`))



### Checking whether there are still tickets outside the bounds ----------
#A_verifier <-resultats_codachats %>% filter(Date_vf < Date_début| Date_vf > Date_fin)
#tickets_a_verifier <- A_verifier %>%
#  select(Identifiant, Lieu, Date) %>%
#  distinct()

### For missing or illegible tickets: compare the date with the entry date and apply the following corrections -----------------------
#If the ticket's year differs from the entry year: use the entry year
#Same for the month
#If neither the month nor the year match, then remove the date
#The first line if (campaign == "23-11" | campaign == "24-03") checks whether the campaign variable equals "23-11" or "24-03"
if (campaign == "23-11" | campaign == "24-03") {
  if (all(resultats_codachats$Date_vf < resultats_codachats$Date_début | resultats_codachats$Date_vf > resultats_codachats$Date_fin)) {
    
    resultats_codachats <- resultats_codachats %>%
      mutate(
        Date_finale = case_when(
          (Date_vf > Date_fin | Date_vf < Date_début) ~
            case_when(
              year(DateSaisie) != year(Date_vf) ~ make_date(year(DateSaisie), month(Date_vf), day(Date_vf)),
              month(DateSaisie) != month(Date_vf) ~ make_date(year(Date_vf), month(DateSaisie), day(Date_vf)),
              (year(DateSaisie) != year(Date_vf) & month(DateSaisie) != month(Date_vf)) ~ as.Date(DateSaisie),
              TRUE ~ Date_vf
            ),
          TRUE ~ Date_vf
        )
      )
    
    resultats_codachats$Date_vf <- resultats_codachats$Date_finale
  }
}

# List of identifiers to filter
#identifiants_a_filtrer <- c("1730-Epimut", "A1-Epimut", "6222-Episourire", "6273-Episourire", "SP041-CCAS")
#Frame_filtre <- resultats_codachats[resultats_codachats$Identifiant %in% identifiants_a_filtrer, ]
#print(unique(Frame_filtre$Identifiant))
#print(unique(resultats_codachats$Identifiant))
#temp <- resultats_codachats[is.na(resultats_codachats$Date_début) | as.Date(resultats_codachats$Date_vf) > as.Date(resultats_codachats$Date_début), ]
#temp <-resultats_codachats[is.na(resultats_codachats$Date_début) | as.Date(resultats_codachats$Date_vf) < as.Date(resultats_codachats$Date_fin), ]
#ids_in_resultats_not_in_temp <- setdiff(resultats_codachats$Identifiant, temp$Identifiant)
# Finding IDs present in temp but not in resultats_codachats
#ids_in_temp_not_in_resultats <- setdiff(temp$Identifiant, resultats_codachats$Identifiant)
# Output the differences
#cat("IDs in resultats_codachats but not in temp:\n")
#print(ids_in_resultats_not_in_temp)
#cat("\nIDs in temp but not in resultats_codachats:\n")
#print(ids_in_temp_not_in_resultats)
#print(unique(resultats_codachats$Identifiant))

#Correcting the store names 
resultats_codachats$Lieu_vf [resultats_codachats$Lieu_vf== "epi'sourire - dijon(place jeacques prevert)" ] <- "epi'sourire - dijon (place jeacques prevert)"

#Removing the Opticourses data
# Storing the rows before filtering
resultats_avant_filtre <- resultats_codachats
#Removing the rows whose entry date is null: corresponds to the Opticourses participants
resultats_codachats <- resultats_codachats %>%
  filter(!is.na(Date_début))
# Finding the rows removed by comparing before and after filtering
lignes_supprimees <- anti_join(resultats_avant_filtre, resultats_codachats)
# Displaying the removed rows
lignes_supprimees
unique(lignes_supprimees$Identifiant)


### Removing data outside the bounds  ----------------------
# Applying the filter using case_when
resultats_avant_filtre <- resultats_codachats
resultats_codachats <- resultats_codachats %>%
  filter(
    case_when(
      is.na(Date_vf) ~ TRUE, # Keeps the rows where Date_vf is NA
      as.Date(Date_vf) >= as.Date(Date_début) & as.Date(Date_vf) <= as.Date(Date_fin) ~ TRUE, 
      TRUE ~ FALSE # Excludes all other rows
    )
  )

# Finding the rows removed by comparing before and after filtering
lignes_supprimees <- anti_join(resultats_avant_filtre, resultats_codachats)
# Displaying the removed rows
lignes_supprimees


# Calculating the relative week
resultats_codachats$semaine_num <- floor(as.numeric(resultats_codachats$Date - resultats_codachats$date_starting) / 7) + 1
# Capping at 4
resultats_codachats$semaine_num <- pmin(resultats_codachats$semaine_num, 4)
# Keep only the rows where semaine_num >= 1
resultats_codachats <- resultats_codachats[
  resultats_codachats$semaine_num >= 1,
]

## Associating Cod-Achats and Calnut ---------------------------
#MERGING the CALNUT reference table and the entry file "resultats_Codachats" 
#On the entry file, we rename: "CodeCIQUAL" to "CODACHATS_alim_code" and "Categorie1" to "groupe_TI_TdC".
# resultats_codachats and CALNUT are linked by CODACHATS_alim_code
colnames(resultats_codachats)[colnames(resultats_codachats) == 'CodeCIQUAL'] <- 'CODACHATS_alim_code'
resultats_codachats <- left_join(resultats_codachats, CALNUT, by=c("CODACHATS_alim_code"))

#Rename "Category1" with groupe_TI_TdC
colnames(resultats_codachats)[colnames(resultats_codachats) == 'Categorie1'] <- 'groupe_TI_TdC1'

###  Assigning the Libelle_Custom foods to a TI group-----------------
resultats_codachats<- left_join(resultats_codachats, Reclassement_Libelle_Custom, by=c("LibelleCustom"), relationship = "many-to-many")

#diff_df1_df2 <- setdiff(resultats_codachats_test$LibelleCustom, resultats_codachats$LibelleCustom)
#diff_df2_df1 <- setdiff(resultats_codachats$LibelleCustom, resultats_codachats_test$LibelleCustom)

resultats_codachats$Categorie2 <- toupper(resultats_codachats$Categorie2 )

temp <- ifelse(( is.na(resultats_codachats$groupe_TI_TdC1)),(resultats_codachats$Reclassement_TI),(resultats_codachats$groupe_TI_TdC1))
resultats_codachats$groupe_TI_TdC1 <- temp
resultats_codachats$groupe_TI_TdC1 <- ifelse((is.na(resultats_codachats$groupe_TI_TdC)),(resultats_codachats$groupe_TI_TdC1),(resultats_codachats$groupe_TI_TdC))

### Correcting the classification errors, groupe Ti_TDC ---------------
resultats_codachats<- left_join(resultats_codachats, Reclassement_Groupe_TI, by=c("groupe_TI_TdC1"), relationship = "many-to-many")
temp <- ifelse(( is.na(resultats_codachats$New)),(resultats_codachats$groupe_TI_TdC1),(resultats_codachats$New))
resultats_codachats$groupe_TI_TdC1 <- temp
describe(!is.na(resultats_codachats$Categorie2))
print(unique(resultats_codachats$Identifiant))


### Removing rows that appear as duplicates: removing duplicate voucher amounts 
temp <- resultats_codachats %>%
  # Separate the rows with a non-empty "Montant_cheque" and apply distinct()
  filter(!is.na(MontantChequeAlimentaire) & MontantChequeAlimentaire != "") %>%
  distinct() %>%
  # Add back the rows where "Montant_cheque" is empty or NA
  bind_rows(resultats_codachats %>% filter(is.na(MontantChequeAlimentaire) | MontantChequeAlimentaire == ""))
resultats_codachats  <- temp 



#22-11 MEAN : 18  MEDIAN 13  --> Epiceries MEAN 15 / MEDIAN  12
#23-02 : MEAN : 21 / MEDIAN 16 --> Epiceries MEAN 19 / MEDIAN  15
#23-11 :  MEAN 31 /  MEDIAN : 22 --> LE : MEAN 29 / MEDIAN 18.5
#24/03 : MEAN 40 / MEDIAN 36 --> LE : MEAN : 39 / MEDIAN 36



# CLEANING THE COD_ACHATS FILE:  WEIGHT / PRICE / UNITS ----------------------------
## ================================================================
## CLEANING resultats_codachats — VERSION WITH CONSISTENCY CHECKS
## ================================================================
## Changes compared to the original script:
##   1) Each "aliments_specifiques" list was renamed with a unique
##      and explicit name, because the variable was overwritten 4 times
##      in a row in the original script (risk of using the wrong list
##      by mistake in a rule further down).
##   2) A verifier_etape() function is called after each major
##      step: it displays the number of rows modified, the number
##      of NAs created, and the descriptive stats of Nb / Prix / Unite.
## ================================================================

library(dplyr)

## ---- Utility check function -----------------------------
## Compares a "before" and an "after" on the key columns and displays
## a summary of the changes. To be called after each step.
verifier_etape <- function(avant, apres, nom_etape, cols = c("Nb", "Unite", "Prix", "PrixMenu")) {
  cat("\n================ CONTROLE :", nom_etape, "================\n")
  
  # Rows modified on at least one of the tracked columns
  # (proper handling of NA values for the comparison)
  diff_logique <- Reduce(`|`, lapply(cols, function(col) {
    a <- avant[[col]]
    b <- apres[[col]]
    !( (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b) )
  }))
  nb_modifiees <- sum(diff_logique, na.rm = TRUE)
  cat("Lignes modifiées :", nb_modifiees, "/", nrow(avant),
      sprintf("(%.2f%%)\n", 100 * nb_modifiees / nrow(avant)))
  
  # NAs created / removed, column by column
  for (col in cols) {
    na_avant <- sum(is.na(avant[[col]]))
    na_apres <- sum(is.na(apres[[col]]))
    if (na_avant != na_apres) {
      cat(sprintf("  - %s : NA avant = %d | NA après = %d (delta = %+d)\n",
                  col, na_avant, na_apres, na_apres - na_avant))
    }
  }
  
  # Quick stats on Nb and Prix if numeric
  if ("Nb" %in% cols) {
    cat("  - Nb   : avant [min=", round(min(avant$Nb, na.rm = TRUE), 3),
        " médiane=", round(median(avant$Nb, na.rm = TRUE), 3),
        " max=", round(max(avant$Nb, na.rm = TRUE), 3), "]",
        " -> après [min=", round(min(apres$Nb, na.rm = TRUE), 3),
        " médiane=", round(median(apres$Nb, na.rm = TRUE), 3),
        " max=", round(max(apres$Nb, na.rm = TRUE), 3), "]\n", sep = "")
  }
  if ("Prix" %in% cols) {
    cat("  - Prix : avant [min=", round(min(avant$Prix, na.rm = TRUE), 3),
        " médiane=", round(median(avant$Prix, na.rm = TRUE), 3),
        " max=", round(max(avant$Prix, na.rm = TRUE), 3), "]",
        " -> après [min=", round(min(apres$Prix, na.rm = TRUE), 3),
        " médiane=", round(median(apres$Prix, na.rm = TRUE), 3),
        " max=", round(max(apres$Prix, na.rm = TRUE), 3), "]\n", sep = "")
  }
  if ("Unite" %in% cols) {
    # We replace the NAs with an explicit string BEFORE tabulating:
    # otherwise table() gives an NA name (rather than the string "NA") to the
    # missing category, which breaks the [[ ]] indexing further below.
    unite_avant <- ifelse(is.na(avant$Unite), "(manquant)", avant$Unite)
    unite_apres <- ifelse(is.na(apres$Unite), "(manquant)", apres$Unite)
    
    tab_avant <- table(unite_avant)
    tab_apres <- table(unite_apres)
    toutes_unites <- union(names(tab_avant), names(tab_apres))
    for (u in toutes_unites) {
      va <- if (u %in% names(tab_avant)) tab_avant[[u]] else 0
      vp <- if (u %in% names(tab_apres)) tab_apres[[u]] else 0
      if (va != vp) cat(sprintf("  - Unite '%s' : %d -> %d (%+d)\n", u, va, vp, vp - va))
    }
  }
  cat("=====================================================\n")
}

## Starting snapshot, used for the very first check
snapshot_initial <- resultats_codachats


## ================================================================
## STEP 1 — Assigning the "grammes" (grams) unit (Nb > 30, units)
## ================================================================
avant <- resultats_codachats

resultats_codachats <- resultats_codachats %>%
  mutate(
    Unite = case_when(
      Nb > 30 &
        Unite == "unités" &
        LibelleCIQUAL != "Compote de fruits, allégée en sucres" &
        LibelleCIQUAL != "Compote de pomme" &
        LibelleCIQUAL != "Compote de fruits" &
        LibelleCIQUAL != "Sushi ou maki aux produits de la mer" &
        groupe_TI_TdC1 != "OEUFS" &
        groupe_TI_TdC1 != "CAFE_THE" ~ "grammes",
      TRUE ~ Unite
    )
  )

verifier_etape(avant, resultats_codachats, "1. Attribution grammes (Nb>30)", cols = "Unite")


## ================================================================
## STEP 2 — Missing values for Prix/PrixMenu/Nb/Appreciation <= 0
## ================================================================
avant <- resultats_codachats

resultats_codachats$Prix[resultats_codachats$Prix < 0] <- NA
resultats_codachats$PrixMenu[resultats_codachats$PrixMenu < 0] <- NA
resultats_codachats$Nb[resultats_codachats$Nb <= 0] <- NA
resultats_codachats$Appreciation[resultats_codachats$Appreciation < 0] <- NA

resultats_codachats$Prix[resultats_codachats$Prix == 0 & resultats_codachats$Lieu1 != "dons"] <- NA
resultats_codachats$PrixMenu[resultats_codachats$PrixMenu == 0 & resultats_codachats$Lieu1 != "dons"] <- NA

resultats_codachats$Nb[resultats_codachats$Nb == 0] <- NA

verifier_etape(avant, resultats_codachats, "2. Valeurs manquantes (<=0)",
               cols = c("Nb", "Prix", "PrixMenu", "Appreciation"))


## ================================================================
## STEP 3 — Corrections of the main conversion errors
## (Nb<10g -> x1000 ; Prix>100 excluding alcohol/red meat -> NA)
## ================================================================
avant <- resultats_codachats

resultats_codachats <- resultats_codachats %>%
  mutate(
    Nb = case_when(
      Unite == "grammes" &
        Nb < 10 &
        groupe_TI_TdC1 != "EPICES_CONDIMENTS" &
        Lieu2 != "RHD" ~ Nb * 1000,
      TRUE ~ Nb
    )
  )
resultats_codachats <- resultats_codachats %>%
  mutate(
    Prix = case_when(
      Prix > 100 &
        groupe_TI_TdC1 != "ALCOOL" &
        groupe_TI_TdC1 != "VIANDE_ROUGE" ~ NA_real_,
      TRUE ~ Prix
    )
  )
verifier_etape(avant, resultats_codachats, "3. Corrections erreurs de conversion (Nb x1000 / Prix>100 -> NA)",
               cols = c("Nb", "Prix"))


## ================================================================
## STEP 4 — Readjusting the units (kilos/grams/litres/centilitres)
## ================================================================
avant <- resultats_codachats

resultats_codachats <- resultats_codachats %>%
  mutate(
    Unite = case_when(
      Nb > 50 & Unite == "kilos" ~ "grammes",
      Nb > 10 & Nb < 20.5 & Unite == "grammes" ~ "unités",
      Nb < 1 & Unite == "centilitres" ~ "litres",
      Nb > 1000 & Unite == "kilos" ~ "grammes",
      TRUE ~ Unite
    )
  )

verifier_etape(avant, resultats_codachats, "4. Réajustement des unités", cols = "Unite")


## ================================================================
## STEP 5 — Readjusting Nb when price is low and quantity is high (litres)
## ================================================================
avant <- resultats_codachats

library(dplyr)

resultats_codachats <- resultats_codachats %>%
  mutate(
    Nb = case_when(
      Prix < 3 &
        Nb > 17 &
        Unite == "litres" &
        groupe_TI_TdC1 != "EAU" ~ Nb / 10,
      TRUE ~ Nb
    )
  )
verifier_etape(avant, resultats_codachats, "5. Réajustement Nb (prix bas / quantité élevée, litres)", cols = "Nb")

## Price per kg — recalculated for the following steps
resultats_codachats$Prix_kg <- resultats_codachats$Prix / resultats_codachats$Nb


## ================================================================
## STEP 6 — Multiplying Nb by 10 for low-weight foods
## (renamed list: aliments_poids_faible_x10)
## ================================================================
avant <- resultats_codachats

aliments_poids_faible_x10 <- c(
  "Bonbon/ bouchée chocolat fourrage gaufrettes/ biscuit", "Bonbons, tout type", "Champignon, tout type, cru",
  "Champignons à la grecque, appertisés", "Barre chocolatée biscuitée", "Fromage à pâte molle et croûte fleurie double crème environ 30% MG",
  "Miel", "Sucre blanc", "Crème fraîche, 15 à 20% MG, UHT", "Rillettes de poulet",
  "Jambon sec, découenné, dégraissé", "Cacahuète ou Arachide", "Croissant, sans précision", "Viande rouge, cuite (aliment moyen)",
  "Confiserie au chocolat dragéifiée", "Chocolat blanc aux fruits secs (noisettes, amandes, raisins, praliné), tablette",
  "Yaourt, lait fermenté ou spécialité laitière, aux fruits, sucré", "Fruit cru (aliment moyen)", "Rillettes de thon", "Crevette, crue",
  "Crêpe, nature, préemballée, rayon température ambiante", "Brioche fourrée au chocolat", "Dessert (aliment moyen)",
  "Chocolat au lait fourré au praliné, tablette", "Biscuit sec chocolaté, préemballé", "Barre céréalière aux amandes ou noisettes",
  "Nougat ou touron", "Aubergine, crue", "Crème fraîche, 30% MG, UHT", "Banane, pulpe, crue", "Poulet, filet, sans peau, cru",
  "Pâté (aliment moyen)", "Nem ou Pâté impérial", "Sauce kebab", "Crevette, cuite", "Gaufrette ou éventail sans fourrage",
  "Bonbon gélifié", "Pâte d'amande", "Cabillaud, cru", "Chocolat au lait, tablette", "Bonbon / bouchée au chocolat fourrage gaufrettes / biscuit",
  "Barres ou confiserie chocolatées au lait", "Kiwi, pulpe et graines, cru", "Chocolat au lait fourré", "Sucre vanillé",
  "Pain d'épices fourré ou nonette", "Oignon, cru", "Chocolat, en tablette (aliment moyen)")

resultats_codachats <- resultats_codachats %>%
  mutate(
    Nb = case_when(
      LibelleCIQUAL %in% aliments_poids_faible_x10 &
        Nb <= 20 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ Nb * 10,
      TRUE ~ Nb
    )
  )
verifier_etape(avant, resultats_codachats, "6. Nb x10 pour aliments à faible grammage", cols = "Nb")

resultats_codachats$Prix_kg <- resultats_codachats$Prix / resultats_codachats$Nb


## ================================================================
## STEP 7 — Dividing Prix by 10 for certain foods
## (renamed list: aliments_prix_eleve_div10)
## ================================================================
avant <- resultats_codachats

aliments_prix_eleve_div10 <- c("Saumon fumé", "Barres chocolatées", "Pâte à tartiner chocolat et noisette", "Rosette ou Fuseau", "Pâtisserie (aliment moyen)",
                               "Pomme de terre de conservation, crue", "Chocolat, en tablette (aliment moyen)", "Mélange apéritif graine non salée fruit séché",
                               "Mozzarella au lait de vache", "Sandwich baguette, jambon emmental")

resultats_codachats <- resultats_codachats %>%
  mutate(
    Prix = case_when(
      LibelleCIQUAL %in% aliments_prix_eleve_div10 &
        Nb >= 100 &
        Prix >= 10 &
        Prix_kg > 0.05 &
        Unite == "grammes" ~ Prix / 10,
      TRUE ~ Prix
    )
  )

verifier_etape(avant, resultats_codachats, "7. Prix / 10 pour aliments à prix élevé", cols = "Prix")

resultats_codachats$Prix_kg <- resultats_codachats$Prix / resultats_codachats$Nb


## ================================================================
## STEP 8 — Converting certain liquid foods to centilitres
## (renamed list: aliments_liquides_centilitres)
## ================================================================
avant <- resultats_codachats

aliments_liquides_centilitres <- c(
  "Bière \"de spécialités\" ou d'abbaye, régionales ou d'une brasserie (degré d'alcool variable)",
  "Boisson gazeuse, sans jus de fruit, sucrée", "Jus de fruits (aliment moyen)",
  "Boisson préparée à partir de sirop à diluer type menthe, fraise, etc., sucré, dilué dans l'eau",
  "Huile de pépins de raisin")

resultats_codachats <- resultats_codachats %>%
  mutate(
    Unite = case_when(
      LibelleCIQUAL %in% aliments_liquides_centilitres &
        Nb < 100 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ "centilitres",
      TRUE ~ Unite
    )
  )
verifier_etape(avant, resultats_codachats, "8. Conversion en centilitres (aliments liquides)", cols = "Unite")


## ================================================================
## STEP 9 — Unit weight of eggs (63 g / unit)
## ================================================================
avant <- resultats_codachats

resultats_codachats <- resultats_codachats %>%
  mutate(
    Nb = case_when(
      groupe_TI_TdC1 == "OEUFS" &
        Unite == "unités" ~ Nb * 0.063,
      TRUE ~ Nb
    ),
    Unite = case_when(
      groupe_TI_TdC1 == "OEUFS" &
        Unite == "unités" &
        !is.na(Nb) ~ "kilos",
      TRUE ~ Unite
    )
  )
verifier_etape(avant, resultats_codachats, "9. Poids unitaire des œufs (63g)", cols = c("Nb", "Unite"))


## ================================================================
## STEP 10 — One-off correction: raw eggplant (single case)
## ================================================================
avant <- resultats_codachats

resultats_codachats <- resultats_codachats %>%
  mutate(
    Nb = case_when(
      groupe_TI_TdC1 == "OEUFS" &
        Unite == "unités" ~ Nb * 0.063,
      LibelleCIQUAL == "Aubergine, crue" &
        Nb == 75 &
        Prix == 3.98 ~ 750,
      TRUE ~ Nb
    ),
    Unite = case_when(
      groupe_TI_TdC1 == "OEUFS" &
        Unite == "unités" ~ "kilos",
      TRUE ~ Unite
    )
  )

verifier_etape(avant, resultats_codachats, "10. Correction ponctuelle Aubergine crue", cols = "Nb")


## ================================================================
## STEP 11 — Applesauce/compotes: 1 pot = 100 g
## ================================================================
avant <- resultats_codachats

compotes <- c(
  "Compote de fruits allégée en sucres rayon frais",
  "Compote de pomme",
  "Compote de fruits",
  "Compote de fruits, allégée en sucres",
  "Compote (aliment moyen)"
)

resultats_codachats <- resultats_codachats %>%
  mutate(
    Nb = case_when(
      LibelleCIQUAL %in% compotes &
        Unite == "unités" ~ Nb * 0.1,
      TRUE ~ Nb
    ),
    Unite = case_when(
      LibelleCIQUAL %in% compotes &
        Unite == "unités" ~ "kilos",
      TRUE ~ Unite
    )
  )

verifier_etape(avant, resultats_codachats, "11. Compotes (1 pot = 100g)", cols = c("Nb", "Unite"))


## ================================================================
## STEP 12 — Various one-off corrections (Nb / Unite)
## ================================================================
avant <- resultats_codachats

library(dplyr)

# Corrections before recalculating the price per kg
resultats_codachats <- resultats_codachats %>%
  mutate(
    Nb = case_when(
      LibelleCIQUAL == "Sushi ou maki aux produits de la mer" &
        Unite == "unités" ~ Nb * 0.04,
      TRUE ~ Nb
    ),
    Unite = case_when(
      LibelleCIQUAL == "Amande, avec peau" &
        Nb == 0.2 &
        Unite == "centilitres" ~ "kilos",
      
      LibelleCIQUAL == "Sushi ou maki aux produits de la mer" &
        Unite == "unités" ~ "kilos",
      
      TRUE ~ Unite
    )
  )

# Recalculating the price per kg
resultats_codachats <- resultats_codachats %>%
  mutate(
    Prix_kg = Prix / Nb
  )

# Corrections using Prix_kg
resultats_codachats <- resultats_codachats %>%
  mutate(
    Nb = case_when(
      LibelleCIQUAL == "Oeuf, cru" &
        Nb == 30 ~ 300,
      
      LibelleCIQUAL == "Nem ou Pâté impérial" &
        Unite == "unités" ~ Nb * 0.1,
      
      LibelleCIQUAL == "Champignon, morille, crue" &
        Nb == 30 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ 300,
      
      LibelleCIQUAL == "Champignon, lentin ou shiitaké, séché" &
        Nb == 20 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ 200,
      
      LibelleCIQUAL == "Yaourt à la grecque, nature" &
        Nb == 12 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ 120,
      
      LibelleCIQUAL == "Yaourt aromatisé, avec édulcorants, 0% MG" &
        Nb == 12 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ 120,
      
      LibelleCIQUAL == "Yaourt aux fruits, sucré" &
        Nb == 16 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ 160,
      
      LibelleCIQUAL == "Purée de tomate" &
        Nb == 15000 ~ 15,
      
      LibelleCIQUAL == "Fruits de mer (aliment moyen), cru" &
        Nb == 12 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ 120,
      
      LibelleCIQUAL == "Pizza 4 fromages" &
        Nb == 16 &
        Unite == "unités" ~ 1,
      
      LibelleCIQUAL == "Terrine de canard" &
        Nb == 130 &
        Prix == 7.6 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ Nb * 10,
      
      LibelleCIQUAL == "Sandwich baguette, jambon emmental" &
        Prix < 0.45 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ NA_real_,
      
      LibelleCIQUAL == "Toasts ou Canapés salés, garnitures diverses, préemballés" &
        Prix == 8.05 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ Nb * 10,
      
      LibelleCIQUAL == "Rillettes de poulet" &
        Nb == 180 &
        Prix == 8.8 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ Nb * 10,
      
      LibelleCIQUAL == "Bœuf, fauxfilet, cru" &
        Nb == 240 &
        Prix == 12.48 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ Nb * 10,
      
      LibelleCIQUAL == "Bœuf, fauxfilet, cru" &
        Nb == 110 &
        Prix == 7.5 &
        Unite == "grammes" &
        Prix_kg > 0.05 ~ Nb * 10,
      
      LibelleCIQUAL == "Moutarde" &
        Nb < 200 &
        Unite == "grammes" &
        Prix < 0.05 &
        Prix_kg > 0.05 ~ Nb * 10,
      
      LibelleCIQUAL == "banane, crue" &
        Nb > 1070 ~ 1.07,
      
      TRUE ~ Nb
    ),
    Unite = case_when(
      LibelleCIQUAL == "Nem ou Pâté impérial" &
        Unite == "unités" ~ "kilos",
      TRUE ~ Unite
    )
  )
verifier_etape(avant, resultats_codachats, "12. Corrections ponctuelles diverses (Nb/Unite)", cols = c("Nb", "Unite"))


## ================================================================
## STEP 13 — One-off corrections to the units
## (renamed list: aliments_petit_volume_litres)
## ================================================================
avant <- resultats_codachats

aliments_petit_volume_litres <- c("Sauce pour nems à base de nuocmam dilué, préemballée", "Sauce soja, préemballée",
                                  "Bière \"spéciale\" (56° alcool)", "Bière \"spéciale\" (5-6° alcool)")

resultats_codachats <- resultats_codachats %>%
  mutate(
    Unite = case_when(
      LibelleCIQUAL == "Pomme, crue" &
        Nb == 1 &
        Unite == "centilitres" ~ "kilos",
      
      LibelleCIQUAL == "Tomate, bouillie/cuite à l'eau" &
        Nb == 1600 ~ "unités",
      
      LibelleCIQUAL == "Vin rouge" &
        Nb == 1 &
        Unite == "centilitres" ~ "litres",
      
      LibelleCIQUAL %in% aliments_petit_volume_litres &
        Nb <= 3 &
        Unite == "centilitres" ~ "litres",
      
      LibelleCIQUAL == "Sauce soja, préemballée" &
        Nb == 2.7 &
        Unite == "centilitres" ~ "litres",
      
      LibelleCIQUAL == "Fromage (aliment moyen)" &
        Nb == 1600 &
        Unite == "kilos" ~ "grammes",
      
      LibelleCIQUAL == "Vinaigre" &
        Nb == 1 &
        Unite == "grammes" ~ "litres",
      
      LibelleCIQUAL == "Sel blanc, non iodé, non fluoré" &
        Nb == 0.75 &
        Unite == "grammes" ~ "kilos",
      
      LibelleCIQUAL == "Moutarde" &
        Nb < 200 &
        Unite == "centilitres" ~ "grammes",
      
      LibelleCIQUAL == "Cornichon, au vinaigre" &
        Unite == "litres" ~ "kilos",
      
      LibelleCIQUAL == "Cornichon, au vinaigre" &
        Unite == "centilitres" ~ "grammes",
      
      # As in your original script
      LibelleCIQUAL %in% aliments_petit_volume_litres &
        Nb == 1 &
        Unite == "grammes" ~ "kilos",
      
      LibelleCIQUAL == "Oeuf, cru" &
        Unite == "grammes" &
        Prix == 100 ~ "unités",
      
      LibelleCIQUAL == "Croissant, sans précision" &
        Identifiant == "LE116" &
        Nb == 2 ~ "unités",
      
      LibelleCIQUAL == "Crème de lait, 15 à 20% MG, légère, épaisse, rayon frais" &
        Identifiant == "LE126" &
        Nb == 8 ~ "unités",
      
      TRUE ~ Unite
    )
  )
verifier_etape(avant, resultats_codachats, "13. Corrections ponctuelles sur les unités", cols = "Unite")


## ================================================================
## STEP 14 — One-off corrections to the prices
## (renamed list: aliments_prix_a_100)
## ================================================================
avant <- resultats_codachats


aliments_prix_a_100_pates <- c("Pâtes sèches standard, cuites, non salées", "Pâtes sèches standard, crues")
aliments_prix_a_100_abats <- c("Abat, cru (aliment moyen)", "Abat, cuit (aliment moyen)", "Thon, cru ", "Accra de poisson")

resultats_codachats <- resultats_codachats %>%
  mutate(
    Prix = case_when(
      LibelleCIQUAL == "Oeuf, à la coque" &
        Prix == 100 ~ NA_real_,
      
      LibelleCIQUAL %in% aliments_prix_a_100_pates &
        Prix == 100 ~ 1,
      
      LibelleCIQUAL == "Ravioli chinois vapeur à la crevette" &
        Nb == 105 &
        Prix == 7.5 ~ NA_real_,
      
      LibelleCIQUAL %in% aliments_prix_a_100_abats &
        Prix == 100 ~ 10,
      
      TRUE ~ Prix
    )
  )
verifier_etape(avant, resultats_codachats, "14. Corrections ponctuelles sur les prix", cols = "Prix")


## ================================================================
## STEP 15 — Recoding LibelleCIQUAL + hydration coefficient
## ================================================================
avant <- resultats_codachats

resultats_codachats <- resultats_codachats %>%
  mutate(
    LibelleCIQUAL = case_when(
      LibelleCIQUAL == "Champignon, tout type, cru" &
        Identifiant == "6348-Episourire" ~ "Champignon noir, séché",
      TRUE ~ LibelleCIQUAL
    ),
    Nb = case_when(
      LibelleCIQUAL == "Champignon noir, séché" &
        Unite == "grammes" ~ Nb * 14,
      TRUE ~ Nb
    )
  )
verifier_etape(avant, resultats_codachats, "15. Recodage Champignon noir séché + coeff hydratation", cols = c("Nb", "LibelleCIQUAL"))


## ================================================================
## STEP 16 — Imputing Prix / PrixMenu / RHD / donations
## ================================================================
avant <- resultats_codachats

resultats_codachats <- resultats_codachats %>%
  mutate(
    PrixMenu = case_when(
      Lieu2 %in% c("commerce", "Epicerie") & Menu == "Oui" ~ NA_real_,
      Lieu2 == "RHD" ~ Prix,
      Lieu2 == "dons" ~ Prix,
      TRUE ~ PrixMenu
    ),
    
    Prix = case_when(
      Lieu2 %in% c("RHD", "dons") ~ NA_real_,
      TRUE ~ Prix
    ),
    
    Menu = case_when(
      Lieu2 %in% c("commerce", "Epicerie") ~ NA_character_,
      Lieu2 == "RHD" ~ "Oui",
      TRUE ~ Menu
    ),
    
    Nb = case_when(
      Lieu2 == "RHD" &
        Nb < 9 &
        Unite == "grammes" ~ NA_real_,
      TRUE ~ Nb
    ),
    
    Unite = case_when(
      Lieu2 == "RHD" &
        is.na(Nb) &
        Unite == "grammes" ~ NA_character_,
      TRUE ~ Unite
    ),
    
    PrixMenu = case_when(
      is.na(Menu) ~ NA_real_,
      TRUE ~ PrixMenu
    ),
    
    Prix = case_when(
      !is.na(PrixMenu) ~ NA_real_,
      TRUE ~ Prix
    )
  )
verifier_etape(avant, resultats_codachats, "16. Imputation Prix/PrixMenu (RHD, dons, commerce)",
               cols = c("Nb", "Unite", "Prix", "PrixMenu"))


## ================================================================
## STEP 17 — Splitting PrixMenu across tickets (Date_vf / Lieu_vf)
## ================================================================
avant <- resultats_codachats

resultats_codachats <- resultats_codachats %>%
  arrange(Date) %>%
  group_by(Date_vf, Lieu_vf) %>%
  mutate(
    PrixMenu = case_when(
      row_number() == 1 ~ PrixMenu / n(),
      TRUE ~ PrixMenu
    )
  ) %>%
  ungroup()

resultats_codachats$Menu[resultats_codachats$PrixMenu == 0] <- NA
resultats_codachats$Menu[resultats_codachats$Menu == 0] <- NA
verifier_etape(avant, resultats_codachats, "17. Répartition PrixMenu par ticket", cols = c("PrixMenu", "Menu"))


## ================================================================
## FINAL CHECK — comparing snapshot_initial vs the final result
## ================================================================
verifier_etape(snapshot_initial, resultats_codachats, "BILAN GLOBAL (début -> fin du script)",
               cols = c("Nb", "Unite", "Prix", "PrixMenu"))

cat("\n--- Résumé final des unités ---\n")
print(table(resultats_codachats$Unite, useNA = "always"))

cat("\n--- Résumé final de Prix_kg (valeurs extrêmes à surveiller) ---\n")
print(summary(resultats_codachats$Prix_kg))
print(resultats_codachats %>% filter(Prix_kg > 100) %>%
        select(LibelleCIQUAL, Nb, Unite, Prix) %>% head(20))


# IMPUTING MISSING WEIGHT/PRICE VALUES ------------------------------
## Correcting the last remaining Price data----------------
#During the November 2023 campaign, some people entered as "prix menu" purchases made in-store
#These purchases should not appear the way they do when "Oui" is entered for the RHD_DON variable
#We assign NA to the prices entered for these purchases, since we only have the total price for all purchases, and we will apply the weight/price imputation procedure to these same foods
resultats_codachats$PrixMenu <- ifelse((resultats_codachats$Lieu2 =="commerce" & resultats_codachats$Menu== "Oui"), (NA), (resultats_codachats$PrixMenu))
resultats_codachats$PrixMenu <- ifelse((resultats_codachats$Lieu2 =="Epicerie" & resultats_codachats$Menu== "Oui"), (NA), (resultats_codachats$PrixMenu))
resultats_codachats$Menu <- ifelse((resultats_codachats$Lieu2 =="commerce" | resultats_codachats$Lieu2 =="Epicerie"), (NA), (resultats_codachats$Menu))
# If the product comes from RHD (food service): Transfer the price to prixMenu (Nb. everything bought in RHD corresponds here to a menu).
resultats_codachats$PrixMenu <-ifelse(((resultats_codachats$Lieu2 =="RHD")),(resultats_codachats$Prix),(resultats_codachats$PrixMenu))
resultats_codachats$Prix <-ifelse(((resultats_codachats$Lieu2 =="RHD")),(NA),(resultats_codachats$Prix))
resultats_codachats$Menu <- ifelse((resultats_codachats$Lieu2 =="RHD" ), ("Oui"), (resultats_codachats$Menu))
resultats_codachats$Nb <-ifelse(((resultats_codachats$Lieu2 =="RHD") & resultats_codachats$Nb <9 & resultats_codachats$Unite =="grammes"    ),(NA),(resultats_codachats$Nb ))
resultats_codachats$Unite <-ifelse(((resultats_codachats$Lieu2 =="RHD") & is.na(resultats_codachats$Nb) & resultats_codachats$Unite =="grammes" ),(NA),(resultats_codachats$Unite ))

# If the product is a donation: Transfer the price to prixMenu (Nb. everything bought in RHD corresponds here to a menu).
resultats_codachats$PrixMenu <-ifelse(((resultats_codachats$Lieu2 =="dons")),(resultats_codachats$Prix),(resultats_codachats$PrixMenu))
resultats_codachats$Prix <-ifelse(((resultats_codachats$Lieu2 =="dons")),(NA),(resultats_codachats$Prix))
# If it is not a Menu, enter NA in the prixMenu variable
resultats_codachats$PrixMenu <-ifelse((is.na(resultats_codachats$Menu)),(NA),(resultats_codachats$PrixMenu))
resultats_codachats$Prix <-ifelse((is.na(resultats_codachats$PrixMenu)),(resultats_codachats$Prix),(NA))


#If, at this stage, the prices of foods bought in RHD are zero, we assign a value of NA 
resultats_codachats$Menu[resultats_codachats$PrixMenu == 0 ] <- NA
resultats_codachats$Menu[resultats_codachats$Menu == 0 ] <- NA



## Creating the Prix_all (€) and poids (kg) variables-------------------
#two homogeneous variables that bring together all the weight and price data under the same unit
resultats_codachats$Poids <- resultats_codachats$Unite    
resultats_codachats$Prix_all <- resultats_codachats$Prix
#resultats_codachats$Prix_all <-ifelse((is.na(resultats_codachats$Prix_all)),(resultats_codachats$PrixMenu),(resultats_codachats$Prix_all))
resultats_codachats <- resultats_codachats %>%
  mutate(Poids = case_when(
    Unite == "kilos" ~ Nb,
    Unite == "kilo" ~ Nb,
    Unite == "litres" ~ Nb,
    Unite == "centilitres" ~ Nb /100,
    Unite == "grammes" ~ Nb / 1000,
    Unite == "unités" ~  Nb*poids_unitaire/1000,
    TRUE ~ NA_real_
  )) %>%
  mutate(Poids = ifelse(Poids == 0, NA_real_, Poids)) %>%
  mutate(Poids = as.numeric(Poids))


resultats_codachats$Poids <-ifelse((resultats_codachats$Unite == "unités" & is.na(resultats_codachats$Poids) ),(NA),(resultats_codachats$Poids))
resultats_codachats$Nb <- ifelse((resultats_codachats$Nb== 0 ), (NA), (resultats_codachats$Nb))
resultats_codachats$Poids <- ifelse((resultats_codachats$Poids == 0 ), (NA), (resultats_codachats$Poids))
resultats_codachats$Prix_all <- ifelse((resultats_codachats$Prix == 0 & resultats_codachats$Lieu1!= "dons" ), (NA), (resultats_codachats$Prix_all ))

describe(is.na(resultats_codachats$Poids))
describe(is.na(resultats_codachats$Prix_all))


#resultats_codachats<-  resultats_codachats %>%
#  left_join( Poids_unitaires_manquants, by = c("LibelleCIQUAL"))
#resultats_codachats$Poids <- ifelse((resultats_codachats$Lieu2 != "RHD" & resultats_codachats$Unite =="unités"  & is.na(resultats_codachats$Poids)  & is.na(resultats_codachats$Prix) ), (resultats_codachats$Poids_uni*resultats_codachats$Nb), (resultats_codachats$Poids))

describe(is.na(resultats_codachats$Poids))

## Calculating Price / Kg for all foods -------------
resultats_codachats$Prix_kg <- resultats_codachats$Prix_all / resultats_codachats$Poids
#Fprint(unique(resultats_codachats$Prix_kg))


## Calculating the average price per Kg and the average weight for the CODE_CIQUAL X Lieu1 imputation -----------
# Calculating the average price per unit weight for each food
prix_moyen_par_poids1 <- resultats_codachats %>%
  filter(
    !is.na(LibelleCIQUAL),           # not NA
    str_trim(LibelleCIQUAL) != "",   # not an empty string
    !is.na(Prix_all),
    !is.na(Poids)
  ) %>%
  group_by(LibelleCIQUAL, Lieu1) %>%
  summarise(
    prix_moyen_ciqual1     = mean(Prix_kg, na.rm = TRUE),
    nombre_donnees_ciqual1 = n(),
    .groups = "drop"
  )




#Joining the Average Price Data to the Main Table
resultats_codachats <- resultats_codachats %>%
  left_join(prix_moyen_par_poids1, by = c("LibelleCIQUAL", "Lieu1")) 

#prix_calculé_ciqual1: Calculates a weighted price using Poids and prix_moyen_ciqual1.
#poids_calculé_ciqual1: Calculates a derived weight using Prix_all and prix_moyen_ciqual1.
resultats_codachats$prix_calculé_ciqual1  <- resultats_codachats$Poids * resultats_codachats$prix_moyen_ciqual1
resultats_codachats$poids_calculé_ciqual1 <-resultats_codachats$Prix_all/resultats_codachats$prix_moyen_ciqual1

#Counting the Number of Observations per Group
resultats_codachats <- resultats_codachats %>%
  group_by(LibelleCIQUAL, Lieu1) %>%
  mutate( nombre_donnees_ciqual1 = n())

## Calculating the average price per Kg and the average weight for the CODE_CIQUAL X Lieu2 imputation -------------
# Calculating the average price per unit weight for each food
prix_moyen_par_poids2 <- resultats_codachats %>%
  filter(
    !is.na(LibelleCIQUAL),           # not NA
    str_trim(LibelleCIQUAL) != "",   # not an empty string
    !is.na(Prix_all),
    !is.na(Poids)
  ) %>%
  group_by(LibelleCIQUAL, Lieu2) %>%
  summarise(
    prix_moyen_ciqual2     = mean(Prix_kg, na.rm = TRUE),
    nombre_donnees_ciqual2 = n(),
    .groups = "drop"
  )



# Imputing missing values using the na.aggregate function
resultats_codachats <- resultats_codachats %>%
  left_join(prix_moyen_par_poids2,
            by = c("LibelleCIQUAL", "Lieu2"))

resultats_codachats$prix_calculé_ciqual2 <- resultats_codachats$Poids * resultats_codachats$prix_moyen_ciqual2
resultats_codachats$poids_calculé_ciqual2  <-resultats_codachats$Prix_all/resultats_codachats$prix_moyen_ciqual2

resultats_codachats <- resultats_codachats %>%
  group_by(LibelleCIQUAL, Lieu2) %>%
  mutate(nombre_donnees_ciqual2 = n())

## Calculating the average price per Kg and the average weight for the GroupeTI X Lieu2 imputation -------------
# Calculating the average price per unit weight for each food except for "Epices_condiments"
prix_moyen_par_poids3 <- resultats_codachats %>%
  filter(!is.na(Prix_all), !is.na(Poids),
         groupe_TI_TdC1 != "Epices_condiments") %>%
  group_by(groupe_TI_TdC1, Lieu2) %>%
  summarise(
    prix_moyen_groupe_TI_TdC1     = mean(Prix_kg),
    nombre_donnees_groupe_TI_TdC1 = n(),
    .groups = "drop"
  )

# Joining the average prices to the original dataset
resultats_codachats <- resultats_codachats %>%
  left_join(prix_moyen_par_poids3,
            by = c("groupe_TI_TdC1", "Lieu2"))

# Calculating the imputed price and weight for the groups except "Epices_condiments"
resultats_codachats <- resultats_codachats %>%
  mutate(prix_calculé_groupe_TI_TdC1 = if_else(groupe_TI_TdC1 != "Epices_condiments", Poids * prix_moyen_groupe_TI_TdC1, NA_real_),
         poids_calculé_groupe_TI_TdC1 = if_else(groupe_TI_TdC1 != "Epices_condiments", Prix_all / prix_moyen_groupe_TI_TdC1, NA_real_))





# Calculating the number of data points per group and location
resultats_codachats <- resultats_codachats %>%
  group_by(groupe_TI_TdC1, Lieu2) %>%
  mutate(nombre_donnees_groupe_TI_TdC1 = n()) %>%
  ungroup()


#bis <- resultats_codachats %>%
#  filter(Identifiant == "39-Epimut") %>%
#  select(Date, LibelleCIQUAL, groupe_TI_TdC1 , Poids, Prix_all, prix_moyen_ciqual1, prix_moyen_ciqual2,prix_moyen_groupe_TI_TdC1
#         ) %>%
#  print(n = Inf)

## Final imputation ---------------
# Initializing the final columns from the original columns
resultats_codachats$Poids_vf <- resultats_codachats$Poids
resultats_codachats$Prix_vf  <- resultats_codachats$Prix_all

# Corrected function: takes into account the current state of Poids_vf / Prix_vf
calculate_vf <- function(data, prix_calc_col, poids_calc_col, nombre_col) {
  # We start from the already-imputed values
  base_poids <- data$Poids_vf
  base_prix  <- data$Prix_vf
  
  data$Poids_vf <- ifelse(
    is.na(base_poids) & data[[nombre_col]] >= 10,
    data[[poids_calc_col]],
    base_poids
  )
  
  data$Prix_vf <- ifelse(
    is.na(base_prix) & data[[nombre_col]] >= 10,
    data[[prix_calc_col]],
    base_prix
  )
  
  return(data)
}

# Applied in three passes, without overwriting the previous imputations
resultats_codachats <- calculate_vf(
  resultats_codachats,
  "prix_calculé_ciqual1",   "poids_calculé_ciqual1",   "nombre_donnees_ciqual1"
)




resultats_codachats <- calculate_vf(
  resultats_codachats,
  "prix_calculé_ciqual2",   "poids_calculé_ciqual2",   "nombre_donnees_ciqual2"
)

resultats_codachats <- calculate_vf(
  resultats_codachats,
  "prix_calculé_groupe_TI_TdC1", "poids_calculé_groupe_TI_TdC1", "nombre_donnees_groupe_TI_TdC1"
)


data_filtré <- resultats_codachats %>%
  filter(LibelleCIQUAL == "Piment, cru")

#Imputing donations
#The only change made is to the price. If NA, we impute a value of zero
resultats_codachats$Prix_vf <- ifelse((resultats_codachats$Lieu1 =="dons" & is.na(resultats_codachats$Prix_vf)), (0), (resultats_codachats$Prix_vf))
print(unique(resultats_codachats$Identifiant))
describe(is.na(resultats_codachats$Prix_Kg_post_imput))
describe(resultats_codachats$Poids_vf==0)
describe(is.na(resultats_codachats$Poids_vf))
describe(is.na(resultats_codachats$Prix_vf))

### Imputing the weight of RHD (food service) foods----------
####RHD_COL 
resultats_codachats<-  resultats_codachats %>%
  left_join(RHD_COL, by = c("LibelleCIQUAL"))
#resultats_codachats$Poids_vf <- ifelse((is.na(resultats_codachats$Poids_vf) & resultats_codachats$Lieu1 == "RHD_COL" & resultats_codachats$Unite =="unités"), (resultats_codachats$Poids_RHD_COL), (resultats_codachats$Poids_vf))
resultats_codachats$Poids_vf <- ifelse((is.na(resultats_codachats$Poids_vf) & resultats_codachats$Lieu1 == "RHD_COL" ), (resultats_codachats$Poids_RHD_COL), (resultats_codachats$Poids_vf))

#RHD_COM
resultats_codachats<-  resultats_codachats %>%
  left_join(RHD_COM, by = c("LibelleCIQUAL"))
#resultats_codachats$Poids_vf <- ifelse((is.na(resultats_codachats$Poids_vf) & resultats_codachats$Lieu1 == "RHD_COM" & resultats_codachats$Unite =="unités"), (resultats_codachats$Poids_RHD_COM), (resultats_codachats$Poids_vf))
resultats_codachats$Poids_vf <- ifelse((is.na(resultats_codachats$Poids_vf) & resultats_codachats$Lieu1 == "RHD_COM" ), (resultats_codachats$Poids_RHD_COM), (resultats_codachats$Poids_vf))

print(unique(resultats_codachats$Identifiant))
describe(resultats_codachats$Poids_vf==0)
describe(is.na(resultats_codachats$Poids_vf))
describe(is.na(resultats_codachats$Prix_vf))



## Checking the price per kg of the imputed data = ---------------------------
#We calculate the price per kg.
resultats_codachats <- resultats_codachats %>%
  mutate(
    Prix_Kg_post_imput = case_when(
      Prix_vf  == 0          ~ 0,           # if the price is zero → 0
      Poids_vf == 0          ~ NA_real_,    # if the weight is zero → NA
      TRUE                    ~ Prix_vf / Poids_vf
    )
  )

describe(is.na(resultats_codachats$Prix_Kg_post_imput))
describe(resultats_codachats$Poids_vf==0)
describe(is.na(resultats_codachats$Poids_vf))
describe(is.na(resultats_codachats$Prix_vf))


### Removing identifiers that record spending below 25% of the declared food budget---------
## Calculating the sum of prix_vf per identifier
#somme_prix_vf <- resultats_codachats %>%
#  group_by(Identifiant) %>%
#  summarise(somme_prix_vf = sum(Prix_vf, na.rm = TRUE),)
##
### Calculating a quarter of the food budget
#valeurs_uniques_budget <- resultats_codachats %>%
#  group_by(Identifiant) %>%
#  summarise(budget_unique = unique(budget_alim))
#Comparaison <- inner_join(somme_prix_vf , valeurs_uniques_budget, by="Identifiant")
#Comparaison$quart_budget <- as.numeric(Comparaison$budget_unique)/4
#Comparaison$dix_budget <- as.numeric(Comparaison$budget_unique)*10
#
##print(unique(resultats_codachats$Identifiant))
### Displaying the alert message for identifiers where sum prix_vf < budget_unique
#Comparaison <- Comparaison %>%
#  mutate(
#    message_alerte = ifelse( quart_budget > somme_prix_vf,
#                             paste("Alerte: La somme des prix_vf (", somme_prix_vf, ") est inférieure au quart du budget alimentaire (", budget_unique, ")"),
#                             "Aucune alerte"))
#identifiants_alerte <- Comparaison %>%
#  filter(somme_prix_vf < quart_budget )%>%
#  pull(Identifiant)
#
#REMOVING THE OPTICOURSES
# Removing the identifiers flagged with an alert message from resultats_codachats
#resultats_codachats<- resultats_codachats%>%
#  filter(!Identifiant %in% identifiants_alerte )
#

identifiants <- c("LE012", "LE017", "LE021", "LE028", "LE037", "LE040", "LE043", "LE045", "LE049", "LE058", 
                  "LE059", "LE064", "LE068", "LE076", "LE077", "LE083", "LE086", "LE087", "LE099", "LE129", 
                  "LE130", "LE142", "LE146", "LE147", "LE149", "LE152", "LE158", "LE163", "LE169", "LE170", 
                  "LE176", "LE177", "LE184", "LE191", "LE205", "PS287", "LE208", "LE210", "LE224", "LE232", 
                  "LE246", "LE249", "PS001", "PS003", "PS014", "PS016", "PS023", "PS026", "PS041", "PS044", 
                  "PS046", "PS049", "PS058", "PS059", "PS061", "PS072", "PS075", "PS094", "PS104", "PS106", 
                  "PS110", "PS116", "PS137", "PS143", "PS158", "PS164", "PS165", "PS168", "PS169", "PS178", 
                  "PS180", "PS190", "PS193", "PS203", "PS204", "PS206", "PS207", "PS210", "PS215", "PS218", 
                  "PS221", "PS237", "PS259", "PS265", "PS269", "PS272", "PS277", "PS282")

resultats_codachats <- subset(resultats_codachats, !(Identifiant %in% identifiants))


describe(is.na(resultats_codachats$Poids_vf))
describe(is.na(resultats_codachats$Prix_vf))

# IMPUTING NUTRITIONAL AND ENVIRONMENTAL DATA -----------------
##Aggregating the variables of interest by groupe_TI_TdC from the CALNUT dataframe----------
resultats_codachats$Unite <- ifelse(is.na(resultats_codachats$Unite),(resultats_codachats$Unite=="unités"), (resultats_codachats$Unite))
#Data entered only via a TI category have no imputation for the various nutrients / the environmental data and the pct conso and yield_factor
#We therefore impute these average values based on the CALNUT file by TI category
##Filtering the data: filter(!is.na(Poids_vf) & Poids_vf != 0) filters the rows where Poids_vf is not NA and is not equal to zero (!= 0). This excludes all rows where the weight is missing or zero.
##Aggregating the data: the filtered data is then grouped by groupe_TI_TdC1 using group_by.
##Calculating the weighted average: finally, summarise calculates the weighted average of yield_factor for each group defined by groupe_TI_TdC1, using the valid weights specified by Poids_vf and ignoring NA values.
colonnes_a_transformer<- c("yield_factor","pct_conso", "retinol_mcg", "nrj_kcal", "proteines_g", "fibres_g","ag_18_2_lino_g", "ag_18_3_a_lino_g", "ag_20_6_dha_g",
                           "magnesium_mg", "potassium_mg", "calcium_mg", "fer_mg", "cuivre_mg", "zinc_mg",
                           "selenium_mcg", "iode_mcg","vitamine_d_mcg", "vitamine_e_mg", "vitamine_c_mg",
                           "vitamine_b1_mg", "vitamine_b2_mg", "vitamine_b3_mg","vitamine_b6_mg", "vitamine_b9_mcg", "vitamine_b12_mcg",
                           "alcool_g", "sodium_mg", "fructose_g", "glucose_g", "maltose_g", "saccharose_g", "ags_g", "retinol_mcg" , "beta_carotene_mcg", "DQR", "EF", "climat", "couche_ozone", "ions",
                           "ozone", "partic", "acid", "eutro_terr", "eutro_eau", 
                           "eutro_mer", "sol", "toxi_eau", "ress_eau", "ress_ener", "ress_min")

#The idea is to assign the average nutrient values based on the foods most commonly found per category
## Linking the TI table to resultats_codachats ---------
resultats_codachats <-left_join(resultats_codachats,resultats_pondérés,by="groupe_TI_TdC1")
# Replacing missing values with the corresponding average
resultats_codachats <- resultats_codachats %>%
  mutate(across(all_of(colonnes_a_transformer), 
                ~ ifelse(is.na(.x), get(paste0("mean_", cur_column())), .x)))

## Removing the mean_ columns after the replacement if needed
resultats_codachats <- resultats_codachats[, setdiff(names(resultats_codachats), grep("^mean_", names(resultats_codachats), value = TRUE))]

describe(is.na(resultats_codachats$Poids_vf))
describe(is.na(resultats_codachats$Prix_vf))
# CALCULATING THE INDICATORS ------------------
## Weight (Kg/person/day) ----------------------------------
#To link the COD-Appro and FFQ quantities, the weight of the supplies must systematically be multiplied
#by yield_factor*pct_conso:
#This gives the weight consumed for the supply logs.
resultats_codachats$Poids_consomme_vf <- ifelse((resultats_codachats$Lieu2 != "RHD"),(resultats_codachats$Poids_vf*resultats_codachats$yield_factor*resultats_codachats$pct_conso),(resultats_codachats$Poids_vf))
## Checking the price per kg of the imputed data = ---------------------------




Nourriture_consommee <- data.frame(resultats_codachats$Identifiant, resultats_codachats$groupe_TI_TdC1, resultats_codachats$Poids_consomme_vf  )
names(Nourriture_consommee)[1:3] = c("Identifiant", "groupe_TI_TdC","Poids_vf_consommee")
#print(unique(Nourriture_consommee$groupe_TI_TdC))
# List of food categories
categories <- unique(Nourriture_consommee$groupe_TI_TdC)
# Loop to create the corresponding columns in Nourriture_consommee
for (categorie in categories) {
  Nourriture_consommee[[paste0(categorie, "_CARNET")]] <- ifelse(Nourriture_consommee$groupe_TI_TdC == categorie, Nourriture_consommee$Poids_vf_consommee, 0)}
# Remove the "groupe_TI_TdC" and "Poids_vf_consomme" columns if needed
Nourriture_consommee <- subset(Nourriture_consommee, select = -c(groupe_TI_TdC, Poids_vf_consommee, NA_CARNET))


# Aggregation for the Carnet_POIDS dataframe
Carnet_POIDS <- aggregate(UC_TI ~ Identifiant, resultats_codachats, mean)
resultats_codachats$Combien.de.personnes.vivent.dans.votre.foyer <- as.numeric(resultats_codachats$Combien.de.personnes.vivent.dans.votre.foyer)
#Carnet_POIDS <- aggregate(Combien.de.personnes.vivent.dans.votre.foyer ~ Identifiant, resultats_codachats, mean)
# List of column names to aggregate
# Exclude the "Identifiant" column
# Loop to aggregate the data by column
for (colonne in colonnes) {
  Temp <- aggregate(formula(paste0(colonne, " ~ Identifiant")), data = Nourriture_consommee, FUN = sum)
  Carnet_POIDS <- left_join(Carnet_POIDS, Temp, by = "Identifiant")
}

Carnet_POIDS$AUTRE_CARNET <- NULL

# Divide the columns by the UC column multiplied by the number of entry days
Carnet_POIDS[, 3:ncol(Carnet_POIDS)] <- Carnet_POIDS[, 3:NCOL(Carnet_POIDS)] / (Carnet_POIDS$UC_TI*Nj)
#Carnet_POIDS[, 3:ncol(Carnet_POIDS)] <- Carnet_POIDS[, 3:NCOL(Carnet_POIDS)] / (Carnet_POIDS$Combien.de.personnes.vivent.dans.votre.foyer*Nj)
# Calculate the sum of the columns for each row
Carnet_POIDS$SOMME_CARNET_POIDS <- rowSums(Carnet_POIDS[, 3:ncol(Carnet_POIDS)], na.rm = TRUE)
# Calculate the sum of the columns excluding beverages
Carnet_POIDS$SOMME_CARNET_HORS_BOISSON <- with(Carnet_POIDS, SOMME_CARNET_POIDS - 
                                                 ALCOOL_CARNET - 
                                                 FRUITS_JUS_CARNET - 
                                                 CAFE_THE_CARNET - 
                                                 LAIT_CARNET - 
                                                 EAU_CARNET - 
                                                 SODAS_LIGHT_CARNET - 
                                                 SODAS_SUCRES_CARNET)



## Kcal (Kcal/person/day)------------------------------------------
#For each food, we impute its nutritional value in kcal / kg based on the weight consumed of each food.
resultats_codachats$kcal_aliment_vf <- resultats_codachats$nrj_kcal*10*resultats_codachats$Poids_consomme_vf
#For each food consumed, we impute its nutritional value in kj / kg based on the weight consumed.
Kcal_consommee <- data.frame(resultats_codachats$Identifiant, resultats_codachats$groupe_TI_TdC1, resultats_codachats$kcal_aliment_vf  )
names(Kcal_consommee)[1:3] = c("Identifiant", "groupe_TI_TdC","kcal_aliment_vf")
# List of food categories
categories <- unique(Kcal_consommee$groupe_TI_TdC)
# Loop to create the corresponding columns in Kcal_consommee
for (categorie in categories) {
  Kcal_consommee[[paste0(categorie, "_CARNET")]] <- ifelse(Kcal_consommee$groupe_TI_TdC == categorie,
                                                           Kcal_consommee$kcal_aliment_vf, 0)}
# Remove the "groupe_TI_TdC" and "Poids_vf_consomme" columns if needed
Kcal_consommee <- subset(Kcal_consommee, select = -c(groupe_TI_TdC, kcal_aliment_vf, NA_CARNET))

# Aggregation for the Carnet_KCAL dataframe

#Carnet_KCAL <- aggregate(Combien.de.personnes.vivent.dans.votre.foyer ~ Identifiant, resultats_codachats, mean)
Carnet_KCAL <- aggregate(UC_TI ~ Identifiant, resultats_codachats, mean)
# List of column names to aggregate
# Exclude the "Identifiant" column
# Loop to aggregate the data by column
for (colonne in colonnes) {
  Temp <- aggregate(formula(paste0(colonne, " ~ Identifiant")), data = Kcal_consommee, FUN = sum)
  Carnet_KCAL <- left_join(Carnet_KCAL, Temp, by = "Identifiant")}
# Divide the columns by the UC column multiplied by Nj
#Carnet_KCAL[, 3:ncol(Carnet_KCAL)] <- Carnet_KCAL[, 3:NCOL(Carnet_KCAL)] / (Carnet_KCAL$Combien.de.personnes.vivent.dans.votre.foyer * Nj)
Carnet_KCAL$AUTRE_CARNET <- NULL
Carnet_KCAL[, 3:ncol(Carnet_KCAL)] <- Carnet_KCAL[, 3:NCOL(Carnet_KCAL)] / (Carnet_KCAL$UC_TI* Nj)
# Calculate the sum of the columns  for each row
Carnet_KCAL$SOMME_CARNET_KCAL <- rowSums(Carnet_KCAL[, 3:ncol(Carnet_KCAL)], na.rm = TRUE)
# Calculate the sum of the columns excluding beverages
Carnet_KCAL$SOMME_CARNET_HORS_BOISSON <- with(Carnet_KCAL, SOMME_CARNET_KCAL - 
                                                ALCOOL_CARNET - 
                                                FRUITS_JUS_CARNET - 
                                                CAFE_THE_CARNET - 
                                                LAIT_CARNET - 
                                                EAU_CARNET - 
                                                SODAS_LIGHT_CARNET - 
                                                SODAS_SUCRES_CARNET)

Carnet_KCAL$KCAL_SANS_ALCOOL <-  with(Carnet_KCAL, SOMME_CARNET_KCAL - ALCOOL_CARNET )
Carnet_KCAL$KCAL_SANS_BOISSON <-  with(Carnet_KCAL, SOMME_CARNET_KCAL - ALCOOL_CARNET -  SODAS_LIGHT_CARNET - SODAS_SUCRES_CARNET - EAU_CARNET - CAFE_THE_CARNET - FRUITS_JUS_CARNET - LAIT_CARNET )


# Calculating the sum of calories per individual
calories_par_individu <- Carnet_KCAL %>%
  group_by(Identifiant) %>%
  summarise(somme_calories = sum(SOMME_CARNET_KCAL, na.rm = TRUE))

# Creating the histogram of the calorie distribution
ggplot(calories_par_individu, aes(x = somme_calories)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution des Calories par Individu",
       x = "Somme des Calories",
       y = "Nombre d'Individus") +
  theme_minimal()


## Calculating MAR and MER
##MAR /MER--------------------------------------

###Calculating vitamin A --------------------------
###Calculating vitamin A --------------------------
resultats_codachats$vit_a_mcg <- (resultats_codachats$retinol_mcg + (resultats_codachats$beta_carotene_mcg/6)) 

#Adding the last modified columns
resultats_codachats$proteines_g_alim <- resultats_codachats$Poids_consomme_vf * resultats_codachats$proteines_g *10 /(resultats_codachats$UC_TI*Nj)
resultats_codachats$proteines_kcal_alim <- ((resultats_codachats$proteines_g*4) * resultats_codachats$Poids_consomme_vf *10 )/(resultats_codachats$UC_TI*Nj)

resultats_codachats$ag_18_2_lino_g_alim  <- (resultats_codachats$Poids_consomme_vf * resultats_codachats$ag_18_2_lino_g*10 )/(resultats_codachats$UC_TI*Nj)
resultats_codachats$ag_18_2_lino_kcal_alim   <- (resultats_codachats$Poids_consomme_vf *resultats_codachats$ag_18_2_lino_g*9*10 )/(resultats_codachats$UC_TI*Nj)

resultats_codachats$ag_18_3_a_lino_g_alim<- (resultats_codachats$Poids_consomme_vf *  resultats_codachats$ag_18_3_a_lino_g*10 )/(resultats_codachats$UC_TI*Nj)
resultats_codachats$ag_18_3_a_lino_kcal_alim <- (resultats_codachats$Poids_consomme_vf *resultats_codachats$ag_18_3_a_lino_g*9*10 )/(resultats_codachats$UC_TI*Nj)
resultats_codachats$ags_g_alim  <- (resultats_codachats$ags_g* resultats_codachats$Poids_consomme_vf  * 10)/(resultats_codachats$UC_TI*Nj)
resultats_codachats$ags_kcal_alim <- (resultats_codachats$ags_g *9* resultats_codachats$Poids_consomme_vf  * 10)/(resultats_codachats$UC_TI*Nj)


### Calculating nutrient quantities per food -----------------
# Selecting the columns to transform

colonnes_a_transformer<- c("retinol_mcg", "nrj_kcal", "beta_carotene_mcg","proteines_g", "fibres_g","ag_18_2_lino_g", "ag_18_3_a_lino_g", "ag_20_6_dha_g",
                           "magnesium_mg", "potassium_mg", "calcium_mg", "fer_mg", "cuivre_mg", "zinc_mg","phosphore_mg",
                           "selenium_mcg", "iode_mcg","vitamine_d_mcg", "vitamine_e_mg", "vitamine_c_mg",
                           "vitamine_b1_mg", "vitamine_b2_mg", "vitamine_b3_mg","vitamine_b6_mg", "vitamine_b9_mcg", "vitamine_b12_mcg",
                           "alcool_g", "sodium_mg", "fructose_g", "glucose_g", "maltose_g", "saccharose_g", "ags_g", "retinol_mcg" , "beta_carotene_mcg", "vit_a_mcg")


# Checking that all columns are present
colonnes_manquantes <- setdiff(colonnes_a_transformer, names(resultats_codachats))
if (length(colonnes_manquantes) > 0) {
  stop("Les colonnes suivantes ne sont pas reconnues : ", paste(colonnes_manquantes, collapse = ", "))
}

# If everything is correct, apply the transformation
resultats_codachats <- resultats_codachats %>%
  mutate(across(all_of(colonnes_a_transformer),
                ~ . * Poids_consomme_vf * 10 / (UC_TI * Nj),
                .names = "{.col}_alim"))


#HENI_alim 
resultats_codachats$HENI_score <- (resultats_codachats$HENI  * 1000* resultats_codachats$Poids_consomme_vf)

### Sum by ID of the nutrients of interest --------------------------
resultats_codachats$budget_alim <- as.numeric(resultats_codachats$budget_alim)
colonnes_a_sommer <- names(resultats_codachats)[grep("_alim$|HENI_score", names(resultats_codachats))]
print(colonnes_a_sommer)  # Debugging check

somme_par_identifiant <- resultats_codachats %>%
  group_by(Identifiant) %>%
  summarise(across(
    all_of(colonnes_a_sommer), 
    ~ sum(.x, na.rm = TRUE)
    
  ))


#Sum of sugars
somme_par_identifiant$sucre_aj_g_appro_alim <- somme_par_identifiant$fructose_g_alim+ somme_par_identifiant$glucose_g_alim + somme_par_identifiant$maltose_g_alim + somme_par_identifiant$saccharose_g_alim

#Calculating nutrients without alcohol
cols_to_extract <- c("Identifiant", "UC_TI","KCAL_SANS_ALCOOL" , "SOMME_CARNET_KCAL") 
extracted_df <- Carnet_KCAL[, cols_to_extract]
somme_par_identifiant <-inner_join(somme_par_identifiant,extracted_df , by="Identifiant")
cols_to_extract <- c("Identifiant", "Sexe") 
extracted_df <- metadata[, cols_to_extract]
somme_par_identifiant <-inner_join(somme_par_identifiant,extracted_df , by="Identifiant")

#Calculating last columns
somme_par_identifiant$proteines_kcal_2000 <- (somme_par_identifiant$proteines_kcal_alim*100)/(somme_par_identifiant$KCAL_SANS_ALCOOL)
somme_par_identifiant$fibres_g_2000 <- (somme_par_identifiant$fibres_g_alim*2000)/  somme_par_identifiant$SOMME_CARNET_KCAL
somme_par_identifiant$ag_18_3_a_lino_g_2000 <- (somme_par_identifiant$ag_18_3_a_lino_kcal_alim*100)/(somme_par_identifiant$KCAL_SANS_ALCOOL)
somme_par_identifiant$ag_18_2_lino_g_2000 <- (somme_par_identifiant$ag_18_2_lino_kcal_alim*100)/(somme_par_identifiant$KCAL_SANS_ALCOOL)
somme_par_identifiant$ag_20_6_dha_g_2000 <- (somme_par_identifiant$ag_20_6_dha_g_alim*2000)/(somme_par_identifiant$SOMME_CARNET_KCAL)

somme_par_identifiant$ags_kcal_2000 <- (somme_par_identifiant$ags_kcal_alim *100) /(somme_par_identifiant$KCAL_SANS_ALCOOL)

### Readjustment / 2000 KCAL---------------------------------------
exclude_cols <-  c("proteines_kcal_alim", "ags_kcal_alim", "ag_18_2_lino_g_alim", "ag_18_3_a_lino_g_alim","ag_18_3_a_lino_kcal_alim",
                   "ags_g_alim","proteines_g_alim" ,"fructose_g_alim"  ,"maltose_g_alim"       ,   "glucose_g_alim"    , "saccharose_g_alim", "alcool_g_alim",
                   "ag_18_2_lino_kcal_alim", "fibres_g_alim", "ag_20_6_dha_g_alim","DQR_alim", "EF_alim", "climat_alim","couche_ozone_alim" , 
                   "ions_alim","ozone_alim" , "partic_alim" ,"acid_alim","eutro_terr_alim","eutro_eau_alim"    ,     "eutro_mer_alim"    ,    
                   "sol_alim"   ,"toxi_eau_alim","ress_eau_alim", "ress_ener_alim"    , "budget_alim"   ,"ress_min_alim")
alim_cols <- grep("_alim$", names(somme_par_identifiant), value = TRUE)
alim_cols <- setdiff(alim_cols, exclude_cols)
for (col in alim_cols) {
  somme_par_identifiant[[col]] <- (somme_par_identifiant[[col]] * 2000) / somme_par_identifiant$SOMME_CARNET_KCAL
  new_col_name <- sub("_alim$", "_2000", col)
  names(somme_par_identifiant)[names(somme_par_identifiant) == col] <- new_col_name
}

### Calculating MAR ratios------------------------
# Common recommendations, regardless of gender
somme_par_identifiant$ratio_prot <- ifelse(somme_par_identifiant$proteines_kcal_2000 / 10 > 1, 1, somme_par_identifiant$proteines_kcal_2000/ 10)
somme_par_identifiant$ratio_fibre <- ifelse(somme_par_identifiant$fibres_g_2000 / 30 > 1, 1, somme_par_identifiant$fibres_g_2000 / 30)
somme_par_identifiant$ratio_lino <- ifelse(somme_par_identifiant$ag_18_2_lino_g_2000/ 4 > 1, 1, somme_par_identifiant$ag_18_2_lino_g_2000 / 4)
somme_par_identifiant$ratio_alphalino <- ifelse(somme_par_identifiant$ag_18_3_a_lino_g_2000/ 1 > 1, 1, somme_par_identifiant$ag_18_3_a_lino_g_2000/ 1)
somme_par_identifiant$ratio_dha <- ifelse(somme_par_identifiant$ag_20_6_dha_g_2000 / 0.25 > 1, 1, somme_par_identifiant$ag_20_6_dha_g_2000 / 0.25)
somme_par_identifiant$ratio_potassium <- ifelse(somme_par_identifiant$potassium_mg_2000 / 3500 > 1, 1, somme_par_identifiant$potassium_mg_2000 / 3500)
somme_par_identifiant$ratio_calcium <- ifelse(somme_par_identifiant$calcium_mg_2000 / 950 > 1, 1, somme_par_identifiant$calcium_mg_2000 / 950)
somme_par_identifiant$ratio_selenium <- ifelse(somme_par_identifiant$selenium_mcg_2000 / 70 > 1, 1, somme_par_identifiant$selenium_mcg_2000 / 70)
somme_par_identifiant$ratio_iode <- ifelse(somme_par_identifiant$iode_mcg_2000 / 150 > 1, 1, somme_par_identifiant$iode_mcg_2000 / 150)
somme_par_identifiant$ratio_vit_d <- ifelse(somme_par_identifiant$vitamine_d_mcg_2000 / 15 > 1, 1, somme_par_identifiant$vitamine_d_mcg_2000 / 15)
somme_par_identifiant$ratio_vit_c <- ifelse(somme_par_identifiant$vitamine_c_mg_2000 / 110 > 1, 1, somme_par_identifiant$vitamine_c_mg_2000 / 110)
somme_par_identifiant$ratio_vit_b2 <- ifelse(somme_par_identifiant$vitamine_b2_mg_2000 / 1.6 > 1, 1, somme_par_identifiant$vitamine_b2_mg_2000 / 1.6)
somme_par_identifiant$ratio_vit_b12 <- ifelse(somme_par_identifiant$vitamine_b12_mcg_2000 / 4 > 1, 1, somme_par_identifiant$vitamine_b12_mcg_2000 / 4)
somme_par_identifiant$ratio_vit_b9 <- ifelse(somme_par_identifiant$vitamine_b9_mcg_2000 / 330 > 1, 1, somme_par_identifiant$vitamine_b9_mcg_2000 / 330)


#somme_par_identifiant$ratio_prot <- somme_par_identifiant$proteines_kcal_2000 / 10 
#somme_par_identifiant$ratio_fibre <- somme_par_identifiant$fibres_g_2000 / 30 
#somme_par_identifiant$ratio_lino <- somme_par_identifiant$ag_18_2_lino_g_2000/ 4 
#somme_par_identifiant$ratio_alphalino <- somme_par_identifiant$ag_18_3_a_lino_g_2000/ 1 
#somme_par_identifiant$ratio_dha <- somme_par_identifiant$ag_20_6_dha_g_2000 / 0.25
#somme_par_identifiant$ratio_potassium <- somme_par_identifiant$potassium_mg_2000 / 3500 
#somme_par_identifiant$ratio_calcium <- somme_par_identifiant$calcium_mg_2000 / 950 
#somme_par_identifiant$ratio_selenium <- somme_par_identifiant$selenium_mcg_2000 / 70
#somme_par_identifiant$ratio_iode <- somme_par_identifiant$iode_mcg_2000 / 150 
#somme_par_identifiant$ratio_vit_d <- somme_par_identifiant$vitamine_d_mcg_2000 / 15
#somme_par_identifiant$ratio_vit_c <- somme_par_identifiant$vitamine_c_mg_2000 / 110
#somme_par_identifiant$ratio_vit_b2 <- somme_par_identifiant$vitamine_b2_mg_2000 / 1.6
#somme_par_identifiant$ratio_vit_b12 <- somme_par_identifiant$vitamine_b12_mcg_2000 / 4
#somme_par_identifiant$ratio_vit_b9 <- somme_par_identifiant$vitamine_b9_mcg_2000 / 330


# Define a function to calculate the ratio
calculate_ratio <- function(sexe, valeur, seuil_femme, seuil_homme) {
  if (sexe == "Femme") {
    return(ifelse(valeur / seuil_femme > 1, 1, valeur / seuil_femme))
  } else if (sexe == "Homme") {
    return(ifelse(valeur / seuil_homme > 1,1, valeur / seuil_homme))
  } else {
    return(NA)
  }
}

## Apply the function for each nutrient
somme_par_identifiant$ratio_magnesium <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$magnesium_mg_2000, 300, 380)
somme_par_identifiant$ratio_fer <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$fer_mg_2000, 16, 11)
somme_par_identifiant$ratio_cuivre <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$cuivre_mg_2000, 1.5, 1.9) 
somme_par_identifiant$ratio_zinc <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$zinc_mg_2000, 9.3, 11.7)
somme_par_identifiant$ratio_vit_a <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vit_a_mcg_2000, 650, 750)
somme_par_identifiant$ratio_vit_e <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_e_mg_2000, 9, 10)
somme_par_identifiant$ratio_vit_b1 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b1_mg_2000,0.84, 0.84) # #0.1 IN 239 KCAL TO CONVERT TO MJ AND J0. 
somme_par_identifiant$ratio_vit_b3 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b3_mg_2000, 13.4, 13.4) ##1.6*239 KCAL #14.9, 18.5
somme_par_identifiant$ratio_vit_b6 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b6_mg_2000, 1.6, 1.7)


#WE USE THE AVERAGE MALE / FEMALE RECOMMENDATION
#somme_par_identifiant$ratio_magnesium <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$magnesium_mg_2000, 340, 340) 
#somme_par_identifiant$ratio_fer <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$fer_mg_2000, 12.25, 12.25) 
#somme_par_identifiant$ratio_cuivre <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$cuivre_mg_2000, 1.7, 1.7 ) 
#somme_par_identifiant$ratio_zinc <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$zinc_mg_2000, 10.5, 10.5)
#somme_par_identifiant$ratio_vit_a <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vit_a_mcg_2000, 700, 700) 
#somme_par_identifiant$ratio_vit_e <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_e_mg_2000, 9.5, 9.5) 
#somme_par_identifiant$ratio_vit_b1 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b1_mg_2000,0.84, 0.84) # #0.1 IN 239 KCAL TO CONVERT TO MJ AND J0. 
#somme_par_identifiant$ratio_vit_b3 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b3_mg_2000, 13.4, 13.4) ##1.6*239 KCAL #14.9, 18.5
#somme_par_identifiant$ratio_vit_b6 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b6_mg_2000, 1.65, 1.65) 

### Calculating MAR -----------------------------------

somme_par_identifiant$MAR <- ((somme_par_identifiant$ratio_prot + somme_par_identifiant$ratio_fibre + somme_par_identifiant$ratio_lino + somme_par_identifiant$ratio_alphalino + somme_par_identifiant$ratio_dha + 
                                 somme_par_identifiant$ratio_magnesium + somme_par_identifiant$ratio_potassium + somme_par_identifiant$ratio_calcium + somme_par_identifiant$ratio_fer + somme_par_identifiant$ratio_cuivre +
                                 somme_par_identifiant$ratio_zinc + somme_par_identifiant$ratio_selenium + somme_par_identifiant$ratio_iode + somme_par_identifiant$ratio_vit_a + somme_par_identifiant$ratio_vit_d + 
                                 somme_par_identifiant$ratio_vit_e + somme_par_identifiant$ratio_vit_c + somme_par_identifiant$ratio_vit_b1 + somme_par_identifiant$ratio_vit_b2 + somme_par_identifiant$ratio_vit_b3 + 
                                 somme_par_identifiant$ratio_vit_b6 + somme_par_identifiant$ratio_vit_b9 + somme_par_identifiant$ratio_vit_b12)/23)*100;
mean(somme_par_identifiant$MAR, na.rm=TRUE)

### Ratio for MER ----------------------------------------
# Define a function to calculate the ratio
somme_par_identifiant$ratio_ags <- ifelse((somme_par_identifiant$ags_kcal_2000 / 12 < 1),( 1), (somme_par_identifiant$ags_kcal_2000/ 12 ))
somme_par_identifiant$ratio_sodium  <- ifelse(somme_par_identifiant$sodium_mg_2000/ 2300 < 1, 1, somme_par_identifiant$sodium_mg_2000/ 2300 )
somme_par_identifiant$ratio_sucre_aj<- ifelse(somme_par_identifiant$sucre_aj_g_appro_2000/100 < 1, 1, somme_par_identifiant$sucre_aj_g_appro_2000/100)

### Calculating MER -----------------------------------
somme_par_identifiant$MER <- (((somme_par_identifiant$ratio_ags + somme_par_identifiant$ratio_sodium + somme_par_identifiant$ratio_sucre_aj)*100)/3)-100
mean(somme_par_identifiant$MER)
mean(somme_par_identifiant$MAR)


somme_par_identifiant$t_ratio_prot <- (somme_par_identifiant$proteines_kcal_2000 / 10)*100
somme_par_identifiant$t_ratio_fibre <-(somme_par_identifiant$fibres_g_2000 / 30)*100
somme_par_identifiant$t_ratio_lino <-(somme_par_identifiant$ag_18_2_lino_g_2000/ 4 )*100# (!!)
somme_par_identifiant$t_ratio_alphalino <-(somme_par_identifiant$ag_18_3_a_lino_g_2000/ 1)*100
somme_par_identifiant$t_ratio_dha <-(somme_par_identifiant$ag_20_6_dha_g_2000 / 0.25 )*100
somme_par_identifiant$t_ratio_potassium <-(somme_par_identifiant$potassium_mg_2000 / 3500 )*100
somme_par_identifiant$t_ratio_calcium <-(somme_par_identifiant$calcium_mg_2000 / 950 )*100
somme_par_identifiant$t_ratio_selenium <-(somme_par_identifiant$selenium_mcg_2000 / 70)*100
somme_par_identifiant$t_ratio_iode <-(somme_par_identifiant$iode_mcg_2000 / 150 )*100
somme_par_identifiant$t_ratio_vit_d <-(somme_par_identifiant$vitamine_d_mcg_2000 / 15)*100
somme_par_identifiant$t_ratio_vit_c <-(somme_par_identifiant$vitamine_c_mg_2000 / 110)*100
somme_par_identifiant$t_ratio_vit_b2 <-(somme_par_identifiant$vitamine_b2_mg_2000 / 1.6)*100
somme_par_identifiant$t_ratio_vit_b12 <-(somme_par_identifiant$vitamine_b12_mcg_2000 / 4 )*100
somme_par_identifiant$t_ratio_vit_b9 <-(somme_par_identifiant$vitamine_b9_mcg_2000 / 330)*100
somme_par_identifiant$t_ratio_magnesium <-  (somme_par_identifiant$magnesium_mg_2000/ 340)*100
somme_par_identifiant$t_ratio_fer <-  (somme_par_identifiant$fer_mg_2000 / 12.25)*100
somme_par_identifiant$t_ratio_cuivre <-  (somme_par_identifiant$cuivre_mg_2000 / 1.7 )*100
somme_par_identifiant$t_ratio_zinc <- ( somme_par_identifiant$zinc_mg_2000/ 10.5)*100
somme_par_identifiant$t_ratio_vit_a <- ( somme_par_identifiant$vit_a_mcg_2000/ 700 )*100
somme_par_identifiant$t_ratio_vit_e <-  (somme_par_identifiant$vitamine_e_mg_2000/ 9.5)*100
somme_par_identifiant$t_ratio_vit_b1 <-  (somme_par_identifiant$vitamine_b1_mg_2000/ 0.84)*100 # #0.1 IN 239 KCAL TO CONVERT TO MJ AND J0. 
somme_par_identifiant$t_ratio_vit_b3 <-  (somme_par_identifiant$vitamine_b3_mg_2000/13.4)*100##1.6*239 KCAL #14.9t_ratio 18.5
somme_par_identifiant$t_ratio_vit_b6 <-  (somme_par_identifiant$vitamine_b6_mg_2000/ 1.65)*100
somme_par_identifiant$t_ratio_ags <- (somme_par_identifiant$ags_kcal_2000 / 12 )*100
somme_par_identifiant$t_ratio_sodium  <-(somme_par_identifiant$sodium_mg_2000/ 2300 )*100
somme_par_identifiant$t_ratio_sucre_aj <-(somme_par_identifiant$sucre_aj_g_appro_2000/100)*100

ratio_cols <- c("t_ratio_prot", "t_ratio_fibre", "t_ratio_lino", "t_ratio_alphalino", 
                "t_ratio_dha", "t_ratio_potassium", "t_ratio_calcium", "t_ratio_selenium", 
                "t_ratio_iode", "t_ratio_vit_d", "t_ratio_vit_c", "t_ratio_vit_b2", 
                "t_ratio_vit_b12", "t_ratio_vit_b9", "t_ratio_magnesium", "t_ratio_fer", 
                "t_ratio_cuivre", "t_ratio_zinc", "t_ratio_vit_a", "t_ratio_vit_e", 
                "t_ratio_vit_b1", "t_ratio_vit_b3", "t_ratio_vit_b6", "t_ratio_ags", 
                "t_ratio_sodium", "t_ratio_sucre_aj")

moyennes_ratios <- colMeans(somme_par_identifiant[, ratio_cols], na.rm = TRUE)
table_ratios <- data.frame(Ratio = names(moyennes_ratios), Moyenne = moyennes_ratios)
print(table_ratios)

## Unit price (€/kg) -----------------------
#Creating the prix_unitaire dataframe:
#It extracts three columns from resultats_codachats: Identifiant, groupe_TI_TdC1, and Prix_Kg_post_imput.
#These columns are used to form a new prix_unitaire dataframe
prix_unitaire <- data.frame(
  Identifiant = resultats_codachats$Identifiant,
  groupe_TI_TdC = resultats_codachats$groupe_TI_TdC1,
  prix_kg = resultats_codachats$Prix_Kg_post_imput,
  prix_vf = resultats_codachats$Prix_vf,
  poids_vf = resultats_codachats$Poids_consomme_vf
)

prix_unitaire <- prix_unitaire %>%
  filter(!is.na(prix_kg))

prix_unitaire_summary <- prix_unitaire %>%
  # 1) Group by Identifiant and category
  group_by(Identifiant, groupe_TI_TdC) %>%
  # 2) Calculate the totals
  summarise(
    somme_poids = sum(poids_vf, na.rm = TRUE),
    somme_prix  = sum(prix_vf,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 3) Calculate the unit price
  mutate(
    prix_unitaire = somme_prix / somme_poids
  )


prix_unitaire_wide <- prix_unitaire_summary %>%
  dplyr::select(Identifiant, groupe_TI_TdC, prix_unitaire) %>%
  pivot_wider(
    names_from  = groupe_TI_TdC,
    values_from = prix_unitaire,
    names_glue  = "{groupe_TI_TdC}_CARNET_PRIX"
  )

# 1. We calculate the average price per kg for each Identifiant,
#    only if the category is in the target vector
FV_prix_kg <- resultats_codachats %>%
  filter(groupe_TI_TdC1 %in% c("FRUITS", "LEGUMES",  "NOIX", "FRUITS_SECS"), !is.na(Prix_Kg_post_imput)) %>%
  group_by(Identifiant) %>%
  summarise(prix_kg_moy = sum(Prix_vf, na.rm = TRUE)/ sum(Poids_consomme_vf, na.rm = TRUE))


FRUITS_prix_kg_tous <- resultats_codachats %>%
  filter(groupe_TI_TdC1 %in% c("FRUITS",  "NOIX", "FRUITS_SECS"), !is.na(Prix_Kg_post_imput)) %>%
  group_by(Identifiant) %>%
  summarise(prix_kg_moy = sum(Prix_vf, na.rm = TRUE)/ sum(Poids_consomme_vf, na.rm = TRUE))

FEC_prix_kg <- resultats_codachats %>%
  filter(groupe_TI_TdC1 %in% c( "FEC_NON_RAF", "FEC_RAF"), !is.na(Prix_Kg_post_imput)) %>%
  group_by(Identifiant) %>%
  summarise(prix_kg_moy = sum(Prix_vf, na.rm = TRUE)/ sum(Poids_consomme_vf, na.rm = TRUE))

PDTS_LAITIERS_prix_kg <- resultats_codachats %>%
  filter(groupe_TI_TdC1 %in% c( "LAIT", "LAITAGES", "FROMAGES"), !is.na(Prix_Kg_post_imput)) %>%
  group_by(Identifiant) %>%
  summarise(prix_kg_moy = sum(Prix_vf, na.rm = TRUE)/ sum(Poids_consomme_vf, na.rm = TRUE))

POULET_OEUFS_prix_kg <- resultats_codachats %>%
  filter(groupe_TI_TdC1 %in% c( "POULET", "OEUFS"), !is.na(Prix_Kg_post_imput)) %>%
  group_by(Identifiant) %>%
  summarise(prix_kg_moy = sum(Prix_vf, na.rm = TRUE)/ sum(Poids_consomme_vf, na.rm = TRUE))

AUTRE_PDTS_ANIMAUX_prix_kg <- resultats_codachats %>%
  filter(groupe_TI_TdC1 %in% c( "CHARCUTERIE_HORS_JB", "JAMBON_BLANC"), !is.na(Prix_Kg_post_imput)) %>%
  group_by(Identifiant) %>%
  summarise(prix_kg_moy = sum(Prix_vf, na.rm = TRUE)/ sum(Poids_consomme_vf, na.rm = TRUE))

PLATS_PREP_prix_kg <- resultats_codachats %>%
  filter(groupe_TI_TdC1 %in% c( "PLATS_PREP_CARNES", "PLATS_PREP_VEGETARIENS", "QUICHES_PIZZAS_TARTES_SALEES_POIDS"), !is.na(Prix_Kg_post_imput)) %>%
  group_by(Identifiant) %>%
  summarise(prix_kg_moy = sum(Prix_vf, na.rm = TRUE)/ sum(Poids_consomme_vf, na.rm = TRUE))

VIANDE_ROUGE_PORC_prix_kg <- resultats_codachats %>%
  filter(groupe_TI_TdC1 %in% c( "VIANDE_ROUGE", "PORC"), !is.na(Prix_Kg_post_imput)) %>%
  group_by(Identifiant) %>%
  summarise(prix_kg_moy = sum(Prix_vf, na.rm = TRUE)/ sum(Poids_consomme_vf, na.rm = TRUE))

VIANDES_prix_kg <- resultats_codachats %>%
  filter(groupe_TI_TdC1 %in% c( "VIANDE_ROUGE", "POULET", "PLATS_PREP_CARNES", 
                                "JAMBON_BLANC", "CHARCUTERIE_HORS_JB", "PORC"), !is.na(Prix_Kg_post_imput)) %>%
  group_by(Identifiant) %>%
  summarise(prix_kg_moy = sum(Prix_vf, na.rm = TRUE)/ sum(Poids_consomme_vf, na.rm = TRUE))


MG_prix_kg <- resultats_codachats %>%
  filter(groupe_TI_TdC1 %in% c( "MGA", "MGV"), !is.na(Prix_Kg_post_imput)) %>%
  group_by(Identifiant) %>%
  summarise(prix_kg_moy = sum(Prix_vf, na.rm = TRUE)/ sum(Poids_consomme_vf, na.rm = TRUE))

PDTS_DISCRETIONNAIRES_prix_kg <- resultats_codachats %>%
  filter(groupe_TI_TdC1 %in% c( "SNACKS_AUTRES", "CEREALES_PD", "DESSERTS_LACTES","PDTS_SUCRES", "SAUCES"), !is.na(Prix_Kg_post_imput)) %>%
  group_by(Identifiant) %>%
  summarise(prix_kg_moy = sum(Prix_vf, na.rm = TRUE)/ sum(Poids_consomme_vf, na.rm = TRUE))

SSB_prix_kg <- resultats_codachats %>%
  filter(groupe_TI_TdC1 %in% c( "SODAS_SUCRES", "SODAS_LIGHT_POIDS", "FRUITS_JUS"), !is.na(Prix_Kg_post_imput)) %>%
  group_by(Identifiant) %>%
  summarise(prix_kg_moy = sum(Prix_vf, na.rm = TRUE)/ sum(Poids_consomme_vf, na.rm = TRUE))

df_list <- list(
  FV                     = FV_prix_kg,
  FRUITS_TOUS            = FRUITS_prix_kg_tous,
  FEC                    = FEC_prix_kg,
  LAITIERS               = PDTS_LAITIERS_prix_kg,
  POULET_OEUFS           = POULET_OEUFS_prix_kg,
  AUTRE_ANIM             = AUTRE_PDTS_ANIMAUX_prix_kg,
  PLATS_PREP             = PLATS_PREP_prix_kg,
  VIANDE_ROUGE_PORC      = VIANDE_ROUGE_PORC_prix_kg,
  VIANDES                = VIANDES_prix_kg,
  MG                     = MG_prix_kg,
  DISCRETIONNAIRES       = PDTS_DISCRETIONNAIRES_prix_kg,
  SSB                    = SSB_prix_kg
)

df_list_renamed <- imap(df_list, ~ {
  if ("prix_kg_moy" %in% names(.x)) {
    rename(.x,
           !!paste0(.y, "_prix_kg") := prix_kg_moy
    )
  } else {
    .x
  }
})

Carnet_PRIX_Unitaire <- reduce(
  df_list_renamed,
  full_join,
  by = "Identifiant"
)

Carnet_PRIX_Unitaire <- reduce(df_list_renamed, full_join, by = "Identifiant")

Carnet_PRIX_Unitaire <- left_join(Carnet_PRIX_Unitaire, prix_unitaire_wide, by="Identifiant")


Carnet_PRIX_Unitaire <- Carnet_PRIX_Unitaire %>%
  rename_with(
    ~ str_replace_all(., "CARNET_PRIX", "prix_kg"),
    .cols = contains("CARNET_PRIX")
  )

Carnet_PRIX_Unitaire <- Carnet_PRIX_Unitaire %>%
  mutate(across(starts_with("prix_kg"), ~ replace_na(.x, 0)))


## Carbon footprint (g/CO2/UC/day)  -----------------
### Selecting the columns to transform----------------
colonnes_a_transformer <- c("climat", "couche_ozone","ions","ozone",	"partic",	"acid",	"eutro_terr", "eutro_eau","eutro_mer",	"sol",	"toxi_eau",	"ress_eau",	"ress_ener",	"ress_min")


###  Multiply by the food weight and by 1000 to convert to kg for each env indicator-------------- 
resultats_codachats <- resultats_codachats  %>%
  mutate(across(all_of(colonnes_a_transformer),~ . * Poids_consomme_vf  / (UC_TI*Nj), .names = "{.col}_env" ))


resultats_codachats$climat_env <- resultats_codachats$climat_env *1000
### Sum by identifier ---------------
colonnes_a_sommer_env <- names(resultats_codachats)[grep("_env$", names(resultats_codachats))]

somme_par_identifiant_env <- resultats_codachats %>%
  group_by(Identifiant) %>%
  summarise(across(all_of(colonnes_a_sommer_env), ~ sum(.x, na.rm = TRUE)))





#List of compliant individuals ---------------------
if (campaign == "23-02" | campaign == "24-03") {
  # Joining with the Recap_envoi_cheques dataframe
  tickets_treated <- inner_join(Recap_envoi_cheques, resultats_codachats, by = "Identifiant")
  
  # Calculating the eligible amounts and the voucher amounts for each ticket
  tickets_c <- tickets_treated %>%
    group_by(Identifiant, Date_vf, Lieu_vf) %>%
    summarise(
      montant_eligible = sum(ifelse(groupe_TI_TdC1 %in% c("FRUITS", "LEGUMES", "FRUITS_SECS", "LEG_SECS", "NOIX"), Prix_vf, 0)),
      montant_cheques = first(MontantChequeAlimentaire),  # Take into account only the first row of the ticket
      .groups = 'drop_last'
    )
  
  tickets_c$montant_eligible <- ifelse(is.na(tickets_c$montant_eligible),(0),(tickets_c$montant_eligible))
  tickets_c <- tickets_c %>% filter(montant_cheques != 0)
  montant_eligible_bis <- tickets_c
  
  
  tickets_c$score_a <- (tickets_c$montant_eligible/tickets_c$montant_cheques)
  tickets_c$score_b <- ifelse((tickets_c$score_a>1),(1),(tickets_c$score_a))
  tickets_c$score_c <- tickets_c$montant_cheques*tickets_c$score_b
  
  # Calculating the compliance for each individual
  compliance_results <- tickets_c %>%
    group_by(Identifiant) %>%
    summarise(
      somme_score_c = sum(score_c),
      somme_montant_cheques = sum(montant_cheques),
      compliance = somme_score_c/somme_montant_cheques
    )
  
  median(compliance_results$compliance)
  proportion_compliance_above_70 <- compliance_results %>%
    summarise(
      proportion_above_70 = sum(compliance >= 0.70) / n()
    )
  print(proportion_compliance_above_70)
}
#Determining the initial small consumers ---------------------------------------
if (campaign == "22-11" | campaign == "23-11") { 
  
  # Filtering the rows containing eligible products
  produits_eligibles <- resultats_codachats %>%
    filter(groupe_TI_TdC1 %in% c("FRUITS", "LEGUMES", "FRUITS_SECS", "LEG_SECS", "NOIX"))
  
  # Calculating the sum of the eligible product prices by Identifiant
  tickets <- produits_eligibles %>%
    group_by(Identifiant) %>%
    summarise(montant_eligible = sum(Prix_vf, na.rm = TRUE))
  
  
  # Initializing the cheque_theo variable
  resultats_codachats$cheque_theo <- 0
  
  library(dplyr)
  
  if (campaign == "22-11") { 
    resultats_codachats <- resultats_codachats %>%
      mutate(
        UC_TI_arrondi = ceiling(UC_TI * 2) / 2,  # Round up to the nearest 0.5
        cheque_theo = case_when(
          # Case "Epimut" or "Episourire"
          grepl("Epimut|Episourire", Identifiant) & round(UC_TI_arrondi * 10) == 10 ~ 28,
          grepl("Epimut|Episourire", Identifiant) & round(UC_TI_arrondi * 10) == 15 ~ 40,
          grepl("Epimut|Episourire", Identifiant) & round(UC_TI_arrondi * 10) == 20 ~ 58,
          grepl("Epimut|Episourire", Identifiant) & round(UC_TI_arrondi * 10) == 25 ~ 68,
          grepl("Epimut|Episourire", Identifiant) & round(UC_TI_arrondi * 10) == 30 ~ 82,
          grepl("Epimut|Episourire", Identifiant) & round(UC_TI_arrondi * 10) == 35 ~ 92,
          grepl("Epimut|Episourire", Identifiant) & round(UC_TI_arrondi * 10) == 40 ~ 116,
          grepl("Epimut|Episourire", Identifiant) & round(UC_TI_arrondi * 10) == 45 ~ 126,
          grepl("Epimut|Episourire", Identifiant) & round(UC_TI_arrondi * 10) == 50 ~ 140,
          grepl("Epimut|Episourire", Identifiant) & round(UC_TI_arrondi * 10) == 55 ~ 150,
          grepl("Epimut|Episourire", Identifiant) & round(UC_TI_arrondi * 10) == 60 ~ 164,
          grepl("Epimut|Episourire", Identifiant) & round(UC_TI_arrondi * 10) == 65 ~ 184,
          grepl("Epimut|Episourire", Identifiant) & round(UC_TI_arrondi * 10) == 70 ~ 198,
          grepl("Epimut|Episourire", Identifiant) & round(UC_TI_arrondi * 10) == 75 ~ 208,
          grepl("Epimut|Episourire", Identifiant) & round(UC_TI_arrondi * 10) == 80 ~ 222,
          
          # General case for the other identifiers
          round(UC_TI_arrondi * 10) == 10 ~ 40,
          round(UC_TI_arrondi * 10) == 15 ~ 64,
          round(UC_TI_arrondi * 10) == 20 ~ 88,
          round(UC_TI_arrondi * 10) == 25 ~ 102,
          round(UC_TI_arrondi * 10) == 30 ~ 126,
          round(UC_TI_arrondi * 10) == 35 ~ 146,
          round(UC_TI_arrondi * 10) == 40 ~ 170,
          round(UC_TI_arrondi * 10) == 45 ~ 190,
          round(UC_TI_arrondi * 10) == 50 ~ 214,
          round(UC_TI_arrondi * 10) == 55 ~ 238,
          round(UC_TI_arrondi * 10) == 60 ~ 258,
          round(UC_TI_arrondi * 10) == 65 ~ 272,
          round(UC_TI_arrondi * 10) == 70 ~ 296,
          round(UC_TI_arrondi * 10) == 75 ~ 316,
          round(UC_TI_arrondi * 10) == 80 ~ 340,
          
          # Default value if UC_TI is not recognized
          TRUE ~ NA_real_
        )
      )
  }
  
  if (campaign == "23-11") { 
    resultats_codachats <- resultats_codachats %>%
      mutate(
        UC_TI_arrondi = ceiling(UC_TI * 2) / 2,  # Round up to the nearest 0.5
        cheque_theo = case_when(
          round(UC_TI_arrondi * 10) == 10 ~ 44,
          round(UC_TI_arrondi * 10) == 15 ~ 68,
          round(UC_TI_arrondi * 10) == 20 ~ 88,
          round(UC_TI_arrondi * 10) == 25 ~ 112,
          round(UC_TI_arrondi * 10) == 30 ~ 132,
          round(UC_TI_arrondi * 10) == 35 ~ 156,
          round(UC_TI_arrondi * 10) == 40 ~ 186,
          round(UC_TI_arrondi * 10) == 45 ~ 210,
          round(UC_TI_arrondi * 10) == 50 ~ 230,
          round(UC_TI_arrondi * 10) == 55 ~ 254,
          round(UC_TI_arrondi * 10) == 60 ~ 274,
          
          # Default value if UC_TI is not recognized
          TRUE ~ NA_real_
        )
      )
  }
  
  resultats_codachats_df <- resultats_codachats %>%
    distinct(Identifiant, UC_TI, cheque_theo, .keep_all = TRUE)
  
  tickets <- left_join(tickets, resultats_codachats_df, by = "Identifiant") 
  
  # Joining with the Recap_envoi_cheques dataframe
  tickets <- left_join(tickets, Recap_envoi_cheques, by = "Identifiant")
  
  resultats_diff <- tickets %>%
    filter(!is.na(cheque_theo) & !is.na(`Montant mensuel total`) & cheque_theo != `Montant mensuel total`)
  
  # Calculating the aid differential
  tickets <- tickets %>%
    mutate(Différentiel_aide = (montant_eligible / cheque_theo)) }


#Share of ORGANIC food in the diet ---------------------------------------
Bio <- resultats_codachats %>%
  filter(Labels %in% c("Bio", "Biologique"))

# Calculating the sum of the eligible product prices by Identifiant
tickets_Bio <- Bio %>%
  group_by(Identifiant) %>%
  summarise(montant_eligible = sum(Prix_vf, na.rm = TRUE), 
            KCAL_BIO = sum(kcal_aliment_vf/(Nj*UC_TI), na.rm = TRUE))

# Joining with the Recap_envoi_cheques dataframe
temp1 <- data.frame(
  Identifiant = Carnet_KCAL$Identifiant,
  SOMME_CARNET_KCAL = Carnet_KCAL$SOMME_CARNET_KCAL)

temp2 <- resultats_codachats %>%
  group_by(Identifiant) %>%
  summarise(Somme_Depense = sum(Prix_vf, na.rm = TRUE))

tickets_Bio  <- inner_join(tickets_Bio , temp1, by = "Identifiant")
tickets_Bio  <- inner_join(tickets_Bio , temp2, by = "Identifiant")
tickets_Bio$Part_Kcal_BIO <- tickets_Bio$KCAL_BIO / tickets_Bio$SOMME_CARNET_KCAL
tickets_Bio$Part_Kcal_BIO[is.na(tickets_Bio$Part_Kcal_BIO)] <- 0
tickets_Bio$Part_Depense_BIO <- tickets_Bio$montant_eligible / tickets_Bio$Somme_Depense
tickets_Bio$Part_Depense_BIO[is.na(tickets_Bio$Part_Depense_BIO)] <- 0


#Share of food bought in supermarkets  ---------------------------------------
Supermarché <- resultats_codachats %>%
  filter(Lieu1 %in% c("Supermarchés", "Hypermarchés"))

# Calculating the sum of the eligible product prices by Identifiant
tickets_super <- Supermarché %>%
  group_by(Identifiant) %>%
  summarise(montant_eligible = sum(Prix_vf, na.rm = TRUE), 
            KCAL_Supermachés = sum(kcal_aliment_vf/(Nj*UC_TI), na.rm = TRUE))

# Joining with the Recap_envoi_cheques dataframe
temp1 <- data.frame(
  Identifiant = Carnet_KCAL$Identifiant,
  SOMME_CARNET_KCAL = Carnet_KCAL$SOMME_CARNET_KCAL)

temp2 <- resultats_codachats %>%
  group_by(Identifiant) %>%
  summarise(Somme_Depense = sum(Prix_vf, na.rm = TRUE))

tickets_super  <- inner_join(tickets_super , temp1, by = "Identifiant")
tickets_super  <- inner_join(tickets_super , temp2, by = "Identifiant")
tickets_super$Part_Kcal_Supermarchés <- (tickets_super$KCAL_Supermachés / tickets_super$SOMME_CARNET_KCAL)*100
tickets_super$Part_Kcal_Supermarchés[is.na(tickets_super$Part_Kcal_Supermarchés)] <- 0
tickets_super$Part_Depense_Supermarchés <- (tickets_super$montant_eligible / tickets_super$Somme_Depense)*100
tickets_super$Part_Depense_Supermarchés[is.na(tickets_super$Part_Depense_Supermarchés)] <- 0


#Share of food bought at the grocer's  ---------------------------------------
Epicerie <- resultats_codachats %>%
  filter(Lieu1 %in% c("Epicerie"))

# Calculating the sum of the eligible product prices by Identifiant
tickets_Epicerie <- Epicerie %>%
  group_by(Identifiant) %>%
  summarise(montant_eligible = sum(Prix_vf, na.rm = TRUE), 
            KCAL_Epicerie = sum(kcal_aliment_vf/(Nj*UC_TI), na.rm = TRUE))

# Joining with the Recap_envoi_cheques dataframe
temp1 <- data.frame(
  Identifiant = Carnet_KCAL$Identifiant,
  SOMME_CARNET_KCAL = Carnet_KCAL$SOMME_CARNET_KCAL)

temp2 <- resultats_codachats %>%
  group_by(Identifiant) %>%
  summarise(Somme_Depense = sum(Prix_vf, na.rm = TRUE))

tickets_Epicerie  <- inner_join(tickets_Epicerie , temp1, by = "Identifiant")
tickets_Epicerie  <- inner_join(tickets_Epicerie , temp2, by = "Identifiant")
tickets_Epicerie$Part_Kcal_Epiceries <- (tickets_Epicerie$KCAL_Epicerie / tickets_Epicerie$SOMME_CARNET_KCAL)*100
tickets_Epicerie$Part_Kcal_Epiceries[is.na(tickets_Epicerie$Part_Kcal_Epiceries)] <- 0
tickets_Epicerie$Part_Depense_Epiceries <- (tickets_Epicerie$montant_eligible / tickets_Epicerie$Somme_Depense)*100
tickets_Epicerie$Part_Depense_Epiceries[is.na(tickets_Epicerie$Part_Depense_Epiceries)] <- 0

#Food budget   ---------------------------------------

Dépense_alim <- resultats_codachats %>%
  group_by(Identifiant) %>%
  summarise(Dépense_alim = sum(Prix_vf, na.rm = TRUE))



#Determining the aggregated variables ---------------------------------------

##PRICE_WEIGHT _ TOTAL -------------

CARNET_ELIGIBLE <- resultats_codachats %>%
  filter(
    groupe_TI_TdC1 %in% c("FRUITS", "LEGUMES", "FRUITS_SECS", "NOIX", "LEG_SECS"),
    !is.na(Prix_Kg_post_imput)
  ) %>%
  group_by(Identifiant) %>%
  summarise(
    total_poids  = sum(Poids_consomme_vf, na.rm = TRUE),  # Q
    total_prix   = sum(Prix_vf,           na.rm = TRUE),  # M
    UC_moy       = mean(UC_TI),                           
    
    # Quantity and amount per CU
    q0 = ifelse(is.na(UC_moy) | UC_moy == 0, NA, total_poids / UC_moy),
    M0 = ifelse(is.na(UC_moy) | UC_moy == 0, NA, total_prix / UC_moy),
    p0           = total_prix / total_poids,      # or total_prix / total_poids
    M0cor = q0*p0,
    .groups      = "drop"
  )

mean(CARNET_ELIGIBLE$p0bis*CARNET_ELIGIBLE$q0)
mean(CARNET_ELIGIBLE$M0)
# Let's compare the two directly:
check_diff <- CARNET_ELIGIBLE %>%
  left_join(FV_prix_kg, by = "Identifiant") %>%
  mutate(ecart = p0 - prix_kg_moy)

summary(check_diff$ecart)

Poids_achat <- resultats_codachats %>%
  filter(
    groupe_TI_TdC1 %in% c("FRUITS", "LEGUMES", "FRUITS_SECS", "NOIX", "LEG_SECS"),
    !is.na(Prix_Kg_post_imput)
  ) %>%
  group_by(Identifiant, Lieu2) %>%
  summarise(
    total_poids  = sum(Poids_consomme_vf, na.rm = TRUE),  # Q
    total_prix   = sum(Prix_vf,           na.rm = TRUE),
    UC_moy       = mean(UC_TI))


totaux_individu <- Poids_achat %>%
  group_by(Identifiant) %>%
  summarise(
    poids_total_indiv = sum(total_poids),
    prix_total_indiv  = sum(total_prix),
    .groups = "drop"
  )

# 3) Calculating the proportion specifically for the social grocery store
proportion_epicerie_sociale <- Poids_achat %>%
  filter(Lieu2 %in% c("Epicerie", "Dons")) %>%
  left_join(totaux_individu, by = "Identifiant") %>%
  mutate(
    poids_epic      = total_poids / UC_moy,
    Montant_epic    = total_prix  / UC_moy,
    prop_poids_epic = total_poids / poids_total_indiv,
    prop_prix_epic  = total_prix  / prix_total_indiv,
    prix_epic       = if_else(is.na(total_prix/total_poids), 0, total_prix/total_poids),
    prix_unit       = if_else(
      is.na((prix_total_indiv - total_prix)/(poids_total_indiv - total_poids)),
      0,
      (prix_total_indiv - total_prix)/(poids_total_indiv - total_poids)
    )
  ) %>%
  dplyr::select(
    Identifiant,
    prop_poids_epic,
    prop_prix_epic,
    prix_unit,
    prix_epic,
    poids_epic,
    Montant_epic
  )

CARNET_ELIGIBLE <- left_join(CARNET_ELIGIBLE,proportion_epicerie_sociale , by="Identifiant" )
CARNET_ELIGIBLE <- CARNET_ELIGIBLE %>%
  mutate(across(everything(), ~replace_na(., 0)))


# Result: proportion per individual at the social grocery store
proportion_epicerie_sociale
mean(proportion_epicerie_sociale$prop_prix_epic)
mean(proportion_epicerie_sociale$prop_poids_epic)
mean(proportion_epicerie_sociale$prix_unit)
mean(proportion_epicerie_sociale$prix_epic)

##Type of eligible products  --------------
#Raw/Frozen/Canned
filtre <- resultats_codachats %>% 
  filter(groupe_TI_TdC1 %in% c("FRUITS", "LEGUMES", "LEG_SECS", "FRUITS_SECS", "NOIX"))

filtre <- filtre %>% 
  mutate(
    gamme = case_when(
      str_detect(LibelleCIQUAL, regex("Surgelé|Surgelée", ignore_case = TRUE)) ~ "Surgelé_POIDS",
      str_detect(LibelleCIQUAL, regex("sauce|sauté|crème|poêlé|appertisé|Olive|cuisinée|Ratatouille|égoutté|conserve|bocal|bouilli|Macédoine|cuit", ignore_case = TRUE)) ~ "Conserve_POIDS",
      str_detect(LibelleCIQUAL, regex("Cru|fraîche|Fraîche|Mesclun|Mâche", ignore_case = TRUE)) ~ "Frais_POIDS",
      str_detect(LibelleCIQUAL, regex("Soupe", ignore_case = TRUE)) ~ "Soupe_POIDS",
      str_detect(LibelleCIQUAL, regex("Purée|Compote|Petit pot|Hoummous|Tapenade", ignore_case = TRUE)) ~ "Purée_POIDS",
      str_detect(LibelleCIQUAL, regex("Nuggets|Seitan|Escalope|Galette|Haché|Pavé|Tofu|Falafel|Boulette", ignore_case = TRUE)) ~ "Préparation_POIDS",
      str_detect(LibelleCIQUAL, regex("Noisette|Noix|Sèche|Sec|graine|séchée|grillée|Cacahuète|Amande|graîne", ignore_case = TRUE)) ~ "Sec_POIDS",
      is.na(LibelleCIQUAL) ~ "Frais_POIDS",   # <-- clean correction here
      TRUE ~ NA_character_
    ),
    
    # Adding the prefix
    gamme = ifelse(!is.na(gamme), paste0(groupe_TI_TdC1, "_", gamme), NA)
  )


# Long table: one total per Identifiant x range
table_gamme_long <- filtre %>% 
  group_by(Identifiant, gamme) %>% 
  summarise(
    Poids_consomme_vf = sum(Poids_consomme_vf, na.rm = TRUE),
    UC_TI = dplyr::first(UC_TI),
    .groups = "drop"
  ) %>% 
  mutate(
    Poids_consomme_vf = Poids_consomme_vf / (UC_TI * Nj)
  ) 



# Wide table: one column per range type
table_gamme_wide <- table_gamme_long %>% 
  pivot_wider(
    names_from = gamme,
    values_from = Poids_consomme_vf,
    values_fill = 0
  )

table_gamme_wide <- table_gamme_wide[, !names(table_gamme_wide) %in% "UC_TI"]


# Breakdown by Identifiant x range x Lieu1
table_lieu_gamme_long <- resultats_codachats  %>% 
  filter(groupe_TI_TdC1 %in% c("FRUITS", "LEGUMES", "LEG_SECS", "FRUITS_SECS", "NOIX")) %>% 
  group_by(Identifiant, groupe_TI_TdC1, Lieu2) %>% 
  summarise(
    Poids_consomme_vf = sum(Poids_consomme_vf, na.rm = TRUE),
    UC_TI = dplyr::first(UC_TI),
    .groups = "drop"
  ) %>% 
  mutate(
    Poids_par_UC_Nj = Poids_consomme_vf / (UC_TI * Nj)
  )

table_lieu_gamme_long <- table_lieu_gamme_long %>%
  mutate(gamme_lieu = paste0(groupe_TI_TdC1, "_", Lieu2))
table_lieu_gamme_long <- table_lieu_gamme_long[, !(names(table_lieu_gamme_long) %in% c("Poids_consomme_vf", "UC_TI","gamme","Lieu2"))]
table_lieu_gamme_long$groupe_TI_TdC1 <- NULL

# Switching to wide format: one column per gamme_lieu type
table_gamme <- table_lieu_gamme_long %>%
  pivot_wider(
    names_from  = gamme_lieu,
    values_from = Poids_par_UC_Nj,
    values_fill = 0
  )





# =============================================================================
# CALCULATING THE sPNNS-GS2 (without ORGANIC, without oily fish, without salt)
# =============================================================================

## 1. Checking the available fish groups ------


## 2. Calculating the servings per identifier ------
portions_id <- resultats_codachats %>%
  mutate(
    nb_portions = case_when(
      groupe_TI_TdC1 %in% c("FRUITS", "LEGUMES")     ~ Poids_consomme_vf / 0.080,
      groupe_TI_TdC1 == "NOIX"                        ~ Poids_consomme_vf / 0.030,
      groupe_TI_TdC1 == "LEG_SECS"                    ~ Poids_consomme_vf / 0.200,
      groupe_TI_TdC1 == "FEC_NON_RAF"                 ~ Poids_consomme_vf / 0.050,
      groupe_TI_TdC1 %in% c("LAIT", "LAITAGES",
                            "DESSERTS_LACTES")        ~ Poids_consomme_vf / 0.125,
      groupe_TI_TdC1 == "FROMAGES"                    ~ Poids_consomme_vf / 0.030,
      groupe_TI_TdC1 == "POISSONS"                    ~ Poids_consomme_vf / 0.100,
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(Identifiant) %>%
  summarise(
    # Fruits & vegetables (servings/day)
    portions_FV = sum(
      ifelse(groupe_TI_TdC1 %in% c("FRUITS", "LEGUMES"),
             nb_portions / (UC_TI * Nj), 0), na.rm = TRUE),
    
    # Nuts (servings/day)
    portions_NOIX = sum(
      ifelse(groupe_TI_TdC1 == "NOIX",
             nb_portions / (UC_TI * Nj), 0), na.rm = TRUE),
    
    # Legumes (servings/week)
    portions_LEG_sem = sum(
      ifelse(groupe_TI_TdC1 == "LEG_SECS",
             nb_portions / (UC_TI * Nj), 0), na.rm = TRUE) * 7,
    
    # Whole grains (servings/day)
    portions_WG = sum(
      ifelse(groupe_TI_TdC1 == "FEC_NON_RAF",
             nb_portions / (UC_TI * Nj), 0), na.rm = TRUE),
    
    # Dairy products — cheeses and dairy products separated then summed into equivalents
    portions_LAIT = sum(
      ifelse(groupe_TI_TdC1 %in% c("LAIT", "LAITAGES", "DESSERTS_LACTES"),
             nb_portions / (UC_TI * Nj), 0), na.rm = TRUE) +
      sum(ifelse(groupe_TI_TdC1 == "FROMAGES",
                 nb_portions / (UC_TI * Nj), 0), na.rm = TRUE),
    
    # Fish (servings/week) — adjust according to the exact name in your data
    portions_POISSON_sem = sum(
      ifelse(groupe_TI_TdC1 == "POISSONS",
             nb_portions / (UC_TI * Nj), 0), na.rm = TRUE) * 7,
    
    # Red meat (g/week)
    g_VR_sem = sum(
      ifelse(groupe_TI_TdC1 == "VIANDE_ROUGE",
             Poids_consomme_vf / (UC_TI * Nj), 0), na.rm = TRUE) * 7 * 1000,
    
    # Deli meats (g/week)
    g_CHARC_sem = sum(
      ifelse(groupe_TI_TdC1 %in% c("CHARCUTERIE_HORS_JB", "JAMBON_BLANC"),
             Poids_consomme_vf / (UC_TI * Nj), 0), na.rm = TRUE) * 7 * 1000,
    
    .groups = "drop"
  )

## 3. Added fats ------
MG_kcal <- resultats_codachats %>%
  filter(groupe_TI_TdC1 %in% c("MGA", "MGV")) %>%
  group_by(Identifiant) %>%
  summarise(
    MG_kcal_j = sum(kcal_aliment_vf / (UC_TI * Nj), na.rm = TRUE),
    .groups = "drop"
  )

## 4. Sugary + diet + juice drinks (ml/day) ------
SSB_ml <- resultats_codachats %>%
  filter(groupe_TI_TdC1 %in% c("SODAS_SUCRES", "SODAS_LIGHT_POIDS", "FRUITS_JUS")) %>%
  group_by(Identifiant) %>%
  summarise(
    SSB_ml_j = sum(Poids_consomme_vf / (UC_TI * Nj) * 1000, na.rm = TRUE),
    .groups = "drop"
  )

## 5. Alcohol (g/week) ------
alcool_g <- resultats_codachats %>%
  filter(groupe_TI_TdC1 == "ALCOOL") %>%
  group_by(Identifiant) %>%
  summarise(
    alcool_g_sem = sum(
      alcool_g * Poids_consomme_vf / (UC_TI * Nj), na.rm = TRUE) * 7,
    .groups = "drop"
  )

## 6. Assembly ------
pnns2_data <- somme_par_identifiant %>%
  select(Identifiant, Sexe, SOMME_CARNET_KCAL, KCAL_SANS_ALCOOL,
         sucre_aj_g_appro_2000) %>%
  left_join(portions_id, by = "Identifiant") %>%
  left_join(MG_kcal,     by = "Identifiant") %>%
  left_join(SSB_ml,      by = "Identifiant") %>%
  left_join(alcool_g,    by = "Identifiant") %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

## 7. Calculating the components ------
pnns2_data <- pnns2_data %>%
  mutate(
    
    # Fruits & vegetables (weight = 3)
    comp_FV = case_when(
      portions_FV >= 7.5 ~ 2,
      portions_FV >= 5   ~ 1,
      portions_FV >= 3.5 ~ 0.5,
      TRUE               ~ 0
    ),
    
    # Nuts (weight = 1)
    comp_NOIX = case_when(
      portions_NOIX == 0    ~ 0,
      portions_NOIX < 0.5   ~ 0.5,
      portions_NOIX <= 1.5  ~ 1,
      TRUE                  ~ 0  # overconsumption penalized
    ),
    
    # Legumes (weight = 1)
    comp_LEG = case_when(
      portions_LEG_sem == 0  ~ 0,
      portions_LEG_sem < 2   ~ 0.5,
      TRUE                   ~ 1
    ),
    
    # Whole grains (weight = 2)
    comp_WG = case_when(
      portions_WG == 0  ~ 0,
      portions_WG < 1   ~ 0.5,
      portions_WG < 2   ~ 1,
      TRUE              ~ 1.5
    ),
    
    # Dairy products (weight = 1) — parabolic relationship, optimal = 2 servings/day
    comp_LAIT = case_when(
      portions_LAIT < 0.5  ~ 0,
      portions_LAIT < 1.5  ~ 0.5,
      portions_LAIT < 2.5  ~ 1,
      TRUE                 ~ 0
    ),
    
    # Fish (weight = 2) — optimal = 2 servings/week
    comp_POISSON = case_when(
      portions_POISSON_sem < 1.5  ~ 0,
      portions_POISSON_sem < 2.5  ~ 1,
      portions_POISSON_sem < 3.5  ~ 0.5,
      TRUE                        ~ 0
    ),
    
    # Added fats (weight = 2) — normalized to 2000 kcal
    pct_MG = ifelse(SOMME_CARNET_KCAL > 0,
                    (MG_kcal_j * 2000 / SOMME_CARNET_KCAL) / 2000 * 100,
                    NA_real_),
    comp_MG = ifelse(pct_MG <= 16, 1.5, 0),
    
    # Red meat (weight = 2)
    comp_VR = case_when(
      g_VR_sem <= 500  ~  0,
      g_VR_sem <= 750  ~ -1,
      TRUE             ~ -2
    ),
    
    # Deli meats (weight = 3)
    comp_CHARC = case_when(
      g_CHARC_sem <= 150  ~  0,
      g_CHARC_sem <= 300  ~ -1,
      TRUE                ~ -2
    ),
    
    # Sweet products (weight = 3) — % kcal out of 2000 kcal
    pct_sucre = (sucre_aj_g_appro_2000 * 4 / 2000) * 100,
    comp_SUCRE = case_when(
      pct_sucre < 10  ~  0,
      pct_sucre < 15  ~ -1,
      TRUE            ~ -2
    ),
    
    # Sugary/diet/juice drinks (weight = 3)
    comp_SSB = case_when(
      SSB_ml_j == 0   ~  0,
      SSB_ml_j < 250  ~ -0.5,
      SSB_ml_j < 750  ~ -1,
      TRUE            ~ -2
    ),
    
    # Alcohol (weight = 3)
    comp_ALCOOL = case_when(
      alcool_g_sem == 0    ~  0.5,
      alcool_g_sem <= 100  ~  0,
      alcool_g_sem <= 200  ~ -1,
      TRUE                 ~ -2
    )
  )

## 8. Final weighted score ------
# Formula: Σ (component_i × weight_i / max(|component_i|))
pnns2_data <- pnns2_data %>%
  mutate(
    sPNNS_GS2 =
      comp_FV      * 3 / 2   +   # max abs = 2
      comp_NOIX    * 1 / 1   +   # max abs = 1
      comp_LEG     * 1 / 1   +   # max abs = 1
      comp_WG      * 2 / 1.5 +   # max abs = 1.5
      comp_LAIT    * 1 / 1   +   # max abs = 1
      comp_POISSON * 2 / 1   +   # max abs = 1
      comp_MG      * 2 / 1.5 +   # max abs = 1.5
      comp_VR      * 2 / 2   +   # max abs = 2
      comp_CHARC   * 3 / 2   +   # max abs = 2
      comp_SUCRE   * 3 / 2   +   # max abs = 2
      comp_SSB     * 3 / 2   +   # max abs = 2
      comp_ALCOOL  * 3 / 2       # max abs = 2
  )

cat("Distribution du sPNNS-GS2 :\n")
print(summary(pnns2_data$sPNNS_GS2))
cat("Moyenne :", round(mean(pnns2_data$sPNNS_GS2, na.rm = TRUE), 2), "\n")
cat("Écart-type :", round(sd(pnns2_data$sPNNS_GS2, na.rm = TRUE), 2), "\n")

## 9. Integrating into the final table ------
pnns2_data <- pnns2_data %>%
  select(Identifiant, sPNNS_GS2)

#Building the final tables ------------------------------------------------
Carnet_id <- metadata 
Carnet_id <- Carnet_id %>%
  select_if(~ !all(is.na(.)) & !all(. == ""))

somme_par_identifiant$HENI_alim <- somme_par_identifiant$HENI_score
#We establish the processing lists based on compliance with the targeting declared in the FFQ
if (campaign == "23-02" |campaign == "24-03") {
  new_df <- compliance_results[, c("Identifiant", "compliance")]
  Carnet_id <- left_join(Carnet_id, new_df, by='Identifiant')
  Carnet_id$compliance <- ifelse(is.na(Carnet_id$compliance),(1),(Carnet_id$compliance))
  new_df <- compliance_results[, c("Identifiant", "somme_montant_cheques")]
  Carnet_id <- left_join(Carnet_id, new_df, by='Identifiant')
  Carnet_id$groupe <- ifelse(!is.na(Carnet_id$`Montant mensuel total`),(1),(0))
}


if (campaign == "22-11" |campaign == "23-11") { Carnet_id$Periode <-0   }
if (campaign == "23-02" |campaign == "24-03") { Carnet_id$Periode <-1   }
Carnet_id$Mesure <- "Carnet"
if (campaign == "22-11" |campaign == "23-11") { Carnet_id$Periode
  new_df <- tickets[, c("Identifiant" ,"cheque_theo","Différentiel_aide")]
  Carnet_id <- left_join(Carnet_id, new_df, by='Identifiant')}
Carnet_POIDS <- Carnet_POIDS[, !grepl("UC_TI", names(Carnet_POIDS))]
Carnet_id <- inner_join(Carnet_id, Carnet_POIDS, by='Identifiant')
Carnet_id <- inner_join(Carnet_id, table_gamme_wide, by='Identifiant')
Carnet_id <- inner_join(Carnet_id, table_gamme, by='Identifiant')
Carnet_id <- inner_join(Carnet_id, CARNET_ELIGIBLE, by='Identifiant')
new_df <- Carnet_KCAL[, c("Identifiant", "SOMME_CARNET_KCAL", "KCAL_SANS_BOISSON")]
Carnet_id <- inner_join(Carnet_id, new_df, by='Identifiant')
new_df <- somme_par_identifiant[, c("Identifiant", "HENI_alim", "MAR", "MER")]
Carnet_id <- inner_join(Carnet_id, new_df, by='Identifiant')
Carnet_id <- inner_join(Carnet_id, somme_par_identifiant_env, by='Identifiant')
#Carnet_PRIX_Unitaire <- Carnet_PRIX_Unitaire[, !grepl("^Combien.de.personnes.vivent.dans.votre.foyer", names(Carnet_PRIX_Unitaire))]
Carnet_PRIX_Unitaire <- Carnet_PRIX_Unitaire[, !grepl("UC_TI", names(Carnet_PRIX_Unitaire))]
Carnet_id <- inner_join(Carnet_id, Carnet_PRIX_Unitaire, by='Identifiant')
new_df <- tickets_Bio [, c("Identifiant", "Part_Kcal_BIO", "Part_Depense_BIO")]
Carnet_id <- left_join(Carnet_id, new_df, by='Identifiant')
new_df <- tickets_super[, c("Identifiant", "Part_Kcal_Supermarchés", "Part_Depense_Supermarchés")]
Carnet_id <- left_join(Carnet_id, new_df, by='Identifiant')
new_df <- tickets_Epicerie[, c("Identifiant", "Part_Kcal_Epiceries", "Part_Depense_Epiceries")]
Carnet_id <- left_join(Carnet_id, new_df, by='Identifiant')
new_df <- Dépense_alim[, c("Identifiant", "Dépense_alim")]
Carnet_id <- left_join(Carnet_id, new_df, by='Identifiant')

Carnet_id <- left_join(Carnet_id, pnns2_data, by='Identifiant')

#Building the final data table ------------------------------------------------
fichier_nettoyé <- 
  data.frame(
    Identifiant = resultats_codachats$Identifiant,
    Date = resultats_codachats$Date_vf,
    Lieu = resultats_codachats$Lieu_vf,
    Lieu1 = resultats_codachats$Lieu1,
    Lieu2 = resultats_codachats$Lieu2,
    LibelleCIQUAL = resultats_codachats$LibelleCIQUAL,
    groupe_TI_TdC = resultats_codachats$groupe_TI_TdC1,
    Poids_achat = resultats_codachats$Poids_vf,
    Poids_consomme = resultats_codachats$Poids_consomme_vf,
    Prix = resultats_codachats$Prix_vf, 
    Prix_Kg = resultats_codachats$Prix_Kg_post_imput,
    Montant_cheque = resultats_codachats$MontantChequeAlimentaire,
    Labels = resultats_codachats$Labels,
    Appeciation = resultats_codachats$Appreciation
  )

# DOWNLOAD ----------------------------

# Create a new workbook object
wb <- createWorkbook()

addWorksheet(wb, "Tableau_d'indicateurs")
writeData(wb, sheet = "Tableau_d'indicateurs", Carnet_id  )

addWorksheet(wb, "Données_brutes_nettoyées")
writeData(wb, sheet = "Données_brutes_nettoyées", fichier_nettoyé )


if (campaign == "22-11") {
  saveWorkbook(wb,(paste0("Carnets_Tableaux_nov_22_attrition.xlsx")))
}else{ 
  if (campaign == "23-02") {
    saveWorkbook(wb,(paste0("Carnets_Tableaux_mars_23.xlsx"))) 
  } else {
    if (campaign == "23-11") {
      saveWorkbook(wb,(paste0("Carnets_Tableaux_nov_23_attrition.xlsx"))) 
    } else { 
      saveWorkbook(wb,("Carnets_Tableaux_mars_24.xlsx"))
    }}}
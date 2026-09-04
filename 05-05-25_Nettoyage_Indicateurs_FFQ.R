# LOADING THE WORKING ENVIRONMENT  --------------
## Importing packages -------------------
rm(list = ls())
library(haven);library(readxl);library(tidyverse);library(openxlsx);library(readxl);library(dplyr);library(broom);library(scales)
library(modelsummary);library(ggplot2);library(effsize);library(lfe);library(ggpubr);library(vtable);library("openxlsx");
library("dplyr");library("tidyr");library("ggplot2");library("gridExtra");library("RColorBrewer");library(reshape2);library(Metrics)
library(questionr)


### Enter the campaign date ---------------------
campaign<- "24-03" #22-11 #23-02 #23-11 #24/03 

### Importing Nov_2022 data ------------------
questionnaire_nov_22<- read.xlsx(paste("22-11_FFQ.xlsx",sep=""))
questionnaire_nov_22<- questionnaire_nov_22%>% mutate_all(~gsub("\"","",.))
questionnaire_nov_22<- questionnaire_nov_22%>% mutate_all(~gsub("\\(", "",.))
questionnaire_nov_22<- questionnaire_nov_22%>% mutate_all(~gsub("\\)", "",.))

### Importing Mars_2023 data ------------------
questionnaire_mars_23<- read.xlsx(paste("23-02_FFQ.xlsx",sep=""))
questionnaire_mars_23<- questionnaire_mars_23%>% mutate_all(~gsub("\"","",.))

### Importing Nov_2023 data -------------------
questionnaire_nov_23<- read.xlsx(paste("23-11_FFQ.xlsx",sep=""))
questionnaire_nov_23<- questionnaire_nov_23%>% mutate_all(~gsub("\"","",.))

### Importing Mars_2024 data --------------------
questionnaire_mars_24<- read.xlsx(paste("24-03_FFQ.xlsx",sep=""))
questionnaire_mars_24<- questionnaire_mars_24%>% mutate_all(~gsub("\"","",.))

### Importing the appendix tables data ----------------

CALNUT<- read_excel("Alim_CALNUT_CODAPPRO_FFQ.xlsx")
Encodage <- read_xlsx(paste("Freq_FFQ.xlsx"))
Taille_Portion <- read_xlsx(paste("Taille portion.xlsx"))
Recap_envoi_cheques <- read_excel("Recap_envoi_cheque.xlsx")



# CREATING A UNIFORM TABLE FOR ALL CAMPAIGNS  ------
## Defining the functions used to recode the variables -------------
### Function to get the column name based on the campaign -----------
#The get_column_name function in R is designed to return the name of a questionnaire column based on a given campaign.
get_column_name <- function(campaign) {
  switch(campaign,
         "22-11" = "questionnaire_nov_22","23-02" = "questionnaire_mars_23","23-11" = "questionnaire_nov_23","24-03" = "questionnaire_mars_24",stop("Campaign non reconnue"))}

### Function to recode variables based on the campaign ----------

#The recoder_variables function renames columns in a data frame
#based on an encoding table and a specific campaign.

#data: a data frame whose columns need to be renamed.
#Encodage: a data frame containing the mapping between old column names and new names. This data frame must have a column for each campaign as well as an Aliment column containing the new names.
#campaign: a character string specifying the campaign.
#The function starts by copying the data frame data into tableau_recodé to avoid modifying the original.
#The function calls get_column_name(campaign) to get the name of the column corresponding to the campaign in the Encodage table.
recoder_variables <- function(data, Encodage, campaign) {
  tableau_recodé <- data
  column_name <- get_column_name(campaign)
  
  #For each row of the Encodage table:
  #ancien_nom: the original column name for this row and this campaign.
  #nouveau_nom: the new name to give this column.
  #grep is used to find all the columns in tableau_recodé whose name contains ancien_nom.    
  for (i in 1:nrow(Encodage)) {
    ancien_nom <- Encodage[[column_name]][i]
    nouveau_nom <- Encodage$Aliment[i]
    colonnes_similaires <- grep(ancien_nom, names(tableau_recodé), value = TRUE)
    #If similar columns are found, each column is renamed using make.names to ensure the new name is valid.
    #The function returns tableau_recodé, the data frame with the renamed columns.
    if (length(colonnes_similaires) > 0) {
      for (col in colonnes_similaires) {
        names(tableau_recodé)[names(tableau_recodé) == col] <- make.names(nouveau_nom)
      }} }
  return(tableau_recodé)}

## Applying the recoding function to obtain a first data table whose variable names are uniform across all campaigns --------
if (campaign == "22-11") { data <- questionnaire_nov_22 }else{ 
  if (campaign == "23-02") { data <- questionnaire_mars_23  } else {
    if (campaign == "23-11") {data <- questionnaire_nov_23 } else {  data <- questionnaire_mars_24  }}}
data <- recoder_variables(data, Encodage , campaign)

## Creating a table that groups together all the variables from the different campaigns ---------------------
#Extracting the first column of Encodage
colonne_chaligne <- Encodage[, 1]
#t(colonne_chaligne) transposes the colonne_chaligne vector, converting rows into columns.
#data.frame(t(colonne_chaligne)) creates a new data frame Frame with this transposition.
Frame <- data.frame(t(colonne_chaligne))
#Frame[1, ] extracts the first row of Frame, which contains the desired column names.
#names(Frame) <- new_column_names sets these values as the column names of Frame.
new_column_names <- Frame[1,]
names(Frame) <- new_column_names
#Removing the first row of Frame
Frame <- Frame[-1, ]

## Imputing the values of data into the new Frame table ------------
#If the number of rows of Frame differs from that of data, this condition adjusts Frame so that it has the same number of rows as data.
#intersect(names(data), names(Frame)) finds the column names common to data and Frame.
#This line copies the values of the common columns from data to Frame.
if (nrow(Frame) != nrow(data)) { Frame <- Frame[1:nrow(data), ]}
colonnes_communes <- intersect(names(data), names(Frame))
Frame[colonnes_communes] <- data[colonnes_communes]


## Translating consumption frequencies into numeric data ---------
#The values in data are initially entered as character strings.
#The next step aims to translate these consumption frequencies into numeric values
### Translation dictionary for foods ------
Frame[Frame =="NA"]<- 0
Frame[Frame =="Jamais" ]<- 0
Frame[Frame =="Une fois par semaine" ]<- 1/7
Frame[Frame =="Entre 2 et 3 fois par semaine" ]<- 2.5/7
Frame[Frame ==   "Entre 2 fois et 3 fois par semaine" ]<- 2.5/7
Frame[Frame ==  "Deux à trois fois par semaine" ]<- 2.5/7
Frame[Frame ==  "Entre 4 et 5 fois par semaine" ]<- 4.5/7
Frame[Frame == "Quatre à cinq fois par semaine" ]<- 4.5/7
Frame[Frame ==  "1 fois par jour ou presque" ]<- 1
Frame[Frame ==  "Une fois par jour ou presque" ]<- 1
Frame[Frame == "Une fois ou presque par jour" ]<- 1
Frame[Frame ==  "Deux fois par jour" ]<- 2
Frame[Frame == "Plusieurs fois par jour" ]<- 2.5
Frame[Frame ==  "Trois fois par jour ou plus" ]<- 3

### Translation dictionary for drinks (glasses) ------
Frame[Frame == "Aucun" ]<- 0
Frame[Frame == "Un verre par semaine" ]<- 1/7
Frame[Frame == "Entre 2 et 3 verres par semaine" ]<- 2.5/7
Frame[Frame ==  "Entre 4 et 5 verres par semaine" ]<- 4.5/7
Frame[Frame ==  "Un verre par jour ou presque" ]<-1
Frame[Frame ==  "2 à 4 verres par jour" ]<- 3
Frame[Frame ==  "4 à 8 verres par jour" ]<- 6
Frame[Frame ==  "Plus de 8 verres par jour" ]<- 8
Frame[Frame ==   "Pas d'alcool durant cette période"]<- 0
Frame[Frame ==   "Au moins un verre durant cette période"]<-1
Frame[Frame ==   "Plusieurs bols ou tasses par jour"]<-2
Frame[Frame ==   "Entre 2 et 5 bols ou tasses par semaine"]<-3.5/7
Frame[Frame ==   "Un bol ou tasse par semaine"]<-1/7
Frame[Frame ==   "Un bol ou tasse par jour ou presque"]<-1/7

### Translation dictionary for drinks (cups) -------
Frame[Frame == "Aucun" ]<- 0
Frame[Frame == "Un bol (ou tasse) par semaine" ]<- 1/7
Frame[Frame == "Entre 2 et 5 bols (ou tasses) par semaine" ]<- 3.5/7
Frame[Frame == "Un bol (ou tasse) par jour ou presque" ]<- 1
Frame[Frame == "Plusieurs bols (ou tasses) par jour" ]<- 2
Frame[Frame == "Moins de 250 ml par jour" ]<- 0.25
Frame[Frame == "Entre 250 et 750 ml par jour" ]<- 0.4
Frame[Frame == "Entre 750 ml et 1,25 L par jour" ]<- 1
Frame[Frame == "Entre 1,25 L et 1,75 L par jour" ]<- 1.5
Frame[Frame == "Plus de 1,75 L par jour" ]<- 1.75

### Standardizing identifiers in Frame---------------

### Function to create identifiers---------------------

create_identifiant <- function(Frame, num_col, store_col) { 
  Frame$Identifiant <- paste(Frame[[num_col]], Frame[[store_col]], sep = "-")
  return(Frame)}

### Harmonizing identifiers --------------

if (campaign == "22-11" | campaign == "23-02" ) {
  Frame  <- create_identifiant(Frame, "Numero.d.identifiant", "Nom.de.l.epicerie")
  Frame <- Frame %>% 
    relocate("Identifiant", .after = "N.Obs")
  Frame <- Frame[, !names(Frame) %in% "Numero.d.identifiant"]
}

if (campaign == "23-11" | campaign=="24-03") { 
  Frame <- Frame %>% rename("Identifiant"="Numero.d.identifiant")
}

print(unique(Frame$Identifiant))

class(Frame$Identifiant)
### Dictionary of IDs that have changed ------------

Frame$Identifiant[Frame$Identifiant == "PS004" ] <- "LE255"
Frame$Identifiant[Frame$Identifiant == "LE148" ] <- "PS284"
Frame$Identifiant[Frame$Identifiant == "LE195" ] <- "PS285"
Frame$Identifiant[Frame$Identifiant == "LE088" ] <- "PS286"
Frame$Identifiant[Frame$Identifiant == "LE207" ] <- "PS287"
Frame$Identifiant[Frame$Identifiant == "LE093" ] <- "PS288"
print(unique(Frame$Identifiant))


if (campaign == "22-11" | campaign == "23-02" ) {
  Frame <-Frame %>%
    mutate(Identifiant = gsub("-CCAS \\(inclus Pôle emploi et SPF\\)", "-CCAS", Identifiant))}


Frame$Identifiant[ Frame$Identifiant=="8447-CCAS" ] <- "8747-CCAS"
Frame$Identifiant[ Frame$Identifiant=="PE19-CCAS" ] <- "PE019-CCAS"
Frame$Identifiant[ Frame$Identifiant=="1564-Epimut" ]<- "1654-Epimut"
Frame$Identifiant[ Frame$Identifiant=="P E013-CCAS" ] <- "PE013-CCAS"
Frame$Identifiant[ Frame$Identifiant=="SP032-CCAS" ] <- "SP040-CCAS"
Frame$Identifiant[ Frame$Identifiant=="SP-052-CCAS" ] <- "SP052-CCAS"
Frame$Identifiant[ Frame$Identifiant=="SP-017-CCAS" ] <- "SP017-CCAS"
Frame$Identifiant[ Frame$Identifiant=="pe003-CCAS" ]<- "pe003-CCAS"

Frame_bis <- Frame

### Harmonizing socio-demographic data between campaigns

# CREATING METADATA ------------------------------------

## Extracting the variables of interest from Frame-----------
# Using subset to select the columns of interest
metadata <- Frame_bis[, c("Identifiant", "Sexe", "Quel.age.avez.vous.", "Quel.est.votre.pays.de.naissance.",
                          "Combien.de.personnes.vivent.dans.votre.foyer", "Quelle.est.votre.situation.matrimoniale.",
                          "Avez.vous.des.enfants.a.charge.", "De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans",
                          "De.15.a.17.ans", "De.18.ans.et.plus", "Quand.avez.vous.eu.recours.a.l.aide.alimentaire.pour.la.premiere.fois.",
                          "Part.de.votre.budget.alimentaire.consacree.aux.produits.alimentaires.biologiques.",
                          "Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.",
                          "Quelle.est.votre.situation.professionnelle.actuelle.", "La.semaine.dernierevous.arrive.t.il.de.jeter.des.restes.apres.un.repas.",
                          "Si.ouien.avez.vous.jete.tous.les.jours.", "Si.noncombien.de.fois.la.semaine.derniere.",
                          "Gaspillage.restes.", "Gapillage.produits.non.entames", "Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.",
                          "Revenu.mensuel", "Budget.mensuel.alimentation.", "Budget.hebdomadaire.alimentation.",
                          "Avez.vous.reçu.des.cheques.alimentaires.de.la.ville.de.Dijon.recemment.",
                          "Les.avez.vous.utilises.", "Qu.avez.vous.achete.avec.",
                          "Les.cheques.que.vous.avez.reçus.etaient.ils.limites.a.certaines.categories.de.produits.",
                          "Qu.avez.vous.fait.des.cheques.que.vous.n.avez.pas.utilises.",
                          "Date.de.saisie")]



if (campaign == "24-03") {
  questionnaire_nov_23 <- questionnaire_nov_23 %>% 
    rename(
      Identifiant = "247..Q1",
      Sexe = "12..Q2",
      `Quel.age.avez.vous.` = "13..Q3", 
      `Combien.de.personnes.vivent.dans.votre.foyer` = "15..Q5", 
      `Quelle.est.votre.situation.matrimoniale.` = "14..Q6",
      `Avez.vous.des.enfants.a.charge.` = "19..Q7", 
      `De.moins.de.3.ans` = "20..Q71a",
      `De.3.a.10.ans` = "21..Q71b",
      `De.11.a.14.ans` = "22..Q71c",
      `De.15.a.17.ans` = "23..Q71d", 
      `De.18.ans.et.plus` = "24..Q71e",
    )
  
  metadata_bis <- questionnaire_nov_23 %>%
    dplyr::select(
      Identifiant, Sexe, `Quel.age.avez.vous.`,
      `Combien.de.personnes.vivent.dans.votre.foyer`,
      `Quelle.est.votre.situation.matrimoniale.`,
      `Avez.vous.des.enfants.a.charge.`,
      `De.moins.de.3.ans`, `De.3.a.10.ans`, `De.11.a.14.ans`,
      `De.15.a.17.ans`, `De.18.ans.et.plus`,
    )
  
  metadata <- merge(metadata_bis, metadata, by = "Identifiant", all.x = TRUE)
  
  # Select columns and rename as needed
  metadata <- metadata %>%
    dplyr::select(
      -matches("\\.y$")  # Remove columns ending with .y
    ) %>%
    dplyr::rename_with(
      ~ gsub("\\.x$", "", .), ends_with(".x")  # Remove .x suffix from column names
    )
}



if (campaign == "23-02") {
  # Create the new identifier column
  questionnaire_nov_22 <- questionnaire_nov_22 %>%
    mutate(Identifiant = paste(`1..Numéro.d'identifiant.:`, `2..Nom.de.l'épicerie.:`, sep = "-"))
  
  questionnaire_nov_22  <- questionnaire_nov_22 %>% 
    rename(
      `Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu` = "224..Quel.est.le.diplôme.d'enseignement.général.ou.technique.le.plus.élevé.que.vous.ayez.obtenu.?")
  
  metadata_bis <- questionnaire_nov_22 %>%
    dplyr::select(
      Identifiant,`Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu` )
  
  metadata <- merge(metadata_bis, metadata, by = "Identifiant", all.x = TRUE)
  
  # Select columns and rename as needed
  metadata <- metadata %>%
    dplyr::select(
      -matches("\\.y$")  # Remove columns ending with .y
    ) %>%
    dplyr::rename_with(
      ~ gsub("\\.x$", "", .), ends_with(".x")  # Remove .x suffix from column names
    )
}





metadata$Identifiant[ metadata$Identifiant=="8447-CCAS" ] <- "8747-CCAS"
metadata$Identifiant[ metadata$Identifiant=="PE19-CCAS" ] <- "PE019-CCAS"
metadata$Identifiant[ metadata$Identifiant=="1564-Epimut" ]<- "1654-Epimut"
metadata$Identifiant[ metadata$Identifiant=="P E013-CCAS" ] <- "PE013-CCAS"
metadata$Identifiant[ metadata$Identifiant=="SP032-CCAS" ] <- "SP040-CCAS"
metadata$Identifiant[ metadata$Identifiant=="SP-052-CCAS" ] <- "SP052-CCAS"
metadata$Identifiant[ metadata$Identifiant=="SP-017-CCAS" ] <- "SP017-CCAS"
metadata$Identifiant[ metadata$Identifiant=="pe003-CCAS" ]<- "pe003-CCAS"
metadata$Identifiant[metadata$Identifiant == "PS004" ] <- "LE255"
metadata$Identifiant[metadata$Identifiant == "LE148" ] <- "PS284"
metadata$Identifiant[metadata$Identifiant == "LE195" ] <- "PS285"
metadata$Identifiant[metadata$Identifiant == "LE088" ] <- "PS286"
metadata$Identifiant[metadata$Identifiant == "LE207" ] <- "PS287"
metadata$Identifiant[metadata$Identifiant == "LE093" ] <- "PS288"
print(unique(Frame$Identifiant))


#ADJUSTING CONSUMPTION UNITS (CU)
# Calculating the total number of children
metadata$De.moins.de.3.ans <- as.numeric(metadata$De.moins.de.3.ans)
metadata$De.3.a.10.ans <- as.numeric(metadata$De.3.a.10.ans)
metadata$De.11.a.14.ans <- as.numeric(metadata$De.11.a.14.ans)
metadata$De.15.a.17.ans <- as.numeric(metadata$De.15.a.17.ans)
metadata$De.18.ans.et.plus <- as.numeric(metadata$De.18.ans.et.plus)

# Replace NA with 0 in all the specified columns
metadata$De.moins.de.3.ans[is.na(metadata$De.moins.de.3.ans)] <- 0
metadata$De.3.a.10.ans[is.na(metadata$De.3.a.10.ans)] <- 0
metadata$De.11.a.14.ans[is.na(metadata$De.11.a.14.ans)] <- 0
metadata$De.15.a.17.ans[is.na(metadata$De.15.a.17.ans)] <- 0
metadata$De.18.ans.et.plus[is.na(metadata$De.18.ans.et.plus)] <- 0
metadata$somme_enfants <- (metadata$De.moins.de.3.ans +
                             metadata$De.3.a.10.ans +
                             metadata$De.11.a.14.ans +
                             metadata$De.15.a.17.ans +
                             metadata$De.18.ans.et.plus)
metadata$adultes_mat <- ifelse(metadata$Quelle.est.votre.situation.matrimoniale.=="En couple non marié (PACS, concubinage…)"|
                                 metadata$Quelle.est.votre.situation.matrimoniale.=="Marié(e)" ,(2), (1))
metadata$Combien.de.personnes.vivent.dans.votre.foyer <- as.numeric(metadata$Combien.de.personnes.vivent.dans.votre.foyer)
metadata$somme_enfants <- as.numeric(metadata$somme_enfants)
metadata$adultes_mat <- as.numeric(metadata$adultes_mat)
metadata$adultes <- ifelse(metadata$Combien.de.personnes.vivent.dans.votre.foyer == 1, 
                           1, 
                           pmax(metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$somme_enfants, 
                                metadata$adultes_mat, 
                                na.rm = TRUE))

metadata$enfant_18_cor <- ifelse(
  (metadata$adultes + metadata$De.moins.de.3.ans + 
     metadata$De.3.a.10.ans + metadata$De.11.a.14.ans +
     metadata$De.15.a.17.ans + metadata$De.18.ans.et.plus - metadata$Combien.de.personnes.vivent.dans.votre.foyer) == 0,
  metadata$De.18.ans.et.plus,
  ifelse(
    metadata$De.18.ans.et.plus > 0,
    pmax(0, metadata$De.18.ans.et.plus - (metadata$adultes + metadata$De.moins.de.3.ans + 
                                            metadata$De.3.a.10.ans + metadata$De.11.a.14.ans +
                                            metadata$De.15.a.17.ans + metadata$De.18.ans.et.plus - metadata$Combien.de.personnes.vivent.dans.votre.foyer)),
    ifelse(
      !is.na((metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.18.ans.et.plus / (metadata$De.moins.de.3.ans + 
                                                                                                                          metadata$De.3.a.10.ans + metadata$De.11.a.14.ans +
                                                                                                                          metadata$De.15.a.17.ans + metadata$De.18.ans.et.plus)),
      (metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.18.ans.et.plus / (metadata$De.moins.de.3.ans + 
                                                                                                                   metadata$De.3.a.10.ans + metadata$De.11.a.14.ans +
                                                                                                                   metadata$De.15.a.17.ans + metadata$De.18.ans.et.plus),
      0
    )
  )
)

#Corrected under-3s
metadata$enfant_moins_3_ans_cor <- ifelse(
  rowSums(metadata[, c("adultes", "De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE) - metadata$Combien.de.personnes.vivent.dans.votre.foyer == 0,
  metadata$De.moins.de.3.ans,
  ifelse(
    !is.na((metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.moins.de.3.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE)),
    (metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.moins.de.3.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE),
    0
  )
)

#Children 3-10 years old
metadata$enfant_3_10_ans_cor <- ifelse(
  rowSums(metadata[, c("adultes", "De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE) - metadata$Combien.de.personnes.vivent.dans.votre.foyer == 0,
  metadata$De.3.a.10.ans,
  ifelse(
    !is.na((metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.3.a.10.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE)),
    (metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.3.a.10.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE),
    0
  )
)

#Children 11-14 years old
metadata$enfant_11_14_ans_cor <- ifelse(
  rowSums(metadata[, c("adultes", "De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE) - metadata$Combien.de.personnes.vivent.dans.votre.foyer == 0,
  metadata$De.11.a.14.ans,
  ifelse(
    !is.na((metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.11.a.14.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE)),
    (metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.11.a.14.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE),
    0
  )
)

#Children 15-17 years old
metadata$enfant_15_17_ans_cor <- ifelse(
  rowSums(metadata[, c("adultes", "De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE) - metadata$Combien.de.personnes.vivent.dans.votre.foyer == 0,
  metadata$De.15.a.17.ans,
  ifelse(
    !is.na((metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.15.a.17.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE)),
    (metadata$Combien.de.personnes.vivent.dans.votre.foyer - metadata$adultes) * metadata$De.15.a.17.ans / rowSums(metadata[, c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans", "De.15.a.17.ans", "enfant_18_cor")], na.rm = TRUE),
    0
  )
)

metadata$UC_TI <- metadata$adultes + metadata$enfant_15_17_ans_cor+ metadata$enfant_18_cor + 0.5*(metadata$enfant_moins_3_ans_cor + metadata$enfant_3_10_ans_cor+ metadata$enfant_11_14_ans_cor)
metadata$UC_INSEE <- ifelse(
  metadata$adultes == 1, 
  1, 
  1 + 0.5 * (metadata$adultes - 1)
) + 
  (metadata$enfant_moins_3_ans_cor + metadata$enfant_3_10_ans_cor + metadata$enfant_11_14_ans_cor) * 0.3 + 
  (metadata$De.15.a.17.ans + metadata$enfant_18_cor) * 0.5

## Calculating consumption units and income / CU------------
metadata$Income_UC_INSEE <- as.numeric(metadata$Revenu.mensuel)/as.numeric(metadata$UC_INSEE)
metadata <- subset(metadata, !(is.na(Sexe)))



##Determining age classes
calculate_central_values <- function(classes) {
  # Removing non-numeric characters and extracting the bounds
  lower_bounds <- as.numeric(gsub("\\D*(\\d+)-\\d+\\D*", "\\1", classes))
  upper_bounds <- as.numeric(gsub("\\D*\\d+-(\\d+)\\D*", "\\1", classes))
  
  # Calculating the central values
  central_values <- (lower_bounds + upper_bounds) / 2
  
  return(central_values)
}

## Applying the function to the age classes in metadata--------------
metadata$Age_Central <- calculate_central_values(metadata$Quel.age.avez.vous.)


#Correcting the labels of demographic variables

metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.<- ifelse((metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "BTS, DUT, DEST, DEUG (y compris formation paramédicale ou sociale)"),("BTS, DUT, DEST, DEUG y compris formation paramédicale ou sociale"),(metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.))
metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.<- ifelse((metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "Baccalauréat général"),("Baccalauréat"),(metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.))
metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.<- ifelse((metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.== "2ème ou 3ème cycle universitaire, grande école"),("2e ou 3e cycle universitaire, grande école"),(metadata$Quel.est.le.diplôme.d.enseignement.general.ou.technique.le.plus.eleve.que.vous.ayez.obtenu.))

metadata$Quelle.est.votre.situation.professionnelle.actuelle.<- ifelse((metadata$Quelle.est.votre.situation.professionnelle.actuelle.== "Autre inactif invalide, handicapé, en congé maladie > 3 mois, titulaire d’une pension de réversion"),("Autre inactif (invalide, handicapé, en congé maladie > 3 mois, titulaire d’une pension de réversion)"),(metadata$Quelle.est.votre.situation.professionnelle.actuelle.))
metadata$Quelle.est.votre.situation.professionnelle.actuelle.<- ifelse((metadata$Quelle.est.votre.situation.professionnelle.actuelle.== "Femme ou homme au foyer y compris congé parental"),("Femme ou homme au foyer (y compris congé parental)"),(metadata$Quelle.est.votre.situation.professionnelle.actuelle.))
metadata$Quelle.est.votre.situation.professionnelle.actuelle.<- ifelse((metadata$Quelle.est.votre.situation.professionnelle.actuelle.== "Retraité(e) (ancien salarié) ou préretraité(e)"),("Retraitée ancien salarié ou préretraitée"),(metadata$Quelle.est.votre.situation.professionnelle.actuelle.))

metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu. <- ifelse((metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Minimas sociaux RSA, Allocations familiales..."),("Minimas sociaux (RSA, Allocations familiales...)"),(metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.))
metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu. <- ifelse((metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.== "Travail salarié, autoentrepreneur..."),("Travail (salarié, autoentrepreneur...)"),(metadata$Quelle.est.dans.votre.foyer.la.principale.source.de.revenu.))

metadata$Quel.est.votre.pays.de.naissance. <- ifelse((metadata$Quel.est.votre.pays.de.naissance. == "Afrique Sub-saharienne ou Moyen-Orient (jusqu'en Iran)"),("Afrique sub-saharienne ou Moyen-Orient jusqu’à l’Iran"),(metadata$Quel.est.votre.pays.de.naissance. ))

metadata$Quelle.est.votre.situation.matrimoniale. <- ifelse((metadata$Quelle.est.votre.situation.matrimoniale. == "Divorcée ou séparée"),("Divorcé(e) ou séparé(e)"),(metadata$Quelle.est.votre.situation.matrimoniale. ))
metadata$Quelle.est.votre.situation.matrimoniale. <- ifelse((metadata$Quelle.est.votre.situation.matrimoniale. == "En couple non marié PACS, concubinage…"),("En couple non marié (PACS, concubinage…)"),(metadata$Quelle.est.votre.situation.matrimoniale. ))
metadata$Quelle.est.votre.situation.matrimoniale. <- ifelse((metadata$Quelle.est.votre.situation.matrimoniale. == "Mariée"),("Marié(e)"),(metadata$Quelle.est.votre.situation.matrimoniale. ))
metadata$Quelle.est.votre.situation.matrimoniale. <- ifelse((metadata$Quelle.est.votre.situation.matrimoniale. == "Veufve"),("Veuf(ve)"),(metadata$Quelle.est.votre.situation.matrimoniale. ))

##Determining single-parent households -----------
metadata$Foyer_monoparental <- ifelse((metadata$Avez.vous.des.enfants.a.charge. =="Oui" & (metadata$Quelle.est.votre.situation.matrimoniale. == "Veuf(ve)" |metadata$Quelle.est.votre.situation.matrimoniale. == "Célibataire" |metadata$Quelle.est.votre.situation.matrimoniale. == "Divorce(e) ou séparé(e)")), (1), (0))

#Correcting food budgets-------------------------
if (campaign == "22-11") {
  metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "6354-Episourire"),(250),(metadata$Budget.mensuel.alimentation.))
  metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "770-Epimut"),(200),(metadata$Budget.mensuel.alimentation.))
  
}else{ 
  if (campaign == "23-02") {
    metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "10499-Episourire"),(200),(metadata$Budget.mensuel.alimentation.))
    metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "1280-Epimut"),(100),(metadata$Budget.mensuel.alimentation.))
    metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "1530-Epimut"),(150),(metadata$Budget.mensuel.alimentation.))
    metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "5455-Episourire"),(100),(metadata$Budget.mensuel.alimentation.))
    metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "5894-CCAS"),(100),(metadata$Budget.mensuel.alimentation.))
    metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "PE023-CCAS"),(150),(metadata$Budget.mensuel.alimentation.))
    metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "6222-Episourire"),(200),(metadata$Budget.mensuel.alimentation.))
    metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "1730-Epimut"),(200),(metadata$Budget.mensuel.alimentation.))
  } else {
    if (campaign == "24-03") {
      metadata$Budget.mensuel.alimentation. <- ifelse((metadata$Identifiant == "PS009"),(200),(metadata$Budget.mensuel.alimentation.))
      metadata$Budget.hebdomadaire.alimentation. <- ifelse((metadata$Identifiant == "PS213"),(180),(metadata$Budget.hebdomadaire.alimentation.))
    }}}

## Replace 0 with NA--------------------
metadata$Income_UC_INSEE[metadata$Income_UC_INSEE == 0] <- NA

#Waste
# Convert to 7 if "Oui", otherwise NA
metadata$Si.ouien.avez.vous.jete.tous.les.jours. <- ifelse(metadata$Si.ouien.avez.vous.jete.tous.les.jours. == "Oui", 7, NA)
metadata$Si.noncombien.de.fois.la.semaine.derniere. <- ifelse(is.na(metadata$Si.noncombien.de.fois.la.semaine.derniere.), 
                                                              metadata$Si.ouien.avez.vous.jete.tous.les.jours., 
                                                              metadata$Si.noncombien.de.fois.la.semaine.derniere.)

# Convert to numeric if not already done
metadata$Si.noncombien.de.fois.la.semaine.derniere. <- as.numeric(metadata$Si.noncombien.de.fois.la.semaine.derniere.)
metadata$Gapillage.produits.non.entames  <- ifelse(!is.na(as.numeric(metadata$Gapillage.produits.non.entames )), metadata$Gapillage.produits.non.entames , "NA")
metadata$Gapillage.produits.non.entames <- as.numeric(metadata$Gapillage.produits.non.entames)
metadata$freq_hebdo_gaspillage <- metadata$Si.noncombien.de.fois.la.semaine.derniere. + metadata$Gapillage.produits.non.entames



metadata<- metadata[, !names(metadata) %in% c("De.moins.de.3.ans", "De.3.a.10.ans", "De.11.a.14.ans",
                                              "De.15.a.17.ans", "De.15.a.17.ans", "De.18.ans.et.plus")]



print(unique(metadata$Identifiant))


#Adding SP041 data to MARS23
if (campaign == "23-02") {
  file_path <- "FFQ_Tableaux_nov_22.xlsx"
  metadata22 <- read_excel(file_path, sheet = "Metadata") 
  ligne_SP041_CCAS <- metadata22 %>% filter(Identifiant == "SP041-CCAS")
  # Extracting the columns from metadata22 and ligne_SP041_CCAS
  colonnes_manquantes <- setdiff(colnames(metadata), colnames(ligne_SP041_CCAS))
  for (col in colonnes_manquantes) {
    ligne_SP041_CCAS[[col]] <- NA
  }
  #ligne_SP041_CCAS <- ligne_SP041_CCAS %>%
  #  select(all_of(colnames(metadata)))
  ligne_SP041_CCAS <- ligne_SP041_CCAS[ , colnames(metadata), drop = FALSE]
  
  
  metadata <- bind_rows(metadata, ligne_SP041_CCAS)
}

# CALCULATING CORRECTION COEFFICIENTS TO CORRECT THE FREQUENCIES -----------------------------------------
#
## Function to replace NA with zero ---------
replace_na_with_zero <- function(x) {
  ifelse(is.na(x), 0, x)
}
## Creating a function to extract the numeric data in the table---------------
FREQ_intake <- function(data, x) {
  result <- as.numeric(data[[x]])
  result[is.na(result)] <- 0
  return(result)
}

#The purpose of this R code is to calculate and correct the consumption frequency of certain food categories
#that belong to a general food category: raw vegetables, fruits, vegetables, bread, dairy products, fish, meat, deli meats

## Correcting consumption frequencies ------------
### Description of how the code works: example with raw vegetables -----------
# Here cat and sous_cat are character strings representing respectively the name of the
#general category for raw vegetables and the names of the columns containing the consumption frequencies of the different raw vegetable categories.
cat <- c("des.crudites.gen")
sous_cat <- c("Des.salades.composees.uniquement.de.plusieurs.legumes.crus.tomates.et.concombrescarottes.et.salade.verte.",
              "de.la.salade.vertede.la.machede.la.roquettedes.epinardsdu.cresson",
              "des.carottes.rapees", "de.l.avocat.au.moins.un.demi.avocat" ,"d.autres.crudites")

#For each row of the Frame DataFrame, the sum of the consumption frequencies of the different raw vegetable categories is calculated.
#For each sub-category in sous_cat, the code looks for the corresponding columns in Frame.
#The values of these columns are converted to numeric and added up to obtain somme_freq.
#somme_freq is then assigned to the new somme_freq_crudites column for the corresponding row.
Frame$somme_freq_crudites <- NA_real_
#Frame$somme_freq_aliment is a new column in Frame initialized with missing (NA) numeric values.
for (row in 1:nrow(Frame)) {somme_freq <- 0
for (sc in sous_cat) { ncol <- grep(sc, colnames(Frame))
if (length(ncol) > 0) { values <- as.numeric(Frame[row, ncol])
somme_freq <- somme_freq + sum(values, na.rm = TRUE)}}
Frame$somme_freq_crudites[row] <- somme_freq}
#For each sub-category in sous_cat, the code looks for the corresponding columns (ncol1) as well
#as the columns corresponding to cat (ncol2).
#If these columns exist and are not NA, the values of the sub-category columns are normalized.
#replace_na_with_zero is used to replace missing values with zeros.
#FREQ_intake is used to extract the consumption frequency.
#The values of the sub-category columns are multiplied by the ratio
#of the average frequency of the general category (cat) to the average of the sums of the sub-category frequencies.
for (i in seq_along(sous_cat)) { ncol1 <- grep(sous_cat[i], colnames(Frame))
ncol2 <- grep(cat, colnames(Frame))
if (length(ncol1) > 0 && length(ncol2) > 0) {
  if (!is.na(ncol1) && !is.na(ncol2)) { Frame[, ncol1] <- replace_na_with_zero(FREQ_intake(Frame, ncol1)) * 
    replace_na_with_zero(mean(FREQ_intake(Frame, ncol2)) / mean(Frame$somme_freq_crudites))}}}

### Correcting cooked vegetables -----------
cat <- c("des.legumes.cuits")
sous_cat <- c("de.la.soupe.de.legumes","des.haricots.verts", "des.endivesdes.epinardsdu.cresson", "des.poireaux",
              "du.chou.vertchou.fleurBruxellesbrocolis", "des.carottes.cuites", "des.courgettesdes.auberginesdes.poivronsdes.tomates.cuites.ratatouille.",
              "des.petits.pois", "des.artichautsdu.fenouildes.aspergesdu.celeri", "des.champignons", 
              "des.legumes.secs.lentillesharicots.secspois.chichesfeves.","du.mais1",
              "du.potirondes.patates.douces", "de.l.oignon.2")
Frame$somme_freq_legumes <- NA_real_
for (row in 1:nrow(Frame)) {somme_freq <- 0
for (sc in sous_cat) {ncol <- grep(sc, colnames(Frame))
if (length(ncol) > 0) { values <- as.numeric(Frame[row, ncol])
somme_freq <- somme_freq + sum(values, na.rm = TRUE)}}
Frame$somme_freq_legumes[row] <- somme_freq}
for (i in seq_along(sous_cat)) { ncol1 <- grep(sous_cat[i], colnames(Frame))
ncol2 <- grep(cat, colnames(Frame))
if (length(ncol1) > 0 && length(ncol2) > 0) { if (!is.na(ncol1) && !is.na(ncol2)) {
  Frame[, ncol1] <- replace_na_with_zero(FREQ_intake(Frame, ncol1)) * replace_na_with_zero(mean(FREQ_intake(Frame, ncol2)) / mean(Frame$somme_freq_legumes))}}}

### Correcting legumes ------------
cat <- c("des.legumes.secs.gen")
sous_cat <- c("des.tartinables.a.base.de.legumes.secs.houmous","des.falafels",
              "du.tofudes.steaks.vegetaux.et.autres.similis.carnes", "Lentilles")
Frame$somme_freq_legumineuses <- NA_real_
for (row in 1:nrow(Frame)) {somme_freq <- 0
for (sc in sous_cat) {ncol <- grep(sc, colnames(Frame))
if (length(ncol) > 0) { values <- as.numeric(Frame[row, ncol])
somme_freq <- somme_freq + sum(values, na.rm = TRUE)}}
Frame$somme_freq_legumineuses[row] <- somme_freq}
for (i in seq_along(sous_cat)) { ncol1 <- grep(sous_cat[i], colnames(Frame))
ncol2 <- grep(cat, colnames(Frame))
if (length(ncol1) > 0 && length(ncol2) > 0) { if (!is.na(ncol1) && !is.na(ncol2)) {
  Frame[, ncol1] <- replace_na_with_zero(FREQ_intake(Frame, ncol1)) * replace_na_with_zero(mean(FREQ_intake(Frame, ncol2)) / mean(Frame$somme_freq_legumineuses))}}}

### Correcting fruits -----------
cat <- c("des.fruits.y.compris.seches.et.a.coque")
sous_cat <- c("des.compotes","des.fruits.en.sirop","des.abricotspechesprunescerises", 
              "des.fraisesframboises", "du.raisin", "du.melonde.la.pasteque", 
              "des.bananes", "des.kiwis", "des.agrumes.orangesmandarinespamplemoussescitrons.",
              "des.pommesdes.poires", "des.fruits.exotiques.ananasmangueslitcheesgoyaves.",
              "des.fruits.seches.abricotsdattesfiguespruneaux.", "des.fruits.a.coque.noixnoisettesamandes.")
Frame$somme_freq_fruits <- NA_real_
for (row in 1:nrow(Frame)) {somme_freq <- 0
for (sc in sous_cat) {ncol <- grep(sc, colnames(Frame))
if (length(ncol) > 0) { values <- as.numeric(Frame[row, ncol])
somme_freq <- somme_freq + sum(values, na.rm = TRUE)}}
Frame$somme_freq_fruits[row] <- somme_freq}
for (i in seq_along(sous_cat)) { ncol1 <- grep(sous_cat[i], colnames(Frame))
ncol2 <- grep(cat, colnames(Frame))
if (length(ncol1) > 0 && length(ncol2) > 0) { if (!is.na(ncol1) && !is.na(ncol2)) {
  Frame[, ncol1] <- replace_na_with_zero(FREQ_intake(Frame, ncol1)) * replace_na_with_zero(mean(FREQ_intake(Frame, ncol2)) / mean(Frame$somme_freq_fruits))}}}


### Correcting fish ---------
cat <- c("du.poisson.en.general.y.compris.coquillages.et.crustaces")
sous_cat <- c("du.poisson.cabillaudlieumerlansoletruite.frais.ou.congele.sauf.poisson.pane",
              "du.poisson.a.l.huile.thonsardines.", "du.poisson.fume.saumontruite", 
              "du.poisson.sale.ou.en.saumure.morueharenganchoissprats", "du.poisson.pane.cabillaudcolin", 
              "des.plats.cuisines.a.base.de.poisson", "des.coquillages.mouleshuitrescoquilles.st.Jacques",
              "des.crustaces.crevettescrabe")
Frame$somme_freq_poissons <- NA_real_
for (row in 1:nrow(Frame)) {somme_freq <- 0
for (sc in sous_cat) {ncol <- grep(sc, colnames(Frame))
if (length(ncol) > 0) { values <- as.numeric(Frame[row, ncol])
somme_freq <- somme_freq + sum(values, na.rm = TRUE)}}
Frame$somme_freq_poissons[row] <- somme_freq}
for (i in seq_along(sous_cat)) { ncol1 <- grep(sous_cat[i], colnames(Frame))
ncol2 <- grep(cat, colnames(Frame))
if (length(ncol1) > 0 && length(ncol2) > 0) { if (!is.na(ncol1) && !is.na(ncol2)) {
  Frame[, ncol1] <- replace_na_with_zero(FREQ_intake(Frame, ncol1)) * replace_na_with_zero(mean(FREQ_intake(Frame, ncol2)) / mean(Frame$somme_freq_poissons))}}}

### Correcting meats ------------------
cat <- c("de.la.viande,.hors.abats.et.charcuterie")
sous_cat <- c("de.la.viande.de.boeuf.sauf.steak.hache",
              "des.steaks.haches", "de.la.viande.de.porc.sauf.charcuterie", 
              "de.la.viande.de.veau", "de.la.viande.d.agneaude.mouton", 
              "de.la.volaille.pouletdinde.du.lapin", "du.foie.de.genisse.volaille",
              "des.andouillettesdu.boudin.et.autres.abats")

Frame$somme_freq_viande <- NA_real_
for (row in 1:nrow(Frame)) {somme_freq <- 0
for (sc in sous_cat) {ncol <- grep(sc, colnames(Frame))
if (length(ncol) > 0) { values <- as.numeric(Frame[row, ncol])
somme_freq <- somme_freq + sum(values, na.rm = TRUE)}}
Frame$somme_freq_viande[row] <- somme_freq}
for (i in seq_along(sous_cat)) { ncol1 <- grep(sous_cat[i], colnames(Frame))
ncol2 <- grep(cat, colnames(Frame))
if (length(ncol1) > 0 && length(ncol2) > 0) { if (!is.na(ncol1) && !is.na(ncol2)) {
  Frame[, ncol1] <- replace_na_with_zero(FREQ_intake(Frame, ncol1)) * replace_na_with_zero(mean(FREQ_intake(Frame, ncol2)) / mean(Frame$somme_freq_viande))}}}

### Correcting deli meats ------------------
cat <- c("de.la.charcuteriedes.abats.ou.des.oeufs")
sous_cat <- c("du.foie.genissevolaillesautres.",
              "du.pate.ou.des.rillettes", "du.jambon.blanc", 
              "du.jambon.crubacon", "des.saucisses.fraiches.ou.fumees.y.compris.merguez", 
              "du.saucisson.sec.ou.salamiy.compris.a.l.aperitif", "du.cervelas.ou.de.la.mortadelle",
              "de.la.langue.de.boeufdes.tripesdu.boudindes.andouillettesdes.ris.de.veaudes.rognons",
              "des.oeufssur.le.plat.en.omelette1", "des.oeufspochesdurs.ou.a.la.coque.1")
Frame$somme_freq_charcut <- NA_real_
for (row in 1:nrow(Frame)) {somme_freq <- 0
for (sc in sous_cat) {ncol <- grep(sc, colnames(Frame))
if (length(ncol) > 0) { values <- as.numeric(Frame[row, ncol])
somme_freq <- somme_freq + sum(values, na.rm = TRUE)}}
Frame$somme_freq_charcut[row] <- somme_freq}
for (i in seq_along(sous_cat)) { ncol1 <- grep(sous_cat[i], colnames(Frame))
ncol2 <- grep(cat, colnames(Frame))
if (length(ncol1) > 0 && length(ncol2) > 0) { if (!is.na(ncol1) && !is.na(ncol2)) {
  Frame[, ncol1] <- replace_na_with_zero(FREQ_intake(Frame, ncol1)) * replace_na_with_zero(mean(FREQ_intake(Frame, ncol2)) / mean(Frame$somme_freq_charcut))}}}


### Correcting dairy products --------------
cat <- c("du.fromage.et.des.produits.laitiers.y.compris.les.laits.vegetaux")
sous_cat <- c("de.l.Emmentaldu.Gruyeredu.Comtedu.Beaufort.rape.sur.les.plats.patesriz.",
              "de.l.Emmentaldu.Gruyeredu.Comtedu.Beaufort.en.morceaux", "du.Roquefortdu.Bleu.quelle.qu.en.soit.l.origine", 
              "du.fromage.de.chevre", "du.fromage.a.pate.molle.camembertcoulommiersbrie.", 
              "du.fromage.a.tartiner.cancoillotteSaint.MoretVache.qui.rit.", "de.la.mozzarella",
              "autres.types.de.fromages.camembertbrie.", "du.fromage.blanc.ou.des.yaourts.a.0.de.matieres.grasses.natureaux.fruits.", 
              "du.fromage.blancdes.petits.suisses.ou.des.yaourts.a.2030.ou.40.de.matieres.grasses",
              "du.fromage.blanc.a.0.de.matieres.grasses.natureaux.fruits.", "du.fromage.blanc.a.2030.ou.40.de.matieres.grasses.natureaux.fruits.",
              "des.entremets.cremes.desserts.de.type.Danetteliegeoismoussesflans.", "des.entremets.au.soja.ou.yaourts.au.soja.ou.autres.yaourts.aux.laits.vegetaux")
Frame$somme_freq_produits_laitiers <- NA_real_
for (row in 1:nrow(Frame)) {somme_freq <- 0
for (sc in sous_cat) {ncol <- grep(sc, colnames(Frame))
if (length(ncol) > 0) { values <- as.numeric(Frame[row, ncol])
somme_freq <- somme_freq + sum(values, na.rm = TRUE)}}
Frame$somme_freq_produits_laitiers[row] <- somme_freq}
for (i in seq_along(sous_cat)) { ncol1 <- grep(sous_cat[i], colnames(Frame))
ncol2 <- grep(cat, colnames(Frame))
if (length(ncol1) > 0 && length(ncol2) > 0) { if (!is.na(ncol1) && !is.na(ncol2)) {
  Frame[, ncol1] <- replace_na_with_zero(FREQ_intake(Frame, ncol1)) * replace_na_with_zero(mean(FREQ_intake(Frame, ncol2)) / mean(Frame$somme_freq_produits_laitiers))}}}

### Correcting bread -----------
if (campaign == "23-11" | campaign == "24-03" ) {
  cat <- c("du.paindes.biscottes.ou.des.cereales.de.type.petit.dejeuner")
  sous_cat <- c("du.pain.blancde.mie.hors.petit.dejeuner.",
                "du.pain.blanc.au.petit.dejeuner", "du.painspeciaux.hors.petit.dejeuner.",
                "du.pain.complet.et.autres.pains.speciaux.au.petit.dejeuner", "des.biscottesdes.craquottesdes.pains.grilles.type.suedois.hors.petit.dejeuner", 
                "des.biscottesdes.craquottesdes.pains.grilles.au.petit.dejeuner", "des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli")
  Frame$somme_freq_pain <- NA_real_
  for (row in 1:nrow(Frame)) {somme_freq <- 0
  for (sc in sous_cat) {ncol <- grep(sc, colnames(Frame))
  if (length(ncol) > 0) { values <- as.numeric(Frame[row, ncol])
  somme_freq <- somme_freq + sum(values, na.rm = TRUE)}}
  Frame$somme_freq_pain[row] <- somme_freq}
  for (i in seq_along(sous_cat)) { ncol1 <- grep(sous_cat[i], colnames(Frame))
  ncol2 <- grep(cat, colnames(Frame))
  if (length(ncol1) > 0 && length(ncol2) > 0) { if (!is.na(ncol1) && !is.na(ncol2)) {
    Frame[, ncol1] <- replace_na_with_zero(FREQ_intake(Frame, ncol1)) * replace_na_with_zero(mean(FREQ_intake(Frame, ncol2)) / mean(Frame$somme_freq_pain))}}}
  
}

print(unique(Frame$Identifiant))






# ASSIGNING PORTION SIZES -------------------------------
## Defining the function to replace codes with weights, with debugging messages -------------
#The remplacer_poids function takes two arguments, taille and aliment, and does the following:
#Displaying a message: shows a message indicating the food and size passed as arguments are being processed.
#Filtering and extracting the weight: uses the Taille_Portion_long data frame and applies the following steps:
#Filters the rows where the Aliment column matches the given food and the Taille column matches the given size.
#Extracts the values of the Poids column from the filtered rows.
#Handling cases with no match:
#Checks whether the length of the poids vector is zero (meaning no match was found).
#If no match is found, displays a message indicating there is no match for the given food and size, then returns NA.
#If a match is found, the function returns the extracted weight.
remplacer_poids <- function(taille, aliment) {message("Traitement de l'aliment: ", aliment, " et de la taille: ", taille)
  poids <- Taille_Portion_long %>%
    filter(Aliment == aliment, Taille == taille) %>%
    pull(Poids)
  if (length(poids) == 0) {message("Pas de correspondance trouvée pour ", aliment, " avec la taille ", taille)
    return(NA)}
  return(poids)}

##Building the portion weight table---------------
### Filtering the portion columns -------------------------------------
#Some food groups have a portion size (vegetables, raw vegetables, fish,
#steak, sweet tarts, savory tarts...)
#We start by isolating these columns and sizing a new dataframe "data_duplicated"
#that only takes these values into account
colonnes_portion <- grep("portion$", names(Frame), value = TRUE)
Frame_duplicated <- subset(Frame, select = c("Identifiant", colonnes_portion))

###Duplicating the portion columns as many times as a food belongs to a general category: vegetable, raw vegetable... ---------
#The for loop examines each unique category in the Catégorie column of the Taille_Portion data frame.
#Here is what this loop does in detail:
#Iterating over categories: for each unique category in Taille_Portion$Catégorie:
#Checking for NA values: if the category is NA, it moves on to the next iteration without running the rest of the code:
#Selecting columns: selects the columns of Frame whose names start with the category name
#Calculating the number of repetitions: calculates how many times the category appears in Taille_Portion$Catégorie
#Duplicating columns: for each occurrence of the category, adds the renamed columns to Frame_duplicated:
for (categorie in unique(Taille_Portion$Catégorie)) {
  if (is.na(categorie)) next 
  # Find the columns whose name starts with the category value
  column_names <- grep(paste0("^", categorie), names(Frame), value = TRUE)
  
  # Select the corresponding columns
  columns <- Frame[, column_names, drop = FALSE]
  
  nb_repeats <- sum(Taille_Portion$Catégorie == categorie, na.rm = TRUE)
  for (i in 1:nb_repeats) {
    Frame_duplicated <- cbind(Frame_duplicated, columns)
  }
}

#This step aims to harmonize the structure of the dataframe
#Each row corresponds to an identifier, and the goal is for the categories to have been
#copied as many times as there are specific foods.
# Initializing Poids with the data from Frame_duplicated
# Displaying the DataFrame with the duplicated columns
Poids <- print(Frame_duplicated)

# Separating the first two columns from the others
debut <- Poids[, 1]
fin <- Poids[, -c(1)]

# Sorting the remaining columns alphabetically
fin_trie <- fin[, order(names(fin))]

# Merging the two parts
Poids <- cbind(debut, fin_trie)

# Identifying the columns ending with a digit
colonnes_a_garder <- grep("\\d$", names(Poids), value = TRUE)

# Selecting only the columns ending with a digit
Poids <- Poids[, colonnes_a_garder]

# Identifying the column names
colnames_data <- names(Poids)

# Counting the occurrences of each column name
occurrences <- table(colnames_data)

# Identifying the duplicated columns that do not end with a digit
colonnes_a_supprimer <- names(occurrences[occurrences > 1])
colonnes_a_supprimer <- colonnes_a_supprimer[!grepl("\\d$", colonnes_a_supprimer)]
# Removing the duplicated columns that do not end with a digit
data_filtre <- Poids[, !names(Poids) %in% colonnes_a_supprimer]
# Displaying the filtered DataFrame
print("DataFrame filtré:")
Poids <- print(data_filtre)
### Renaming the copied columns with the specific food name ----------
#This step renames the copied portions in the weight table with the specific portion-size foods
new_column_names <- Taille_Portion$Aliment[match(names(Poids), Taille_Portion$Catégorie2)]
names(Poids) <- new_column_names

### Applying the function to replace portion sizes with the
# Transforming the Taille_Portion table into long format
#The pivot_longer function converts the specified columns (all columns except Aliment) into a long format. This means the columns will be "stacked" into two new columns named Taille (for the old column names) and Poids (for the old column values).
#The na.omit function removes rows that contain missing values (NA).
#The filter function excludes rows where the Taille column equals "Catégorie" or "Catégorie2".


Taille_Portion$`Plus petit que A`<- as.character(Taille_Portion$`Plus petit que A`)
Taille_Portion$A <- as.character(Taille_Portion$`Plus petit que A`)
Taille_Portion$B <- as.character(Taille_Portion$B)
Taille_Portion$C <- as.character(Taille_Portion$C)
Taille_Portion$`Plus grand que C` <- as.character(Taille_Portion$`Plus grand que C`)
Taille_Portion$`Type A`<- as.character(Taille_Portion$`Type A`)
Taille_Portion$`Type B`<- as.character(Taille_Portion$`Type B`)
Taille_Portion$`Type C`<- as.character(Taille_Portion$`Type C`)
Taille_Portion$`Type D`<- as.character(Taille_Portion$`Type D`)
Taille_Portion$`Type E`<- as.character(Taille_Portion$`Type E`)
Taille_Portion$`Type F`<- as.character(Taille_Portion$`Type F`)
Taille_Portion$Poids_unitaire <- as.character(Taille_Portion$Poids_unitaire) 


Taille_Portion_long <- Taille_Portion %>%
  pivot_longer(cols = -Aliment, names_to = "Taille", values_to = "Poids") %>%
  na.omit() %>%
  filter(Taille != "Catégorie" & Taille != "Catégorie2")


### Replacing sizes with weights by applying the remplacer_poids function---------------------
Poids_modifie <- Poids
for (col in names(Poids_modifie)) {
  Poids_modifie[[col]] <- sapply(Poids_modifie[[col]], function(taille) remplacer_poids(taille, col))
}

###This step adds the unit weight of foods whose portion does not vary---------------
#Filtering the data: subset(Taille_Portion_long, Taille == "Poids_unitaire"): selects only the rows of Taille_Portion_long where the Taille column equals "Poids_unitaire". The result is stored in filtered_df1.
#For loop to update Poids_modifie: for each row of filtered_df1, extracts the values of the Aliment and Poids columns.
#Creates an entry in the Poids_modifie list where the food name is the key and the weight value is the associated value.

filtered_df1 <- subset(Taille_Portion_long, Taille == "Poids_unitaire")
for (i in 1:nrow(filtered_df1)) {
  aliment <- filtered_df1$Aliment[i]
  valeur <- filtered_df1$Poids[i]
  Poids_modifie[[aliment]] <- valeur
}

print(unique(Frame$Identifiant))


# Finalizing the table
Poids_modifie <- cbind(Frame$Identifiant, Poids_modifie)
names(Poids_modifie)[1] <- "Identifiant"
Poids_modifie[,-1] <- lapply(Poids_modifie[,-1], as.numeric)



# CALCULATING THE WEIGHT OF FOODS CONSUMED    -------------------------------
#In this step, the weight of foods is calculated by multiplying the frequencies by the portion sizes for all foods
FFQ_POIDS <-data.frame(Frame$Identifiant)
names(FFQ_POIDS)[1] = "Identifiant"

FFQ_POIDS_Int <-data.frame(Frame$Identifiant)
names(FFQ_POIDS_Int )[1] = "Identifiant"

##ALCOHOL_FFQ ---------------
FFQ_POIDS$ALCOOL_FFQ <- rep(0, nrow(Frame))
categories <- c("de.cidre.ou.biere", "de.vin.blancrouge.ou.rose", "d.aperitifs.pastischerryportomartini.", "d.alcools.forts.whiskyginvodkapremix.")

terms <- numeric(nrow(Frame))
categories <- c("de.cidre.ou.biere", "de.vin.blancrouge.ou.rose", "d.aperitifs.pastischerryportomartini.", "d.alcools.forts.whiskyginvodkapremix.")
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) / 7
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$ALCOOL_FFQ <- terms


##COFFEE_TEA_FFQ--------------
FFQ_POIDS$CAFE_THE_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.cafe.y.compris.decafeine", colnames(Frame))
ncol2 <- grep("de.the",colnames(Frame))
if (campaign == "22-11" | campaign == "23-02" ) {
  term1 <- replace_na_with_zero(FREQ_intake(Frame, ncol1)*replace_na_with_zero(Poids_modifie$de.cafe.y.compris.decafeine))  
  term2 <- replace_na_with_zero(FREQ_intake(Frame, ncol2)*replace_na_with_zero(Poids_modifie$de.the))
  FFQ_POIDS_Int$de.cafe.y.compris.decafeine <-term1 
  FFQ_POIDS_Int$de.the <- term2 
  FFQ_POIDS$CAFE_THE_FFQ <- term1 + term2
} else {
  term1 <- replace_na_with_zero(FREQ_intake(Frame, ncol1))  
  term2 <- replace_na_with_zero(FREQ_intake(Frame, ncol2))  
  FFQ_POIDS_Int$de.cafe.y.compris.decafeine <-term1 
  FFQ_POIDS_Int$de.the <- term2 
  FFQ_POIDS$CAFE_THE_FFQ <- term1 + term2 
}

##CEREALS_PD_FFQ---------------
FFQ_POIDS$CEREALES_PD_FFQ  <- rep(0,nrow(Frame))
ncol1 <- grep("des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli",colnames(Frame))
FFQ_POIDS$CEREALES_PD_FFQ  <- FREQ_intake(Frame,ncol1)*Poids_modifie$des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli
FFQ_POIDS_Int$des.cereales.de.type.petit.dejeuner.corn.flakesCheerios.au.chocolatcereales.souffleesmuesli <- FFQ_POIDS$CEREALES_PD_FFQ

#DELI_MEATS_EXCL_HAM_FFQ
FFQ_POIDS$CHARCUTERIE_HORS_JB_FFQ <- rep(0,nrow(Frame))
categories <- c("du.saucisson.sec.ou.salamiy.compris.a.l.aperitif", "du.cervelas.ou.de.la.mortadelle",
                "du.pate.ou.des.rillettes", "du.jambon.crubacon",  "des.saucisses.fraiches.ou.fumees.y.compris.merguez")

terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$CHARCUTERIE_HORS_JB_FFQ <- terms

##DAIRY_DESSERTS_FFQ----------------
FFQ_POIDS$DESSERTS_LACTES_FFQ <- rep(0,nrow(Frame))
categories <- c("de.la.glace", "des.entremets.cremes.desserts.de.type.Danetteliegeoismoussesflans.",
                "des.entremets.au.soja.ou.yaourts.au.soja.ou.autres.yaourts.aux.laits.vegetaux")

terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$DESSERTS_LACTES_FFQ <- terms

##WATER_FFQ--------------
FFQ_POIDS$EAU_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("d.eau.en.bouteille.ou.bonbonne.verre", colnames(Frame))
ncol2 <- grep("d.eau.du.robinet.verre", colnames(Frame))

if (campaign == "22-11" | campaign == "23-02" ) {
  term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$d.eau.en.bouteille.ou.bonbonne.verre) 
  term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$d.eau.du.robinet.verre)
  FFQ_POIDS$EAU_FFQ <- term1 + term2
  FFQ_POIDS_Int$d.eau.en.bouteille.ou.bonbonne.verre <-term1 
  FFQ_POIDS_Int$d.eau.du.robinet.verre <- term2 
}else{ 
  term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)) 
  term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2))
  FFQ_POIDS$EAU_FFQ <- term1 + term2
  FFQ_POIDS_Int$d.eau.en.bouteille.ou.bonbonne.verre <-term1 
  FFQ_POIDS_Int$d.eau.du.robinet.verre <- term2 
}
##SPICES_CONDIMENTS_FFQ--------------------
FFQ_POIDS$EPICES_CONDIMENTS_FFQ <- rep(0,nrow(Frame))

##STARCHES_NON_REFINED_FFQ----------------------
FFQ_POIDS$FEC_NON_RAF_FFQ <- rep(0,nrow(Frame))
categories <- c("du.painspeciaux.hors.petit.dejeuner.","du.pain.complet.et.autres.pains.speciaux.au.petit.dejeuner",
                "du.mais.ou.de.la.polenta","des.pommes.de.terre.a.l.eau.ou.au.four","des.pommes.de.terre.rissolees.ou.sautees",
                "de.la.puree.de.pomme.de.terre","d.autres.feculents.quinoamaniocbanane.plantainigname.",
                "des.pates.completes.ou.semi.completes","du.riz.complet.ou.semi.complet","du.mais1")

terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$FEC_NON_RAF_FFQ <- terms

##STARCHES_REFINED_FFQ---------------------
FFQ_POIDS$FEC_RAF_FFQ <- rep(0,nrow(Frame))
categories <- c("de.la.semouledu.ble.tabouleen.accompagnement.autre.que.dans.un.couscousEbly",
                "du.riz.blanc", "des.pates.macaronisspaghettiscoquillettes", "du.pain.blancde.mie.hors.petit.dejeuner.",
                "des.biscottesdes.craquottesdes.pains.grilles.au.petit.dejeuner","des.biscottesdes.craquottesdes.pains.grilles.type.suedois.hors.petit.dejeuner",
                "du.pain.blanc.au.petit.dejeuner")

terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$FEC_RAF_FFQ <- terms

##CHEESES_FFQ----------------------------
FFQ_POIDS$FROMAGES_FFQ <- rep(0,nrow(Frame))
categories <- c("de.l.Emmentaldu.Gruyeredu.Comtedu.Beaufort.en.morceaux","du.Roquefortdu.Bleu.quelle.qu.en.soit.l.origine",
                "du.fromage.de.chevre","autres.types.de.fromages.camembertbrie.","de.l.Emmentaldu.Gruyeredu.Comtedu.Beaufort.rape.sur.les.plats.patesriz.",
                "du.fromage.a.pate.molle.camembertcoulommiersbrie.","du.fromage.a.tartiner.cancoillotteSaint.MoretVache.qui.rit.","de.la.mozzarella")
terms <- numeric(nrow(Frame))
non_reconnues <- c()

for (cat in categories) {
  ncol <- grep(cat, colnames(Frame))
  
  if (length(ncol) == 0) {
    # Add to the list of unrecognized categories
    non_reconnues <- c(non_reconnues, cat)
  } else {
    # Apply the formula if the column is found
    term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[cat]])
    terms <- terms + term
    FFQ_POIDS_Int[[cat]] <- term
  }
}

FFQ_POIDS$FROMAGES_FFQ <- terms

# Displaying unrecognized categories
if (length(non_reconnues) > 0) {
  warning("Colonnes non reconnues dans 'Frame' :\n", paste(non_reconnues, collapse = "\n"))
}

##FRUITS_FFQ -----------------------
FFQ_POIDS$FRUITS_FFQ <- rep(0,nrow(Frame))
categories <- c("des.compotes","des.fruits.en.sirop","des.abricotspechesprunescerises","des.fraisesframboises",
                "du.raisin","du.melonde.la.pasteque","des.bananes","des.kiwis","des.agrumes.orangesmandarinespamplemoussescitrons.",
                "des.pommesdes.poires","des.fruits.exotiques.ananasmangueslitcheesgoyaves.")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$FRUITS_FFQ <- terms

##FRUIT_JUICE_FFQ---------------
FFQ_POIDS$FRUITS_JUS_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre", colnames(Frame))
if (campaign == "22-11" | campaign == "23-02" ) {
  FFQ_POIDS$FRUITS_JUS_FFQ<-  replace_na_with_zero(FREQ_intake(Frame, ncol1)*replace_na_with_zero(Poids_modifie$de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre)) 
  FFQ_POIDS_Int$de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre <- FFQ_POIDS$FRUITS_JUS_FFQ
} else {
  FFQ_POIDS$FRUITS_JUS_FFQ <-  replace_na_with_zero(FREQ_intake(Frame, ncol1)) 
  FFQ_POIDS_Int$de.jus.d.orangede.pamplemoussesd.ananasde.pommesde.raisins.verre <- FFQ_POIDS$FRUITS_JUS_FFQ
}

##DRIED_FRUITS_FFQ ------------------
FFQ_POIDS$FRUITS_SECS_FFQ  <- rep(0,nrow(Frame))
ncol1 <- grep("des.fruits.seches.abricotsdattesfiguespruneaux.",colnames(Frame))
FFQ_POIDS$FRUITS_SECS_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$des.fruits.seches.abricotsdattesfiguespruneaux.)
FFQ_POIDS_Int$des.fruits.seches.abricotsdattesfiguespruneaux. <- FFQ_POIDS$FRUITS_SECS_FFQ

##WHITE_HAM-------------------
FFQ_POIDS$JAMBON_BLANC_FFQ  <- rep(0,nrow(Frame))
ncol1 <- grep("du.jambon.blanc",colnames(Frame))
FFQ_POIDS$JAMBON_BLANC_FFQ <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$du.jambon.blanc)
FFQ_POIDS_Int$du.jambon.blanc <- FFQ_POIDS$JAMBON_BLANC_FFQ


##MILK_FFQ---------------------
FFQ_POIDS$LAIT_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.lait.entier.", colnames(Frame))
ncol2 <- grep("de.lait.demi.ecreme",colnames(Frame))
ncol3 <- grep("de.lait.ecreme",colnames(Frame))
ncol4 <- grep("du.cacao.ou.chocolat.en.poudre",colnames(Frame))
if (campaign == "22-11" | campaign == "23-02" ) {
  term1 <- replace_na_with_zero(FREQ_intake(Frame, ncol1) *  Poids_modifie$de.lait.entier. )
  term2 <-replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$de.lait.demi.ecreme  )
  term3 <-replace_na_with_zero(FREQ_intake(Frame,ncol3)*Poids_modifie$de.lait.ecreme ) 
  term4 <-replace_na_with_zero(FREQ_intake(Frame,ncol4)*Poids_modifie$du.cacao.ou.chocolat.en.poudre)
  FFQ_POIDS$LAIT_FFQ <- term1 + term2 + term3 + term4 
  FFQ_POIDS_Int$de.lait.entier. <- term1
  FFQ_POIDS_Int$de.lait.demi.ecreme <- term2
  FFQ_POIDS_Int$de.lait.ecreme <- term3
  FFQ_POIDS_Int$du.cacao.ou.chocolat.en.poudre <- term4
}else{ 
  term1 <- replace_na_with_zero(FREQ_intake(Frame, ncol1))
  term2 <-replace_na_with_zero(FREQ_intake(Frame,ncol2))
  FFQ_POIDS$LAIT_FFQ <- term1 + term2
  FFQ_POIDS_Int$de.lait.entier. <- term1
  FFQ_POIDS_Int$de.lait.demi.ecreme <- term2
}

##DAIRY_PRODUCTS_FFQ------------------
FFQ_POIDS$LAITAGES_FFQ <- rep(0,nrow(Frame))
categories <- c("du.fromage.blanc.ou.des.yaourts.a.0.de.matieres.grasses.natureaux.fruits.","du.fromage.blancdes.petits.suisses.ou.des.yaourts.a.2030.ou.40.de.matieres.grasses",
                "du.fromage.blanc.a.0.de.matieres.grasses.natureaux.fruits.","du.fromage.blanc.a.2030.ou.40.de.matieres.grasses.natureaux.fruits.")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$LAITAGES_FFQ <- terms

##DRIED_LEGUMES_FFQ-------------------
FFQ_POIDS$LEG_SECS_FFQ <- rep(0,nrow(Frame))
categories <- c("des.legumes.secs.lentillesharicots.secspois.chichesfeves.","Lentilles",
                "des.tartinables.a.base.de.legumes.secs.houmous","des.falafels","du.tofudes.steaks.vegetaux.et.autres.similis.carnes")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$LEG_SECS_FFQ <- terms

##VEGETABLES_FFQ-------------------
FFQ_POIDS$LEGUMES_FFQ <- rep(0,nrow(Frame))
categories <- c("des.haricots.verts","des.endivesdes.epinardsdu.cresson","des.poireaux",
                "du.chou.vertchou.fleurBruxellesbrocolis","des.carottes.cuites","des.courgettesdes.auberginesdes.poivronsdes.tomates.cuites.ratatouille.",
                "des.petits.pois","des.artichautsdu.fenouildes.aspergesdu.celeri","des.champignons","du.potirondes.patates.douces","de.la.soupe.de.legumes",
                "de.la.salade.vertede.la.machede.la.roquettedes.epinardsdu.cresson","des.carottes.rapees","de.l.avocat.au.moins.un.demi.avocat"
                ,"d.autres.crudites","Des.salades.composees.uniquement.de.plusieurs.legumes.crus.tomates.et.concombrescarottes.et.salade.verte.")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$LEGUMES_FFQ <- terms
if (campaign == "23-11" | campaign == "24-03" ) {
  ncol1 <- grep("de.l.oignon.2",colnames(Frame))
  term1 <-  replace_na_with_zero((FREQ_intake(Frame,ncol1)*0.05)) 
  FFQ_POIDS$LEGUMES_FFQ <- FFQ_POIDS$LEGUMES_FFQ + term1}
FFQ_POIDS_Int$de.l.oignon.2 <- term1

##ANIMAL_FAT_FFQ -----------------------
FFQ_POIDS$MGA_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates.",colnames(Frame))
ncol2 <- grep("de.la.creme.fraiche",colnames(Frame))
term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates. )
term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$de.la.creme.fraiche)
FFQ_POIDS$MGA_FFQ <- term1 + term2 
FFQ_POIDS_Int$du.beurre.en.ajout.sur.du.paindu.biscottesur.les.pates. <- term1
FFQ_POIDS_Int$de.la.creme.fraiche <- term2

##VEGETABLE_FAT_FFQ------------------
FFQ_POIDS$MGV_FFQ <- rep(0,nrow(Frame))
categories <- c("de.l.huile.de.tournesold.arachide","de.la.margarine",
                "de.l.huile.melangee","de.l.huile.de.colzanoix","de.l.huile.d.olive.hors.vinaigrette")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$MGV_FFQ <- terms

##NUTS_FFQ----------------
FFQ_POIDS$NOIX_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("des.fruits.a.coque.noixnoisettesamandes.",colnames(Frame))
FFQ_POIDS$NOIX_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$des.fruits.a.coque.noixnoisettesamandes.)
FFQ_POIDS_Int$des.fruits.a.coque.noixnoisettesamandes. <- FFQ_POIDS$NOIX_FFQ 

##EGGS_FFQ-----------------
FFQ_POIDS$OEUFS_FFQ <- rep(0,nrow(Frame))
if (campaign == "22-11" | campaign == "23-02" ) {
  FFQ_POIDS$OEUFS_FFQ <- rep(0,nrow(Frame))
  categories <- c("des.oeufssur.le.plat.en.omelette1","des.oeufspochesdurs.ou.a.la.coque.1")
  terms <- numeric(nrow(Frame))
  for (i in seq_along(categories)) {
    ncol <- grep(categories[i], colnames(Frame))
    term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
    terms <- terms + term
    FFQ_POIDS_Int[[categories[i]]] <- term
  }
  FFQ_POIDS$OEUFS_FFQ <- terms
  
}else{
  ncol1 <- grep("des.oeufssur.le.plat.en.omelette2",colnames(Frame))
  ncol2 <- grep("des.oeufspochesdurs.ou.a.la.coque.2",colnames(Frame))
  ncol3 <- grep ("Quand.vous.mangez.des.oeufscombien.en.mangez.vous.par.repas.omeletteau.platdur", colnames(Frame))
  term1 <-  replace_na_with_zero((FREQ_intake(Frame,ncol1)*replace_na_with_zero(FREQ_intake(Frame,ncol3))*0.06)) 
  term2 <-  replace_na_with_zero((FREQ_intake(Frame,ncol2)*replace_na_with_zero(FREQ_intake(Frame,ncol3))*0.06))
  FFQ_POIDS$OEUFS_FFQ <- term1 + term2  
  FFQ_POIDS_Int$des.oeufssur.le.plat.en.omelette2 <- term1
  FFQ_POIDS_Int$des.oeufspochesdurs.ou.a.la.coque.2<- term2
}


##SWEET_PRODUCTS_FFQ ---------------------
FFQ_POIDS$PDTS_SUCRES_FFQ <- rep(0,nrow(Frame))
categories <- c("de.la.tarte.aux.fruitsau.flan.","de.la.patisserie.maison.tartegateau.au.chocolatcrepe.","de.la.briochedu.cakedu.quatre.quarts",
                "des.biscuitspur.beurresecsa.la.confiturefourresau.chocolat.","Des.gateaux.patissiers.tout.faits.browniecrepepain.d.epice.","des.gateux.patissiers.au.chocolata.la.creme.",
                "de.la.pate.a.tartiner.au.chocolat.type.Nutella","des.barres.chocolatees.MarsBounty.","des.barres.de.cereales.Granny.","des.bonbons","des.viennoiseries.croissantspains.au.chocolat.",
                "du.chocolat.noirau.laitaux.noisettes.","du.mielde.la.confitureou.marmelade","du.sorbet","d.autres.types.de.produits.sucres")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$PDTS_SUCRES_FFQ <- terms
if (campaign == "23-11" | campaign == "24-03" ) { 
  ncol1 <- grep("Lorsque.vous.buvez.du.cafe.the.ou.mangez.un.yaourt.fromage.blanccombien.de.cuilleres.ou.carres.de.sucre.rajoutez.vous.",colnames(Frame))
  term1 <-  replace_na_with_zero((FREQ_intake(Frame,ncol1)*0.07))
  FFQ_POIDS$PDTS_SUCRES_FFQ <- FFQ_POIDS$PDTS_SUCRES_FFQ + term1
  FFQ_POIDS_Int$Lorsque.vous.buvez.du.cafe.the.ou.mangez.un.yaourt.fromage.blanccombien.de.cuilleres.ou.carres.de.sucre.rajoutez.vous. <- term1 }

##READY_MEALS_FFQ------------------
FFQ_POIDS$PLATS_PREP_CARNES_FFQ  <- rep(0,nrow(Frame))
categories <- c("des.raviolislasagnespates.fourrees","du.cassoulet","du.couscous","des.salades.composees.toutes.faites.avec.feculents.et.viande",
                "de.la.paella","de.la.choucroute.avec.de.la.charcuterie", "du.chili.con.carne", "des.plats.cuisines.alleges","des.plats.cuisines.a.base.de.poisson")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$PLATS_PREP_CARNES_FFQ <- terms

##VEGETARIAN_READY_MEALS_FFQ---------------
FFQ_POIDS$PLATS_PREP_VEGETARIENS_FFQ  <- rep(0,nrow(Frame))
categories <- c("du.gratin.dauphinois", "des.raviolislasagnespates.fourrees.sans.viande",
                "des.salades.composees.toutes.faites.avec.feculents.sans.viande","des.salades.composees.toutes.faites.seulement.de.legumes",
                "du.taboule.tout.fait")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$PLATS_PREP_VEGETARIENS_FFQ <- terms

##FISH_FFQ--------------------
FFQ_POIDS$POISSONS_FFQ  <- rep(0,nrow(Frame))
categories <- c("du.poisson.cabillaudlieumerlansoletruite.frais.ou.congele.sauf.poisson.pane","du.poisson.a.l.huile.thonsardines.",
                "du.poisson.fume.saumontruite","du.poisson.sale.ou.en.saumure.morueharenganchoissprats","du.poisson.pane.cabillaudcolin",
                "des.coquillages.mouleshuitrescoquilles.st.Jacques","des.crustaces.crevettescrabe")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$POISSONS_FFQ <- terms

##PORK_FFQ-----------------
FFQ_POIDS$PORC_FFQ<- rep(0,nrow(Frame))
ncol1 <- grep("de.la.viande.de.porc.sauf.charcuterie",colnames(Frame))
FFQ_POIDS$PORC_FFQ <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$de.la.viande.de.porc.sauf.charcuterie)
FFQ_POIDS_Int$de.la.viande.de.porc.sauf.charcuterie <- FFQ_POIDS$PORC_FFQ

##CHICKEN_FFQ------------------
FFQ_POIDS$POULET_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.la.volaille.pouletdinde.du.lapin",colnames(Frame))
FFQ_POIDS$POULET_FFQ <- replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$de.la.volaille.pouletdinde.du.lapin)
FFQ_POIDS_Int$de.la.volaille.pouletdinde.du.lapin <- FFQ_POIDS$POULET_FFQ

##QUICHES_PIZZAS_SAVORY_TARTS-----------------
FFQ_POIDS$QUICHES_PIZZAS_TARTES_SALEES_FFQ  <- rep(0,nrow(Frame))
categories <- c("de.la.pizza1","de.la.pizza.sans.viande","des.tartes.salees.quiche.1","des.tartes.salees.quichesans.viande")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$QUICHES_PIZZAS_TARTES_SALEES_FFQ <- terms  

##SAUCES_FFQ-----------------
FFQ_POIDS$SAUCES_FFQ  <- rep(0,nrow(Frame))
categories <- c("de.la.mayonnaise","de.la.sauce.vinaigrette.avec.crudites.","de.la.sauce.soja","de.la.sauce.de.type.ketchuptomatebarbecue.")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$SAUCES_FFQ <- terms

##SNACKS_OTHER_FFQ---------------
FFQ_POIDS$SNACKS_AUTRES_FFQ  <- rep(0,nrow(Frame))
categories <- c("des.cacahuetes","des.gateaux.aperitifs.sales","des.olives","des.chips.au.repasa.l.aperitif.",
                "des.friands.ou.croque.monsieur1","des.friands.ou.croque.monsieursans.viande","des.sandwichs1",
                "des.sandwichs.sans.viande.y.compris.tacospanini","des.hamburgers1","des.hamburgers.sans.viande","des.frites")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$SNACKS_AUTRES_FFQ <- terms

##LIGHT_SODAS_FFQ-------------------
FFQ_POIDS$SODAS_LIGHT_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.lait.vegetal.sojarizavoine.", colnames(Frame))
ncol2 <- grep("de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.llight.verre", colnames(Frame))
if (campaign == "22-11" | campaign == "23-02" ) {
  term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$de.lait.vegetal.sojarizavoine.) 
  term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.llight.verre)
  FFQ_POIDS$SODAS_LIGHT_FFQ <- term1 + term2
  FFQ_POIDS_Int$de.lait.vegetal.sojarizavoine. <- term1
  FFQ_POIDS_Int$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.llight.verre <- term2
}else{ 
  term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)) 
  term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2))
  FFQ_POIDS$SODAS_LIGHT_FFQ <- term1 + term2
  FFQ_POIDS_Int$de.lait.vegetal.sojarizavoine. <- term1
  FFQ_POIDS_Int$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.llight.verre <- term2
}

##SUGARY_SODAS_FFQ------------------
FFQ_POIDS$SODAS_SUCRES_FFQ <- rep(0,nrow(Frame))
ncol1 <- grep("de.sirop.verre", colnames(Frame))
ncol2 <- grep("de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.non.light.verre", colnames(Frame))
if (campaign == "22-11" | campaign == "23-02" ) {
  term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)*Poids_modifie$de.sirop.verre) 
  term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2)*Poids_modifie$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.non.light.verre)
  FFQ_POIDS_Int$de.sirop.verre <- term1
  FFQ_POIDS_Int$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.non.light.verre <- term2
}else{ 
  term1 <-  replace_na_with_zero(FREQ_intake(Frame,ncol1)) 
  term2 <-  replace_na_with_zero(FREQ_intake(Frame,ncol2))
  FFQ_POIDS_Int$de.sirop.verre <- term1
  FFQ_POIDS_Int$de.cola.type.Coca.Cola.ou.Pepsilimonade.ou.soda.type.SpriteFanta.non.light.verre <- term2
}
FFQ_POIDS$SODAS_SUCRES_FFQ <- term1 + term2

##RED_MEAT_FFQ--------------
FFQ_POIDS$VIANDE_ROUGE_FFQ  <- rep(0,nrow(Frame))
categories <- c("de.la.viande.de.boeuf.sauf.steak.hache","des.steaks.haches","de.la.viande.de.veau",
                "de.la.viande.d.agneaude.mouton","des.andouillettesdu.boudin.et.autres.abats",
                "de.la.langue.de.boeufdes.tripesdu.boudindes.andouillettesdes.ris.de.veaudes.rognons",
                "du.foie.de.genisse.volaille","du.foie.genissevolaillesautres.")
terms <- numeric(nrow(Frame))
for (i in seq_along(categories)) {
  ncol <- grep(categories[i], colnames(Frame))
  term <- replace_na_with_zero(FREQ_intake(Frame, ncol) * Poids_modifie[[categories[i]]]) 
  terms <- terms + term
  FFQ_POIDS_Int[[categories[i]]] <- term
}
FFQ_POIDS$VIANDE_ROUGE_FFQ <- terms

#Sum
FFQ_POIDS$SOMME_FFQ_POIDS<- rowSums(FFQ_POIDS[,2:35])
FFQ_POIDS$SOMME_FFQ_HORS_BOISSON <- replace_na_with_zero(FFQ_POIDS$SOMME_FFQ_POIDS- 
                                                           FFQ_POIDS$ALCOOL_FFQ - 
                                                           FFQ_POIDS$FRUITS_JUS_FFQ - 
                                                           FFQ_POIDS$CAFE_THE_FFQ - 
                                                           FFQ_POIDS$LAIT_FFQ -
                                                           FFQ_POIDS$EAU_FFQ -
                                                           FFQ_POIDS$SODAS_LIGHT_FFQ -
                                                           FFQ_POIDS$SODAS_SUCRES_FFQ)

#CALCULATING KCAL -----------------------------------
df_long <- FFQ_POIDS_Int %>%
  pivot_longer(cols = -Identifiant, names_to = "FFQ_TI", values_to = "Poids") 

df_long <- df_long %>%
  filter(!is.na(Poids))

df_long <- inner_join(df_long, CALNUT, by= "FFQ_TI", relationship = "many-to-many")
print(unique(df_long$Identifiant))

#CALCULATING KILOCALORIES PER FOOD TI
df_long$nrj_kcal_alim <- df_long$nrj_kcal*df_long$Poids*10
FFQ_KCAL <- aggregate(nrj_kcal_alim ~  Identifiant + groupe_TI_TdC   , df_long, FUN = sum)
FFQ_KCAL<- pivot_wider(
  FFQ_KCAL,
  id_cols = Identifiant,
  names_from = groupe_TI_TdC,
  values_from = nrj_kcal_alim
)


# Calculating the sum of the columns for each row
FFQ_KCAL$SOMME_FFQ_KCAL <- rowSums(FFQ_KCAL[, 2:ncol(FFQ_KCAL)], na.rm = TRUE)


# Calculating the sum of the columns excluding beverages
FFQ_KCAL$SOMME_CARNET_HORS_BOISSON <- with(FFQ_KCAL, SOMME_FFQ_KCAL - 
                                             ALCOOL - 
                                             FRUITS_JUS - 
                                             CAFE_THE - 
                                             LAIT - 
                                             EAU - 
                                             SODAS_LIGHT - 
                                             SODAS_SUCRES)

FFQ_KCAL$KCAL_SANS_ALCOOL <-  with(FFQ_KCAL, SOMME_FFQ_KCAL - ALCOOL)
FFQ_KCAL$KCAL_SANS_BOISSON <-  with(FFQ_KCAL, SOMME_FFQ_KCAL - ALCOOL- LAIT - SODAS_LIGHT - SODAS_SUCRES - CAFE_THE- EAU- FRUITS_JUS )
FFQ_KCAL[, 2:34] <- (FFQ_KCAL[, 2:34] / FFQ_KCAL$SOMME_FFQ_KCAL)
FFQ_KCAL[, 2:34] <- FFQ_KCAL[, 2:34] * 100
FFQ_KCAL$SOMME_POURCENT_FFQ <- rowSums(FFQ_KCAL[, 2:34], na.rm = TRUE)


#CALCULATING MAR/MER--------------------------------------

## Calculating MAR and MERF

###Calculating vitamin A --------------------------
###Calculating vitamin A --------------------------
df_long$vit_a_mcg <- (df_long$retinol_mcg + (df_long$beta_carotene_mcg/6)) 
#Adding the last modified columns
df_long$proteines_g_alim <- df_long$Poids* df_long$proteines_g *10 
df_long$proteines_kcal_alim <- ((df_long$proteines_g*4) * df_long$Poids *10 )
df_long$ag_18_2_lino_g_alim  <- (df_long$Poids * df_long$ag_18_2_lino_g*10 )
df_long$ag_18_2_lino_kcal_alim   <- (df_long$Poids *df_long$ag_18_2_lino_g*9*10 )
df_long$ag_18_3_a_lino_g_alim<- (df_long$Poids *  df_long$ag_18_3_a_lino_g*10 )
df_long$ag_18_3_a_lino_kcal_alim <- (df_long$Poids*df_long$ag_18_3_a_lino_g*9*10 )
df_long$ags_g_alim  <- (df_long$ags_g* df_long$Poids * 10)
df_long$ags_kcal_alim <- (df_long$ags_g *9* df_long$Poids  * 10)


### Calculating nutrient quantities per food -----------------
# Selecting the columns to transform
colonnes_a_transformer <- c("fibres_g","ag_20_6_dha_g", "magnesium_mg", "potassium_mg", "calcium_mg", "fer_mg", "cuivre_mg", "zinc_mg",
                            "selenium_mcg", "iode_mcg","vit_a_mcg","vitamine_d_mcg", "vitamine_e_mg", "vitamine_c_mg",
                            "vitamine_b1_mg", "vitamine_b2_mg", "vitamine_b3_mg","vitamine_b6_mg", "vitamine_b9_mcg", "vitamine_b12_mcg",
                            "alcool_g", "sodium_mg", "fructose_g", "glucose_g", "maltose_g", "saccharose_g")

# Checking that all columns are present
colonnes_manquantes <- setdiff(colonnes_a_transformer, names(df_long))
if (length(colonnes_manquantes) > 0) {
  stop("Les colonnes suivantes ne sont pas reconnues : ", paste(colonnes_manquantes, collapse = ", "))
}

# If everything is correct, apply the transformation
df_long <- df_long %>%
  mutate(across(all_of(colonnes_a_transformer),
                ~ . * Poids * 10,
                .names = "{.col}_alim"))

### Sum by ID of the nutrients of interest --------------------------
colonnes_a_sommer <- names(df_long)[grep("_alim$", names(df_long))]
print(colonnes_a_sommer)  # Debugging check

somme_par_identifiant <- df_long %>%
  group_by(Identifiant) %>%
  summarise(across(colonnes_a_sommer, ~ sum(.x, na.rm = TRUE)))

print(df_long$ags_kcal_alim)

#Sum of sugars
somme_par_identifiant$sucre_aj_g_appro_alim <- somme_par_identifiant$fructose_g_alim+ somme_par_identifiant$glucose_g_alim + somme_par_identifiant$maltose_g_alim + somme_par_identifiant$saccharose_g_alim

#Calculating nutrients without alcohol
cols_to_extract <- c("Identifiant", "KCAL_SANS_ALCOOL" , "SOMME_FFQ_KCAL") 
extracted_df <- FFQ_KCAL[, cols_to_extract]
somme_par_identifiant <-inner_join(somme_par_identifiant,extracted_df , by="Identifiant")
cols_to_extract <- c("Identifiant", "Sexe") 
extracted_df <- metadata[, cols_to_extract]
somme_par_identifiant <-inner_join(somme_par_identifiant,extracted_df , by="Identifiant")

#Calculating the last columns
somme_par_identifiant$proteines_kcal_2000 <- (somme_par_identifiant$proteines_kcal_alim*100)/(somme_par_identifiant$KCAL_SANS_ALCOOL)
somme_par_identifiant$fibres_g_2000 <- (somme_par_identifiant$fibres_g_alim*2000)/  somme_par_identifiant$SOMME_FFQ_KCAL
somme_par_identifiant$ag_18_3_a_lino_g_2000 <- (somme_par_identifiant$ag_18_3_a_lino_kcal_alim*100)/(somme_par_identifiant$KCAL_SANS_ALCOOL)
somme_par_identifiant$ag_18_2_lino_g_2000 <- (somme_par_identifiant$ag_18_2_lino_kcal_alim*100)/(somme_par_identifiant$KCAL_SANS_ALCOOL)
somme_par_identifiant$ag_20_6_dha_g_2000 <- (somme_par_identifiant$ag_20_6_dha_g_alim*2000)/(somme_par_identifiant$SOMME_FFQ_KCAL)

somme_par_identifiant$ags_kcal_2000 <- (somme_par_identifiant$ags_kcal_alim *100) /(somme_par_identifiant$KCAL_SANS_ALCOOL)

### Readjustment / 2000 KCAL---------------------------------------
exclude_cols <-  c("proteines_kcal_alim", "ags_kcal_alim", "ag_18_2_lino_g_alim", "ag_18_3_a_lino_g_alim","ag_18_3_a_lino_kcal_alim",
                   "ags_g_alim","proteines_g_alim" ,"fructose_g_alim"  ,"maltose_g_alim"       ,   "glucose_g_alim"    , "saccharose_g_alim", "alcool_g_alim",
                   "ag_18_2_lino_kcal_alim", "fibres_g_alim", "ag_20_6_dha_g_alim")
alim_cols <- grep("_alim$", names(somme_par_identifiant), value = TRUE)
alim_cols <- setdiff(alim_cols, exclude_cols)
for (col in alim_cols) {
  somme_par_identifiant[[col]] <- (somme_par_identifiant[[col]] * 2000) / somme_par_identifiant$SOMME_FFQ_KCAL
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


# Define a function to calculate the ratio
calculate_ratio <- function(sexe, valeur, seuil_femme, seuil_homme) {
  if (sexe == "Femme") {
    return(ifelse(valeur / seuil_femme > 1, 1, valeur / seuil_femme))
  } else if (sexe == "Homme") {
    return(ifelse(valeur / seuil_homme > 1, 1, valeur / seuil_homme))
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
somme_par_identifiant$ratio_vit_b1 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b1_mg_2000,0.84, 0.84)
somme_par_identifiant$ratio_vit_b3 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b3_mg_2000, 13.4, 13.4) 
somme_par_identifiant$ratio_vit_b6 <- mapply(calculate_ratio, somme_par_identifiant$Sexe, somme_par_identifiant$vitamine_b6_mg_2000, 1.6, 1.7)

somme_par_identifiant <- na.omit(somme_par_identifiant)

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


somme_par_identifiant$HENI <- NA

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



#CALCULATING environmental indicators --------------------------------------
df_long <- FFQ_POIDS_Int %>%
  pivot_longer(cols = -Identifiant, names_to = "FFQ_TI", values_to = "Poids") 

df_long <- df_long %>%
  filter(!is.na(Poids))

df_long <- inner_join(df_long, CALNUT, by= "FFQ_TI", relationship = "many-to-many")

## Selecting the columns to transform----------------
colonnes_a_transformer <- c("climat", "couche_ozone","ions","ozone",	"partic",	"acid",	"eutro_terr", "eutro_eau","eutro_mer",	"sol",	"toxi_eau",	"ress_eau",	"ress_ener",	"ress_min")

#  Multiply by the food weight and by 1000 to convert to kg for each env indicator
df_long <- df_long %>%
  mutate(across(all_of(colonnes_a_transformer),~ . * Poids , .names = "{.col}_env" ))

df_long$climat_env <- df_long$climat_env*1000
## Sum by ID of the results for each food  --------------------------

colonnes_a_sommer_env <- grep("_env$", names(df_long), value = TRUE)
df_selected <- df_long[, colonnes_a_sommer_env, drop = FALSE]

somme_par_identifiant_env <- df_long %>%
  group_by(Identifiant) %>%
  summarise(across(all_of(colonnes_a_sommer_env), ~ sum(.x, na.rm = TRUE)))




#PNNS-GS---------
# =============================================================================
# CALCULATING THE PNNS-GS2 (simplified version: without oily fish, salt, organic)
# To be inserted into the pipeline AFTER calculating FFQ_POIDS, FFQ_KCAL,
# somme_par_identifiant and building FFQ_id
# =============================================================================
# Components included (main recommendations):
#   ADEQUACY  : fruits & vegetables, nuts, legumes, whole grain starches,
#               dairy products, fish (total)
#   MODERATION: red meat, deli meats, added fats,
#               sweet products, sugary/diet/juice drinks, alcohol
# Components EXCLUDED: oily fish, salt, organic
# =============================================================================

pnns <- FFQ_POIDS %>%
  left_join(
    FFQ_KCAL %>%
      select(Identifiant, SOMME_FFQ_KCAL, ALCOOL),
    by = "Identifiant"
  ) %>%
  mutate(eiwa = SOMME_FFQ_KCAL - ALCOOL) %>%
  select(-SOMME_FFQ_KCAL, -ALCOOL)

# =============================================================================
# 1. FRUITS & VEGETABLES  (weight = 3)
#    80 g servings; target ≥ 5 servings/day
#    Score: <3.5 = 0 ; 3.5-5 = 0.5 ; 5-7.5 = 1 ; ≥7.5 = 2
# =============================================================================

# Values already in g/day — no /7
pnns$fv_g_j <- pnns$FRUITS_FFQ + pnns$LEGUMES_FFQ +
  pnns$FRUITS_SECS_FFQ + pnns$FRUITS_JUS_FFQ

pnns$fv_portions <- pnns$fv_g_j / 80   # servings/day

pnns$score_fv_raw <- ifelse(pnns$fv_portions < 3.5,  0,
                            ifelse(pnns$fv_portions < 5,     0.5,
                                   ifelse(pnns$fv_portions < 7.5,   1, 2)))

pnns$comp_fv <- (pnns$score_fv_raw / 2) * 3   # weight = 3


# =============================================================================
# 2. NUTS  (weight = 1)
#    Serving = 30 g; target: ~1 handful/day (0.5 to 1.5 servings)
#    Score: 0 servings = 0 ; 0-0.5 = 0 ; 0.5-1.5 = 1 ; ≥1.5 = 0
# =============================================================================

# Values already in g/day — no /7
pnns$noix_portions <- pnns$NOIX_FFQ / 30   # servings/day

pnns$score_noix_raw <- ifelse(pnns$noix_portions == 0,   0,
                              ifelse(pnns$noix_portions < 0.5,  0,
                                     ifelse(pnns$noix_portions < 1.5,  1, 0)))

pnns$comp_noix <- (pnns$score_noix_raw / 1) * 1   # weight = 1, max = 1


# =============================================================================
# 3. LEGUMES  (weight = 1)
#    Serving = 200 g; target ≥ 2 servings/week
#    Score: 0/week = 0 ; 0-2/week = 0.5 ; ≥2/week = 1
# =============================================================================

# g/day * 7 → g/week to apply the weekly thresholds
pnns$leg_secs_portions_sem <- (pnns$LEG_SECS_FFQ * 7) / 200

pnns$score_leg_raw <- ifelse(pnns$leg_secs_portions_sem == 0,  0,
                             ifelse(pnns$leg_secs_portions_sem < 2,   0.5, 1))

pnns$comp_leg <- (pnns$score_leg_raw / 1) * 1   # weight = 1, max = 1


# =============================================================================
# 4. WHOLE GRAIN STARCHES  (weight = 2)
#    Serving ≈ 50 g; target expressed in servings/day
#    Score: 0 = 0 ; 0-1 = 0.5 ; 1-2 = 1 ; ≥2 = 1.5
# =============================================================================

# Values already in g/day — no /7
pnns$fec_comp_portions <- pnns$FEC_NON_RAF_FFQ / 50   # servings/day

pnns$score_fec_raw <- ifelse(pnns$fec_comp_portions == 0,  0,
                             ifelse(pnns$fec_comp_portions < 1,   0.5,
                                    ifelse(pnns$fec_comp_portions < 2,   1, 1.5)))

pnns$comp_fec <- (pnns$score_fec_raw / 1.5) * 2   # weight = 2, max = 1.5


# =============================================================================
# 5. DAIRY PRODUCTS  (weight = 1)
#    Serving: milk 150 g; yogurt 125 g; cheese 30 g
#    Target: 2 servings/day — parabolic relationship
#    Score: <0.5=0 ; 0.5-1.5=0.5 ; 1.5-2.5=1 ; ≥2.5=0
# =============================================================================

# Values already in g/day — no /7
pnns$pl_portions <- pnns$LAIT_FFQ    / 150 +
  pnns$LAITAGES_FFQ / 125 +
  pnns$FROMAGES_FFQ / 30    # servings/day

pnns$score_pl_raw <- ifelse(pnns$pl_portions < 0.5,  0,
                            ifelse(pnns$pl_portions < 1.5,  0.5,
                                   ifelse(pnns$pl_portions < 2.5,  1, 0)))

pnns$comp_pl <- (pnns$score_pl_raw / 1) * 1   # weight = 1, max = 1


# =============================================================================
# 6. FISH & SEAFOOD  (weight = 2)  — TOTAL fish (not just oily fish)
#    Serving = 100 g; target 2 servings/week
#    Parabolic score: <1.5/week=0 ; 1.5-2.5=1 ; 2.5-3.5=0.5 ; ≥3.5=0
# =============================================================================

# g/day * 7 → servings/week to apply the weekly thresholds
pnns$poisson_portions_sem <- (pnns$POISSONS_FFQ * 7) / 100

pnns$score_poisson_raw <- ifelse(pnns$poisson_portions_sem < 1.5, 0,
                                 ifelse(pnns$poisson_portions_sem < 2.5, 1,
                                        ifelse(pnns$poisson_portions_sem < 3.5, 0.5, 0)))

pnns$comp_poisson <- (pnns$score_poisson_raw / 1) * 2   # weight = 2, max = 1


# =============================================================================
# 7. RED MEAT  (weight = 2)  — moderation
#    Thresholds in g/week: <500=0 ; 500-750=-1 ; ≥750=-2
# =============================================================================

# g/day * 7 → g/week
pnns$vr_g_sem <- pnns$VIANDE_ROUGE_FFQ * 7

pnns$score_vr_raw <- ifelse(pnns$vr_g_sem < 500,   0,
                            ifelse(pnns$vr_g_sem < 750,  -1, -2))

pnns$comp_vr <- (pnns$score_vr_raw / -2) * 2   # weight = 2, max abs = 2


# =============================================================================
# 8. DELI MEATS  (weight = 3)  — moderation
#    Thresholds in g/week: <150=0 ; 150-300=-1 ; ≥300=-2
# =============================================================================

# g/day * 7 → g/week
pnns$charc_g_sem <- (pnns$CHARCUTERIE_HORS_JB_FFQ + pnns$JAMBON_BLANC_FFQ) * 7

pnns$score_charc_raw <- ifelse(pnns$charc_g_sem < 150,   0,
                               ifelse(pnns$charc_g_sem < 300,  -1, -2))

pnns$comp_charc <- (pnns$score_charc_raw / -2) * 3   # weight = 3, max abs = 2


# =============================================================================
# 9. ADDED FATS  (weight = 2)  — moderation
#    Target: ≤16% of EIWA
#    Score: >16% = 0 ; ≤16% = 1.5  +  recommended oil ratio (0 or 1)
#    NB: fat kcal = (MGA + MGV) * 9  (values already in g/day)
# =============================================================================

# Values already in g/day — no /7
pnns$mga_kcal_j <- (pnns$MGA_FFQ + pnns$MGV_FFQ) * 9

pnns$mga_pct_eiwa <- ifelse(pnns$eiwa > 0,
                            (pnns$mga_kcal_j / pnns$eiwa) * 100, NA)

pnns$score_mga_base <- ifelse(!is.na(pnns$mga_pct_eiwa) &
                                pnns$mga_pct_eiwa <= 16, 1.5, 0)

pnns$ratio_huile_rec <- ifelse((pnns$MGV_FFQ + pnns$MGA_FFQ) > 0,
                               pnns$MGV_FFQ / (pnns$MGV_FFQ + pnns$MGA_FFQ),
                               NA)

pnns$score_mga_ratio <- ifelse(!is.na(pnns$ratio_huile_rec) &
                                 pnns$ratio_huile_rec >= 0.5, 1, 0)

pnns$score_mga_raw <- pnns$score_mga_base + pnns$score_mga_ratio  # max = 2.5

pnns$comp_mga <- (pnns$score_mga_raw / 2.5) * 2   # weight = 2, max abs = 2.5


# =============================================================================
# 10. SWEET PRODUCTS  (weight = 3)  — moderation
#     Target: <10% EIWA; score: <10%=0 ; 10-15%=-1 ; ≥15%=-2
#     sugar kcal = PDTS_SUCRES_FFQ (g/day) * 4
# =============================================================================

# Values already in g/day — no /7
pnns$sucres_kcal_j <- pnns$PDTS_SUCRES_FFQ * 4

pnns$sucres_pct_eiwa <- ifelse(pnns$eiwa > 0,
                               (pnns$sucres_kcal_j / pnns$eiwa) * 100, NA)

pnns$score_sucre_raw <- ifelse(!is.na(pnns$sucres_pct_eiwa) &
                                 pnns$sucres_pct_eiwa < 10,   0,
                               ifelse(!is.na(pnns$sucres_pct_eiwa) &
                                        pnns$sucres_pct_eiwa < 15,  -1, -2))

pnns$comp_sucre <- (pnns$score_sucre_raw / -2) * 3   # weight = 3, max abs = 2


# =============================================================================
# 11. SUGARY / DIET / JUICE DRINKS  (weight = 3)  — moderation
#     ml/day; score: 0=0 ; 0-250=-0.5 ; 250-750=-1 ; ≥750=-2
# =============================================================================

# Values already in g/day (≈ ml) — no /7
pnns$boissons_sucrees_ml_j <- pnns$SODAS_SUCRES_FFQ +
  pnns$SODAS_LIGHT_FFQ  +
  pnns$FRUITS_JUS_FFQ

pnns$score_boissons_raw <- ifelse(pnns$boissons_sucrees_ml_j == 0,    0,
                                  ifelse(pnns$boissons_sucrees_ml_j < 250,  -0.5,
                                         ifelse(pnns$boissons_sucrees_ml_j < 750,  -1, -2)))

pnns$comp_boissons <- (pnns$score_boissons_raw / -2) * 3   # weight = 3


# =============================================================================
# 12. ALCOHOL  (weight = 3)  — moderation
#     g/week; score: 0=+0.5 ; 0-100=0 ; 100-200=-1 ; >200=-2
# =============================================================================

# g/day * 7 → g/week
pnns$alcool_g_sem <- pnns$ALCOOL_FFQ * 7

pnns$score_alcool_raw <- ifelse(pnns$alcool_g_sem == 0,     0.5,
                                ifelse(pnns$alcool_g_sem <= 100,   0,
                                       ifelse(pnns$alcool_g_sem <= 200,  -1, -2)))

pnns$comp_alcool <- (pnns$score_alcool_raw / -2) * 3   # weight = 3, max abs = 2


# =============================================================================
# CALCULATING THE SIMPLIFIED PNNS-GS2 SCORE
# Formula: Σ (component_i × weight_i / max(|component_i|))
# Here each comp_X is already weighted and normalized by the absolute max.
# =============================================================================

pnns$sPNNS_GS2 <- pnns$comp_fv       +   # weight 3, adequacy
  pnns$comp_noix     +   # weight 1, adequacy
  pnns$comp_leg      +   # weight 1, adequacy
  pnns$comp_fec      +   # weight 2, adequacy
  pnns$comp_pl       +   # weight 1, adequacy
  pnns$comp_poisson  +   # weight 2, adequacy
  pnns$comp_vr       +   # weight 2, moderation (negative)
  pnns$comp_charc    +   # weight 3, moderation (negative)
  pnns$comp_mga      +   # weight 2, mixed
  pnns$comp_sucre    +   # weight 3, moderation (negative)
  pnns$comp_boissons +   # weight 3, moderation (negative)
  pnns$comp_alcool       # weight 3, mixed


pnns2_data <- pnns %>%
  select(Identifiant, sPNNS_GS2)
# Filters to exclude outlier FFQs----------------------------
#temp <- metadata[, c('Identifiant', 'Sexe')]
#temp$borne_inf <- ifelse((temp$Sexe=="Femme"), (500),(800))
#temp$borne_sup <-ifelse((temp$Sexe=="Femme"), (3500),(4000)) 
#temp2 <- somme_par_identifiant[, c('Identifiant', 'SOMME_FFQ_KCAL')]
#temp <- left_join(temp, temp2, by="Identifiant")
#temp <- temp %>%
#  filter( borne_inf <= SOMME_FFQ_KCAL  & SOMME_FFQ_KCAL <= borne_sup )
#Liste <- unique(temp$Identifiant)
#metadata <- subset(metadata, Identifiant %in% Liste)
#Frame <- subset(Frame, Identifiant %in% Liste)
#Frame_bis <- subset(Frame_bis, Identifiant %in% Liste)
#Poids_modifie <- subset(Poids_modifie, Identifiant %in% Liste)
#FFQ_POIDS <- subset(FFQ_POIDS, Identifiant %in% Liste)
#FFQ_KCAL <- subset(FFQ_KCAL, Identifiant %in% Liste)
#somme_par_identifiant <- subset(somme_par_identifiant, Identifiant %in% Liste)
#somme_par_identifiant_env <- subset(somme_par_identifiant_env, Identifiant %in% Liste)
###Processing list#### 
#Excluding Opticourses -----------------------------
identifiants <- c("LE012", "LE017", "LE021", "LE028", "LE037", "LE040", "LE043", "LE045", "LE049", "LE058", 
                  "LE059", "LE064", "LE068", "LE076", "LE077", "LE083", "LE086", "LE087", "LE099", "LE129", 
                  "LE130", "LE142", "LE146", "LE147", "LE149", "LE152", "LE158", "LE163", "LE169", "LE170", 
                  "LE176", "LE177", "LE184", "LE191", "LE205", "PS287", "LE208", "LE210", "LE224", "LE232", 
                  "LE246", "LE249", "PS001", "PS003", "PS014", "PS016", "PS023", "PS026", "PS041", "PS044", 
                  "PS046", "PS049", "PS058", "PS059", "PS061", "PS072", "PS075", "PS094", "PS104", "PS106", 
                  "PS110", "PS116", "PS137", "PS143", "PS158", "PS164", "PS165", "PS168", "PS169", "PS178", 
                  "PS180", "PS190", "PS193", "PS203", "PS204", "PS206", "PS207", "PS210", "PS215", "PS218", 
                  "PS221", "PS237", "PS259", "PS265", "PS269", "PS272", "PS277", "PS282")

metadata <- subset(metadata, !(Identifiant %in% identifiants))
Frame_bis <- subset(Frame_bis, !(Identifiant %in% identifiants))
Poids_modifie <- subset(Poids_modifie, !(Identifiant %in% identifiants))
FFQ_POIDS <- subset(FFQ_POIDS, !(Identifiant %in% identifiants))
FFQ_KCAL <- subset(FFQ_KCAL, !(Identifiant %in% identifiants))
somme_par_identifiant<- subset(somme_par_identifiant, !(Identifiant %in% identifiants))
somme_par_identifiant_env <- subset(somme_par_identifiant_env, !(Identifiant %in% identifiants))

#Building the final table ------------------------------------------------
FFQ_id <- metadata  
#Building the treatment lists based on compliance with the check-sending process
if (campaign == "23-02" |campaign == "24-03") {
  new_df <- Recap_envoi_cheques[, c("Identifiant", "Montant mensuel total")]
  FFQ_id<- left_join(FFQ_id, new_df, by='Identifiant')
  FFQ_id$groupe <- ifelse (!is.na(FFQ_id$`Montant mensuel total`),(1),(0))
  FFQ_id$`Montant mensuel total` <- NULL}
if (campaign == "22-11" |campaign == "23-11") { FFQ_id$Periode <-0   }
if (campaign == "23-02" |campaign == "24-03") { FFQ_id$Periode <-1   }
FFQ_id$Mesure <- "FFQ"
FFQ_id <- inner_join(FFQ_id, FFQ_POIDS, by='Identifiant')
FFQ_id <- inner_join(FFQ_id, pnns2_data, by = "Identifiant")
FFQ_id <- inner_join(FFQ_id, FFQ_KCAL, by='Identifiant')
new_df <- somme_par_identifiant[, c("Identifiant", "MAR", "MER", "t_ratio_prot", "t_ratio_fibre", "t_ratio_lino", "t_ratio_alphalino", 
                                    "t_ratio_dha", "t_ratio_potassium", "t_ratio_calcium", "t_ratio_selenium", 
                                    "t_ratio_iode", "t_ratio_vit_d", "t_ratio_vit_c", "t_ratio_vit_b2", 
                                    "t_ratio_vit_b12", "t_ratio_vit_b9", "t_ratio_magnesium", "t_ratio_fer", 
                                    "t_ratio_cuivre", "t_ratio_zinc", "t_ratio_vit_a", "t_ratio_vit_e", 
                                    "t_ratio_vit_b1", "t_ratio_vit_b3", "t_ratio_vit_b6", "t_ratio_ags", 
                                    "t_ratio_sodium", "t_ratio_sucre_aj", "HENI")]
FFQ_id <- inner_join(FFQ_id, new_df, by='Identifiant')
FFQ_id <- inner_join(FFQ_id, somme_par_identifiant_env, by='Identifiant')
FFQ_id <- subset(FFQ_id, SOMME_FFQ_KCAL > 0)





# DOWNLOAD ----------------------------

# Create a new workbook object
wb <- createWorkbook()

# Add each dataframe to a different sheet
addWorksheet(wb, "Tableau_d'indicateurs")
writeData(wb, sheet = "Tableau_d'indicateurs", FFQ_id)

addWorksheet(wb, "Metadata")
writeData(wb, sheet = "Metadata", metadata)


addWorksheet(wb, "Frequences_corrigées")
writeData(wb, sheet = "Frequences_corrigées", Frame)

addWorksheet(wb, "Poids_corrigés")
writeData(wb, sheet = "Poids_corrigés", FFQ_POIDS_Int)


if (campaign == "22-11") {
  saveWorkbook(wb,(paste0("FFQ_Tableaux_nov_22.xlsx")))
}else{ 
  if (campaign == "23-02") {
    saveWorkbook(wb,(paste0("FFQ_Tableaux_mars_23.xlsx"))) 
  } else {
    if (campaign == "23-11") {
      saveWorkbook(wb,(paste0("FFQ_Tableaux_nov_23.xlsx"))) 
    } else { 
      saveWorkbook(wb,(paste0("FFQ_Tableaux_mars_24.xlsx")))  
    }}}
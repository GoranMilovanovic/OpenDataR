## ----echo = T, message = F-----------------------------------------------
rm(list = ls())
library(dplyr)
library(tidyr)
library(stringr)
library(knitr)

### --- Working Directory
wDir <- '../ComInfoPublicImportance'
setwd(wDir)

### --- Load Raw Data Set
fileLoc <- 'zalbepristup.csv'
rawData <- read.table(fileLoc,
                      header = T,
                      sep = "|",
                      check.names = F,
                      stringsAsFactors = F) 

### --- Inspect Data Set
dim(rawData)

## ----echo = T------------------------------------------------------------
glimpse(rawData)

## ----echo = T------------------------------------------------------------
sum(duplicated(rawData$Code))

## ----echo = T------------------------------------------------------------
duplicatedCodes <- rawData$Code[which(duplicated(rawData$Code))]
duplicatedCodes

## ----echo = T------------------------------------------------------------
duplicatedCodes <- rawData$Code[which(duplicated(rawData$Code))]
inspectDuplicates <- rawData[rawData$Code %in% duplicatedCodes, ]
inspectDuplicates$ArticalStatus

## ----echo = T------------------------------------------------------------
inspectDuplicates$CreateDate

## ----echo = T------------------------------------------------------------
rawData <- rawData[-which(rawData$Code %in% duplicatedCodes), ]
dim(rawData)

## ----echo = T------------------------------------------------------------
table(rawData$ArticalType)

## ----echo = T------------------------------------------------------------
rawData$ArticalType <- NULL

## ----echo = T------------------------------------------------------------
table(rawData$ApplicantGroup)

## ----echo = T------------------------------------------------------------
rawData$ApplicantGroup <- recode(rawData$ApplicantGroup,
                                 Advokati = "Lawyers",
                                 Građani = "Citizens",
                                 Medij = "Media",
                                 Mediji = "Media",
                                 `NVO i druga udruženja` = "NGO/OtherOrgs",
                                 `Organi Vlasti` = "Government Agencies",
                                 Ostali = "Other",
                                 `Političke Stranke` = "Political Parties",
                                 `Republičke agencije, direkcije, zavodi, fondovi i dr.` = "Republic Institutions - Various",
                                 `Sindikati` = "Labor Unions",
                                 `Sportske organizacije` = "Sport Orgs",
                                 `Ustanove osnovnog i srednjeg obrazovanja` = "Elemenatary and High School Education Institutions",
                                 `Ustanove socijalne zaštite` = "Social Welfare Institutions",
                                 `Ustanove u oblasti zdravstva` = "Public Health Instituions"
                                 )

## ----echo = T------------------------------------------------------------
table(rawData$ApplicantGroup)

## ----echo = T------------------------------------------------------------
length(unique(rawData$ApplicantCity))

## ----echo = T------------------------------------------------------------
rawData$ApplicantCity[rawData$ApplicantCity == 'Nepoznat'] <- NA
sum(is.na(rawData$ApplicantCity))

## ----echo = T------------------------------------------------------------
rawData$ApplicantCity <- str_to_title(rawData$ApplicantCity)

## ----echo = T------------------------------------------------------------
table(rawData$ArticalStatus)

## ----echo = T------------------------------------------------------------
rawData$ArticalStatus <- recode(rawData$ArticalStatus,
                                Активан = "Active",
                                Архивиран = "Archived",
                                Завршен = "Completed")
# Percentage:
articalTypes <- names(table(rawData$ArticalStatus))
percents <- paste0(round((table(rawData$ArticalStatus)/length(rawData$ArticalStatus))*100, 2), "%")
names(percents) <- articalTypes
percents

## ----echo = T------------------------------------------------------------
table(rawData$AuthorityGroup)

## ----echo = T------------------------------------------------------------
rawData$AuthorityGroup <- recode(rawData$AuthorityGroup,
                                 Građani = "Citizens",
                                 `Gradski i opštinski organi i mesne zajednice` = "City and Municipality Agencies, Community Centers",
                                 `Javna preduzeća` = "Public Companies",
                                 `Javne službe i druge organizacije lokalne samouprave` = "Public Departments and Other Organizations of Local Self-Government",
                                 `Lokalna javna preduzeća` = "Local Public Companies",
                                 `Mediji` = "Media",
                                 `Ministarstva` = "Ministries",
                                 `Najviši organi` = "Highest Agencies",
                                 `Nezavisni državni organi i tela` = "Independent Government Agencies and Bodies",
                                 `NVO i druga udruženja` = "NGO/OtherOrgs",
                                 `Organi lokalne samouprave` = "Local Self-Government Agencies",
                                 `Organi Vlasti` = "Government Agencies",
                                 `Ostali` = "Other",
                                 `Pokrajinski organi` = "Province Agencies",
                                 `Pravosudni organi` = "Juridical Agencies",
                                 `Republička javna preduzeća` = "Republic Public Companies",
                                 `Republičke agencije, direkcije, zavodi, fondovi i dr.` = "Republic Institutions - Various",
                                 `Republički organi` = "Republic Agencies",
                                 `Sindikati` = "Labor Unions",
                                 `Sportske organizacije` = "Sport Orgs",
                                 `Ustanove osnovnog i srednjeg obrazovanja` = "Elemenatary and High School Education Institutions",
                                 `Ustanove socijalne zaštite` = "Social Welfare Institutions",
                                 `Ustanove u oblasti nauke, kulture i informisanja` = "Science, Culture, and Information Institutions",
                                 `Ustanove u oblasti obrazovanja` = "Education Institutions",
                                 `Ustanove u oblasti odbrane` = "Defense Institutions",
                                 `Ustanove u oblasti pravosuđa` = "Juridical Institutions",
                                 `Ustanove u oblasti privrede, poljoprivrede, šumarstva i vodoprivrede` = "Economy, Agriculture, Forestry, and Water Management Institutions",
                                 `Ustanove u oblasti zdravstva` = "Public Health Institutions",
                                 `Ustanove visokog obrazovanja` = "Higher Education Institutions"
                                 )

## ----echo = T------------------------------------------------------------
table(rawData$AuthorityGroup)

## ----echo = T------------------------------------------------------------
length(unique((rawData$AuthorityCity)))

## ----echo = T------------------------------------------------------------
rawData$AuthorityCity[rawData$AuthorityCity == 'Nepoznat'] <- NA
sum(is.na(rawData$AuthorityCity))

## ----echo = T------------------------------------------------------------
rawData$AuthorityCity <- str_to_title(rawData$AuthorityCity)

## ----echo = T------------------------------------------------------------
table(rawData$BasicOfStarting)

## ----echo = T------------------------------------------------------------
rawData$BasicOfStarting <- recode(rawData$BasicOfStarting,
                                  `Žalba na ćutanje uprave` = "Silence of authority",
                                  `Žalba na obaveštenje sa elementima odluke` = "Complaint on information w. elements of decision",
                                  `Žalba na odgovor` = "Complaint on response",
                                  `Žalba na zakljucak i rešenje` = "Complaint on conclusion and decision"
                                  )
rawData$BasicOfStarting[rawData$BasicOfStarting == "No data"] <- NA

## ----echo = T------------------------------------------------------------
table(rawData$BasicOfStarting)

## ----echo = T------------------------------------------------------------
table(rawData$Reason)

## ----echo = T------------------------------------------------------------
rawData$Reason <- recode(rawData$Reason,
                         `čl. 13. - zloupotreba prava` = "Article 13 - Abuse of Right",
                         `čl. 13. - zloupotreba prava,` = "Article 13 - Abuse of Right",
                         `čl. 13. - zloupotreba prava,čl. 14. - privatnost,` = "Article 13 - Abuse of Right, Article 14 - Privacy",
                         `čl. 14. - privatnost` = "Article 14 - Privacy",
                         `čl. 14. - privatnost,`= "Article 14 - Privacy",
                         `čl. 9. - tajnost, poverljivost` = "Article 9 = Secrecy, Confidentiality",
                         `čl. 9. - tajnost, poverljivost,` = "Article 9 = Secrecy, Confidentiality",
                         `Ostalo` = "Other",
                         `Ostalo,` = "Other")
rawData$Reason[rawData$Reason == "No data"] <- NA

## ----echo = T------------------------------------------------------------
table(rawData$Reason)

## ----echo = T------------------------------------------------------------
table(rawData$Decision)

## ----echo = T------------------------------------------------------------
rawData$Decision <- recode(rawData$Decision,
                         `Rešenje - nalaže se` = "Decision - Ordered to",
                         `Rešenje - odbija kao neosnovana` = "Decision - Rejected (Ungrounded)",
                         `Rešenje - odbija se zahtev` = "Decision - Request Rejected",
                         `Rešenje - poništava se` = "Decision - Invalidated",
                         `Rešenje - poništava se i nalaže se` = "Decision - Invalidated/Ordered to",
                         `Rešenje - poništava se i vraća na ponovni postupak` = "Decision - Invalidated/Retrial",
                         `Službena beleška` = "Official Note",
                         `Zaključak o obustavi zbog naknadnog postupanja` = "Conclusion on suspension due to subsequent treatment",
                         `Zaključak o obustavi zbog odustanka` = "Conclusion on suspension due to withdrawal",
                         `Zaključak o obustavi zbog smrti stranke` = "Conclusion on suspension due to the death of party",
                         `Zaključak o odbacivanju - neblagovremena` = "Conclusion on rejection - untimely",
                         `Zaključak o odbacivanju - nedopuštena` = "Conclusion on rejection - inadmissible",
                         `Zaključak o odbacivanju - neuredna` = "Conclusion on rejection - messy",
                         `Zaključak o odbacivanju - od neovlašćenog lica` = "Conclusion on rejection - unauthorized person",
                         `Zaključak o odbacivanju - preurenjena` = "Conclusion on rejection - premature",
                         `Zaključak o odbacivanju - zbog nenadležnosti` = "Conclusion on rejection - lack of jurisdiction"
                         )
rawData$Decision[rawData$Decision == "No data"] <- NA

## ----echo = T------------------------------------------------------------
table(rawData$Decision)

## ----echo = T------------------------------------------------------------
table(rawData$Outcome)

## ----echo = T------------------------------------------------------------
rawData$Outcome <- recode(rawData$Outcome,
                         `Delimično izvršeno` = "Partially executed",
                         Izvršeno = "Executed",
                         Neizvršeno = "Not executed")
rawData$Outcome[rawData$Outcome == "No data"] <- NA

## ----echo = T------------------------------------------------------------
table(rawData$Outcome)

## ----echo = T------------------------------------------------------------
table(rawData$Oblast)

## ----echo = T------------------------------------------------------------
colnames(rawData)[which(colnames(rawData) == "Oblast")] <- "Domain"

## ----echo = T------------------------------------------------------------
rawData$Domain <- recode(rawData$Domain,
                         `Budžet (plate, donacije, sponzorstva...)` = "Budgetary",
                         `Budžet (plate, donacije, sponzorstva...),` = "Budgetary",
                         `Budžet (plate, donacije, sponzorstva...),Nepoznato,` = "Budgetary",
                         `Evidencija javne imovine` = "Evidence of public property",
                         `Evidencija javne imovine,` = "Evidence of public property",
                         `Investicije` = "Investment",
                         `Investicije,` = "Investment",
                         `Javne nabavke` = "Public procurement",
                         `Javne nabavke,` = "Public procurement",
                         `Katastar` = "Cadastre",
                         `Katastar,` = "Cadastre",
                         `Ostalo` = "Other",
                         `Ostalo,` = "Other",
                         `Policija i bezbednost` = "Police and Security",
                         `Policija i bezbednost,` = "Police and Security",
                         `Postupci organa uprave` = "Procedures of administrative bodies",
                         `Postupci organa uprave,` = "Procedures of administrative bodies",
                         `Postupci organa uprave,Nepoznato,` = "Procedures of administrative bodies",
                         `Pravosuđe` = "Jurisdiction",
                         `Pravosuđe,` = "Jurisdiction",
                         `Privatizacije i restruktuiranje` = "Privatization and restructuring",
                         `Privatizacije i restruktuiranje,` = "Privatization and restructuring",
                         `Raspolaganje javnom imovinom` = "Public property disposal",
                         `Raspolaganje javnom imovinom,` = "Public property disposal",
                         `Rehabilitacija` = "Rehabilitation",
                         `Rehabilitacija,` = "Rehabilitation",
                         `Restitucija` = "Restitution",
                         `Restitucija,` = "Restitution",
                         `Ugrožavanje i zaštita ljudi` = "Endangering and protection of people",
                         `Ugrožavanje i zaštita ljudi,` = "Endangering and protection of people",
                         `Ugrožavanje i zaštita životne sredine` = "Endangering and protection of the environment",
                         `Ugrožavanje i zaštita životne sredine,` = "Endangering and protection of the environment",
                         `Zaštita životinja` = "Animal protection",
                         `Zaštita životinja,` = "Animal protection"
                         )
rawData$Domain[rawData$Domain == "No data" | rawData$Domain == "Nepoznato" | rawData$Domain == "Nepoznato,"] <- NA

## ----echo = T------------------------------------------------------------
table(rawData$Domain)

## ----echo = T------------------------------------------------------------
rawData$CreateDateGroup <- str_sub(rawData$CreateDate, start = 1, end = 3)
table(rawData$CreateDateGroup)

## ----echo = T------------------------------------------------------------
rawData$DecisionDateGroup <- str_sub(rawData$DecisionDate, start = 1, end = 3)
table(rawData$DecisionDateGroup)

## ----echo = T------------------------------------------------------------
table(rawData$DecisionDateGroup, rawData$ArticalStatus)

## ----echo = T------------------------------------------------------------
unique(rawData$DecisionDate[grepl("^No", rawData$DecisionDateGroup)])

## ----echo = T------------------------------------------------------------
rawData$DecisionDate[which(grepl("^No", rawData$DecisionDateGroup))] <- NA

## ----echo = T------------------------------------------------------------
unique(rawData$DecisionDate[grepl("^202", rawData$DecisionDateGroup)])

## ----echo = T------------------------------------------------------------
rawData[which(grepl("^202", rawData$DecisionDateGroup)), ] <- NA
dim(rawData)

## ----echo = T------------------------------------------------------------
rawData$DecisionDateGroup <- str_sub(rawData$DecisionDate, start = 1, end = 3)
table(rawData$DecisionDateGroup, rawData$ArticalStatus, useNA = "always")

## ----echo = T------------------------------------------------------------
rawData$DecisionDate[which(grepl("^$", rawData$DecisionDate))] <- NA
rawData$DecisionDateGroup <- str_sub(rawData$DecisionDate, start = 1, end = 3)
table(rawData$DecisionDateGroup, rawData$ArticalStatus, useNA = "always")

## ----echo = T------------------------------------------------------------
unique(rawData$DecisionDate[which(grepl("^190", rawData$DecisionDate))])

## ----echo = T------------------------------------------------------------
rawData$DecisionDate[which(grepl("^190", rawData$DecisionDate))] <- NA
rawData$DecisionDateGroup <- str_sub(rawData$DecisionDate, start = 1, end = 3)
table(rawData$DecisionDateGroup, rawData$ArticalStatus, useNA = "always")

## ----echo = T------------------------------------------------------------
rawData$OutcomeDateGroup <- str_sub(rawData$OutcomeDate, start = 1, end = 3)
table(rawData$OutcomeDateGroup)

## ----echo = T------------------------------------------------------------
table(rawData$OutcomeDateGroup, rawData$ArticalStatus)

## ----echo = T------------------------------------------------------------
unique(rawData$OutcomeDate[grepl("^No", rawData$OutcomeDateGroup)])

## ----echo = T------------------------------------------------------------
rawData$OutcomeDate[which(grepl("^No", rawData$OutcomeDateGroup))] <- NA

## ----echo = T------------------------------------------------------------
rawData$OutcomeDateGroup <- str_sub(rawData$OutcomeDate, start = 1, end = 3)
table(rawData$OutcomeDateGroup, rawData$ArticalStatus, useNA = "always")

## ----echo = T------------------------------------------------------------
rawData$OutcomeDate[which(grepl("^$", rawData$OutcomeDate))] <- NA
rawData$OutcomeDateGroup <- str_sub(rawData$OutcomeDate, start = 1, end = 3)
table(rawData$OutcomeDateGroup, rawData$ArticalStatus, useNA = "always")

## ----echo = T------------------------------------------------------------
unique(rawData$OutcomeDate[which(grepl("^190", rawData$OutcomeDate))])

## ----echo = T------------------------------------------------------------
rawData$OutcomeDate[which(grepl("^190", rawData$OutcomeDate))] <- NA
rawData$OutcomeDateGroup <- str_sub(rawData$OutcomeDate, start = 1, end = 3)
table(rawData$OutcomeDateGroup, rawData$ArticalStatus, useNA = "always")

## ----echo = T------------------------------------------------------------
w1 <- which(is.na(rawData$ArticalStatus) & is.na(rawData$DecisionDate))
w2 <- which(is.na(rawData$ArticalStatus) & is.na(rawData$OutcomeDate))
rawData[w1, ]

## ----echo = T------------------------------------------------------------
w1 == w2

## ----echo = T------------------------------------------------------------
rawData <- rawData[-w1, ]

## ----echo = T------------------------------------------------------------
rawData[c('CreateDateGroup', 'DecisionDateGroup', 'OutcomeDateGroup')] <- NULL

## ----echo =  T-----------------------------------------------------------
write.csv(rawData, file = "Complaints_FreedomOfInformation.csv")


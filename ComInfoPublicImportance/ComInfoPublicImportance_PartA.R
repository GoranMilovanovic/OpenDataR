#' # Open Data Notebooks 2017-02A :: ODN2017-02A
#' ## Case Study: Commissioner for Information of Public Importance and Personal Data Protection, Republic of Serbia: "Complaints in The Field of Freedom of Information" Data Set
#' ### Part A: Import, clean up, and translate to English
#' 
#' ***
#' 
#' #### Data Set: zalbepristup.csv
#' #### Source: [data.gov.rs](http://data.gov.rs)
#' #### Accessed on 05 Feb 2017 from [data.gov.rs/sr/datasets/](http://data.gov.rs/sr/datasets/zalbe-iz-oblasti-prava-na-pristup-informacijama/)
#' #### Description: Complaints in The Field of Freedom of Information
#' 
#' *** 
#' ![](../img/GoranSMilovanovic.jpg)
#' 
#' **Author:** [Goran S. Milovanovic](http//www.exactness.net), [Data Science Serbia](http//www.datascience.rs)
#' 
#' **Notebook:** 02/05/2017, Belgrade, Serbia
#' 
#' ![](../img/DataScienceSerbia_Logo.png)
#' 
#' ***
#' 
#' The notebook focuses on an exploratory analysis of the open data set on the *Complaints in the field of freedom of information*, provided at the [Open Data Portal of the Republic of Serbia](http://data.gov.rs/sr/) *that is currently under development*. The data set was kindly provided to the Open Data Portal by the [Commissioner for Information of Public Importance and Personal Data Protection](http://www.poverenik.rs/en.html) of the Republic of Serbia. Many more open data sets will be indexed and uploaded to the [Open Data Portal of the Republic of Serbia](http://data.gov.rs/sr/) in the forthcoming weeks and months. 
#' 
#' As of the data set: (a) no metadata were provided; (b) the translation of legal terms from Serbian to English is mine, meaning: a lot of Google Translate suggestions were used (I'm a psychologists, not a lawyer); (c) mixture of latin and cyrilic alphabet was detected in the data; (d) thorough cleaning takes place here, in Part A; exploratory analysis + data visualizations will be presented soon (Part B).
#' 
#' ***
#' 
#' **Disclaimer.** The [Open Data Portal of the Republic of Serbia](http://data.gov.rs/sr/) is a young initiative that is currently under development. Neither the owner of this GitHub account as an individual, or [Data Science Serbia](http//www.datascience.rs) as an organization, hold any responsibility for the changes in the URLs of the data sets, or the changes in the content of the data sets published on  the [Open Data Portal of the Republic of Serbia](http://data.gov.rs/sr/). The results of the exploratory analyses and statistical models that are presented on this GitHub account are developed for illustrative purposes only, having in mind the goal of popularization of Open Data exclusively. The owner of this GitHub account strongly advises to consult him (e-mail: [goran.s.milovanovic@gmail.com](mailto:goran.s.milovanovic@gmail.com) and [Data Science Serbia](http//www.datascience.rs) before using the results presented here in public debate or media, and/or for any purposes other than motivating the usage and development of Open Data.  
#' 
#' ***
#' 
#' ### 1. Setup
#' 
#' Load libraries + raw data:
#' 
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

#' 
#' Take a sneak peek at the data set:
#' 
## ----echo = T------------------------------------------------------------
glimpse(rawData)

#' 
#' ### Recoding: translate to English and check for consistency
#' 
## ----echo = T------------------------------------------------------------
sum(duplicated(rawData$Code))

#' 
#' There are 23 duplicated Code values (complaint IDs, presumeably); checking it out:
#' 
## ----echo = T------------------------------------------------------------
duplicatedCodes <- rawData$Code[which(duplicated(rawData$Code))]
duplicatedCodes

#' 
#' Inspect the `ArticalStatus` value of the duplicated entries:
#' 
## ----echo = T------------------------------------------------------------
duplicatedCodes <- rawData$Code[which(duplicated(rawData$Code))]
inspectDuplicates <- rawData[rawData$Code %in% duplicatedCodes, ]
inspectDuplicates$ArticalStatus

#' 
#' `"Активан"` means `Active` in Serbian; inspect the `CreateDate` field:
#' 
## ----echo = T------------------------------------------------------------
inspectDuplicates$CreateDate

#' 
#' Almost all duplicated `Code` values refer to recently filed and still active complaints; we will assume that complaint procedure is still under way and/or the database was not updated or made consistent in respect to the must recent changes and delete them.
#' 
## ----echo = T------------------------------------------------------------
rawData <- rawData[-which(rawData$Code %in% duplicatedCodes), ]
dim(rawData)

#' 
## ----echo = T------------------------------------------------------------
table(rawData$ArticalType)

#' 
#' `Žalba` means `Complaint` in Serbian; thus, nothing informative here: delete `ArticleType`:
#' 
## ----echo = T------------------------------------------------------------
rawData$ArticalType <- NULL

#' 
## ----echo = T------------------------------------------------------------
table(rawData$ApplicantGroup)

#' 
#' Translate `ApplicantGroup` to English:
#' 
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

#' 
#' Check:
#' 
## ----echo = T------------------------------------------------------------
table(rawData$ApplicantGroup)

#' 
#' `Applicant City`:
#' 
## ----echo = T------------------------------------------------------------
length(unique(rawData$ApplicantCity))

#' 
#' Recode `Nepoznat` (`Unknown` in English) from `ApplicantCity` to `NA`:
#' 
## ----echo = T------------------------------------------------------------
rawData$ApplicantCity[rawData$ApplicantCity == 'Nepoznat'] <- NA
sum(is.na(rawData$ApplicantCity))

#' 
#' Fix city names in `ApplicantCity`:
#' 
## ----echo = T------------------------------------------------------------
rawData$ApplicantCity <- str_to_title(rawData$ApplicantCity)

#' 
## ----echo = T------------------------------------------------------------
table(rawData$ArticalStatus)

#' 
#' Translate `ArticleStatus` to English:
#' 
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

#' 
#' Inspect `AuthorityGroup`:
#' 
## ----echo = T------------------------------------------------------------
table(rawData$AuthorityGroup)

#' 
#' Translate `AuthorityGroup` to English:
#' 
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

#' 
#' Check `AuthorityGroup`:
#' 
## ----echo = T------------------------------------------------------------
table(rawData$AuthorityGroup)

#' 
#' Inspect `AuthorityCity`:
#' 
## ----echo = T------------------------------------------------------------
length(unique((rawData$AuthorityCity)))

#' 
#' Recode `Nepoznat` (`Unknown` in English) from `AuthorityCity` to `NA`:
#' 
## ----echo = T------------------------------------------------------------
rawData$AuthorityCity[rawData$AuthorityCity == 'Nepoznat'] <- NA
sum(is.na(rawData$AuthorityCity))

#' 
#' Fix city names in `AuthorityCity`:
#' 
## ----echo = T------------------------------------------------------------
rawData$AuthorityCity <- str_to_title(rawData$AuthorityCity)

#' 
#' Inspect `BasicOfStarting`:
#' 
## ----echo = T------------------------------------------------------------
table(rawData$BasicOfStarting)

#' 
## ----echo = T------------------------------------------------------------
rawData$BasicOfStarting <- recode(rawData$BasicOfStarting,
                                  `Žalba na ćutanje uprave` = "Silence of authority",
                                  `Žalba na obaveštenje sa elementima odluke` = "Complaint on information w. elements of decision",
                                  `Žalba na odgovor` = "Complaint on response",
                                  `Žalba na zakljucak i rešenje` = "Complaint on conclusion and decision"
                                  )
rawData$BasicOfStarting[rawData$BasicOfStarting == "No data"] <- NA

#' 
#' Check:
#' 
## ----echo = T------------------------------------------------------------
table(rawData$BasicOfStarting)

#' 
#' Inspect `Reason`:
#' 
## ----echo = T------------------------------------------------------------
table(rawData$Reason)

#' 
#' Translate `Reason` values to English:
#' 
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

#' 
#' Check `Reason`:
#' 
## ----echo = T------------------------------------------------------------
table(rawData$Reason)

#' 
#' Inspect `Decision`:
#' 
## ----echo = T------------------------------------------------------------
table(rawData$Decision)

#' 
#' Translate `Decision` to English:
#' 
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

#' 
#' Check `Decision`:
#' 
## ----echo = T------------------------------------------------------------
table(rawData$Decision)

#' 
#' Inspect `Outcome`:
#' 
## ----echo = T------------------------------------------------------------
table(rawData$Outcome)

#' 
#' Translate `Outcome` to English:
#' 
## ----echo = T------------------------------------------------------------
rawData$Outcome <- recode(rawData$Outcome,
                         `Delimično izvršeno` = "Partially executed",
                         Izvršeno = "Executed",
                         Neizvršeno = "Not executed")
rawData$Outcome[rawData$Outcome == "No data"] <- NA

#' 
#' Check `Outcome`:
#' 
## ----echo = T------------------------------------------------------------
table(rawData$Outcome)

#' 
#' Inspect `Oblast` (`Domain` in English):
#' 
## ----echo = T------------------------------------------------------------
table(rawData$Oblast)

#' 
#' Translate column name to English:
#' 
## ----echo = T------------------------------------------------------------
colnames(rawData)[which(colnames(rawData) == "Oblast")] <- "Domain"

#' 
#' Translate `Domain` values to English:
#' 
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

#' 
#' Check `Domain`:
#' 
## ----echo = T------------------------------------------------------------
table(rawData$Domain)

#' 
#' Inspect `CreateDate`:
#' 
## ----echo = T------------------------------------------------------------
rawData$CreateDateGroup <- str_sub(rawData$CreateDate, start = 1, end = 3)
table(rawData$CreateDateGroup)

#' 
#' Inspect `DecisionDate`:
#' 
## ----echo = T------------------------------------------------------------
rawData$DecisionDateGroup <- str_sub(rawData$DecisionDate, start = 1, end = 3)
table(rawData$DecisionDateGroup)

#' 
#' Ok. Let's see:
#' 
## ----echo = T------------------------------------------------------------
table(rawData$DecisionDateGroup, rawData$ArticalStatus)

#' 
#' What are these dates that begin with `No`?
#' 
## ----echo = T------------------------------------------------------------
unique(rawData$DecisionDate[grepl("^No", rawData$DecisionDateGroup)])

#' 
#' Ok. Fix:
#' 
## ----echo = T------------------------------------------------------------
rawData$DecisionDate[which(grepl("^No", rawData$DecisionDateGroup))] <- NA

#' 
#' What about the dates from the next decade?
#' 
## ----echo = T------------------------------------------------------------
unique(rawData$DecisionDate[grepl("^202", rawData$DecisionDateGroup)])

#' 
#' These entires are most probably not archived correctly; fix this:
#' 
## ----echo = T------------------------------------------------------------
rawData[which(grepl("^202", rawData$DecisionDateGroup)), ] <- NA
dim(rawData)

#' 
#' How does the data set looks now:
#' 
## ----echo = T------------------------------------------------------------
rawData$DecisionDateGroup <- str_sub(rawData$DecisionDate, start = 1, end = 3)
table(rawData$DecisionDateGroup, rawData$ArticalStatus, useNA = "always")

#' 
#' Empty cells will be transformed to `NAs`:
#' 
## ----echo = T------------------------------------------------------------
rawData$DecisionDate[which(grepl("^$", rawData$DecisionDate))] <- NA
rawData$DecisionDateGroup <- str_sub(rawData$DecisionDate, start = 1, end = 3)
table(rawData$DecisionDateGroup, rawData$ArticalStatus, useNA = "always")

#' 
#' Inspect what is encoded as if it has happend in the `1990s` again:
#' 
## ----echo = T------------------------------------------------------------
unique(rawData$DecisionDate[which(grepl("^190", rawData$DecisionDate))])

#' 
#' Since all entries where `DecisionDate` is encoded as `1900-01-01` are still active, we will assume that the appropriate code for them is really `NA`:
#' 
## ----echo = T------------------------------------------------------------
rawData$DecisionDate[which(grepl("^190", rawData$DecisionDate))] <- NA
rawData$DecisionDateGroup <- str_sub(rawData$DecisionDate, start = 1, end = 3)
table(rawData$DecisionDateGroup, rawData$ArticalStatus, useNA = "always")

#' 
#' Ok. We have only three complaints with no `ArticalStatus` code, and only 6 archived and one completed with no `DecisionDate` info; 4133 complaints are active at the present moment and their `DecisionDate` values are naturally set to `NA`.
#' 
#' Inspect `OutcomeDate`
#' 
## ----echo = T------------------------------------------------------------
rawData$OutcomeDateGroup <- str_sub(rawData$OutcomeDate, start = 1, end = 3)
table(rawData$OutcomeDateGroup)

#' 
#' Ok. Let's do the same as we did for the `DecisionDate` column:
#' 
## ----echo = T------------------------------------------------------------
table(rawData$OutcomeDateGroup, rawData$ArticalStatus)

#' 
#' Dates that begin with `No` are probably `No Data` entries:
#' 
## ----echo = T------------------------------------------------------------
unique(rawData$OutcomeDate[grepl("^No", rawData$OutcomeDateGroup)])

#' 
#' Ok. Fix:
#' 
## ----echo = T------------------------------------------------------------
rawData$OutcomeDate[which(grepl("^No", rawData$OutcomeDateGroup))] <- NA

#' 
#' How does the data set looks now:
#' 
## ----echo = T------------------------------------------------------------
rawData$OutcomeDateGroup <- str_sub(rawData$OutcomeDate, start = 1, end = 3)
table(rawData$OutcomeDateGroup, rawData$ArticalStatus, useNA = "always")

#' 
#' Empty cells will be transformed to `NAs`:
#' 
## ----echo = T------------------------------------------------------------
rawData$OutcomeDate[which(grepl("^$", rawData$OutcomeDate))] <- NA
rawData$OutcomeDateGroup <- str_sub(rawData$OutcomeDate, start = 1, end = 3)
table(rawData$OutcomeDateGroup, rawData$ArticalStatus, useNA = "always")

#' 
#' Inspect what is encoded as if it has happend in the `1990s`:
#' 
## ----echo = T------------------------------------------------------------
unique(rawData$OutcomeDate[which(grepl("^190", rawData$OutcomeDate))])

#' 
#' Again, we will assume that the appropriate code here is really `NA`:
#' 
## ----echo = T------------------------------------------------------------
rawData$OutcomeDate[which(grepl("^190", rawData$OutcomeDate))] <- NA
rawData$OutcomeDateGroup <- str_sub(rawData$OutcomeDate, start = 1, end = 3)
table(rawData$OutcomeDateGroup, rawData$ArticalStatus, useNA = "always")

#' 
#' There seem to be three entries with `NA` values on `ArticleStatus`, `DecisionDate`, and `OutcomeDate`. Check these:
#' 
## ----echo = T------------------------------------------------------------
w1 <- which(is.na(rawData$ArticalStatus) & is.na(rawData$DecisionDate))
w2 <- which(is.na(rawData$ArticalStatus) & is.na(rawData$OutcomeDate))
rawData[w1, ]

#' 
## ----echo = T------------------------------------------------------------
w1 == w2

#' 
#' Remove these entries from the data set:
#' 
## ----echo = T------------------------------------------------------------
rawData <- rawData[-w1, ]

#' 
#' Drop auxiliary columns:
#' 
## ----echo = T------------------------------------------------------------
rawData[c('CreateDateGroup', 'DecisionDateGroup', 'OutcomeDateGroup')] <- NULL

#' 
#' Final check for missing values: are there any rows with information completelly missing?
#' 
## ----echo = T------------------------------------------------------------
w <- which(rowSums(is.na(rawData)) == dim(rawData)[2])
w

#' 
#' Save the working data set:
#' 
## ----echo =  T-----------------------------------------------------------
write.csv(rawData, file = "Complaints_FreedomOfInformation.csv")

#' 
#' 
#' *** 
#' 
#' [Goran S. Milovanovic](http//www.exactness.net), [Data Science Serbia](http//www.datascience.rs), 02/05/2017, Belgrade, Serbia
#' 
#' ![](../img/DataScienceSerbia_Logo.png)
#' 
#' ***

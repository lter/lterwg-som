# README -----------------------------------------------------------------------

# Generate creator and metadataProvider details from database paper author list


# libraries --------------------------------------------------------------------

library(EML)
library(tidyverse)
library(fuzzyjoin)


# names ------------------------------------------------------------------------

creatorNames <- tibble(
  fromdoc = c(
    "William R. Wieder1",
    "Derek Pierson2",
    "Stevan R. Earl3",
    "Kate Lajtha2",
    "Sara Baer4",
    "Ford Ballantyne5",
    "Asmeret Asefaw Berhe6",
    "Sharon Billings7",
    "Laurel M. Brigham8",
    "Stephany S. Chacon2,9",
    "Jennifer Fraterrigo10",
    "Serita D. Frey11",
    "Katerina Georgiou12",
    "Marie-Anne de Graaff13",
    "A. Stuart Grandy11",
    "Melannie D. Hartman14",
    "Sarah E. Hobbie15",
    "Chris Johnson16",
    "Jason Kaye17",
    "Emily Kyker-Snowman11",
    "Marcy E. Litvak18",
    "Michelle C. Mack19",
    "Avni Malhotra20",
    "Jessica A. M. Moore21",
    "Knute Nadelhoffer22",
    "Craig Rasmussen23",
    "Whendee L. Silver24",
    "Benjamin N. Sulman25",
    "Xanthe Walker19",
    "Samantha Weintraub26"
  )
  ) %>%
mutate(
  #   full = str_match(fromdoc, "\\D+"),
  first = str_match(fromdoc, "^\\w+"),
  middle = str_match(fromdoc, "(?=\\w+\\.)\\w+"),
  last = str_match(str_match(fromdoc, "\\w+$"), "[A-z]+"),
  inst = str_match(fromdoc, "\\d+")
  ) %>%
mutate(
  first = case_when(
    grepl("Graaff", last, ignore.case = TRUE) ~ "Marie-Anne",
    TRUE ~ first
    ),
  last = case_when(
    grepl("Graaff", last, ignore.case = TRUE) ~ "de Graaff",
    TRUE ~ last
    ),
  first = case_when(
    grepl("grandy", last, ignore.case = TRUE) ~ "A",
    TRUE ~ first
    ),
  middle = case_when(
    grepl("grandy", last, ignore.case = TRUE) ~ "S",
    TRUE ~ middle
    ),
  last = case_when(
    grepl("Stephany", first, ignore.case = TRUE) ~ "Chacon",
    TRUE ~ last
    ),
  middle2 = case_when(
    grepl("Moore", last, ignore.case = TRUE) ~ "M"
    ),
  middle = case_when(
    grepl("Berhe", last, ignore.case = TRUE) ~ "A",
    TRUE ~ middle
  ),
  first_last = paste(first, last)
)


# emails -----------------------------------------------------------------------

emails <- tibble(
  emails = c(
    "William Wieder <wwieder@ucar.edu>",
    "smeret AB <asmeret@gmail.com>",
    "avni malhotra <avniji@gmail.com>",
    "Benjamin Sulman <bsulman@gmail.com>",
    "Charles T Driscoll Jr <ctdrisco@syr.edu>",
    "Chris E Johnson <cejohns@syr.edu>",
    "Craig Rasmussen <crasmuss@email.arizona.edu>",
    "Derek Pierson <piersond@oregonstate.edu>",
    "Emily Kyker-Snowman <ek2002@wildcats.unh.edu>",
    "Ford Ballantyne <fb4@uga.edu>",
    "Frey Serita <Serita.Frey@unh.edu>",
    "Jason Kaye <jpk12@psu.edu>",
    "Jennifer M Fraterrigo (jmf@illinois.edu) <jmf@illinois.edu>",
    "Kate Lajtha <lajthak@science.oregonstate.edu>",
    "Katerina Georgiou <georgiou.kat@gmail.com>",
    "Knute Nadelhoffer <knute@umich.edu>",
    "Lybrand Rebecca <rebecca.lybrand@oregonstate.edu>",
    "Marcy Litvak <mlitvak@unm.edu>",
    "Marie-Anne de Graaff <Marie-AnnedeGraaff@boisestate.edu>",
    "Melannie Hartman <Melannie.Hartman@colostate.edu>",
    "Merritt Turetsky <merritt.turetsky@colorado.edu>",
    "Michelle Cailin Mack <Michelle.Mack@nau.edu>",
    "Moore Jessica <jessica.am.bryant@gmail.com>",
    "Nina Wurzburger (ninawurz@uga.edu) <ninawurz@uga.edu>",
    "Samantha Weintraub <sweintraub@battelleecology.org>",
    "Sara G Baer <sgbaer@ku.edu>",
    "Sarah Hobbie <shobbie@umn.edu>",
    "Sharon Billings <sharonb@ku.edu>",
    "Stephany Chacon <stephany.chacon@oregonstate.edu>",
    "Stevan Earl <stevan.earl@asu.edu>",
    "Stuart Grandy <Stuart.Grandy@unh.edu>",
    "Whendee Silver <wsilver@berkeley.edu>",
    "Laurel Brigham <Laurel.Brigham@colorado.edu>"
  )
  ) %>%
mutate(
  address = str_match(emails, "<.*?>"),
  address = gsub("<|>", "", address),
  name = str_match(emails, "^.*<"),
  name = gsub("<", "", name),
  name = str_trim(name, side = c("both"))
)


# names and emails -------------------------------------------------------------

namesEmails <- stringdist_left_join(creatorNames, emails, by = c("first_last" = "name"), max_dist = 1) %>% 
  mutate(
    address = case_when(
      grepl("Sara Baer", first_last, ignore.case = TRUE) ~ "sgbaer@ku.edu",
      TRUE ~ address
    ),
    address = case_when(
      grepl("Asmeret Berhe", first_last, ignore.case = TRUE) ~ "asmeret@gmail.com",
      TRUE ~ address
    ),
    address = case_when(
      grepl("Jennifer Fraterrigo", first_last, ignore.case = TRUE) ~ "jmf@illinois.edu",
      TRUE ~ address
    ),
    address = case_when(
      grepl("Serita Frey", first_last, ignore.case = TRUE) ~ "Serita.Frey@unh.edu",
      TRUE ~ address
    ),
    address = case_when(
      grepl("A Grandy", first_last, ignore.case = TRUE) ~ "Stuart.Grandy@unh.edu",
      TRUE ~ address
    ),
    address = case_when(
      grepl("Chris Johnson", first_last, ignore.case = TRUE) ~ "cejohns@syr.edu",
      TRUE ~ address
    ),
    address = case_when(
      grepl("Emily Snowman", first_last, ignore.case = TRUE) ~ "ek2002@wildcats.unh.edu",
      TRUE ~ address
    ),
    address = case_when(
      grepl("Michelle Mack", first_last, ignore.case = TRUE) ~ "Michelle.Mack@nau.edu",
      TRUE ~ address
    ),
    address = case_when(
      grepl("Avni Malhotra", first_last, ignore.case = TRUE) ~ "avniji@gmail.com",
      TRUE ~ address
    ),
    address = case_when(
      grepl("Jessica Moore", first_last, ignore.case = TRUE) ~ "jessica.am.bryant@gmail.com",
      TRUE ~ address
    ),
    address = case_when(
      grepl("Xanthe Walker", first_last, ignore.case = TRUE) ~ "xanthe.walker@nau.edu",
      TRUE ~ address
    )
  )


# orcids and affiliations ------------------------------------------------------

orcidsAffiliations <- tibble(
  affiliations = c(
    "1 Institute of Arctic and Alpine Research, University of Colorado Boulder and the Climate and Global Dynamics Laboratory, National Center for Atmospheric Research, Boulder CO, USA. ORCiD 0000-0001-7116-1985",
    "2 Department of Crop and Soil Sciences, Oregon State University, Corvallis OR, USA ORCiD 0000-0003-3413-1693, 0000-0002-6430-4818",
    "3 Global Institute of Sustainability, Arizona State University, Tempe, AZ, USA ORCiD 0000-0002-4465-452X",
    "4 Department of Ecology and Evolutionary Biology and Kansas Biological Survey, University of Kansas, Lawrence, KS, USA ORCiD 0000-0001-6135-9760 ",
    "5 Odum School of Ecology, University of Georgia, USA",
    "6 Department of Life and Environmental Sciences; University of California, Merced; Merced, CA, USA ORCiD 0000-0002-6986-7943",
    "7 Department of Ecology and Evolutionary Biology and Kansas Biological Survey, University of Kansas, Lawrence, KS, USA ORCiD 0000-0003-1611-526X",
    "8 Department of Ecology and Evolutionary Biology and Institute of Arctic and Alpine Research, University of Colorado, Boulder, CO, USA ORCiD 0000-0001-5592-9165",
    "9 Climate and Ecosystem Sciences, Lawrence Berkeley National Laboratory, Berkeley, CA, USA ORCiD 0000-0001-7599-9152",
    "10 Department of Natural Resources and Environmental Sciences, University of Illinois, Urbana, IL, USA ORCiD 0000-0002-0357-1007",
    "11 Department of Natural Resources and the Environment, University of New Hampshire, Durham, NH, USA ORCiD 0000-0002-9221-5919, 0000-0002-3214-2295, 0000-0003-1782-1916",
    "12 Department of Earth System Science, Stanford University, Stanford, CA, USA ORCiD 0000-0002-2819-3292",
    "13 Department of Biological Sciences, Boise State University, Boise, ID, USA ORCiD 0000-0001-5668-7647",
    "14 Climate and Global Dynamics Laboratory, National Center for Atmospheric Research, Boulder CO, and Natural Resource Ecology Laboratory, Colorado State University, Fort Collins CO, USA ORCiD 0000-0002-0675-2292",
    "15 Department of Ecology, Evolution and Behavior, University of Minnesota, St. Paul, MN ORCiD 0000-0001-5159-031X",
    "16 Department of Civil and Environmental Engineering, Syracuse University, Syracuse, NY 13244 ORCiD 0000-0001-9079-813X",
    "17 Department of Ecosystem Science and Management, The Pennsylvania State University, University Park, PA, USA ORCiD 0000-0001-9762-0801",
    "18 Department of Biology, University of New Mexico, Albuquerque, NM, USA ORCiD 0000-0002-4255-2263",
    "19 Center for Ecosystem Science and Society and Department of Biological Sciences, Northern Arizona University, Flagstaff, AZ USA ORCiD Michelle Mack: 0000-0003-1279-4242, Xanthe Walker: 0000-0002-2448-691X",
    "20 Department of Earth System Science, Stanford University, Stanford, CA, USA ORCiD 0000-0002-7850-6402",
    "21 Bioscience Division, Oak Ridge National Laboratory, Oak Ridge, TN, USA ORCiD 0000-0002-5387-0662",
    "22 Department of Ecology and Evolutionary Biology, University of Michigan, Ann Arbor, MI, USA ORCiD 0000-0001-9775-894X",
    "23 Department of Environmental Science, The University of Arizona, Tucson AZ, USA ORCiD 0000-0003-4344-4800",
    "24 Department of Environmental Science, Policy, and Management, University of California, Berkeley, CA, USA ORCiD 0000-0003-0372-8745",
    "25 Climate Change Science Institute and Environmental Sciences Division, Oak Ridge National Laboratory, Oak Ridge, TN, USA ORCiD 0000-0002-3265-6691",
    "26 National Ecological Observatory Network, Battelle, Boulder, CO USA ORCiD 0000-0003-4789-5086"
  )
  ) %>% 
mutate(
  inst = str_match(affiliations, "^\\d+"),
  orcid = str_match(affiliations, "\\d+-\\d+-\\d+-\\d+|X"),
  org = str_match(affiliations, "^(.*?,.*?),"),
  org = gsub("^\\d+\\s", "", org),
  org = gsub(",$", "", org)
)

# unnest
orcidsAffiliations$org <- orcidsAffiliations$org[,1]

# customize select orgs
orcidsAffiliations <- orcidsAffiliations %>% 
  mutate(
    org = case_when(
      inst == "24" ~ "Department of Environmental Science, Policy, and Management, University of California, Berkeley",
      TRUE ~ org 
      ),
    org = case_when(
      inst == "15" ~ "Department of Ecology, Evolution and Behavior, University of Minnesota",
      TRUE ~ org 
      ),
    org = case_when(
      inst == "6" ~ "Department of Life and Environmental Sciences; University of California, Merced",
      TRUE ~ org 
      ),
    org = case_when(
      inst == "1" ~ "Institute of Arctic and Alpine Research, University of Colorado Boulder",
      TRUE ~ org 
      ),
    org2 = as.character(NA),
    org2 = case_when(
      inst == "1" ~ "Climate and Global Dynamics Laboratory, National Center for Atmospheric Research",
      TRUE ~ org2  
      ),
    org2 = case_when(
      inst == "14" ~ "Natural Resource Ecology Laboratory, Colorado State University",
      TRUE ~ org2  
    )
  )


# orcidsAffiliations + namesEmails ---------------------------------------------

authorList <- left_join(namesEmails, orcidsAffiliations, by = c("inst" = "inst")) %>%
  mutate(
    orcid = case_when(
      last == "Mack" ~ "0000-0003-1279-4242",
      last == "Walker" ~ "0000-0002-2448-691X",
      TRUE ~ orcid
      ),
    firstLast = paste0(tolower(first), last)
  )


# person_creator ---------------------------------------------------------------

person_creator <- function(
  givenName,
  middleName1 = NULL,
  middleName2 = NULL,
  surName,
  email,
  organization1,
  organization2 = NULL,
  orcid = NULL) {

  person <- EML::eml$creator(
    individualName = EML::eml$individualName(
      givenName = c(givenName, middleName1, middleName2),
      surName = surName),
    electronicMailAddress = email,
    organizationName = c(organization1, organization2),
    id = tolower(paste0("creator.", givenName, ".", surName))
  )

  if (!is.null(orcid)) {

    personOrcid <- EML::eml$userId(directory = "https://orcid.org")
    personOrcid$userId <- orcid
    person$userId <- personOrcid

  }

  return(person)

}

creators <- pmap(authorList, ~person_creator(
    givenName = ..2,
    middleName1 = ..3,
    middleName2 = ..6,
    surName = ..4,
    email = ..9,
    organization1 = ..13,
    organization2 = ..14,
    orcid = ..12
    ))


# metadata provider ------------------------------------------------------------

metadataProviderList <- authorList[c(1:3),]

person_metadata_provider <- function(
  givenName,
  middleName1 = NULL,
  middleName2 = NULL,
  surName,
  email,
  organization1,
  organization2 = NULL,
  orcid = NULL) {

  person <- EML::eml$metadataProvider(
    individualName = EML::eml$individualName(
      givenName = c(givenName, middleName1, middleName2),
      surName = surName),
    electronicMailAddress = email,
    organizationName = c(organization1, organization2),
    id = tolower(paste0("metadata_provider.", givenName, ".", surName))
  )

  if (!is.null(orcid)) {

    personOrcid <- EML::eml$userId(directory = "https://orcid.org")
    personOrcid$userId <- orcid
    person$userId <- personOrcid

  }

  return(person)

}

metadataProvider <- pmap(metadataProviderList, ~ person_metadata_provider(
    givenName = ..2,
    middleName1 = ..3,
    middleName2 = ..6,
    surName = ..4,
    email = ..9,
    organization1 = ..13,
    organization2 = ..14,
    orcid = ..12
    ))

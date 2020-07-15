# README -----------------------------------------------------------------------

# Workflow to (1) harvest provenance from EDI for datasets for which a DOI was
# provided, and (2) build geographic coverages based on location details


# libraries --------------------------------------------------------------------

library(rvest)
library(EDIutils)
library(EML)


# provenance -------------------------------------------------------------------

extract_prov <- function(url) {

  landingPage <- read_html(url)

  pageSubset <- landingPage %>%
    html_nodes(".no-list-style") %>%
    html_text()

  # these are all LTER packages so we can ignore the edi scope
  packageId <- str_extract(grep("knb-lter-", pageSubset, value = TRUE)[[1]], "^\\S*")
  #   packageId <- str_extract(grep("knb|edi", pageSubset, value = TRUE)[[1]], "^\\S*")

  packageProv <- emld::as_emld(EDIutils::api_get_provenance_metadata(packageId))
  packageProv$`@context` <- NULL
  packageProv$`@type` <- NULL

  # duplicate creator ids are causing problems so just remove them all
  for (i in 1:length(packageProv$dataSource$creator)) {

    packageProv$dataSource$creator[[i]]$id <- NULL

  }

  return(packageProv)

}


# datasets NOT in EDI
soils_data_harmonization %>%
  filter(!grepl("pasta|edirepository", data_doi)) %>%
  select(data_doi) %>%
  distinct(data_doi)

# these above are in fact all dois to papers (not datasets), except
# http://doi.org/10.1525/elementa.287, which also is a reference to a
# publication, but that does link out to a dataset.
# publication: https://www.elementascience.org/article/10.1525/elementa.287/
# dataset: https://www.ncbi.nlm.nih.gov/bioproject/?term=PRJNA395599

# datasets in EDI
pastaDatasets <- soils_data_harmonization %>%
  filter(grepl("pasta|edirepository", data_doi)) %>%
  select(data_doi) %>%
  distinct(data_doi) %>%
  pull(data_doi) %>%
  map(~ str_split(.x, ",|;")) %>%
  unlist() %>%
  str_trim() %>%
  as_tibble() %>%
  mutate(
    value = case_when(
      grepl("^1", value) ~ paste0("https://doi.org/", value),
      grepl("^doi:", value) ~ gsub("doi:", "https://doi.org/", value),
      grepl("^doi.org", value) ~ paste0("https://", value),
      TRUE ~ value
    )
    ) %>%
  pull(value) %>%
  as.list()

provenance <- map(pastaDatasets, ~ extract_prov(.x))


# the extract_prov function failed to remove one creator id for the Coweeta
# dataset with the following title so this additional processing step was
# required. Careful that this dataset may quite likely not be the 27th value in
# the list of datasets in future iterations.

# title: Fine root dynamics along an elevational gradient in the southern Appalachian
# mountains in the Coweeta Hydrologic Laboratory from 1993 to 1994

# lapply(provenance, function(x) { x$dataSource$creator$id })

provenance[[27]]$dataSource$creator$id <- NULL


# geographic coverages ---------------------------------------------------------

# geographicDescription <- "CAP LTER study area"
# coverage <- set_coverage(begin = "2014-09-01",
#                          end = "2015-03-30",
#                          geographicDescription = geographicDescription,
#                          west = -112.100, east = -111.877,
#                          north = +33.608, south = +33.328)

uniqueLocations <- soils_data_harmonization %>%
  select(network, site_code, location_name, lat, long) %>%
  mutate(network = as.character(network)) %>%
  filter(!is.na(lat) | !is.na(long)) %>%
  distinct()


geoCoverage <- uniqueLocations %>%

  pmap(function(...) {

    current <- tibble(...)

    if (!is.na(current$location_name)) {

      description <- paste0(
        "network: ", current$network, "; ",
        "site: ", current$site_code, "; ",
        "location: ", current$location_name
      )

    } else {

      description <- paste0(
        "network: ", current$network, "; ",
        "site: ", current$site_code
      )

    }

    #     description <- paste0(
    #       "network: ", current$network, "; ",
    #       "site: ", current$site_code, "; ",
    #       "location: ", current$location_name)

    geographic <- EML::eml$geographicCoverage(
      geographicDescription = description,
      boundingCoordinates = EML::eml$boundingCoordinates(
        westBoundingCoordinate = current$long,
        eastBoundingCoordinate = current$long,
        northBoundingCoordinate = current$lat,
        southBoundingCoordinate = current$lat
      )
    )

    return(geographic)

    })

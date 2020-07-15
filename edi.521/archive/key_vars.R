
# README -----------------------------------------------------------------------

# Noticed a lot of variables in the tarball that were not in the key file
# template (e.g., layer, dominant_species). Those discrepencies are explored in
# the following workflow, with the mismatched variables, which are essentially
# variables that users added to the key file, either incorporated into the
# correct variable, deleted, or added as a new variable. This additional
# processing was then added to the data-aggregation workflow for creating the
# tarball.


# libraries --------------------------------------------------------------------

library(googlesheets4)


# key file vars ----------------------------------------------------------------

key_location <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1dUr1Vwds51u0SVeRm6O7CNcv_IHOsAySucWKQz4W28o/edit#gid=1375348640",
  sheet = "Location_data"
)

key_profile <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1dUr1Vwds51u0SVeRm6O7CNcv_IHOsAySucWKQz4W28o/edit#gid=1375348640",
  sheet = "Profile_data (Key-Key)"
)

key_vars <- bind_rows(
  key_location %>%
    mutate(id = "loc") %>%
    select(id, var),
  key_profile %>%
    mutate(id = "pro") %>%
    select(id, var)
  ) %>%
pull(var)


# tarball vars -----------------------------------------------------------------

sort(colnames(soils_data_harmonization))
colnames(soils_data_harmonization)[which(!colnames(soils_data_harmonization) %in% key_vars)]
colnames(boundData)[which(!colnames(boundData) %in% key_vars)]

# [1] "google_dir"	"data_file"	"google_id"	        "layer"	"L1_level"	"tx_L1_level"	"L2_level"	"dominant_species"	"doi_number"
# [10] "L3_level"	"L4_level"	"comment_location"	"aspect"	"tx_L2_level"	"tx_L3_level"	"p_ex"	"L5_level"	"tx_L4_level"
# [19] "comments"	"loi"	        "litterfall_anpp"	"ca"	"mg"	"k"	"na"	"tx_L5_level"	"tx_L6_level"
# [28] "al"	        "no3_n"	        "nh4_n"	                "align"	"bd_methods_notes"	"eco_type"	"fe_HCl"	"c_tot_se"	"n_tot_se"
# [37] "n_replicates"	"layer_thick_calc"	"lyr_soc_stock_calc" "lyr_n_stock_calc"	"control_sample"

soils_data_harmonization %>% filter(!is.na(layer)) %>% select(google_dir, data_file, layer) %>% distinct(google_dir, data_file, layer)

# confirm overlapping variables do not have overlapping data
soils_data_harmonization %>%
  filter(
    !is.na(ca) & !is.na(Ca),
    !is.na(mg) & !is.na(Mg),
    !is.na(k) & !is.na(K),
    !is.na(na) & !is.na(Na),
    !is.na(doi_number) & !is.na(data_doi),
    !is.na(aspect_class) & !is.na(aspect),
    !is.na(clay) & !is.na(layer),
    !is.na(loc_comments) & !is.na(comments),
    !is.na(loc_comments) & !is.na(comment_location),
    !is.na(loi) & !is.na(lyr_loi),
    !is.na(eco_type) & !is.na(eco_region)
  )


# additional edits -------------------------------------------------------------

# following are the changes added when constructing the tarball; these are
# implmented in lterwg-som/data-aggregatation

varsToRemove <- c(
  "layer",            # copied to clay
  "dominant_species", # user added / not standardized
  "comment_location", # copied to loc_comments
  "doi_number",       # copied to data_doi
  "aspect",           # copied to aspect_class
  "comments",         # copied to loc_comments
  "loi",              # copied to lyr_loi
  "no3_n",            # user added / not standardized
  "nh4_n",            # user added / not standardized
  "align",            # copied to align_1
  "eco_type",         # copied to eco_region
  "al",               # user added / not standardized
  "k",                # copied to K
  "ca",               # copied to Ca
  "na",               # copied to NA
  "mg",               # copied to Mg
  "fe_HCl",           # user added / not standardized
  "n_replicates"      # user added / not standardized
)

fixes <- soils_data_harmonization %>%
  mutate(
    # AND misidentified layer as clay
    clay = case_when(
      !is.na(layer) ~ layer,
      TRUE ~ clay
      ),
    # doi_number should be data_doi
    data_doi = case_when(
      !is.na(doi_number) ~ doi_number,
      TRUE ~ data_doi
      ),
    # comment_location should be loc_comments
    loc_comments = case_when(
      !is.na(comment_location) ~ comment_location,
      TRUE ~ loc_comments
      ),
    # aspect should be aspect_class
    aspect_class = case_when(
      !is.na(aspect) ~ aspect,
      TRUE ~ aspect_class
      ),
    # comments should be loc_comments
    loc_comments = case_when(
      !is.na(comments) ~ comments,
      TRUE ~ loc_comments
      ),
    # loi should be lyr_loi
    lyr_loi = case_when(
      !is.na(loi) ~ loi,
      TRUE ~ lyr_loi
      ),
    # ca should be Ca
    Ca = case_when(
      !is.na(ca) ~ ca,
      TRUE ~ Ca
      ),
    # mg should be Mg
    Mg = case_when(
      !is.na(mg) ~ mg,
      TRUE ~ Mg
      ),
    # na should be Na
    Na = case_when(
      !is.na(na) ~ na,
      TRUE ~ Na
      ),
    # k should be K
    K = case_when(
      !is.na(k) ~ k,
      TRUE ~ K
      ),
    # align should be align_1 but first remove boolean align from
    # NEON_initialChar
    align = case_when(
      google_dir == "NEON_initialChar" ~ NA_character_,
      TRUE ~ align
      ),
    align_1 = case_when(
      !is.na(align) ~ align,
      TRUE ~ align_1
      ),
    # eco_type should be eco_region
    eco_region = case_when(
      !is.na(eco_type) ~ eco_type,
      TRUE ~ eco_region
    )
    ) %>%
select(-all_of(varsToRemove))

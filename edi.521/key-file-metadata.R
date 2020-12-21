# README -----------------------------------------------------------------------

# use metadata in key file to populate as much of the attribute and factor
# metadata as possible

# libraries --------------------------------------------------------------------

library(googlesheets4)
library(tidyverse)


# key file metadata ------------------------------------------------------------

# extract location and profile tabs of key file
locationData <- googlesheets4::read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1dUr1Vwds51u0SVeRm6O7CNcv_IHOsAySucWKQz4W28o/edit?usp=sharing",
  sheet = 1,
  col_types = "c")

profileData <- googlesheets4::read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1dUr1Vwds51u0SVeRm6O7CNcv_IHOsAySucWKQz4W28o/edit?usp=sharing",
  sheet = 2) %>%
mutate_all(as.character)


# get attrs --------------------------------------------------------------------

attributes <- read_csv("soils_data_harmonization_attrs.csv")


# join key metadata with attrs -------------------------------------------------

attributes %>%
  left_join(locationData %>% select(Var_long, var, Unit), by = c("attributeName" = "var")) %>%
  mutate(
    attributeDefinition = as.character(attributeDefinition),
    attributeDefinition = Var_long,
    unit = as.character(unit),
    unit = Unit
    ) %>%
  select(-Var_long, -Unit) %>%
  left_join(profileData %>% select(Var_long, var, hardUnit), by = c("attributeName" = "var")) %>%
  mutate(
    attributeDefinition = case_when(
      !is.na(Var_long) ~ Var_long,
      TRUE ~ attributeDefinition
      ),
    unit = case_when(
      !is.na(hardUnit) ~ hardUnit,
      TRUE ~ unit
    )
    ) %>%
  select(-Var_long, -hardUnit) %>%
  mutate(
    definition = as.character(definition),
    definition = case_when(
      grepl("char", columnClasses) ~ attributeDefinition,
      TRUE ~ definition
      ),
    unit = case_when(
      grepl("percent|unitless", unit) ~ "dimensionless",
      TRUE ~ unit
    )
    ) %>%
  write_csv("soils_data_harmonization_attrs.csv")


# var list and order -----------------------------------------------------------

var_names_and_order <- c(
  "google_dir",
  "data_file",
  "curator_PersonName",
  "curator_organization",
  "curator_email",
  "modification_date",
  "author_PersonName",
  "author_email",
  "author_orcid_id",
  "addit_contact_person",
  "addit_contact_email",
  "bibliographical_reference",
  "data_doi",
  "network",
  "site_code",
  "location_name",
  "lat",
  "long",
  "datum",
  "elevation",
  "map",
  "mat",
  "clim_avg_yrs",
  "tx_start",
  "aspect_deg",
  "aspect_class",
  "slope",
  "slope_shape",
  "drainage_class",
  "depth_water",
  "parent_transport",
  "parent_material",
  "rock_chem",
  "land_cover",
  "eco_region",
  "loc_texture_class",
  "plant",
  "lit_c",
  "lit_n",
  "lit_p",
  "lit_cn",
  "lit_lig",
  "npp",
  "anpp",
  "bnpp",
  "bnpp_notes",
  "litterfall_anpp",
  "agb",
  "bgb",
  "bgb_lowerdiam",
  "bgb_upperdiam",
  "bgb_type",
  "bgb_notes",
  "bgb_c",
  "bgb_n",
  "bgb_cn",
  "wood_lit_c",
  "align_1",
  "align_2",
  "loc_comments",
  "time_series",
  "gradient",
  "experiments",
  "control_id",
  "number_treatments",
  "merge_align",
  "key_version",
  "L1",
  "L1_level",
  "L2",
  "L2_level",
  "L3",
  "L3_level",
  "L4",
  "L4_level",
  "L5",
  "L5_level",
  "tx_L1",
  "tx_L1_level",
  "tx_L2",
  "tx_L2_level",
  "tx_L3",
  "tx_L3_level",
  "tx_L4",
  "tx_L4_level",
  "tx_L5",
  "tx_L5_level",
  "tx_L6",
  "tx_L6_level",
  "observation_date",
  "veg_note_profile",
  "lyr_c_tot",
  "lyr_soc",
  "lyr_soc_stock",
  "lyr_loi",
  "lyr_som_WalkleyBlack",
  "lyr_n_tot",
  "lyr_n_tot_stock",
  "lyr_c_to_n",
  "lyr_15n",
  "lyr_13c",
  "layer_top",
  "layer_bot",
  "layer_mid",
  "hzn",
  "bd_samp",
  "bd_tot",
  "ph_cacl",
  "ph_h2o",
  "ph_other",
  "sand",
  "silt",
  "clay",
  "coarse_frac",
  "coarse_tot",
  "profile_texture_class",
  "caco3",
  "Ca",
  "Mg",
  "K",
  "Na",
  "cat_exch",
  "base_sum",
  "cec_sum",
  "ecec",
  "bs",
  "mbc_trans",
  "mbn_raw",
  "p_ex_1",
  "p_ex_2",
  "p_ex_3",
  "p_ex_4",
  "n_min",
  "soil_taxon",
  "soil_series",
  "comment_profile",
  "lyr_al_py",
  "lyr_si_py",
  "lyr_fe_ox",
  "lyr_al_ox",
  "lyr_si_ox",
  "lyr_fe_dith",
  "lyr_al_dith",
  "lyr_si_dith",
  "frc_scheme",
  "frc_low_cutoff",
  "frc_high_cutoff",
  "frc_notes",
  "frc_c_tot",
  "frc_oc",
  "frc_n_tot",
  "frc_c_to_n",
  "bd_methods_notes",
  "layer_thick_calc",
  "lyr_soc_stock_calc",
  "lyr_n_stock_calc",
  "control_sample"
)

# rebuild with existing attrs --------------------------------------------------

attributes <- read_csv("soils_data_harmonization_attrs.csv")

old_attrs <- read_csv("archive/soils_data_harmonization_attrs.csv") %>%
  select(
    attributeName,
    formatString_old = formatString,
    unit_old = unit,
    definition_old = definition,
    attributeDefinition_old = attributeDefinition
  )

attributes <- attributes %>%
  left_join(old_attrs, by = c("attributeName")) %>%
  mutate_if(is.logical, as.character) %>%
  mutate(
    formatString = case_when(
      !is.na(formatString_old) ~ formatString_old,
      TRUE ~ formatString),
    unit = case_when(
      !is.na(unit_old) ~ unit_old,
      TRUE ~ unit),
    definition = case_when(
      !is.na(definition_old) ~ definition_old,
      TRUE ~ definition),
    attributeDefinition = case_when(
      !is.na(attributeDefinition_old) ~ attributeDefinition_old,
      TRUE ~ attributeDefinition)
    ) %>%
  select(-contains("old"))

write_csv(attributes, "soils_data_harmonization_attrs.csv")

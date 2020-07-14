# Package ID: edi.521.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: SOils DAta Harmonization database (SoDaH): an open-source synthesis of soil data from research networks.
# Data set creator:  William Wieder - Institute of Arctic and Alpine Research, University of Colorado Boulder 
# Data set creator:  Derek Pierson - Department of Crop and Soil Sciences, Oregon State University 
# Data set creator:  Stevan Earl - Global Institute of Sustainability, Arizona State University 
# Data set creator:  Kate Lajtha - Department of Crop and Soil Sciences, Oregon State University 
# Data set creator:  Sara Baer - Department of Ecology and Evolutionary Biology and Kansas Biological Survey, University of Kansas 
# Data set creator:  Ford Ballantyne - Odum School of Ecology, University of Georgia 
# Data set creator:  Asmeret Berhe - Department of Life and Environmental Sciences; University of California, Merced 
# Data set creator:  Sharon Billings - Department of Ecology and Evolutionary Biology and Kansas Biological Survey, University of Kansas 
# Data set creator:  Laurel Brigham - Department of Ecology and Evolutionary Biology and Institute of Arctic and Alpine Research, University of Colorado 
# Data set creator:  Stephany Chacon - Department of Crop and Soil Sciences, Oregon State University 
# Data set creator:  Jennifer Fraterrigo - Department of Natural Resources and Environmental Sciences, University of Illinois 
# Data set creator:  Serita Frey - Department of Natural Resources and the Environment, University of New Hampshire 
# Data set creator:  Katerina Georgiou - Department of Earth System Science, Stanford University 
# Data set creator:  Marie-Anne de Graaff - Department of Biological Sciences, Boise State University 
# Data set creator:  A Grandy - Department of Natural Resources and the Environment, University of New Hampshire 
# Data set creator:  Melannie Hartman - Climate and Global Dynamics Laboratory, National Center for Atmospheric Research 
# Data set creator:  Sarah Hobbie - Department of Ecology, Evolution and Behavior, University of Minnesota 
# Data set creator:  Chris Johnson - Department of Civil and Environmental Engineering, Syracuse University 
# Data set creator:  Jason Kaye - Department of Ecosystem Science and Management, The Pennsylvania State University 
# Data set creator:  Emily Snowman - Department of Natural Resources and the Environment, University of New Hampshire 
# Data set creator:  Marcy Litvak - Department of Biology, University of New Mexico 
# Data set creator:  Michelle Mack - Center for Ecosystem Science and Society and Department of Biological Sciences, Northern Arizona University 
# Data set creator:  Avni Malhotra - Department of Earth System Science, Stanford University 
# Data set creator:  Jessica Moore - Bioscience Division, Oak Ridge National Laboratory 
# Data set creator:  Knute Nadelhoffer - Department of Ecology and Evolutionary Biology, University of Michigan 
# Data set creator:  Craig Rasmussen - Department of Environmental Science, The University of Arizona 
# Data set creator:  Whendee Silver - Department of Environmental Science, Policy, and Management, University of California, Berkeley 
# Data set creator:  Benjamin Sulman - Climate Change Science Institute and Environmental Sciences Division, Oak Ridge National Laboratory 
# Data set creator:  Xanthe Walker - Center for Ecosystem Science and Society and Department of Biological Sciences, Northern Arizona University 
# Data set creator:  Samantha Weintraub - National Ecological Observatory Network, Battelle 
# Metadata Provider:  William Wieder - Institute of Arctic and Alpine Research, University of Colorado Boulder 
# Metadata Provider:  Derek Pierson - Department of Crop and Soil Sciences, Oregon State University 
# Metadata Provider:  Stevan Earl - Global Institute of Sustainability, Arizona State University 
# Contact:    - Information Manager Central Arizona–Phoenix LTER  - caplter.data@asu.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta-s.lternet.edu/package/data/eml/edi/521/1/2d9de0e6e1bcd0ca1bfe81569a28ed3e" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
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
                    "control_sample"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt1$google_dir)!="factor") dt1$google_dir<- as.factor(dt1$google_dir)
if (class(dt1$data_file)!="factor") dt1$data_file<- as.factor(dt1$data_file)
if (class(dt1$curator_PersonName)!="factor") dt1$curator_PersonName<- as.factor(dt1$curator_PersonName)
if (class(dt1$curator_organization)!="factor") dt1$curator_organization<- as.factor(dt1$curator_organization)
if (class(dt1$curator_email)!="factor") dt1$curator_email<- as.factor(dt1$curator_email)                                   
# attempting to convert dt1$modification_date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1modification_date<-as.Date(dt1$modification_date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1modification_date) == length(tmp1modification_date[!is.na(tmp1modification_date)])){dt1$modification_date <- tmp1modification_date } else {print("Date conversion failed for dt1$modification_date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1modification_date) 
if (class(dt1$author_PersonName)!="factor") dt1$author_PersonName<- as.factor(dt1$author_PersonName)
if (class(dt1$author_email)!="factor") dt1$author_email<- as.factor(dt1$author_email)
if (class(dt1$author_orcid_id)!="factor") dt1$author_orcid_id<- as.factor(dt1$author_orcid_id)
if (class(dt1$addit_contact_person)!="factor") dt1$addit_contact_person<- as.factor(dt1$addit_contact_person)
if (class(dt1$addit_contact_email)!="factor") dt1$addit_contact_email<- as.factor(dt1$addit_contact_email)
if (class(dt1$bibliographical_reference)!="factor") dt1$bibliographical_reference<- as.factor(dt1$bibliographical_reference)
if (class(dt1$data_doi)!="factor") dt1$data_doi<- as.factor(dt1$data_doi)
if (class(dt1$network)!="factor") dt1$network<- as.factor(dt1$network)
if (class(dt1$site_code)!="factor") dt1$site_code<- as.factor(dt1$site_code)
if (class(dt1$location_name)!="factor") dt1$location_name<- as.factor(dt1$location_name)
if (class(dt1$lat)=="factor") dt1$lat <-as.numeric(levels(dt1$lat))[as.integer(dt1$lat) ]               
if (class(dt1$lat)=="character") dt1$lat <-as.numeric(dt1$lat)
if (class(dt1$long)=="factor") dt1$long <-as.numeric(levels(dt1$long))[as.integer(dt1$long) ]               
if (class(dt1$long)=="character") dt1$long <-as.numeric(dt1$long)
if (class(dt1$datum)!="factor") dt1$datum<- as.factor(dt1$datum)
if (class(dt1$elevation)=="factor") dt1$elevation <-as.numeric(levels(dt1$elevation))[as.integer(dt1$elevation) ]               
if (class(dt1$elevation)=="character") dt1$elevation <-as.numeric(dt1$elevation)
if (class(dt1$map)=="factor") dt1$map <-as.numeric(levels(dt1$map))[as.integer(dt1$map) ]               
if (class(dt1$map)=="character") dt1$map <-as.numeric(dt1$map)
if (class(dt1$mat)=="factor") dt1$mat <-as.numeric(levels(dt1$mat))[as.integer(dt1$mat) ]               
if (class(dt1$mat)=="character") dt1$mat <-as.numeric(dt1$mat)
if (class(dt1$clim_avg_yrs)!="factor") dt1$clim_avg_yrs<- as.factor(dt1$clim_avg_yrs)
if (class(dt1$aspect_deg)=="factor") dt1$aspect_deg <-as.numeric(levels(dt1$aspect_deg))[as.integer(dt1$aspect_deg) ]               
if (class(dt1$aspect_deg)=="character") dt1$aspect_deg <-as.numeric(dt1$aspect_deg)
if (class(dt1$aspect_class)!="factor") dt1$aspect_class<- as.factor(dt1$aspect_class)
if (class(dt1$slope)=="factor") dt1$slope <-as.numeric(levels(dt1$slope))[as.integer(dt1$slope) ]               
if (class(dt1$slope)=="character") dt1$slope <-as.numeric(dt1$slope)
if (class(dt1$slope_shape)!="factor") dt1$slope_shape<- as.factor(dt1$slope_shape)
if (class(dt1$drainage_class)!="factor") dt1$drainage_class<- as.factor(dt1$drainage_class)
if (class(dt1$depth_water)=="factor") dt1$depth_water <-as.numeric(levels(dt1$depth_water))[as.integer(dt1$depth_water) ]               
if (class(dt1$depth_water)=="character") dt1$depth_water <-as.numeric(dt1$depth_water)
if (class(dt1$parent_transport)!="factor") dt1$parent_transport<- as.factor(dt1$parent_transport)
if (class(dt1$parent_material)!="factor") dt1$parent_material<- as.factor(dt1$parent_material)
if (class(dt1$rock_chem)!="factor") dt1$rock_chem<- as.factor(dt1$rock_chem)
if (class(dt1$land_cover)!="factor") dt1$land_cover<- as.factor(dt1$land_cover)
if (class(dt1$eco_region)!="factor") dt1$eco_region<- as.factor(dt1$eco_region)
if (class(dt1$loc_texture_class)!="factor") dt1$loc_texture_class<- as.factor(dt1$loc_texture_class)
if (class(dt1$plant)!="factor") dt1$plant<- as.factor(dt1$plant)
if (class(dt1$lit_c)=="factor") dt1$lit_c <-as.numeric(levels(dt1$lit_c))[as.integer(dt1$lit_c) ]               
if (class(dt1$lit_c)=="character") dt1$lit_c <-as.numeric(dt1$lit_c)
if (class(dt1$lit_n)=="factor") dt1$lit_n <-as.numeric(levels(dt1$lit_n))[as.integer(dt1$lit_n) ]               
if (class(dt1$lit_n)=="character") dt1$lit_n <-as.numeric(dt1$lit_n)
if (class(dt1$lit_p)=="factor") dt1$lit_p <-as.numeric(levels(dt1$lit_p))[as.integer(dt1$lit_p) ]               
if (class(dt1$lit_p)=="character") dt1$lit_p <-as.numeric(dt1$lit_p)
if (class(dt1$lit_cn)=="factor") dt1$lit_cn <-as.numeric(levels(dt1$lit_cn))[as.integer(dt1$lit_cn) ]               
if (class(dt1$lit_cn)=="character") dt1$lit_cn <-as.numeric(dt1$lit_cn)
if (class(dt1$lit_lig)=="factor") dt1$lit_lig <-as.numeric(levels(dt1$lit_lig))[as.integer(dt1$lit_lig) ]               
if (class(dt1$lit_lig)=="character") dt1$lit_lig <-as.numeric(dt1$lit_lig)
if (class(dt1$npp)=="factor") dt1$npp <-as.numeric(levels(dt1$npp))[as.integer(dt1$npp) ]               
if (class(dt1$npp)=="character") dt1$npp <-as.numeric(dt1$npp)
if (class(dt1$anpp)=="factor") dt1$anpp <-as.numeric(levels(dt1$anpp))[as.integer(dt1$anpp) ]               
if (class(dt1$anpp)=="character") dt1$anpp <-as.numeric(dt1$anpp)
if (class(dt1$bnpp)=="factor") dt1$bnpp <-as.numeric(levels(dt1$bnpp))[as.integer(dt1$bnpp) ]               
if (class(dt1$bnpp)=="character") dt1$bnpp <-as.numeric(dt1$bnpp)
if (class(dt1$bnpp_notes)!="factor") dt1$bnpp_notes<- as.factor(dt1$bnpp_notes)
if (class(dt1$litterfall_anpp)=="factor") dt1$litterfall_anpp <-as.numeric(levels(dt1$litterfall_anpp))[as.integer(dt1$litterfall_anpp) ]               
if (class(dt1$litterfall_anpp)=="character") dt1$litterfall_anpp <-as.numeric(dt1$litterfall_anpp)
if (class(dt1$agb)=="factor") dt1$agb <-as.numeric(levels(dt1$agb))[as.integer(dt1$agb) ]               
if (class(dt1$agb)=="character") dt1$agb <-as.numeric(dt1$agb)
if (class(dt1$bgb)=="factor") dt1$bgb <-as.numeric(levels(dt1$bgb))[as.integer(dt1$bgb) ]               
if (class(dt1$bgb)=="character") dt1$bgb <-as.numeric(dt1$bgb)
if (class(dt1$bgb_lowerdiam)=="factor") dt1$bgb_lowerdiam <-as.numeric(levels(dt1$bgb_lowerdiam))[as.integer(dt1$bgb_lowerdiam) ]               
if (class(dt1$bgb_lowerdiam)=="character") dt1$bgb_lowerdiam <-as.numeric(dt1$bgb_lowerdiam)
if (class(dt1$bgb_upperdiam)=="factor") dt1$bgb_upperdiam <-as.numeric(levels(dt1$bgb_upperdiam))[as.integer(dt1$bgb_upperdiam) ]               
if (class(dt1$bgb_upperdiam)=="character") dt1$bgb_upperdiam <-as.numeric(dt1$bgb_upperdiam)
if (class(dt1$bgb_type)!="factor") dt1$bgb_type<- as.factor(dt1$bgb_type)
if (class(dt1$bgb_notes)!="factor") dt1$bgb_notes<- as.factor(dt1$bgb_notes)
if (class(dt1$bgb_c)=="factor") dt1$bgb_c <-as.numeric(levels(dt1$bgb_c))[as.integer(dt1$bgb_c) ]               
if (class(dt1$bgb_c)=="character") dt1$bgb_c <-as.numeric(dt1$bgb_c)
if (class(dt1$bgb_n)=="factor") dt1$bgb_n <-as.numeric(levels(dt1$bgb_n))[as.integer(dt1$bgb_n) ]               
if (class(dt1$bgb_n)=="character") dt1$bgb_n <-as.numeric(dt1$bgb_n)
if (class(dt1$bgb_cn)=="factor") dt1$bgb_cn <-as.numeric(levels(dt1$bgb_cn))[as.integer(dt1$bgb_cn) ]               
if (class(dt1$bgb_cn)=="character") dt1$bgb_cn <-as.numeric(dt1$bgb_cn)
if (class(dt1$wood_lit_c)=="factor") dt1$wood_lit_c <-as.numeric(levels(dt1$wood_lit_c))[as.integer(dt1$wood_lit_c) ]               
if (class(dt1$wood_lit_c)=="character") dt1$wood_lit_c <-as.numeric(dt1$wood_lit_c)
if (class(dt1$align_1)!="factor") dt1$align_1<- as.factor(dt1$align_1)
if (class(dt1$align_2)!="factor") dt1$align_2<- as.factor(dt1$align_2)
if (class(dt1$loc_comments)!="factor") dt1$loc_comments<- as.factor(dt1$loc_comments)
if (class(dt1$time_series)!="factor") dt1$time_series<- as.factor(dt1$time_series)
if (class(dt1$gradient)!="factor") dt1$gradient<- as.factor(dt1$gradient)
if (class(dt1$experiments)!="factor") dt1$experiments<- as.factor(dt1$experiments)
if (class(dt1$control_id)!="factor") dt1$control_id<- as.factor(dt1$control_id)
if (class(dt1$number_treatments)!="factor") dt1$number_treatments<- as.factor(dt1$number_treatments)
if (class(dt1$merge_align)!="factor") dt1$merge_align<- as.factor(dt1$merge_align)
if (class(dt1$key_version)!="factor") dt1$key_version<- as.factor(dt1$key_version)
if (class(dt1$L1)!="factor") dt1$L1<- as.factor(dt1$L1)
if (class(dt1$L1_level)!="factor") dt1$L1_level<- as.factor(dt1$L1_level)
if (class(dt1$L2)!="factor") dt1$L2<- as.factor(dt1$L2)
if (class(dt1$L2_level)!="factor") dt1$L2_level<- as.factor(dt1$L2_level)
if (class(dt1$L3)!="factor") dt1$L3<- as.factor(dt1$L3)
if (class(dt1$L3_level)!="factor") dt1$L3_level<- as.factor(dt1$L3_level)
if (class(dt1$L4)!="factor") dt1$L4<- as.factor(dt1$L4)
if (class(dt1$L4_level)!="factor") dt1$L4_level<- as.factor(dt1$L4_level)
if (class(dt1$L5)!="factor") dt1$L5<- as.factor(dt1$L5)
if (class(dt1$L5_level)!="factor") dt1$L5_level<- as.factor(dt1$L5_level)
if (class(dt1$tx_L1)!="factor") dt1$tx_L1<- as.factor(dt1$tx_L1)
if (class(dt1$tx_L1_level)!="factor") dt1$tx_L1_level<- as.factor(dt1$tx_L1_level)
if (class(dt1$tx_L2)!="factor") dt1$tx_L2<- as.factor(dt1$tx_L2)
if (class(dt1$tx_L2_level)!="factor") dt1$tx_L2_level<- as.factor(dt1$tx_L2_level)
if (class(dt1$tx_L3)!="factor") dt1$tx_L3<- as.factor(dt1$tx_L3)
if (class(dt1$tx_L3_level)!="factor") dt1$tx_L3_level<- as.factor(dt1$tx_L3_level)
if (class(dt1$tx_L4)!="factor") dt1$tx_L4<- as.factor(dt1$tx_L4)
if (class(dt1$tx_L4_level)!="factor") dt1$tx_L4_level<- as.factor(dt1$tx_L4_level)
if (class(dt1$tx_L5)!="factor") dt1$tx_L5<- as.factor(dt1$tx_L5)
if (class(dt1$tx_L5_level)!="factor") dt1$tx_L5_level<- as.factor(dt1$tx_L5_level)
if (class(dt1$tx_L6)!="factor") dt1$tx_L6<- as.factor(dt1$tx_L6)
if (class(dt1$tx_L6_level)!="factor") dt1$tx_L6_level<- as.factor(dt1$tx_L6_level)
if (class(dt1$observation_date)!="factor") dt1$observation_date<- as.factor(dt1$observation_date)
if (class(dt1$veg_note_profile)!="factor") dt1$veg_note_profile<- as.factor(dt1$veg_note_profile)
if (class(dt1$lyr_c_tot)=="factor") dt1$lyr_c_tot <-as.numeric(levels(dt1$lyr_c_tot))[as.integer(dt1$lyr_c_tot) ]               
if (class(dt1$lyr_c_tot)=="character") dt1$lyr_c_tot <-as.numeric(dt1$lyr_c_tot)
if (class(dt1$lyr_soc)=="factor") dt1$lyr_soc <-as.numeric(levels(dt1$lyr_soc))[as.integer(dt1$lyr_soc) ]               
if (class(dt1$lyr_soc)=="character") dt1$lyr_soc <-as.numeric(dt1$lyr_soc)
if (class(dt1$lyr_soc_stock)=="factor") dt1$lyr_soc_stock <-as.numeric(levels(dt1$lyr_soc_stock))[as.integer(dt1$lyr_soc_stock) ]               
if (class(dt1$lyr_soc_stock)=="character") dt1$lyr_soc_stock <-as.numeric(dt1$lyr_soc_stock)
if (class(dt1$lyr_loi)=="factor") dt1$lyr_loi <-as.numeric(levels(dt1$lyr_loi))[as.integer(dt1$lyr_loi) ]               
if (class(dt1$lyr_loi)=="character") dt1$lyr_loi <-as.numeric(dt1$lyr_loi)
if (class(dt1$lyr_som_WalkleyBlack)=="factor") dt1$lyr_som_WalkleyBlack <-as.numeric(levels(dt1$lyr_som_WalkleyBlack))[as.integer(dt1$lyr_som_WalkleyBlack) ]               
if (class(dt1$lyr_som_WalkleyBlack)=="character") dt1$lyr_som_WalkleyBlack <-as.numeric(dt1$lyr_som_WalkleyBlack)
if (class(dt1$lyr_n_tot)=="factor") dt1$lyr_n_tot <-as.numeric(levels(dt1$lyr_n_tot))[as.integer(dt1$lyr_n_tot) ]               
if (class(dt1$lyr_n_tot)=="character") dt1$lyr_n_tot <-as.numeric(dt1$lyr_n_tot)
if (class(dt1$lyr_n_tot_stock)=="factor") dt1$lyr_n_tot_stock <-as.numeric(levels(dt1$lyr_n_tot_stock))[as.integer(dt1$lyr_n_tot_stock) ]               
if (class(dt1$lyr_n_tot_stock)=="character") dt1$lyr_n_tot_stock <-as.numeric(dt1$lyr_n_tot_stock)
if (class(dt1$lyr_c_to_n)=="factor") dt1$lyr_c_to_n <-as.numeric(levels(dt1$lyr_c_to_n))[as.integer(dt1$lyr_c_to_n) ]               
if (class(dt1$lyr_c_to_n)=="character") dt1$lyr_c_to_n <-as.numeric(dt1$lyr_c_to_n)
if (class(dt1$lyr_15n)=="factor") dt1$lyr_15n <-as.numeric(levels(dt1$lyr_15n))[as.integer(dt1$lyr_15n) ]               
if (class(dt1$lyr_15n)=="character") dt1$lyr_15n <-as.numeric(dt1$lyr_15n)
if (class(dt1$lyr_13c)=="factor") dt1$lyr_13c <-as.numeric(levels(dt1$lyr_13c))[as.integer(dt1$lyr_13c) ]               
if (class(dt1$lyr_13c)=="character") dt1$lyr_13c <-as.numeric(dt1$lyr_13c)
if (class(dt1$layer_top)=="factor") dt1$layer_top <-as.numeric(levels(dt1$layer_top))[as.integer(dt1$layer_top) ]               
if (class(dt1$layer_top)=="character") dt1$layer_top <-as.numeric(dt1$layer_top)
if (class(dt1$layer_bot)=="factor") dt1$layer_bot <-as.numeric(levels(dt1$layer_bot))[as.integer(dt1$layer_bot) ]               
if (class(dt1$layer_bot)=="character") dt1$layer_bot <-as.numeric(dt1$layer_bot)
if (class(dt1$layer_mid)=="factor") dt1$layer_mid <-as.numeric(levels(dt1$layer_mid))[as.integer(dt1$layer_mid) ]               
if (class(dt1$layer_mid)=="character") dt1$layer_mid <-as.numeric(dt1$layer_mid)
if (class(dt1$hzn)!="factor") dt1$hzn<- as.factor(dt1$hzn)
if (class(dt1$bd_samp)=="factor") dt1$bd_samp <-as.numeric(levels(dt1$bd_samp))[as.integer(dt1$bd_samp) ]               
if (class(dt1$bd_samp)=="character") dt1$bd_samp <-as.numeric(dt1$bd_samp)
if (class(dt1$bd_tot)=="factor") dt1$bd_tot <-as.numeric(levels(dt1$bd_tot))[as.integer(dt1$bd_tot) ]               
if (class(dt1$bd_tot)=="character") dt1$bd_tot <-as.numeric(dt1$bd_tot)
if (class(dt1$ph_cacl)=="factor") dt1$ph_cacl <-as.numeric(levels(dt1$ph_cacl))[as.integer(dt1$ph_cacl) ]               
if (class(dt1$ph_cacl)=="character") dt1$ph_cacl <-as.numeric(dt1$ph_cacl)
if (class(dt1$ph_h2o)=="factor") dt1$ph_h2o <-as.numeric(levels(dt1$ph_h2o))[as.integer(dt1$ph_h2o) ]               
if (class(dt1$ph_h2o)=="character") dt1$ph_h2o <-as.numeric(dt1$ph_h2o)
if (class(dt1$ph_other)=="factor") dt1$ph_other <-as.numeric(levels(dt1$ph_other))[as.integer(dt1$ph_other) ]               
if (class(dt1$ph_other)=="character") dt1$ph_other <-as.numeric(dt1$ph_other)
if (class(dt1$sand)=="factor") dt1$sand <-as.numeric(levels(dt1$sand))[as.integer(dt1$sand) ]               
if (class(dt1$sand)=="character") dt1$sand <-as.numeric(dt1$sand)
if (class(dt1$silt)=="factor") dt1$silt <-as.numeric(levels(dt1$silt))[as.integer(dt1$silt) ]               
if (class(dt1$silt)=="character") dt1$silt <-as.numeric(dt1$silt)
if (class(dt1$clay)=="factor") dt1$clay <-as.numeric(levels(dt1$clay))[as.integer(dt1$clay) ]               
if (class(dt1$clay)=="character") dt1$clay <-as.numeric(dt1$clay)
if (class(dt1$coarse_frac)=="factor") dt1$coarse_frac <-as.numeric(levels(dt1$coarse_frac))[as.integer(dt1$coarse_frac) ]               
if (class(dt1$coarse_frac)=="character") dt1$coarse_frac <-as.numeric(dt1$coarse_frac)
if (class(dt1$coarse_tot)=="factor") dt1$coarse_tot <-as.numeric(levels(dt1$coarse_tot))[as.integer(dt1$coarse_tot) ]               
if (class(dt1$coarse_tot)=="character") dt1$coarse_tot <-as.numeric(dt1$coarse_tot)
if (class(dt1$profile_texture_class)!="factor") dt1$profile_texture_class<- as.factor(dt1$profile_texture_class)
if (class(dt1$caco3)=="factor") dt1$caco3 <-as.numeric(levels(dt1$caco3))[as.integer(dt1$caco3) ]               
if (class(dt1$caco3)=="character") dt1$caco3 <-as.numeric(dt1$caco3)
if (class(dt1$Ca)=="factor") dt1$Ca <-as.numeric(levels(dt1$Ca))[as.integer(dt1$Ca) ]               
if (class(dt1$Ca)=="character") dt1$Ca <-as.numeric(dt1$Ca)
if (class(dt1$Mg)=="factor") dt1$Mg <-as.numeric(levels(dt1$Mg))[as.integer(dt1$Mg) ]               
if (class(dt1$Mg)=="character") dt1$Mg <-as.numeric(dt1$Mg)
if (class(dt1$K)=="factor") dt1$K <-as.numeric(levels(dt1$K))[as.integer(dt1$K) ]               
if (class(dt1$K)=="character") dt1$K <-as.numeric(dt1$K)
if (class(dt1$Na)=="factor") dt1$Na <-as.numeric(levels(dt1$Na))[as.integer(dt1$Na) ]               
if (class(dt1$Na)=="character") dt1$Na <-as.numeric(dt1$Na)
if (class(dt1$cat_exch)=="factor") dt1$cat_exch <-as.numeric(levels(dt1$cat_exch))[as.integer(dt1$cat_exch) ]               
if (class(dt1$cat_exch)=="character") dt1$cat_exch <-as.numeric(dt1$cat_exch)
if (class(dt1$base_sum)=="factor") dt1$base_sum <-as.numeric(levels(dt1$base_sum))[as.integer(dt1$base_sum) ]               
if (class(dt1$base_sum)=="character") dt1$base_sum <-as.numeric(dt1$base_sum)
if (class(dt1$cec_sum)=="factor") dt1$cec_sum <-as.numeric(levels(dt1$cec_sum))[as.integer(dt1$cec_sum) ]               
if (class(dt1$cec_sum)=="character") dt1$cec_sum <-as.numeric(dt1$cec_sum)
if (class(dt1$ecec)=="factor") dt1$ecec <-as.numeric(levels(dt1$ecec))[as.integer(dt1$ecec) ]               
if (class(dt1$ecec)=="character") dt1$ecec <-as.numeric(dt1$ecec)
if (class(dt1$bs)=="factor") dt1$bs <-as.numeric(levels(dt1$bs))[as.integer(dt1$bs) ]               
if (class(dt1$bs)=="character") dt1$bs <-as.numeric(dt1$bs)
if (class(dt1$mbc_trans)=="factor") dt1$mbc_trans <-as.numeric(levels(dt1$mbc_trans))[as.integer(dt1$mbc_trans) ]               
if (class(dt1$mbc_trans)=="character") dt1$mbc_trans <-as.numeric(dt1$mbc_trans)
if (class(dt1$mbn_raw)=="factor") dt1$mbn_raw <-as.numeric(levels(dt1$mbn_raw))[as.integer(dt1$mbn_raw) ]               
if (class(dt1$mbn_raw)=="character") dt1$mbn_raw <-as.numeric(dt1$mbn_raw)
if (class(dt1$p_ex_1)=="factor") dt1$p_ex_1 <-as.numeric(levels(dt1$p_ex_1))[as.integer(dt1$p_ex_1) ]               
if (class(dt1$p_ex_1)=="character") dt1$p_ex_1 <-as.numeric(dt1$p_ex_1)
if (class(dt1$p_ex_2)=="factor") dt1$p_ex_2 <-as.numeric(levels(dt1$p_ex_2))[as.integer(dt1$p_ex_2) ]               
if (class(dt1$p_ex_2)=="character") dt1$p_ex_2 <-as.numeric(dt1$p_ex_2)
if (class(dt1$p_ex_3)=="factor") dt1$p_ex_3 <-as.numeric(levels(dt1$p_ex_3))[as.integer(dt1$p_ex_3) ]               
if (class(dt1$p_ex_3)=="character") dt1$p_ex_3 <-as.numeric(dt1$p_ex_3)
if (class(dt1$p_ex_4)=="factor") dt1$p_ex_4 <-as.numeric(levels(dt1$p_ex_4))[as.integer(dt1$p_ex_4) ]               
if (class(dt1$p_ex_4)=="character") dt1$p_ex_4 <-as.numeric(dt1$p_ex_4)
if (class(dt1$n_min)=="factor") dt1$n_min <-as.numeric(levels(dt1$n_min))[as.integer(dt1$n_min) ]               
if (class(dt1$n_min)=="character") dt1$n_min <-as.numeric(dt1$n_min)
if (class(dt1$soil_taxon)!="factor") dt1$soil_taxon<- as.factor(dt1$soil_taxon)
if (class(dt1$soil_series)!="factor") dt1$soil_series<- as.factor(dt1$soil_series)
if (class(dt1$comment_profile)!="factor") dt1$comment_profile<- as.factor(dt1$comment_profile)
if (class(dt1$lyr_al_py)=="factor") dt1$lyr_al_py <-as.numeric(levels(dt1$lyr_al_py))[as.integer(dt1$lyr_al_py) ]               
if (class(dt1$lyr_al_py)=="character") dt1$lyr_al_py <-as.numeric(dt1$lyr_al_py)
if (class(dt1$lyr_si_py)=="factor") dt1$lyr_si_py <-as.numeric(levels(dt1$lyr_si_py))[as.integer(dt1$lyr_si_py) ]               
if (class(dt1$lyr_si_py)=="character") dt1$lyr_si_py <-as.numeric(dt1$lyr_si_py)
if (class(dt1$lyr_fe_ox)=="factor") dt1$lyr_fe_ox <-as.numeric(levels(dt1$lyr_fe_ox))[as.integer(dt1$lyr_fe_ox) ]               
if (class(dt1$lyr_fe_ox)=="character") dt1$lyr_fe_ox <-as.numeric(dt1$lyr_fe_ox)
if (class(dt1$lyr_al_ox)=="factor") dt1$lyr_al_ox <-as.numeric(levels(dt1$lyr_al_ox))[as.integer(dt1$lyr_al_ox) ]               
if (class(dt1$lyr_al_ox)=="character") dt1$lyr_al_ox <-as.numeric(dt1$lyr_al_ox)
if (class(dt1$lyr_si_ox)=="factor") dt1$lyr_si_ox <-as.numeric(levels(dt1$lyr_si_ox))[as.integer(dt1$lyr_si_ox) ]               
if (class(dt1$lyr_si_ox)=="character") dt1$lyr_si_ox <-as.numeric(dt1$lyr_si_ox)
if (class(dt1$lyr_fe_dith)=="factor") dt1$lyr_fe_dith <-as.numeric(levels(dt1$lyr_fe_dith))[as.integer(dt1$lyr_fe_dith) ]               
if (class(dt1$lyr_fe_dith)=="character") dt1$lyr_fe_dith <-as.numeric(dt1$lyr_fe_dith)
if (class(dt1$lyr_al_dith)=="factor") dt1$lyr_al_dith <-as.numeric(levels(dt1$lyr_al_dith))[as.integer(dt1$lyr_al_dith) ]               
if (class(dt1$lyr_al_dith)=="character") dt1$lyr_al_dith <-as.numeric(dt1$lyr_al_dith)
if (class(dt1$lyr_si_dith)=="factor") dt1$lyr_si_dith <-as.numeric(levels(dt1$lyr_si_dith))[as.integer(dt1$lyr_si_dith) ]               
if (class(dt1$lyr_si_dith)=="character") dt1$lyr_si_dith <-as.numeric(dt1$lyr_si_dith)
if (class(dt1$frc_scheme)!="factor") dt1$frc_scheme<- as.factor(dt1$frc_scheme)
if (class(dt1$frc_low_cutoff)!="factor") dt1$frc_low_cutoff<- as.factor(dt1$frc_low_cutoff)
if (class(dt1$frc_high_cutoff)!="factor") dt1$frc_high_cutoff<- as.factor(dt1$frc_high_cutoff)
if (class(dt1$frc_notes)!="factor") dt1$frc_notes<- as.factor(dt1$frc_notes)
if (class(dt1$frc_c_tot)=="factor") dt1$frc_c_tot <-as.numeric(levels(dt1$frc_c_tot))[as.integer(dt1$frc_c_tot) ]               
if (class(dt1$frc_c_tot)=="character") dt1$frc_c_tot <-as.numeric(dt1$frc_c_tot)
if (class(dt1$frc_oc)=="factor") dt1$frc_oc <-as.numeric(levels(dt1$frc_oc))[as.integer(dt1$frc_oc) ]               
if (class(dt1$frc_oc)=="character") dt1$frc_oc <-as.numeric(dt1$frc_oc)
if (class(dt1$frc_n_tot)=="factor") dt1$frc_n_tot <-as.numeric(levels(dt1$frc_n_tot))[as.integer(dt1$frc_n_tot) ]               
if (class(dt1$frc_n_tot)=="character") dt1$frc_n_tot <-as.numeric(dt1$frc_n_tot)
if (class(dt1$frc_c_to_n)=="factor") dt1$frc_c_to_n <-as.numeric(levels(dt1$frc_c_to_n))[as.integer(dt1$frc_c_to_n) ]               
if (class(dt1$frc_c_to_n)=="character") dt1$frc_c_to_n <-as.numeric(dt1$frc_c_to_n)
if (class(dt1$bd_methods_notes)!="factor") dt1$bd_methods_notes<- as.factor(dt1$bd_methods_notes)
if (class(dt1$layer_thick_calc)=="factor") dt1$layer_thick_calc <-as.numeric(levels(dt1$layer_thick_calc))[as.integer(dt1$layer_thick_calc) ]               
if (class(dt1$layer_thick_calc)=="character") dt1$layer_thick_calc <-as.numeric(dt1$layer_thick_calc)
if (class(dt1$lyr_soc_stock_calc)=="factor") dt1$lyr_soc_stock_calc <-as.numeric(levels(dt1$lyr_soc_stock_calc))[as.integer(dt1$lyr_soc_stock_calc) ]               
if (class(dt1$lyr_soc_stock_calc)=="character") dt1$lyr_soc_stock_calc <-as.numeric(dt1$lyr_soc_stock_calc)
if (class(dt1$lyr_n_stock_calc)=="factor") dt1$lyr_n_stock_calc <-as.numeric(levels(dt1$lyr_n_stock_calc))[as.integer(dt1$lyr_n_stock_calc) ]               
if (class(dt1$lyr_n_stock_calc)=="character") dt1$lyr_n_stock_calc <-as.numeric(dt1$lyr_n_stock_calc)
if (class(dt1$control_sample)!="factor") dt1$control_sample<- as.factor(dt1$control_sample)
                
# Convert Missing Values to NA for non-dates
                
dt1$author_PersonName <- as.factor(ifelse((trimws(as.character(dt1$author_PersonName))==trimws("NA")),NA,as.character(dt1$author_PersonName)))
dt1$author_email <- as.factor(ifelse((trimws(as.character(dt1$author_email))==trimws("NA")),NA,as.character(dt1$author_email)))
dt1$author_orcid_id <- as.factor(ifelse((trimws(as.character(dt1$author_orcid_id))==trimws("NA")),NA,as.character(dt1$author_orcid_id)))
dt1$addit_contact_person <- as.factor(ifelse((trimws(as.character(dt1$addit_contact_person))==trimws("NA")),NA,as.character(dt1$addit_contact_person)))
dt1$addit_contact_email <- as.factor(ifelse((trimws(as.character(dt1$addit_contact_email))==trimws("NA")),NA,as.character(dt1$addit_contact_email)))
dt1$bibliographical_reference <- as.factor(ifelse((trimws(as.character(dt1$bibliographical_reference))==trimws("NA")),NA,as.character(dt1$bibliographical_reference)))
dt1$data_doi <- as.factor(ifelse((trimws(as.character(dt1$data_doi))==trimws("NA")),NA,as.character(dt1$data_doi)))
dt1$site_code <- as.factor(ifelse((trimws(as.character(dt1$site_code))==trimws("NA")),NA,as.character(dt1$site_code)))
dt1$location_name <- as.factor(ifelse((trimws(as.character(dt1$location_name))==trimws("NA")),NA,as.character(dt1$location_name)))
dt1$lat <- ifelse((trimws(as.character(dt1$lat))==trimws("NA")),NA,dt1$lat)               
suppressWarnings(dt1$lat <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lat))==as.character(as.numeric("NA"))),NA,dt1$lat))
dt1$long <- ifelse((trimws(as.character(dt1$long))==trimws("NA")),NA,dt1$long)               
suppressWarnings(dt1$long <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$long))==as.character(as.numeric("NA"))),NA,dt1$long))
dt1$datum <- as.factor(ifelse((trimws(as.character(dt1$datum))==trimws("NA")),NA,as.character(dt1$datum)))
dt1$elevation <- ifelse((trimws(as.character(dt1$elevation))==trimws("NA")),NA,dt1$elevation)               
suppressWarnings(dt1$elevation <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$elevation))==as.character(as.numeric("NA"))),NA,dt1$elevation))
dt1$map <- ifelse((trimws(as.character(dt1$map))==trimws("NA")),NA,dt1$map)               
suppressWarnings(dt1$map <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$map))==as.character(as.numeric("NA"))),NA,dt1$map))
dt1$mat <- ifelse((trimws(as.character(dt1$mat))==trimws("NA")),NA,dt1$mat)               
suppressWarnings(dt1$mat <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$mat))==as.character(as.numeric("NA"))),NA,dt1$mat))
dt1$clim_avg_yrs <- as.factor(ifelse((trimws(as.character(dt1$clim_avg_yrs))==trimws("NA")),NA,as.character(dt1$clim_avg_yrs)))
dt1$aspect_deg <- ifelse((trimws(as.character(dt1$aspect_deg))==trimws("NA")),NA,dt1$aspect_deg)               
suppressWarnings(dt1$aspect_deg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$aspect_deg))==as.character(as.numeric("NA"))),NA,dt1$aspect_deg))
dt1$aspect_class <- as.factor(ifelse((trimws(as.character(dt1$aspect_class))==trimws("NA")),NA,as.character(dt1$aspect_class)))
dt1$slope <- ifelse((trimws(as.character(dt1$slope))==trimws("NA")),NA,dt1$slope)               
suppressWarnings(dt1$slope <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$slope))==as.character(as.numeric("NA"))),NA,dt1$slope))
dt1$slope_shape <- as.factor(ifelse((trimws(as.character(dt1$slope_shape))==trimws("NA")),NA,as.character(dt1$slope_shape)))
dt1$drainage_class <- as.factor(ifelse((trimws(as.character(dt1$drainage_class))==trimws("NA")),NA,as.character(dt1$drainage_class)))
dt1$depth_water <- ifelse((trimws(as.character(dt1$depth_water))==trimws("NA")),NA,dt1$depth_water)               
suppressWarnings(dt1$depth_water <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$depth_water))==as.character(as.numeric("NA"))),NA,dt1$depth_water))
dt1$parent_transport <- as.factor(ifelse((trimws(as.character(dt1$parent_transport))==trimws("NA")),NA,as.character(dt1$parent_transport)))
dt1$parent_material <- as.factor(ifelse((trimws(as.character(dt1$parent_material))==trimws("NA")),NA,as.character(dt1$parent_material)))
dt1$rock_chem <- as.factor(ifelse((trimws(as.character(dt1$rock_chem))==trimws("NA")),NA,as.character(dt1$rock_chem)))
dt1$land_cover <- as.factor(ifelse((trimws(as.character(dt1$land_cover))==trimws("NA")),NA,as.character(dt1$land_cover)))
dt1$eco_region <- as.factor(ifelse((trimws(as.character(dt1$eco_region))==trimws("NA")),NA,as.character(dt1$eco_region)))
dt1$loc_texture_class <- as.factor(ifelse((trimws(as.character(dt1$loc_texture_class))==trimws("NA")),NA,as.character(dt1$loc_texture_class)))
dt1$plant <- as.factor(ifelse((trimws(as.character(dt1$plant))==trimws("NA")),NA,as.character(dt1$plant)))
dt1$lit_c <- ifelse((trimws(as.character(dt1$lit_c))==trimws("NA")),NA,dt1$lit_c)               
suppressWarnings(dt1$lit_c <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lit_c))==as.character(as.numeric("NA"))),NA,dt1$lit_c))
dt1$lit_n <- ifelse((trimws(as.character(dt1$lit_n))==trimws("NA")),NA,dt1$lit_n)               
suppressWarnings(dt1$lit_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lit_n))==as.character(as.numeric("NA"))),NA,dt1$lit_n))
dt1$lit_p <- ifelse((trimws(as.character(dt1$lit_p))==trimws("NA")),NA,dt1$lit_p)               
suppressWarnings(dt1$lit_p <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lit_p))==as.character(as.numeric("NA"))),NA,dt1$lit_p))
dt1$lit_cn <- ifelse((trimws(as.character(dt1$lit_cn))==trimws("NA")),NA,dt1$lit_cn)               
suppressWarnings(dt1$lit_cn <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lit_cn))==as.character(as.numeric("NA"))),NA,dt1$lit_cn))
dt1$lit_lig <- ifelse((trimws(as.character(dt1$lit_lig))==trimws("NA")),NA,dt1$lit_lig)               
suppressWarnings(dt1$lit_lig <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lit_lig))==as.character(as.numeric("NA"))),NA,dt1$lit_lig))
dt1$npp <- ifelse((trimws(as.character(dt1$npp))==trimws("NA")),NA,dt1$npp)               
suppressWarnings(dt1$npp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$npp))==as.character(as.numeric("NA"))),NA,dt1$npp))
dt1$anpp <- ifelse((trimws(as.character(dt1$anpp))==trimws("NA")),NA,dt1$anpp)               
suppressWarnings(dt1$anpp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$anpp))==as.character(as.numeric("NA"))),NA,dt1$anpp))
dt1$bnpp <- ifelse((trimws(as.character(dt1$bnpp))==trimws("NA")),NA,dt1$bnpp)               
suppressWarnings(dt1$bnpp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$bnpp))==as.character(as.numeric("NA"))),NA,dt1$bnpp))
dt1$bnpp_notes <- as.factor(ifelse((trimws(as.character(dt1$bnpp_notes))==trimws("NA")),NA,as.character(dt1$bnpp_notes)))
dt1$litterfall_anpp <- ifelse((trimws(as.character(dt1$litterfall_anpp))==trimws("NA")),NA,dt1$litterfall_anpp)               
suppressWarnings(dt1$litterfall_anpp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$litterfall_anpp))==as.character(as.numeric("NA"))),NA,dt1$litterfall_anpp))
dt1$agb <- ifelse((trimws(as.character(dt1$agb))==trimws("NA")),NA,dt1$agb)               
suppressWarnings(dt1$agb <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$agb))==as.character(as.numeric("NA"))),NA,dt1$agb))
dt1$bgb <- ifelse((trimws(as.character(dt1$bgb))==trimws("NA")),NA,dt1$bgb)               
suppressWarnings(dt1$bgb <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$bgb))==as.character(as.numeric("NA"))),NA,dt1$bgb))
dt1$bgb_lowerdiam <- ifelse((trimws(as.character(dt1$bgb_lowerdiam))==trimws("NA")),NA,dt1$bgb_lowerdiam)               
suppressWarnings(dt1$bgb_lowerdiam <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$bgb_lowerdiam))==as.character(as.numeric("NA"))),NA,dt1$bgb_lowerdiam))
dt1$bgb_upperdiam <- ifelse((trimws(as.character(dt1$bgb_upperdiam))==trimws("NA")),NA,dt1$bgb_upperdiam)               
suppressWarnings(dt1$bgb_upperdiam <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$bgb_upperdiam))==as.character(as.numeric("NA"))),NA,dt1$bgb_upperdiam))
dt1$bgb_type <- as.factor(ifelse((trimws(as.character(dt1$bgb_type))==trimws("NA")),NA,as.character(dt1$bgb_type)))
dt1$bgb_notes <- as.factor(ifelse((trimws(as.character(dt1$bgb_notes))==trimws("NA")),NA,as.character(dt1$bgb_notes)))
dt1$bgb_c <- ifelse((trimws(as.character(dt1$bgb_c))==trimws("NA")),NA,dt1$bgb_c)               
suppressWarnings(dt1$bgb_c <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$bgb_c))==as.character(as.numeric("NA"))),NA,dt1$bgb_c))
dt1$bgb_n <- ifelse((trimws(as.character(dt1$bgb_n))==trimws("NA")),NA,dt1$bgb_n)               
suppressWarnings(dt1$bgb_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$bgb_n))==as.character(as.numeric("NA"))),NA,dt1$bgb_n))
dt1$bgb_cn <- ifelse((trimws(as.character(dt1$bgb_cn))==trimws("NA")),NA,dt1$bgb_cn)               
suppressWarnings(dt1$bgb_cn <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$bgb_cn))==as.character(as.numeric("NA"))),NA,dt1$bgb_cn))
dt1$wood_lit_c <- ifelse((trimws(as.character(dt1$wood_lit_c))==trimws("NA")),NA,dt1$wood_lit_c)               
suppressWarnings(dt1$wood_lit_c <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$wood_lit_c))==as.character(as.numeric("NA"))),NA,dt1$wood_lit_c))
dt1$align_1 <- as.factor(ifelse((trimws(as.character(dt1$align_1))==trimws("NA")),NA,as.character(dt1$align_1)))
dt1$align_2 <- as.factor(ifelse((trimws(as.character(dt1$align_2))==trimws("NA")),NA,as.character(dt1$align_2)))
dt1$loc_comments <- as.factor(ifelse((trimws(as.character(dt1$loc_comments))==trimws("NA")),NA,as.character(dt1$loc_comments)))
dt1$control_id <- as.factor(ifelse((trimws(as.character(dt1$control_id))==trimws("NA")),NA,as.character(dt1$control_id)))
dt1$number_treatments <- as.factor(ifelse((trimws(as.character(dt1$number_treatments))==trimws("NA")),NA,as.character(dt1$number_treatments)))
dt1$L1 <- as.factor(ifelse((trimws(as.character(dt1$L1))==trimws("NA")),NA,as.character(dt1$L1)))
dt1$L1_level <- as.factor(ifelse((trimws(as.character(dt1$L1_level))==trimws("NA")),NA,as.character(dt1$L1_level)))
dt1$L2 <- as.factor(ifelse((trimws(as.character(dt1$L2))==trimws("NA")),NA,as.character(dt1$L2)))
dt1$L2_level <- as.factor(ifelse((trimws(as.character(dt1$L2_level))==trimws("NA")),NA,as.character(dt1$L2_level)))
dt1$L3 <- as.factor(ifelse((trimws(as.character(dt1$L3))==trimws("NA")),NA,as.character(dt1$L3)))
dt1$L3_level <- as.factor(ifelse((trimws(as.character(dt1$L3_level))==trimws("NA")),NA,as.character(dt1$L3_level)))
dt1$L4 <- as.factor(ifelse((trimws(as.character(dt1$L4))==trimws("NA")),NA,as.character(dt1$L4)))
dt1$L4_level <- as.factor(ifelse((trimws(as.character(dt1$L4_level))==trimws("NA")),NA,as.character(dt1$L4_level)))
dt1$L5 <- as.factor(ifelse((trimws(as.character(dt1$L5))==trimws("NA")),NA,as.character(dt1$L5)))
dt1$L5_level <- as.factor(ifelse((trimws(as.character(dt1$L5_level))==trimws("NA")),NA,as.character(dt1$L5_level)))
dt1$tx_L1 <- as.factor(ifelse((trimws(as.character(dt1$tx_L1))==trimws("NA")),NA,as.character(dt1$tx_L1)))
dt1$tx_L1_level <- as.factor(ifelse((trimws(as.character(dt1$tx_L1_level))==trimws("NA")),NA,as.character(dt1$tx_L1_level)))
dt1$tx_L2 <- as.factor(ifelse((trimws(as.character(dt1$tx_L2))==trimws("NA")),NA,as.character(dt1$tx_L2)))
dt1$tx_L2_level <- as.factor(ifelse((trimws(as.character(dt1$tx_L2_level))==trimws("NA")),NA,as.character(dt1$tx_L2_level)))
dt1$tx_L3 <- as.factor(ifelse((trimws(as.character(dt1$tx_L3))==trimws("NA")),NA,as.character(dt1$tx_L3)))
dt1$tx_L3_level <- as.factor(ifelse((trimws(as.character(dt1$tx_L3_level))==trimws("NA")),NA,as.character(dt1$tx_L3_level)))
dt1$tx_L4 <- as.factor(ifelse((trimws(as.character(dt1$tx_L4))==trimws("NA")),NA,as.character(dt1$tx_L4)))
dt1$tx_L4_level <- as.factor(ifelse((trimws(as.character(dt1$tx_L4_level))==trimws("NA")),NA,as.character(dt1$tx_L4_level)))
dt1$tx_L5 <- as.factor(ifelse((trimws(as.character(dt1$tx_L5))==trimws("NA")),NA,as.character(dt1$tx_L5)))
dt1$tx_L5_level <- as.factor(ifelse((trimws(as.character(dt1$tx_L5_level))==trimws("NA")),NA,as.character(dt1$tx_L5_level)))
dt1$tx_L6 <- as.factor(ifelse((trimws(as.character(dt1$tx_L6))==trimws("NA")),NA,as.character(dt1$tx_L6)))
dt1$tx_L6_level <- as.factor(ifelse((trimws(as.character(dt1$tx_L6_level))==trimws("NA")),NA,as.character(dt1$tx_L6_level)))
dt1$observation_date <- as.factor(ifelse((trimws(as.character(dt1$observation_date))==trimws("NA")),NA,as.character(dt1$observation_date)))
dt1$veg_note_profile <- as.factor(ifelse((trimws(as.character(dt1$veg_note_profile))==trimws("NA")),NA,as.character(dt1$veg_note_profile)))
dt1$lyr_c_tot <- ifelse((trimws(as.character(dt1$lyr_c_tot))==trimws("NA")),NA,dt1$lyr_c_tot)               
suppressWarnings(dt1$lyr_c_tot <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_c_tot))==as.character(as.numeric("NA"))),NA,dt1$lyr_c_tot))
dt1$lyr_soc <- ifelse((trimws(as.character(dt1$lyr_soc))==trimws("NA")),NA,dt1$lyr_soc)               
suppressWarnings(dt1$lyr_soc <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_soc))==as.character(as.numeric("NA"))),NA,dt1$lyr_soc))
dt1$lyr_soc_stock <- ifelse((trimws(as.character(dt1$lyr_soc_stock))==trimws("NA")),NA,dt1$lyr_soc_stock)               
suppressWarnings(dt1$lyr_soc_stock <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_soc_stock))==as.character(as.numeric("NA"))),NA,dt1$lyr_soc_stock))
dt1$lyr_loi <- ifelse((trimws(as.character(dt1$lyr_loi))==trimws("NA")),NA,dt1$lyr_loi)               
suppressWarnings(dt1$lyr_loi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_loi))==as.character(as.numeric("NA"))),NA,dt1$lyr_loi))
dt1$lyr_som_WalkleyBlack <- ifelse((trimws(as.character(dt1$lyr_som_WalkleyBlack))==trimws("NA")),NA,dt1$lyr_som_WalkleyBlack)               
suppressWarnings(dt1$lyr_som_WalkleyBlack <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_som_WalkleyBlack))==as.character(as.numeric("NA"))),NA,dt1$lyr_som_WalkleyBlack))
dt1$lyr_n_tot <- ifelse((trimws(as.character(dt1$lyr_n_tot))==trimws("NA")),NA,dt1$lyr_n_tot)               
suppressWarnings(dt1$lyr_n_tot <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_n_tot))==as.character(as.numeric("NA"))),NA,dt1$lyr_n_tot))
dt1$lyr_n_tot_stock <- ifelse((trimws(as.character(dt1$lyr_n_tot_stock))==trimws("NA")),NA,dt1$lyr_n_tot_stock)               
suppressWarnings(dt1$lyr_n_tot_stock <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_n_tot_stock))==as.character(as.numeric("NA"))),NA,dt1$lyr_n_tot_stock))
dt1$lyr_c_to_n <- ifelse((trimws(as.character(dt1$lyr_c_to_n))==trimws("NA")),NA,dt1$lyr_c_to_n)               
suppressWarnings(dt1$lyr_c_to_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_c_to_n))==as.character(as.numeric("NA"))),NA,dt1$lyr_c_to_n))
dt1$lyr_15n <- ifelse((trimws(as.character(dt1$lyr_15n))==trimws("NA")),NA,dt1$lyr_15n)               
suppressWarnings(dt1$lyr_15n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_15n))==as.character(as.numeric("NA"))),NA,dt1$lyr_15n))
dt1$lyr_13c <- ifelse((trimws(as.character(dt1$lyr_13c))==trimws("NA")),NA,dt1$lyr_13c)               
suppressWarnings(dt1$lyr_13c <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_13c))==as.character(as.numeric("NA"))),NA,dt1$lyr_13c))
dt1$layer_top <- ifelse((trimws(as.character(dt1$layer_top))==trimws("NA")),NA,dt1$layer_top)               
suppressWarnings(dt1$layer_top <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$layer_top))==as.character(as.numeric("NA"))),NA,dt1$layer_top))
dt1$layer_bot <- ifelse((trimws(as.character(dt1$layer_bot))==trimws("NA")),NA,dt1$layer_bot)               
suppressWarnings(dt1$layer_bot <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$layer_bot))==as.character(as.numeric("NA"))),NA,dt1$layer_bot))
dt1$layer_mid <- ifelse((trimws(as.character(dt1$layer_mid))==trimws("NA")),NA,dt1$layer_mid)               
suppressWarnings(dt1$layer_mid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$layer_mid))==as.character(as.numeric("NA"))),NA,dt1$layer_mid))
dt1$hzn <- as.factor(ifelse((trimws(as.character(dt1$hzn))==trimws("NA")),NA,as.character(dt1$hzn)))
dt1$bd_samp <- ifelse((trimws(as.character(dt1$bd_samp))==trimws("NA")),NA,dt1$bd_samp)               
suppressWarnings(dt1$bd_samp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$bd_samp))==as.character(as.numeric("NA"))),NA,dt1$bd_samp))
dt1$bd_tot <- ifelse((trimws(as.character(dt1$bd_tot))==trimws("NA")),NA,dt1$bd_tot)               
suppressWarnings(dt1$bd_tot <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$bd_tot))==as.character(as.numeric("NA"))),NA,dt1$bd_tot))
dt1$ph_cacl <- ifelse((trimws(as.character(dt1$ph_cacl))==trimws("NA")),NA,dt1$ph_cacl)               
suppressWarnings(dt1$ph_cacl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$ph_cacl))==as.character(as.numeric("NA"))),NA,dt1$ph_cacl))
dt1$ph_h2o <- ifelse((trimws(as.character(dt1$ph_h2o))==trimws("NA")),NA,dt1$ph_h2o)               
suppressWarnings(dt1$ph_h2o <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$ph_h2o))==as.character(as.numeric("NA"))),NA,dt1$ph_h2o))
dt1$ph_other <- ifelse((trimws(as.character(dt1$ph_other))==trimws("NA")),NA,dt1$ph_other)               
suppressWarnings(dt1$ph_other <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$ph_other))==as.character(as.numeric("NA"))),NA,dt1$ph_other))
dt1$sand <- ifelse((trimws(as.character(dt1$sand))==trimws("NA")),NA,dt1$sand)               
suppressWarnings(dt1$sand <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$sand))==as.character(as.numeric("NA"))),NA,dt1$sand))
dt1$silt <- ifelse((trimws(as.character(dt1$silt))==trimws("NA")),NA,dt1$silt)               
suppressWarnings(dt1$silt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$silt))==as.character(as.numeric("NA"))),NA,dt1$silt))
dt1$clay <- ifelse((trimws(as.character(dt1$clay))==trimws("NA")),NA,dt1$clay)               
suppressWarnings(dt1$clay <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$clay))==as.character(as.numeric("NA"))),NA,dt1$clay))
dt1$coarse_frac <- ifelse((trimws(as.character(dt1$coarse_frac))==trimws("NA")),NA,dt1$coarse_frac)               
suppressWarnings(dt1$coarse_frac <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$coarse_frac))==as.character(as.numeric("NA"))),NA,dt1$coarse_frac))
dt1$coarse_tot <- ifelse((trimws(as.character(dt1$coarse_tot))==trimws("NA")),NA,dt1$coarse_tot)               
suppressWarnings(dt1$coarse_tot <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$coarse_tot))==as.character(as.numeric("NA"))),NA,dt1$coarse_tot))
dt1$profile_texture_class <- as.factor(ifelse((trimws(as.character(dt1$profile_texture_class))==trimws("NA")),NA,as.character(dt1$profile_texture_class)))
dt1$caco3 <- ifelse((trimws(as.character(dt1$caco3))==trimws("NA")),NA,dt1$caco3)               
suppressWarnings(dt1$caco3 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$caco3))==as.character(as.numeric("NA"))),NA,dt1$caco3))
dt1$Ca <- ifelse((trimws(as.character(dt1$Ca))==trimws("NA")),NA,dt1$Ca)               
suppressWarnings(dt1$Ca <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Ca))==as.character(as.numeric("NA"))),NA,dt1$Ca))
dt1$Mg <- ifelse((trimws(as.character(dt1$Mg))==trimws("NA")),NA,dt1$Mg)               
suppressWarnings(dt1$Mg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Mg))==as.character(as.numeric("NA"))),NA,dt1$Mg))
dt1$K <- ifelse((trimws(as.character(dt1$K))==trimws("NA")),NA,dt1$K)               
suppressWarnings(dt1$K <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$K))==as.character(as.numeric("NA"))),NA,dt1$K))
dt1$Na <- ifelse((trimws(as.character(dt1$Na))==trimws("NA")),NA,dt1$Na)               
suppressWarnings(dt1$Na <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Na))==as.character(as.numeric("NA"))),NA,dt1$Na))
dt1$cat_exch <- ifelse((trimws(as.character(dt1$cat_exch))==trimws("NA")),NA,dt1$cat_exch)               
suppressWarnings(dt1$cat_exch <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$cat_exch))==as.character(as.numeric("NA"))),NA,dt1$cat_exch))
dt1$base_sum <- ifelse((trimws(as.character(dt1$base_sum))==trimws("NA")),NA,dt1$base_sum)               
suppressWarnings(dt1$base_sum <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$base_sum))==as.character(as.numeric("NA"))),NA,dt1$base_sum))
dt1$cec_sum <- ifelse((trimws(as.character(dt1$cec_sum))==trimws("NA")),NA,dt1$cec_sum)               
suppressWarnings(dt1$cec_sum <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$cec_sum))==as.character(as.numeric("NA"))),NA,dt1$cec_sum))
dt1$ecec <- ifelse((trimws(as.character(dt1$ecec))==trimws("NA")),NA,dt1$ecec)               
suppressWarnings(dt1$ecec <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$ecec))==as.character(as.numeric("NA"))),NA,dt1$ecec))
dt1$bs <- ifelse((trimws(as.character(dt1$bs))==trimws("NA")),NA,dt1$bs)               
suppressWarnings(dt1$bs <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$bs))==as.character(as.numeric("NA"))),NA,dt1$bs))
dt1$mbc_trans <- ifelse((trimws(as.character(dt1$mbc_trans))==trimws("NA")),NA,dt1$mbc_trans)               
suppressWarnings(dt1$mbc_trans <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$mbc_trans))==as.character(as.numeric("NA"))),NA,dt1$mbc_trans))
dt1$mbn_raw <- ifelse((trimws(as.character(dt1$mbn_raw))==trimws("NA")),NA,dt1$mbn_raw)               
suppressWarnings(dt1$mbn_raw <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$mbn_raw))==as.character(as.numeric("NA"))),NA,dt1$mbn_raw))
dt1$p_ex_1 <- ifelse((trimws(as.character(dt1$p_ex_1))==trimws("NA")),NA,dt1$p_ex_1)               
suppressWarnings(dt1$p_ex_1 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$p_ex_1))==as.character(as.numeric("NA"))),NA,dt1$p_ex_1))
dt1$p_ex_2 <- ifelse((trimws(as.character(dt1$p_ex_2))==trimws("NA")),NA,dt1$p_ex_2)               
suppressWarnings(dt1$p_ex_2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$p_ex_2))==as.character(as.numeric("NA"))),NA,dt1$p_ex_2))
dt1$p_ex_3 <- ifelse((trimws(as.character(dt1$p_ex_3))==trimws("NA")),NA,dt1$p_ex_3)               
suppressWarnings(dt1$p_ex_3 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$p_ex_3))==as.character(as.numeric("NA"))),NA,dt1$p_ex_3))
dt1$p_ex_4 <- ifelse((trimws(as.character(dt1$p_ex_4))==trimws("NA")),NA,dt1$p_ex_4)               
suppressWarnings(dt1$p_ex_4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$p_ex_4))==as.character(as.numeric("NA"))),NA,dt1$p_ex_4))
dt1$n_min <- ifelse((trimws(as.character(dt1$n_min))==trimws("NA")),NA,dt1$n_min)               
suppressWarnings(dt1$n_min <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$n_min))==as.character(as.numeric("NA"))),NA,dt1$n_min))
dt1$soil_taxon <- as.factor(ifelse((trimws(as.character(dt1$soil_taxon))==trimws("NA")),NA,as.character(dt1$soil_taxon)))
dt1$soil_series <- as.factor(ifelse((trimws(as.character(dt1$soil_series))==trimws("NA")),NA,as.character(dt1$soil_series)))
dt1$comment_profile <- as.factor(ifelse((trimws(as.character(dt1$comment_profile))==trimws("NA")),NA,as.character(dt1$comment_profile)))
dt1$lyr_al_py <- ifelse((trimws(as.character(dt1$lyr_al_py))==trimws("NA")),NA,dt1$lyr_al_py)               
suppressWarnings(dt1$lyr_al_py <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_al_py))==as.character(as.numeric("NA"))),NA,dt1$lyr_al_py))
dt1$lyr_si_py <- ifelse((trimws(as.character(dt1$lyr_si_py))==trimws("NA")),NA,dt1$lyr_si_py)               
suppressWarnings(dt1$lyr_si_py <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_si_py))==as.character(as.numeric("NA"))),NA,dt1$lyr_si_py))
dt1$lyr_fe_ox <- ifelse((trimws(as.character(dt1$lyr_fe_ox))==trimws("NA")),NA,dt1$lyr_fe_ox)               
suppressWarnings(dt1$lyr_fe_ox <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_fe_ox))==as.character(as.numeric("NA"))),NA,dt1$lyr_fe_ox))
dt1$lyr_al_ox <- ifelse((trimws(as.character(dt1$lyr_al_ox))==trimws("NA")),NA,dt1$lyr_al_ox)               
suppressWarnings(dt1$lyr_al_ox <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_al_ox))==as.character(as.numeric("NA"))),NA,dt1$lyr_al_ox))
dt1$lyr_si_ox <- ifelse((trimws(as.character(dt1$lyr_si_ox))==trimws("NA")),NA,dt1$lyr_si_ox)               
suppressWarnings(dt1$lyr_si_ox <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_si_ox))==as.character(as.numeric("NA"))),NA,dt1$lyr_si_ox))
dt1$lyr_fe_dith <- ifelse((trimws(as.character(dt1$lyr_fe_dith))==trimws("NA")),NA,dt1$lyr_fe_dith)               
suppressWarnings(dt1$lyr_fe_dith <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_fe_dith))==as.character(as.numeric("NA"))),NA,dt1$lyr_fe_dith))
dt1$lyr_al_dith <- ifelse((trimws(as.character(dt1$lyr_al_dith))==trimws("NA")),NA,dt1$lyr_al_dith)               
suppressWarnings(dt1$lyr_al_dith <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_al_dith))==as.character(as.numeric("NA"))),NA,dt1$lyr_al_dith))
dt1$lyr_si_dith <- ifelse((trimws(as.character(dt1$lyr_si_dith))==trimws("NA")),NA,dt1$lyr_si_dith)               
suppressWarnings(dt1$lyr_si_dith <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_si_dith))==as.character(as.numeric("NA"))),NA,dt1$lyr_si_dith))
dt1$frc_scheme <- as.factor(ifelse((trimws(as.character(dt1$frc_scheme))==trimws("NA")),NA,as.character(dt1$frc_scheme)))
dt1$frc_low_cutoff <- as.factor(ifelse((trimws(as.character(dt1$frc_low_cutoff))==trimws("NA")),NA,as.character(dt1$frc_low_cutoff)))
dt1$frc_high_cutoff <- as.factor(ifelse((trimws(as.character(dt1$frc_high_cutoff))==trimws("NA")),NA,as.character(dt1$frc_high_cutoff)))
dt1$frc_notes <- as.factor(ifelse((trimws(as.character(dt1$frc_notes))==trimws("NA")),NA,as.character(dt1$frc_notes)))
dt1$frc_c_tot <- ifelse((trimws(as.character(dt1$frc_c_tot))==trimws("NA")),NA,dt1$frc_c_tot)               
suppressWarnings(dt1$frc_c_tot <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$frc_c_tot))==as.character(as.numeric("NA"))),NA,dt1$frc_c_tot))
dt1$frc_oc <- ifelse((trimws(as.character(dt1$frc_oc))==trimws("NA")),NA,dt1$frc_oc)               
suppressWarnings(dt1$frc_oc <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$frc_oc))==as.character(as.numeric("NA"))),NA,dt1$frc_oc))
dt1$frc_n_tot <- ifelse((trimws(as.character(dt1$frc_n_tot))==trimws("NA")),NA,dt1$frc_n_tot)               
suppressWarnings(dt1$frc_n_tot <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$frc_n_tot))==as.character(as.numeric("NA"))),NA,dt1$frc_n_tot))
dt1$frc_c_to_n <- ifelse((trimws(as.character(dt1$frc_c_to_n))==trimws("NA")),NA,dt1$frc_c_to_n)               
suppressWarnings(dt1$frc_c_to_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$frc_c_to_n))==as.character(as.numeric("NA"))),NA,dt1$frc_c_to_n))
dt1$bd_methods_notes <- as.factor(ifelse((trimws(as.character(dt1$bd_methods_notes))==trimws("NA")),NA,as.character(dt1$bd_methods_notes)))
dt1$layer_thick_calc <- ifelse((trimws(as.character(dt1$layer_thick_calc))==trimws("NA")),NA,dt1$layer_thick_calc)               
suppressWarnings(dt1$layer_thick_calc <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$layer_thick_calc))==as.character(as.numeric("NA"))),NA,dt1$layer_thick_calc))
dt1$lyr_soc_stock_calc <- ifelse((trimws(as.character(dt1$lyr_soc_stock_calc))==trimws("NA")),NA,dt1$lyr_soc_stock_calc)               
suppressWarnings(dt1$lyr_soc_stock_calc <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_soc_stock_calc))==as.character(as.numeric("NA"))),NA,dt1$lyr_soc_stock_calc))
dt1$lyr_n_stock_calc <- ifelse((trimws(as.character(dt1$lyr_n_stock_calc))==trimws("NA")),NA,dt1$lyr_n_stock_calc)               
suppressWarnings(dt1$lyr_n_stock_calc <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$lyr_n_stock_calc))==as.character(as.numeric("NA"))),NA,dt1$lyr_n_stock_calc))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(google_dir)
summary(data_file)
summary(curator_PersonName)
summary(curator_organization)
summary(curator_email)
summary(modification_date)
summary(author_PersonName)
summary(author_email)
summary(author_orcid_id)
summary(addit_contact_person)
summary(addit_contact_email)
summary(bibliographical_reference)
summary(data_doi)
summary(network)
summary(site_code)
summary(location_name)
summary(lat)
summary(long)
summary(datum)
summary(elevation)
summary(map)
summary(mat)
summary(clim_avg_yrs)
summary(tx_start)
summary(aspect_deg)
summary(aspect_class)
summary(slope)
summary(slope_shape)
summary(drainage_class)
summary(depth_water)
summary(parent_transport)
summary(parent_material)
summary(rock_chem)
summary(land_cover)
summary(eco_region)
summary(loc_texture_class)
summary(plant)
summary(lit_c)
summary(lit_n)
summary(lit_p)
summary(lit_cn)
summary(lit_lig)
summary(npp)
summary(anpp)
summary(bnpp)
summary(bnpp_notes)
summary(litterfall_anpp)
summary(agb)
summary(bgb)
summary(bgb_lowerdiam)
summary(bgb_upperdiam)
summary(bgb_type)
summary(bgb_notes)
summary(bgb_c)
summary(bgb_n)
summary(bgb_cn)
summary(wood_lit_c)
summary(align_1)
summary(align_2)
summary(loc_comments)
summary(time_series)
summary(gradient)
summary(experiments)
summary(control_id)
summary(number_treatments)
summary(merge_align)
summary(key_version)
summary(L1)
summary(L1_level)
summary(L2)
summary(L2_level)
summary(L3)
summary(L3_level)
summary(L4)
summary(L4_level)
summary(L5)
summary(L5_level)
summary(tx_L1)
summary(tx_L1_level)
summary(tx_L2)
summary(tx_L2_level)
summary(tx_L3)
summary(tx_L3_level)
summary(tx_L4)
summary(tx_L4_level)
summary(tx_L5)
summary(tx_L5_level)
summary(tx_L6)
summary(tx_L6_level)
summary(observation_date)
summary(veg_note_profile)
summary(lyr_c_tot)
summary(lyr_soc)
summary(lyr_soc_stock)
summary(lyr_loi)
summary(lyr_som_WalkleyBlack)
summary(lyr_n_tot)
summary(lyr_n_tot_stock)
summary(lyr_c_to_n)
summary(lyr_15n)
summary(lyr_13c)
summary(layer_top)
summary(layer_bot)
summary(layer_mid)
summary(hzn)
summary(bd_samp)
summary(bd_tot)
summary(ph_cacl)
summary(ph_h2o)
summary(ph_other)
summary(sand)
summary(silt)
summary(clay)
summary(coarse_frac)
summary(coarse_tot)
summary(profile_texture_class)
summary(caco3)
summary(Ca)
summary(Mg)
summary(K)
summary(Na)
summary(cat_exch)
summary(base_sum)
summary(cec_sum)
summary(ecec)
summary(bs)
summary(mbc_trans)
summary(mbn_raw)
summary(p_ex_1)
summary(p_ex_2)
summary(p_ex_3)
summary(p_ex_4)
summary(n_min)
summary(soil_taxon)
summary(soil_series)
summary(comment_profile)
summary(lyr_al_py)
summary(lyr_si_py)
summary(lyr_fe_ox)
summary(lyr_al_ox)
summary(lyr_si_ox)
summary(lyr_fe_dith)
summary(lyr_al_dith)
summary(lyr_si_dith)
summary(frc_scheme)
summary(frc_low_cutoff)
summary(frc_high_cutoff)
summary(frc_notes)
summary(frc_c_tot)
summary(frc_oc)
summary(frc_n_tot)
summary(frc_c_to_n)
summary(bd_methods_notes)
summary(layer_thick_calc)
summary(lyr_soc_stock_calc)
summary(lyr_n_stock_calc)
summary(control_sample) 
                # Get more details on character variables
                 
summary(as.factor(dt1$google_dir)) 
summary(as.factor(dt1$data_file)) 
summary(as.factor(dt1$curator_PersonName)) 
summary(as.factor(dt1$curator_organization)) 
summary(as.factor(dt1$curator_email)) 
summary(as.factor(dt1$author_PersonName)) 
summary(as.factor(dt1$author_email)) 
summary(as.factor(dt1$author_orcid_id)) 
summary(as.factor(dt1$addit_contact_person)) 
summary(as.factor(dt1$addit_contact_email)) 
summary(as.factor(dt1$bibliographical_reference)) 
summary(as.factor(dt1$data_doi)) 
summary(as.factor(dt1$network)) 
summary(as.factor(dt1$site_code)) 
summary(as.factor(dt1$location_name)) 
summary(as.factor(dt1$datum)) 
summary(as.factor(dt1$clim_avg_yrs)) 
summary(as.factor(dt1$aspect_class)) 
summary(as.factor(dt1$slope_shape)) 
summary(as.factor(dt1$drainage_class)) 
summary(as.factor(dt1$parent_transport)) 
summary(as.factor(dt1$parent_material)) 
summary(as.factor(dt1$rock_chem)) 
summary(as.factor(dt1$land_cover)) 
summary(as.factor(dt1$eco_region)) 
summary(as.factor(dt1$loc_texture_class)) 
summary(as.factor(dt1$plant)) 
summary(as.factor(dt1$bnpp_notes)) 
summary(as.factor(dt1$bgb_type)) 
summary(as.factor(dt1$bgb_notes)) 
summary(as.factor(dt1$align_1)) 
summary(as.factor(dt1$align_2)) 
summary(as.factor(dt1$loc_comments)) 
summary(as.factor(dt1$time_series)) 
summary(as.factor(dt1$gradient)) 
summary(as.factor(dt1$experiments)) 
summary(as.factor(dt1$control_id)) 
summary(as.factor(dt1$number_treatments)) 
summary(as.factor(dt1$merge_align)) 
summary(as.factor(dt1$key_version)) 
summary(as.factor(dt1$L1)) 
summary(as.factor(dt1$L1_level)) 
summary(as.factor(dt1$L2)) 
summary(as.factor(dt1$L2_level)) 
summary(as.factor(dt1$L3)) 
summary(as.factor(dt1$L3_level)) 
summary(as.factor(dt1$L4)) 
summary(as.factor(dt1$L4_level)) 
summary(as.factor(dt1$L5)) 
summary(as.factor(dt1$L5_level)) 
summary(as.factor(dt1$tx_L1)) 
summary(as.factor(dt1$tx_L1_level)) 
summary(as.factor(dt1$tx_L2)) 
summary(as.factor(dt1$tx_L2_level)) 
summary(as.factor(dt1$tx_L3)) 
summary(as.factor(dt1$tx_L3_level)) 
summary(as.factor(dt1$tx_L4)) 
summary(as.factor(dt1$tx_L4_level)) 
summary(as.factor(dt1$tx_L5)) 
summary(as.factor(dt1$tx_L5_level)) 
summary(as.factor(dt1$tx_L6)) 
summary(as.factor(dt1$tx_L6_level)) 
summary(as.factor(dt1$observation_date)) 
summary(as.factor(dt1$veg_note_profile)) 
summary(as.factor(dt1$hzn)) 
summary(as.factor(dt1$profile_texture_class)) 
summary(as.factor(dt1$soil_taxon)) 
summary(as.factor(dt1$soil_series)) 
summary(as.factor(dt1$comment_profile)) 
summary(as.factor(dt1$frc_scheme)) 
summary(as.factor(dt1$frc_low_cutoff)) 
summary(as.factor(dt1$frc_high_cutoff)) 
summary(as.factor(dt1$frc_notes)) 
summary(as.factor(dt1$bd_methods_notes)) 
summary(as.factor(dt1$control_sample))
detach(dt1)               
        



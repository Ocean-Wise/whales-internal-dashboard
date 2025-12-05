

column_filter = c(
  "sighting_id",
  "sighting_date",
  "species_name",
  "species_scientific_name",
  "ecotype_name",
  "report_latitude",
  "report_longitude",
  "count_type",
  "report_count",
  "observer_confidence",
  "comments",
  "sighting_platform_name",
  "report_source_type",
  "observer_type_name",
  "sighting_year",
  "sighting_month",
  "sighting_year_month",
  "observer_email",
  "observer_organization"
) 


emails = c(
  "katiekushneryk@gmail.com",
  "capebealelight@gmail.com",
  "daniel.grinell@pc.gc.ca",
  "Queenielai816@gmail.com",
  "Arlene.armstrong@canada.ca",
  "ian.cruickshank@canada.ca",
  "pc.kristabohlen@gmail.com",
  "jennifer.yakimishyn@canada.ca",
  "darren.salisbury@pc.gc.ca",
  "cjgus713@gmail.com",
  "nathalie.choinard-nolet@pc.gc.ca",
  "w.friedo3@gmail.com",
  "cassidybodnar@gmail.com",
  "chase22corbett@gmail.com",
  "ceebatchelor3@gmail.com",
  "maiab4004@gmail.com",
  "ewvannier@gmail.com",
  "j.butts1265@gmail.com",
  "emklingbell@outlook.com",
  "seneca.paquette.jager@gmail.com",
  "michelle.segal@pc.gc.ca",
  "lariharder@gmail.com",
  "mandy_lawrenz@web.de",
  "ali.13.2000@gmail.com",
  "pw.mauricio.arango@gmail.com",
  "meaghan.kujat@pc.gc.ca",
  "olivia.geurtsen@gmail.com",
  "tess.turner@pc.gc.ca",
  "jules.geurtsen@gmail.com",
  "camrynlatimer@gmail.com",
  "kyle.vandelft@pc.gc.ca"
)
nx_bc_ferries = sightings_main %>% 
  dplyr::filter(stringr::str_detect(submitter_user_email, "bcf") | 
                  species_name == "Killer whale" & stringr::str_detect(submitter_organization, "bcf")) %>% 
  dplyr::select(
    sighting_start,
    species_name,
    species_scientific_name,
    ecotype_name,
    report_latitude,
    report_longitude,
    report_count,
    observer_name,
    submitter_user_email,
    submitter_organization,
    sighting_year,
    sighting_month,
    sighting_year_month 
  ) %>% 
  write.csv(., file = "../../../Downloads/PRNPR-sightings.csv")


prnpr_sightings = sightings_main %>% 
  dplyr::filter(observer_email %in% emails) %>% 
  dplyr::select(tidyr::all_of(column_filter)) %>% 
  dplyr::filter(sighting_year == 2025) %>% 
  write.csv(., file = "../../../Downloads/20251111-prnpr-sightings.csv")


killer_whale_data = sightings_master %>% 
  dplyr::filter(species_name == "Killer whale") %>% 
  dplyr::filter(!stringr::str_detect(report_source_entity, ocean_wise_data_only)) %>% 
  dplyr::select(tidyr::all_of(column_filter)) %>% 
  write.csv(., file = "../../../Downloads/20251111-kw-tidyish-sightings.csv")






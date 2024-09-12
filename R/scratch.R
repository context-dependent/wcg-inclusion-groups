library(tidyverse)

d0 <- read_csv("data/211-community-assets.csv")

d |> 
  filter(
    PublicName |> str_detect("Disability")
  ) |> 
  View()

d1 <- d0 |> 
  mutate(
    AlternateName = str_split(AlternateName, "; ")
  ) |> 
  unnest(AlternateName) |> 
  group_by(OBJECTID) |> 
  filter(row_number() <= 25) |> 
  mutate(an_row = glue::glue("AltName{row_number()}")) |> 
  pivot_wider(names_from = an_row, values_from = AlternateName) |> 
  select(
    OBJECTID, 
    ResourceAgencyNum, 
    PublicName, 
    AltName1:AltName4
  ) 

d2 <- d1 |> 
  ungroup() |> 
  mutate(
    pub_name = PublicName |> 
      str_replace_all(
        "Ontario. Ministry of (Children, |)Community and Social Services( - |\\. )", 
        "MCCSS: "
      ) |> 
      str_replace_all(
        "^.*Ontario Disability Support Program.*$", 
        "Ontario Disability Support Program (ODSP)"
      ) |> 
      str_replace_all(
        "Ontario. Ministry of Labour, Training and Skills Development( - |\\. )", 
        "MLTSD: "
      ) |> 
      str_replace_all(
        "Ontario. Ministry of Health - ", 
        "Min. of Health: "
      ) |> 
      str_replace_all(
        "Ontario. Ministry of Economic Development, Job Creation and Trade. ", 
        "MEDJCT: "
      ) |> 
      str_replace_all(
        "Ontario. Ministry of Heritage, Sport, Tourism and Culture [Ii]ndustries(\\. | - )", 
        "MHSTCI: "
      ) |> 
      str_replace_all(
        "Ontario. Ministry of Municipal Affairs and Housing. ", 
        "MMAH: "
      ) |> 
      str_replace_all(
        "Ontario. *Ministry of the Attorney General(\\. | - |, Court Services - |, Hamilton Services - )", 
        "MAG: "
      ) |>
      str_replace_all(
        "Ontario. Ministry of Education(\\. | - )", 
        "Min. of Education: "
      ) |> 
      str_replace_all(
        "Ontario. Ministry of the Solicitor General - ",
        "SolGen: "
      ) |> 
      str_replace_all(
        "Ontario. Ministry of Transportation - ",
        "Min. of Transportation: "
      ) |> 
      str_replace_all(
        "Ontario. Ministry of the Environment , Conservation and Parks - ", 
        "Min. of Environment: "
      ) |> 
      str_replace_all(
        "Ontario. Ministry of Infrastructure - ", 
        "Min. of Infrastructure: "
      ) |>
      str_replace_all(
        "Ontario. Ministry for Seniors and Accessibility - ", 
        "MSA: "
      ) |>
      str_replace_all(
        "Ontario. Ministry of Francophone Affairs - ", 
        "MFA: "
      ) |>
      str_replace_all(
        "Ontario. Ministry of Government and Consumer Services. Consumer Protection Ontario", 
        "MGCS: Consumer Protection Ontario"
      ) |>
      str_replace_all(
        "Toronto. Employment and Social Services - ", 
        "TESS: "
      ) |> 
      str_replace_all(
        ".*Ontario Works.*$", 
        "Ontario Works (OW)"
      ) |> 
      str_replace_all(
        "Probation and Parole.*$", 
        "Probation and Parole"
      ) |> 
      str_replace_all(
        "Court of Justice.*$", 
        "Court of Justice"
      ) |> 
      str_replace_all(
        "Victim / Witness Assistance Program.*", 
        "Victim / Witness Assistance Program"
      ) |> 
      str_replace_all(" +", " "),
    org_name = case_when(
      pub_name |> str_detect(" - ") ~ str_extract(pub_name, r"(^.*?(?= -))"), 
      TRUE ~ pub_name
    ) |> 
    str_remove(r"( \[Child care\])"), 
    AltName1 = case_when(
      org_name == "Ontario Works (OW)" ~ "OW", 
      org_name == "Ontario Disability Support Program (ODSP)" ~ "ODSP",
      TRUE ~ AltName1
    ), 
    AltName2 = case_when(
      org_name == "Ontario Works (OW)" ~ "TESS", 
      TRUE ~ AltName2
    )
  ) 

d2 |> filter(pub_name |> str_detect("Works")) |> 
  select(pub_name, org_name)

d3 <- d2 |> 
  group_by(org_name) |> 
  summarize(
    object_ids = str_c(OBJECTID, collapse = "; "), 
    across(
      AltName1:AltName4, 
      ~ case_when(
        all(is.na(.x)) ~ NA_character_, 
        TRUE ~ str_trunc(stringi::stri_enc_toascii(str_c(.x, collapse = "; ")), 200)
      )
    ), 
    .groups = "drop"
  ) |> 
  mutate(
    row_number = row_number()
  ) 


d3 |> 
  filter(org_name |> str_detect("ODSP")) |> 
  select(matches())
d2 |> filter(OBJECTID %in% c(50, 51, 173)) |> 
  select(pub_name, org_name)
d3 |> 
  write_csv("data/211-community-assets_autofill-3.csv", na = "")

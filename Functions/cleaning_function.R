#Cleaning function to clean column names, remove empty rows, remove columns called comment and delta 
cleaning <- function(data_raw){
  data_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments)
}

#Subset the data to only include the penguins that are not NA for the bill length
remove_empty_flipper_length <- function(data_clean){
  data_clean %>%
    filter(!is.na(flipper_length_mm)) %>%
    select(species, flipper_length_mm)
}

#Subset the data to include only include flipper length and body mass data; remove missing values. This is for the bad_figure
bad_figure_data <- function(data_clean){
  data_clean %>%
    filter(!is.na(flipper_length_mm)) %>%
    filter(!is.na(body_mass_g)) %>%
    select(flipper_length_mm, body_mass_g)
}

#Subset the data to include only include species, flipper length, body mass data; remove missing values
bodymass_flipperlength_species<- function(data_clean){
  data_clean %>%
    filter(!is.na(flipper_length_mm)) %>%
    filter(!is.na(body_mass_g)) %>%
    select(flipper_length_mm, body_mass_g, species)
}



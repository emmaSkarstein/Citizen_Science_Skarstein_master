# Whole procedure

# Load map, covariates and observations

source("R/loading_map_obs_covs.R")




stk <- MakeStacks(data_structured = Data_survey, data_unstructured = Data_artsobs,
                  covariates = Covariates, Mesh = Mesh)

# Split into training and test sets

#...



# Choose from 
# "decimalLatitude", "decimalLongitude",
# "area_km2", "perimeter_m", "distance_to_road", 
# "eurolst_bio10", "catchment_area_km2", "SCI", "HFP"
Use <- c("decimalLongitude","decimalLatitude", "log_area", "perimeter_m", 
         "eurolst_bio10", "SCI")
Use_CS <- c(Use, "distance_to_road", "HFP")

formula1 <- MakeFormula(cov_names = Use, second_sp_field = FALSE)
formula2 <- MakeFormula(cov_names = Use, second_sp_field = TRUE)
formula3 <- MakeFormula(cov_names = Use_CS, second_sp_field = FALSE)
formula4 <- MakeFormula(cov_names = Use_CS, second_sp_field = TRUE)

model1_list <- MakeModel(stk, formula1, Mesh)



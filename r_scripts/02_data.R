###########################################################################
#
#               This script is EDA and data wrangling 
#
###########################################################################

# Get Data ----------------------------------------------------------------
#
# Reading CSV file downloaded from Kaggle
raw_in_df <- as_tibble(read.csv(here::here(
    "data",
    "diabetes.csv")
    )
)
#

# Saving the raw data -----------------------------------------------------
#
saveRDS(
    raw_in_df,
    here::here(
        "data",
        "raw_in_df.RDS"
    )
)

# EDA ---------------------------------------------------------------------
#
# EDA using {skimr}
skim(raw_in_df)
#

# Renaming columns --------------------------------------------------------
#
# Renaming columns to clarify role
data_1_df <-
    raw_in_df %>%
    rename(
        "gravida"                = 	"Pregnancies",
        "glucose"                = 	"Glucose",
        "bloodpressure"          = 	"BloodPressure",
        "skinthickness"          = 	"SkinThickness",
        "insulin"                = 	"Insulin",
        "bmi"                    = 	"BMI",
        "diabetes_pedigree_fcn"	 = 	"DiabetesPedigreeFunction",
        "age"                    = 	"Age",
        "outcome"     	         = 	"Outcome"
    )
#
names(data_1_df)
#
# Munging -----------------------------------------------------------------
#
# Replace '0' with NA in columns 2 through 7 because '0' means nothing here.
# (E.g., blood pressure of 0, BMI of 0, etc.)
data_na_df <-
    data_1_df %>%
    mutate(across(c(2:7), ~na_if(., 0)))
# Checking missingness
missmap(data_na_df)
# Overall missingness of 9%, spread across six of nine columns. 

# Maybe an ID variable will help identify rows for working with missing data
pima_d0_df <-
    data_na_df %>%
    mutate(id = 1:n(), 
           .before = everything()
    )
glimpse(pima_d_df)

# For some classification algorithms, the response variable must be a factor
# (e.g., Random Forests)
# Recoding values of "outcome" and changing it to a factor
pima_d_df <-
    pima_d0_df %>%
    mutate(
        outcome_cat = case_match(
            outcome,
            0 ~ "FALSE",
            1 ~ "TRUE"
        )
    ) %>%
    mutate(across(outcome_cat, as_factor)) %>%
    as_tibble()

skim(pima_d_df)


################################################################################

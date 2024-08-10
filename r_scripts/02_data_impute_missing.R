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
# Imputing using the {mice} package
set.seed(123)
mice_mod_df <- 
    data_na_df %>%
    select(c(2:7)) %>%
    mice(., method = 'rf')
mice_complete_df <- complete(mice_mod_df)
# Transfer the predicted missing values into the main data set
data_df <-
    data_na_df %>%
    mutate(
        sjmisc::replace_columns(.,
                                mice_complete_df,
                                add.unique = FALSE)
    )
missmap(data_df)
# Recoding values of "outcome" and changing it to a factor
data_df <-
    data_df %>%
    mutate(
        outcome = case_match(
            outcome,
            0 ~ "FALSE",
            1 ~ "TRUE"
        )
    ) %>%
    mutate(across(outcome, as_factor))
skim(data_df)
#

# Data viz to see distros by outcome class

# Viz 1 Outcome by Gravida
data_df %>%
    ggplot(aes(gravida, 
               colour = outcome,
               fill   = outcome)) +
    geom_histogram(binwidth = 1) +
    scale_color_manual(values = uagc_mini_3) +
    scale_fill_manual(values = uagc_mini_3) +
    labs(title = "Gravida by Outcome") +
    theme_minimal()
#
# Viz 2 Outcome by Glucose
data_df %>%
    ggplot(aes(glucose, 
               colour = outcome,
               fill   = outcome)) +
    geom_histogram(binwidth = 5) +
    scale_color_manual(values = uagc_mini_3) +
    scale_fill_manual(values = uagc_mini_3) +
    labs(title = "Two Plasma Glucose (mg/dl) Distribution after Oral Ingestion by Outcome") +
    theme_minimal()
#
## Viz 3 Outcome by Blood Pressure
data_df %>%
    ggplot(aes(bloodpressure, 
               colour = outcome,
               fill   = outcome)) +
    geom_histogram(binwidth = 5) +
    scale_color_manual(values = uagc_mini_3) +
    scale_fill_manual(values = uagc_mini_3) +
    labs(title = "Diastolic Blood Pressure (mm Hg) by Outcome") +
    theme_minimal()
#
# Viz 4 Outcome by Skin Thickness
data_df %>%
    ggplot(aes(skinthickness, 
               colour = outcome,
               fill   = outcome)) +
    geom_histogram(binwidth = 1) +
    scale_color_manual(values = uagc_mini_3) +
    scale_fill_manual(values = uagc_mini_3) +
    labs(title = "Skin Thickness (mm) by Outcome") +
    theme_minimal()
#
# Viz 5 Outcome by Insulin
data_df %>%
    ggplot(aes(insulin, 
               colour = outcome,
               fill   = outcome)) +
    geom_histogram(binwidth = 15) +
    scale_color_manual(values = uagc_mini_3) +
    scale_fill_manual(values = uagc_mini_3) +
    labs(title = "Two-Hour Serum Insulin (muU/ml) by Outcome") +
    theme_minimal()
#
# Viz 6 Outcome by BMI
data_df %>%
    ggplot(aes(bmi, 
               colour = outcome,
               fill   = outcome)) +
    geom_histogram(binwidth = 1) +
    scale_color_manual(values = uagc_mini_3) +
    scale_fill_manual(values = uagc_mini_3) +
    labs(title = "BMI (weight in kg/(height in m)^2) by Outcome") +
    theme_minimal()
#
# Viz 7 Outcome by Diabetes Pedigree Function
data_df %>%
    ggplot(aes(diabetes_pedigree_fcn, 
               colour = outcome,
               fill   = outcome)) +
    geom_histogram(binwidth = 0.1) +
    scale_color_manual(values = uagc_mini_3) +
    scale_fill_manual(values = uagc_mini_3) +
    labs(title = "Diabetes Pedigree Function by Outcome") +
    theme_minimal()
#
# Viz 8 Outcome by Age
data_df %>%
    ggplot(aes(age, 
               colour = outcome,
               fill   = outcome)) +
    geom_histogram(binwidth = 1) +
    scale_color_manual(values = uagc_mini_3) +
    scale_fill_manual(values = uagc_mini_3) +
    labs(title = "Age (years) by Outcome") +
    theme_minimal()
# 

# Data viz to see correlations by outcome class
# Viz 9
ggpairs(data_df)















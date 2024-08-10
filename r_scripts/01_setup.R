###################### Use of Naive Bayes Classifier ###########################

######################## Problem Statement ######################## 
# 
# Build a machine learning model to predict whether or not a 
# person has diabetes.
# 
# The "diabetes" dataset from Kaggle is used, with the human 
# subjects being the Pima of the US desert Southwest.
# 

# Options and Packages ----------------------------------------------------
#
# Global options
options("scipen" = 100, "digits" = 4) # override R's tendency to use
#                                       scientific notation
# Loading libraries
librarian::shelf( # 
    tidymodels, # an omnibus package for modeling
    tidyverse,  # an omnibus package for data wrangling
    ggthemes,   # plot modification
    discrim,    # discriminant analysis (needed for naive Bayes)
    xgboost,    
    Amelia,     # missing data (missingness map)
    GGally,     # ggplot extension (ggpairs)
    ranger,     # tidymodels random forest engine
    skimr,      # efficient EDA
    here,       # directory navigation
    kknn,       # weighted k-Nearest Neighbors
    mice,       # multiple imputation with chained equations
)
#

# Helper Functions --------------------------------------------------------
#
# We might get some columns of class integer64, which some packages find
# difficult to handle. We'll define a function to identify columns of this
# class so we can convert it to integer32
is.integer64 <- function(x) {
    class(x) == "integer64"
}
#

# Custom color palette for charts
uagc_colors <-
    c(
        "#0C234B",
        "#AB0520",
        "#007D8A",
        "#98A4A3",
        "#621244",
        "#EF9600",
        "#EFF1F1",
        "#81D3EB",
        "#F9E17D",
        "#0076A8",
        "#D0D0CE",
        "#495A53",
        "#D6D2C4",
        "#E2E9EB",
        "#0C234B",
        "#F1F5F6"
    )
#
# Mini-palette
uagc_mini <-
    c(
        "#E2E9EB",
        "#98A4A3"
    )
uagc_mini_2 <-
    c("#007D8A",
      "#621244",
      "#F9E17D")
uagc_mini_3 <-
    c("#81D3EB",
      "#F9E17D",
      "#0076A8",
      "#D0D0CE")
#
#####
# Defining a 'not in' function for lists
`%!in%` <- Negate(`%in%`)
#
#####
# function for custom model metrics
get_metrics <- function(){
  
  # Create an empty dataframe with column names
  measure_df <- 
    data.frame(
      measure = character(), 
      value   = numeric()
    )
  for (i in 1:10) {
    # Data to add
    data_to_add_df <- 
      x_fit_rs$.metrics[[i]][, c(1,3)] %>%
      pivot_wider(names_from = .metric,
                  values_from = .estimate)
    # Add to df
    measure_df <-
      rbind(measure_df, data_to_add_df)
  }
  # Add fold ID
  measure_df <-
    measure_df %>%
    mutate(cv_fold = 1:n(), 
           .before = everything()
    )
  
  
  return(measure_df)
}
#

## !! Only run if device is returning from log off/standby
## Here are a couple of commands to ensure gtsave runs after the computer
## goes into standby (otherwise, R will throw: 'Error in s$close() : attempt 
## to apply non-function')
# f <- chromote::default_chromote_object() #get the f object
# f$close()
#

################################################################################

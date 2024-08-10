###########################################################################
#
#                       Model Recipies and Workflows 
#
###########################################################################


# Data splitting ----------------------------------------------------------

# Main data
set.seed(123)
#
pima_d_split <- initial_split(
    pima_d_df,
    prop = 0.75,
    strata = outcome
)

# Create splits
pima_d_tr_df <- training(pima_d_split)
pima_d_te_df <- testing(pima_d_split)
 

# Preprocessing -----------------------------------------------------------

# Recipe - just to look at what preprocessing does to the data
# Updating roles and imputing
pima_d_rec_prep <-
    recipe(
        outcome ~ ., 
        data = pima_d_tr_df ) %>%
    update_role(id, outcome_cat, new_role = "ID") %>% 
    step_corr(all_numeric_predictors(), threshold = .8) %>% 
    step_impute_bag(all_predictors()) %>% 
    step_normalize(all_predictors()) %>%
    prep()

# Print a summary of the recipe steps to be performed
tidy(pima_d_rec_prep)

# Baking the recipe
pima_d_baked <- 
    recipe(
        outcome ~ ., 
        data = pima_d_tr_df ) %>%
    update_role(id, new_role = "ID") %>% 
    step_corr(all_numeric_predictors(), threshold = .8) %>% 
    step_impute_bag(all_numeric_predictors()) %>% 
    step_normalize(all_numeric_predictors()) %>%
    prep() %>%
    bake(new_data = NULL)

pima_d_baked

########### On to the actual recipes ###########

pima_d_rec <-
    recipe(
        outcome ~ ., 
        data = pima_d_tr_df ) %>%
    update_role(id, outcome_cat, new_role = "ID") %>% 
    step_corr(all_numeric_predictors(), threshold = .8) %>% 
    step_impute_bag(all_numeric_predictors()) %>% 
    step_normalize(all_numeric_predictors()) 

pima_dcat_rec <-
    recipe(
        outcome_cat ~ ., 
        data = pima_d_tr_df ) %>%
    update_role(id, outcome, new_role = "ID") %>% 
    step_corr(all_numeric_predictors(), threshold = .8) %>% 
    step_impute_bag(all_numeric_predictors()) %>% 
    step_normalize(all_numeric_predictors()) 


# Resampling --------------------------------------------------------------

# Setting up an evaluation tool
set.seed(234)
folds <- vfold_cv(pima_d_tr_df)
folds

# Algorithms --------------------------------------------------------------

# Six algorithms (model specifications)
set.seed(456)
#  Gaussian Naive Bayes
gnb_spec <-
    naive_Bayes() %>%
    set_mode("classification") %>%
    set_engine("klaR") %>%
    set_args(usekernel = FALSE)

#  Random Forest
rf_spec <-
    rand_forest(mode = "classification") %>%
    set_engine("ranger") 

#  K-Nearest Neighbor
knn_spec <-
    nearest_neighbor(
        neighbors   = 5,
        weight_func = "triangular") %>%
    set_mode("classification") %>%
    set_engine("kknn")

#  Logistic Regression
log_spec <-
    logistic_reg() %>%
    set_engine("glm") %>%
    set_mode("classification")

#  XGBoost
xgb_spec <-
    boost_tree() %>%
    set_engine("xgboost") %>%
    set_mode("classification")

#  Radial Basis Function Support Vector Machines
svm_spec <-
    svm_rbf(
        mode      = "classification",
        rbf_sigma = 0.2) 

# Workflows ---------------------------------------------------------------

#  Gaussian Naive Bayes
gnb_wflow <-
    workflow() %>%
    add_model(gnb_spec) %>%
    add_recipe(pima_dcat_rec) 

gnb_wflow_rs <-
    workflow() %>%
    add_model(gnb_spec) %>%
    add_recipe(pima_dcat_rec) %>%
    fit_resamples(
        resamples = folds,
        metrics = metric_set(
            f_meas, kap, 
            mcc, roc_auc
        ),
        control = control_resamples(
            save_pred = TRUE
        )
    )

#  Random Forest
rf_wflow <-
    workflow() %>%
    add_model(rf_spec) %>%
    add_recipe(pima_dcat_rec)

rf_wflow_rs <-
    workflow() %>%
    add_model(rf_spec) %>%
    add_recipe(pima_dcat_rec) %>%
    fit_resamples(
        resamples = folds,
        metrics = metric_set(
            f_meas, kap, 
            mcc, roc_auc
        ),
        control = control_resamples(
            save_pred = TRUE
        )
    )

#  K-Nearest Neighbor
knn_wflow <-
    workflow() %>%
    add_model(knn_spec) %>%
    add_recipe(pima_dcat_rec)

knn_wflow_rs <-
    workflow() %>%
    add_model(knn_spec) %>%
    add_recipe(pima_dcat_rec) %>%
    fit_resamples(
        resamples = folds,
        metrics = metric_set(
            f_meas, kap, 
            mcc, roc_auc
        ),
        control = control_resamples(
            save_pred = TRUE
        )
    )

#  Logistic Regression
log_wflow <-
    workflow() %>%
    add_model(log_spec) %>%
    add_recipe(pima_dcat_rec)

log_wflow_rs <-
    workflow() %>%
    add_model(log_spec) %>%
    add_recipe(pima_dcat_rec) %>%
    fit_resamples(
        resamples = folds,
        metrics = metric_set(
            f_meas, kap, 
            mcc, roc_auc
        ),
        control = control_resamples(
            save_pred = TRUE
        )
    )

#  XGBoost
xgb_wflow <-
    workflow() %>%
    add_model(xgb_spec) %>%
    add_recipe(pima_dcat_rec)

xgb_wflow_rs <-
    workflow() %>%
    add_model(xgb_spec) %>%
    add_recipe(pima_dcat_rec) %>%
    fit_resamples(
        resamples = folds,
        metrics = metric_set(
            f_meas, kap, 
            mcc, roc_auc
        ),
        control = control_resamples(
            save_pred = TRUE
        )
    )

#  Radial Basis Function Support Vector Machines
svm_wflow <-
    workflow() %>%
    add_model(svm_spec) %>%
    add_recipe(pima_dcat_rec)

svm_wflow_rs <-
    workflow() %>%
    add_model(svm_spec) %>%
    add_recipe(pima_dcat_rec) %>%
    fit_resamples(
        resamples = folds,
        metrics = metric_set(
            f_meas, kap, 
            mcc, roc_auc
        ),
        control = control_resamples(
            save_pred = TRUE
        )
    )


# Fitting the models ------------------------------------------------------

# Gaussian Naive Bayes
gnb_fit <- 
    gnb_wflow %>% 
    fit(data = pima_d_tr_df)

x_fit_rs <- 
    gnb_wflow_rs 
gnb_metrics_df <- 
    get_metrics()
gnb_metrics_df

# Random Forest    
rf_fit <- 
    rf_wflow %>% 
    fit(data = pima_d_tr_df)

x_fit_rs <- 
    rf_wflow_rs
rf_metrics_df <- 
    get_metrics()
rf_metrics_df

# k-Nearest Neighbor
knn_fit <- 
    knn_wflow %>% 
    fit(data = pima_d_tr_df)

x_fit_rs <- 
    knn_wflow_rs
knn_metrics_df <- 
    get_metrics()
knn_metrics_df

# Logistic Regression
log_fit <- 
    log_wflow %>% 
    fit(data = pima_d_tr_df)

x_fit_rs <-
    log_wflow_rs
log_metrics_df <- 
    get_metrics()
log_metrics_df

# XGBoost
xgb_fit <- 
    xgb_wflow %>% 
    fit(data = pima_d_tr_df)

x_fit_rs <-
    xgb_wflow_rs
xgb_metrics_df <- 
    get_metrics()
xgb_metrics_df

# Support Vector Machines (radial)
svm_fit <- 
    svm_wflow %>% 
    fit(data = pima_d_tr_df)

x_fit_rs <- 
    svm_wflow_rs
svm_metrics_df <- 
    get_metrics()
svm_metrics_df


# Model evaluation --------------------------------------------------------

# predictions with test data
































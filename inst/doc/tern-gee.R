## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(tern.gee)
fev_data$FEV1_BINARY <- as.integer(fev_data$FEV1 > 30)
head(fev_data)

## -----------------------------------------------------------------------------
fev_fit <- fit_gee(
  vars = list(
    response = "FEV1_BINARY",
    covariates = c("RACE", "SEX", "FEV1_BL"),
    arm = "ARMCD",
    id = "USUBJID",
    visit = "AVISIT"
  ),
  data = fev_data
)
fev_fit

## -----------------------------------------------------------------------------
fev_lsmeans <- lsmeans(fev_fit, data = fev_data)
fev_lsmeans

## -----------------------------------------------------------------------------
fev_counts <- fev_data %>%
  dplyr::select(USUBJID, ARMCD) %>%
  unique()

fev_gee_table <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARMCD", ref_group = "PBO") %>%
  summarize_gee_logistic() %>%
  build_table(fev_lsmeans, alt_counts_df = fev_counts)

fev_gee_table


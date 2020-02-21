
data("anesthesia")

anesthesia <- as.matrix(anesthesia)
pooled_anesthesia <- anesthesia[, c(1, 3)]

ds_fit <- rater(long_data(anesthesia), dawid_skene(),
                iter = 200, chains = 1)

hds_fit <- rater(long_data(anesthesia), hier_dawid_skene(),
                 iter = 200, chains = 1)

ccds_fit <- rater(long_data(anesthesia), class_conditional_dawid_skene(),
                  iter = 200, chains = 1)

ds_fit_optim <- rater(long_data(anesthesia), dawid_skene(), method = "optim")
hds_fit_optim <- rater(long_data(anesthesia), hier_dawid_skene(),
                       method = "optim")
ccds_fit_optim <- rater(long_data(anesthesia), class_conditional_dawid_skene(),
                        method = "optim")

ds_fit_table <- rater(table_data(caries), dawid_skene(), iter = 200, chains = 1)

ds_model <- dawid_skene()
hds_model <- hier_dawid_skene()
ccds_model <- class_conditional_dawid_skene()

J <- 5
I <- 45
K <- 4

J_caries <- 5
I_caries <- 3859
K_caries <- 2

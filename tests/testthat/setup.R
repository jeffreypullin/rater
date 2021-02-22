data("anesthesia")

ds_fit <- rater(anesthesia, dawid_skene(), iter = 200, chains = 1,
                verbose = FALSE)
hds_fit <- rater(anesthesia, hier_dawid_skene(), iter = 200, chains = 1,
                 verbose = FALSE)
ccds_fit <- rater(anesthesia, class_conditional_dawid_skene(), iter = 200,
                  chains = 1, verbose = FALSE, seed = 42)

ds_fit_optim <- rater(anesthesia, dawid_skene(), method = "optim")
hds_fit_optim <- rater(anesthesia, hier_dawid_skene(), method = "optim")
ccds_fit_optim <- rater(anesthesia, class_conditional_dawid_skene(),
                        method = "optim")

ds_fit_grouped <- rater(caries, dawid_skene(), data_format = "grouped",
                        iter = 200, chains = 1, verbose = FALSE)

ds_fit_grouped_optim <- rater(caries, dawid_skene(), method = "optim",
                              data_format = "grouped")

ds_model <- dawid_skene()
hds_model <- hier_dawid_skene()
ccds_model <- class_conditional_dawid_skene()

J <- 5
I <- 45
K <- 4

J_caries <- 5
I_caries <- 3859
K_caries <- 2

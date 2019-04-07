
data("anesthesia")

anesthesia <- as.matrix(anesthesia)
pooled_anesthesia <- anesthesia[, c(1, 3)]
# Note: tests of sampler validity will require proper samples

ds_fit <- mcmc(long_data(anesthesia), dawid_skene(), iter = 200, chains = 1)
multi_fit <- mcmc(multinomial_data(pooled_anesthesia), multinomial(), iter = 200, chains = 1)
hds_fit <- mcmc(long_data(anesthesia), hier_dawid_skene(), iter = 200, chains = 1)

ds_fit_optim <- optim(long_data(anesthesia), dawid_skene())
multi_fit_optim <- optim(multinomial_data(pooled_anesthesia), multinomial())
hds_fit_optim <- optim(long_data(anesthesia), hier_dawid_skene())

ds_fit_table <- mcmc(table_data(caries), dawid_skene(), iter = 200, chains = 1)

ds_model <- dawid_skene()
multi_model <- multinomial()
hds_model <- hier_dawid_skene()

J <- 5
I <- 45
K <- 4

J_caries <- 5
I_caries <- 3859
K_caries <- 2

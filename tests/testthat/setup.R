
data("anesthesia")

anesthesia <- anesthesia
pooled_anesthesia <- anesthesia[c(1, 3)]

# Note: tests of sampler validity will require proper samples

ds_fit <- mcmc(anesthesia, dawid_skene(), iter = 200, chains = 1)
multi_fit <- mcmc(pooled_anesthesia, multinomial(), iter = 200, chains = 1)
hds_fit <- mcmc(anesthesia, hier_dawid_skene(), iter = 200, chains = 1)

ds_model <- dawid_skene()
multi_model <- multinomial()
hds_model <- hier_dawid_skene()

J <- 5
I <- 45
K <- 4

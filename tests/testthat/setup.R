
data("anesthesia")

anesthesia <- anesthesia
pooled_anesthesia <- anesthesia[c(1, 3)]

ds_fit <- mcmc(anesthesia, dawid_skene())
multi_fit <- mcmc(pooled_anesthesia, multinomial())
hds_fit <- mcmc(anesthesia, hier_dawid_skene())

J <- 5
I <- 45
K <- 4

# TODO add models

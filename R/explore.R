## Load and look at the data
##
## https://github.com/pbiecek/PISA2012lite


# if (!require(devtools)) {
#   install.packages("devtools")
#   require(devtools)
# }
# install_github("pbiecek/PISA2012lite")

library(PISA2012lite)
library(data.table)
library(magrittr)
library(ggplot2)
library(cowplot)
theme_set(theme_grey())


data("student2012")
setDT(student2012)

escs_unweighted <- student2012[OECD == "OECD" & !is.na(ESCS), .(ESCS)] %>%
  ggplot(aes(x = ESCS)) +
  geom_density(colour = "red", linetype = "dotted") +
  geom_vline(xintercept = student2012[OECD == "OECD" & !is.na(ESCS), mean(ESCS)], colour = "red", linetype = "dotted") +
  labs(title = "Unweighted")

escs_weighted <- student2012[OECD == "OECD" & !is.na(ESCS), .(ESCS, w = W_FSTUWT / sum(W_FSTUWT))] %>%
  ggplot(aes(x = ESCS, weight = w)) +
  geom_density() +
  geom_vline(xintercept = student2012[OECD == "OECD" & !is.na(ESCS), .(ESCS, w = W_FSTUWT / sum(W_FSTUWT))][, sum(ESCS * w) / sum(w)],
             linetype = "dashed") +
  labs(title = "Weighted")

escs_overlaid <- ggplot() +
  geom_density(data = student2012[OECD == "OECD" & !is.na(ESCS), .(ESCS)],
               aes(ESCS), colour = "red", linetype = "dotted") +
  geom_density(data = student2012[OECD == "OECD" & !is.na(ESCS), .(ESCS, w = W_FSTUWT / sum(W_FSTUWT))],
               aes(x = ESCS, weight = w)) +
  geom_vline(xintercept = student2012[OECD == "OECD" & !is.na(ESCS), mean(ESCS)],
             colour = "red", linetype = "dotted") +
  geom_vline(xintercept = student2012[OECD == "OECD" & !is.na(ESCS),
                                      .(ESCS, w = W_FSTUWT / sum(W_FSTUWT))][, sum(ESCS * w) / sum(w)],
             linetype = "dashed") +
  labs(title = "Overlaid")

plot_grid(escs_unweighted, escs_weighted, escs_overlaid)

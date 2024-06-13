
# https://github.com/susanathey/MCPanel
# also see examples in the folder tests/
# devtools::install_github("susanathey/MCPanel")

rm(list = ls())

library(MCPanel)

if (FALSE) {
    # Try the functions
    estimated_obj <- mcnnm_cv(M, mask, to_estimate_u = 0, to_estimate_v = 0, num_lam_L = 40)

    best_lam_L <- estimated_obj$best_lambda
    estimated_mat <- estimated_obj$L

    ?mcnnm_cv
    mcnnm_cv(M = replicate(5,rnorm(5)), mask = matrix(rbinom(5*5,1,0.8), 5, 5))
}

# Data ----
library(dplyr)
library(tidyr)
library(ranger)
library(ggplot2)
library(latex2exp)



leg <- readRDS("./dataderived/USA_legalgrouped.rds")
USday <- readRDS("./dataderived/USA_daily_daymet_fars.rds")
USreg <- read.csv("./dataraw/USA_statestats/mv1_combined.csv", strip.white = TRUE) %>%
    mutate(RegisteredVehicles = as.integer(RegisteredVehicles))
USday <- USreg %>%
    right_join(USday, by = c("State", "Year")) %>%
    mutate(Nacc1m = 10^6 * Nacc / RegisteredVehicles,
           Fatals1m = 10^6 * Fatals / RegisteredVehicles,
           NFatAcc1m = 10^6 * NFatAcc / RegisteredVehicles)

leg1 <- leg %>% filter(Legal == 1)
USday$Legal <- FALSE
for (i in 1:nrow(leg1)) {
    legindex <- (USday$Date >= leg1$DateSalesEffect[i]) & (USday$State == leg1$State[i])
    USday$Legal[legindex] <- TRUE
}

# aggregate within state
USdaystate <- USday %>%
    group_by(State, Date, Legal, Year, Month, Day, Weekday, Weekend, Holiday) %>%
    summarise(sunriseH = mean(sunriseH),
              sunsetH = mean(sunsetH),
              DayLength = mean(DayLength),
              Precip = mean(Precip, na.rm = TRUE),
              Tmax = mean(Tmax, na.rm = TRUE),
              Tmin = mean(Tmin, na.rm = TRUE),
              Temp = mean(Temp, na.rm = TRUE),
              Nacc1m = sum(Nacc1m),
              Fatals1m = sum(Fatals1m),
              NFatAcc1m = sum(NFatAcc1m)
    ) %>%
    mutate(Trange = Tmax - Tmin)

STATES_psm <- list(c("CA", "WV"), c("MA", "UT"), c("NV", "NY"))
STATES_man <- list(c("CA", "NJ"), c("MA", "FL"), c("NV", "UT"))
Nacc_dates_psm <- data.frame(starts = as.Date(c("2017-01-01", NA, "2016-07-01")),
                             ends = as.Date(c("2019-01-31", NA, "2018-07-31")))
Nacc_dates_man <- data.frame(starts = as.Date(c("2017-01-01", "2017-11-01", NA)),
                             ends = as.Date(c("2019-01-31", "2019-11-30", NA)))

D <- USdaystate %>%
    filter(is.element(State, c(unlist(STATES_psm), unlist(STATES_man)))) %>%
    mutate(#Legal = as.factor(Legal),
        LegalState = interaction(State, Legal, drop = TRUE),
        Month = as.factor(Month),
        Weekday = as.factor(Weekday),
        Weekend = as.factor(Weekend),
        Holiday = as.factor(Holiday))

predictors <- c("Legal", #"LegalState",
                # "Airport_Code",
                # "lat", "lon",
                "State", #"DayLength",
                "Month", #"Year",
                "Weekday", #"Weekend",
                "Holiday",
                "Trange",
                "Precip", "Temp")


# NFatAcc1m ----
RESPONSE <- "NFatAcc1m"

DATAnoNA <- D %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.element(State, c(unlist(STATES_psm), unlist(STATES_man)))) %>%
    dplyr::select(all_of(c(predictors, RESPONSE, "Date"))) %>%
    na.omit()
attributes(DATAnoNA)$na.action <- NULL

Y <- DATAnoNA %>%
    select(all_of(c(RESPONSE, "State", "Date", "Month", "Weekday", "Holiday"))) %>%
    pivot_wider(values_from = NFatAcc1m, #select(all_of(RESPONSE)),
                names_from = State) #%>%
Ymat <- Y %>%
    select(-all_of(c("Month", "Weekday", "Holiday", "Date"))) %>%
    t()
Ytreat <- DATAnoNA %>%
    select(all_of(c("Legal", "State", "Date"))) %>%
    pivot_wider(values_from = Legal,
                names_from = State) %>%
    as.data.frame()
rownames(Ytreat) <- Ytreat$Date
Ytreat <- Ytreat %>%
    select(-Date) %>%
    t()

# RESULTS FOR THE TABLE ----
# see the comment from the package example on stocks.R, line 62:
## If N<<T it is better to only estimate u, if T<<<N it is better to only estimate v.
# Here we have  N<<T.
set.seed(1)
est_model_MCPanel <- mcnnm_cv(M = Ymat[STATES_psm[[1]],],
                              mask = Ytreat[STATES_psm[[1]],],
                              to_estimate_u = 1, to_estimate_v = 0)
Mhat <- est_model_MCPanel$L + replicate(ncol(Ytreat[STATES_psm[[1]],]), est_model_MCPanel$u) +
    t(replicate(nrow(Ytreat[STATES_psm[[1]],]), est_model_MCPanel$v))
effect0 <- (Mhat - Ymat[STATES_psm[[1]],])[Ytreat[STATES_psm[[1]],]]
t.test(effect0)
# data:  effect0
# t = 2.1526e-13, df = 729, p-value = 1
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#     -2.123877e-05  2.123877e-05
# sample estimates:
#     mean of x
# 2.328797e-18
forecast::auto.arima(effect0) # mean 0

set.seed(2)
est_model_MCPanel <- mcnnm_cv(M = Ymat[STATES_psm[[2]],],
                              mask = Ytreat[STATES_psm[[2]],],
                              to_estimate_u = 1, to_estimate_v = 0)
Mhat <- est_model_MCPanel$L + replicate(ncol(Ytreat[STATES_psm[[2]],]), est_model_MCPanel$u) +
    t(replicate(nrow(Ytreat[STATES_psm[[2]],]), est_model_MCPanel$v))
effect0 <- (Mhat - Ymat[STATES_psm[[2]],])[Ytreat[STATES_psm[[2]],]]
t.test(effect0)
# data:  effect0
# t = -4.372e-14, df = 406, p-value = 1
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#     -0.001232633  0.001232633
# sample estimates:
#     mean of x
# -2.741351e-17
forecast::auto.arima(effect0) # mean 0

set.seed(3)
est_model_MCPanel <- mcnnm_cv(M = Ymat[STATES_psm[[3]],],
                              mask = Ytreat[STATES_psm[[3]],],
                              to_estimate_u = 1, to_estimate_v = 0)
Mhat <- est_model_MCPanel$L + replicate(ncol(Ytreat[STATES_psm[[3]],]), est_model_MCPanel$u) +
    t(replicate(nrow(Ytreat[STATES_psm[[3]],]), est_model_MCPanel$v))
effect0 <- (Mhat - Ymat[STATES_psm[[3]],])[Ytreat[STATES_psm[[3]],]]
t.test(effect0)
# data:  effect0
# t = -3.7653e-14, df = 913, p-value = 1
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#     -0.0006900809  0.0006900809
# sample estimates:
#     mean of x
# -1.323953e-17
forecast::auto.arima(effect0) # mean 0

set.seed(4)
tmp <- sort(unique(unlist(STATES_psm)))
est_model_MCPanel <- mcnnm_cv(M = Ymat[tmp,],
                              mask = Ytreat[tmp,],
                              to_estimate_u = 1, to_estimate_v = 0)
Mhat <- est_model_MCPanel$L + replicate(ncol(Ytreat[tmp,]), est_model_MCPanel$u) +
    t(replicate(nrow(Ytreat[tmp,]), est_model_MCPanel$v))
effect0 <- (Mhat - Ymat[tmp,])[Ytreat[tmp,]]
t.test(effect0)
# data:  effect0
# t = 2.3315e-14, df = 2050, p-value = 1
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#     -0.01143618  0.01143618
# sample estimates:
#     mean of x
# 1.35958e-16
forecast::auto.arima(effect0) # mean 0

set.seed(1)
est_model_MCPanel <- mcnnm_cv(M = Ymat[STATES_man[[1]],],
                              mask = Ytreat[STATES_man[[1]],],
                              to_estimate_u = 1, to_estimate_v = 0)
Mhat <- est_model_MCPanel$L + replicate(ncol(Ytreat[STATES_man[[1]],]), est_model_MCPanel$u) +
    t(replicate(nrow(Ytreat[STATES_man[[1]],]), est_model_MCPanel$v))
effect0 <- (Mhat - Ymat[STATES_man[[1]],])[Ytreat[STATES_man[[1]],]]
t.test(effect0)
# data:  effect0
# t = 2.1526e-13, df = 729, p-value = 1
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#     -2.123877e-05  2.123877e-05
# sample estimates:
#     mean of x
# 2.328797e-18
forecast::auto.arima(effect0) # mean 0

set.seed(2)
est_model_MCPanel <- mcnnm_cv(M = Ymat[STATES_man[[2]],],
                              mask = Ytreat[STATES_man[[2]],],
                              to_estimate_u = 1, to_estimate_v = 0)
Mhat <- est_model_MCPanel$L + replicate(ncol(Ytreat[STATES_man[[2]],]), est_model_MCPanel$u) +
    t(replicate(nrow(Ytreat[STATES_man[[2]],]), est_model_MCPanel$v))
effect0 <- (Mhat - Ymat[STATES_man[[2]],])[Ytreat[STATES_man[[2]],]]
t.test(effect0)
# data:  effect0
# t = -4.372e-14, df = 406, p-value = 1
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#     -0.001232633  0.001232633
# sample estimates:
#     mean of x
# -2.741351e-17
forecast::auto.arima(effect0) # mean 0

set.seed(3)
est_model_MCPanel <- mcnnm_cv(M = Ymat[STATES_man[[3]],],
                              mask = Ytreat[STATES_man[[3]],],
                              to_estimate_u = 1, to_estimate_v = 0)
Mhat <- est_model_MCPanel$L + replicate(ncol(Ytreat[STATES_man[[3]],]), est_model_MCPanel$u) +
    t(replicate(nrow(Ytreat[STATES_man[[3]],]), est_model_MCPanel$v))
effect0 <- (Mhat - Ymat[STATES_man[[3]],])[Ytreat[STATES_man[[3]],]]
t.test(effect0)
# data:  effect0
# t = -3.7653e-14, df = 913, p-value = 1
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#     -0.0006900809  0.0006900809
# sample estimates:
#     mean of x
# -1.323953e-17
forecast::auto.arima(effect0) # mean 0

set.seed(4)
tmp <- sort(unique(unlist(STATES_man)))
est_model_MCPanel <- mcnnm_cv(M = Ymat[tmp,],
                              mask = Ytreat[tmp,],
                              to_estimate_u = 1, to_estimate_v = 0)
Mhat <- est_model_MCPanel$L + replicate(ncol(Ytreat[tmp,]), est_model_MCPanel$u) +
    t(replicate(nrow(Ytreat[tmp,]), est_model_MCPanel$v))
effect0 <- (Mhat - Ymat[tmp,])[Ytreat[tmp,]]
t.test(effect0)
# data:  effect0
# t = -0.00014123, df = 2050, p-value = 0.9999
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#     -0.005367769  0.005366996
# sample estimates:
#     mean of x
# -3.865379e-07
forecast::auto.arima(effect0) # mean 0



# Fatals1m ----
RESPONSE <- "Fatals1m"

DATAnoNA <- D %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.element(State, c(unlist(STATES_psm), unlist(STATES_man)))) %>%
    dplyr::select(all_of(c(predictors, RESPONSE, "Date"))) %>%
    na.omit()
attributes(DATAnoNA)$na.action <- NULL

Y <- DATAnoNA %>%
    select(all_of(c(RESPONSE, "State", "Date", "Month", "Weekday", "Holiday"))) %>%
    pivot_wider(values_from = Fatals1m, #select(all_of(RESPONSE)),
                names_from = State) #%>%
# select(-Date) %>%
# t()
# Time covariates
Z <- Y %>%
    select(all_of(c("Month", "Weekday", "Holiday"))) %>%
    mutate(Holiday = as.numeric(Holiday)) %>%
    mutate(Month2sin = sin(2 * pi* as.numeric(Month) / 12),
           Month2cos = cos(2 * pi* as.numeric(Month) / 12),
           Weekday2sin = sin(2 * pi* as.numeric(Weekday) / 7),
           Weekday2cos = cos(2 * pi* as.numeric(Weekday) / 7)) %>%
    select(-Month, -Weekday)
Zmat <- as.matrix(Z) #apply(Z, 2, function(x) as.numeric(x))
Ymat <- Y %>%
    select(-all_of(c("Month", "Weekday", "Holiday", "Date"))) %>%
    t()

Ytreat <- DATAnoNA %>%
    select(all_of(c("Legal", "State", "Date"))) %>%
    pivot_wider(values_from = Legal,
                names_from = State) %>%
    as.data.frame()
rownames(Ytreat) <- Ytreat$Date
Ytreat <- Ytreat %>%
    select(-Date) %>%
    t()

# Unit-level covariates
XPrecip <- DATAnoNA %>%
    select(all_of(c("Precip", "State", "Date"))) %>%
    pivot_wider(values_from = Precip,
                names_from = State) %>%
    select(-Date) %>%
    t() %>%
    rowMeans()
XTemp <- DATAnoNA %>%
    select(all_of(c("Temp", "State", "Date"))) %>%
    pivot_wider(values_from = Temp, #select(all_of(RESPONSE)),
                names_from = State) %>%
    select(-Date) %>%
    t() %>%
    rowMeans()
Xmat <- cbind(XTemp, XPrecip)


set.seed(123)
est_model_MCPanel <- mcnnm_cv(M = Ymat,
                              mask = Ytreat,
                              to_estimate_u = 1, to_estimate_v = 1)

est_model_MCPanel_wc <- mcnnm_wc_cv(M = Ymat,
                                    X = matrix(0L,0,0), #Xmat,
                                    Z = Zmat,
                                    mask = Ytreat,
                                    to_estimate_u = 1, to_estimate_v = 1)

# est_model_MCPanel <- est_model_MCPanel_wc


Mhat <- est_model_MCPanel$L + replicate(ncol(Ytreat), est_model_MCPanel$u) + t(replicate(nrow(Ytreat), est_model_MCPanel$v))

# Combined effect
effect0 <- (Mhat - Ymat)[Ytreat]
summary(effect0)

# Effect by state
effect <- (Mhat - Ymat)
effs <- lapply(1:nrow(effect), function(x) effect[x, Ytreat[x, ]])
names(effs) <- rownames(Ymat)
sapply(effs[names(effs) %in% leg1$State], psych::describe)
sapply(effs[names(effs) %in% leg1$State], t.test)


# # just panel results ----------------
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -0.44574 -0.04615  0.00000  0.00000  0.05033  0.41929
# > sapply(effs[names(effs) %in% leg1$State], psych::describe)
# CA           MA           NV
# vars     1            1            1
# n        730          407          914
# mean     1.813838e-07 2.983627e-07 -2.872513e-07
# sd       0.07870163   0.1054592    0.07032538
# median   -0.01009024  0.00886925   0
# trimmed  -0.004878237 0.002266344  0.007254671
# mad      0.07709346   0.09143665   0.06102317
# min      -0.2023535   -0.4457417   -0.4192734
# max      0.4192895    0.3456796    0.177685
# range    0.621643     0.7914213    0.5969584
# skew     0.6594939    -0.2864321   -1.33495
# kurtosis 1.109864     0.929277     3.412156
# se       0.002912878  0.005227418  0.002326157
# > sapply(effs[names(effs) %in% leg1$State], t.test)
# CA                  MA                  NV
# statistic   6.226961e-05        5.70765e-05         -0.0001234875
# parameter   729                 406                 913
# p.value     0.9999503           0.9999545           0.9999015
# conf.int    numeric,2           numeric,2           numeric,2
# estimate    1.813838e-07        2.983627e-07        -2.872513e-07
# null.value  0                   0                   0
# stderr      0.002912878         0.005227418         0.002326157
# alternative "two.sided"         "two.sided"         "two.sided"
# method      "One Sample t-test" "One Sample t-test" "One Sample t-test"
# data.name   "X[[i]]"            "X[[i]]"            "X[[i]]"


# # weighted results after running the line above # est_model_MCPanel <- est_model_MCPanel_wc
# summary(effect0)
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max.
# -0.6289298 -0.0667939  0.0038672  0.0002368  0.0767421  0.6411367
# > sapply(effs[names(effs) %in% leg1$State], psych::describe)
# CA            MA          NV
# vars     1             1           1
# n        730           407         914
# mean     -6.143515e-06 0.001119361 3.794232e-05
# sd       0.1082745     0.1405982   0.1108357
# median   -0.01323102   0.008462807 0.01152353
# trimmed  -0.006905458  0.002512508 0.01168426
# mad      0.0991253     0.1161132   0.09547406
# min      -0.2697922    -0.572338   -0.6289298
# max      0.6411367     0.4970721   0.2606192
# range    0.9109289     1.06941     0.889549
# skew     0.7859601     -0.1439029  -1.324483
# kurtosis 1.649069      1.010247    3.224452
# se       0.004007419   0.006969193 0.003666118
# > sapply(effs[names(effs) %in% leg1$State], t.test)
# CA                  MA                  NV
# statistic   -0.001533036        0.1606156           0.01034946
# parameter   729                 406                 913
# p.value     0.9987772           0.872476            0.9917447
# conf.int    numeric,2           numeric,2           numeric,2
# estimate    -6.143515e-06       0.001119361         3.794232e-05
# null.value  0                   0                   0
# stderr      0.004007419         0.006969193         0.003666118
# alternative "two.sided"         "two.sided"         "two.sided"
# method      "One Sample t-test" "One Sample t-test" "One Sample t-test"
# data.name   "X[[i]]"            "X[[i]]"            "X[[i]]"


# RESULTS FOR THE TABLE ----
# see the comment from the package example on stocks.R, line 62:
## If N<<T it is better to only estimate u, if T<<<N it is better to only estimate v.
# Here we have  N<<T.
set.seed(1)
est_model_MCPanel <- mcnnm_cv(M = Ymat[STATES_psm[[1]],],
                              mask = Ytreat[STATES_psm[[1]],],
                              to_estimate_u = 1, to_estimate_v = 0)
Mhat <- est_model_MCPanel$L + replicate(ncol(Ytreat[STATES_psm[[1]],]), est_model_MCPanel$u) +
    t(replicate(nrow(Ytreat[STATES_psm[[1]],]), est_model_MCPanel$v))
effect0 <- (Mhat - Ymat[STATES_psm[[1]],])[Ytreat[STATES_psm[[1]],]]
t.test(effect0)
# data:  effect0
# t = -2.023e-13, df = 729, p-value = 1
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#     -0.0002283021  0.0002283021
# sample estimates:
#     mean of x
# -2.352543e-17
forecast::auto.arima(effect0) # mean 0

set.seed(2)
est_model_MCPanel <- mcnnm_cv(M = Ymat[STATES_psm[[2]],],
                              mask = Ytreat[STATES_psm[[2]],],
                              to_estimate_u = 1, to_estimate_v = 0)
Mhat <- est_model_MCPanel$L + replicate(ncol(Ytreat[STATES_psm[[2]],]), est_model_MCPanel$u) +
    t(replicate(nrow(Ytreat[STATES_psm[[2]],]), est_model_MCPanel$v))
effect0 <- (Mhat - Ymat[STATES_psm[[2]],])[Ytreat[STATES_psm[[2]],]]
t.test(effect0)
# data:  effect0
# t = 1.1632e-14, df = 406, p-value = 1
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#     -0.006152919  0.006152919
# sample estimates:
#     mean of x
# 3.640745e-17
forecast::auto.arima(effect0) # mean 0

set.seed(3)
est_model_MCPanel <- mcnnm_cv(M = Ymat[STATES_psm[[3]],],
                              mask = Ytreat[STATES_psm[[3]],],
                              to_estimate_u = 1, to_estimate_v = 0)
Mhat <- est_model_MCPanel$L + replicate(ncol(Ytreat[STATES_psm[[3]],]), est_model_MCPanel$u) +
    t(replicate(nrow(Ytreat[STATES_psm[[3]],]), est_model_MCPanel$v))
effect0 <- (Mhat - Ymat[STATES_psm[[3]],])[Ytreat[STATES_psm[[3]],]]
t.test(effect0)
# data:  effect0
# t = -2.6393e-14, df = 913, p-value = 1
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#     -0.003604054  0.003604054
# sample estimates:
#     mean of x
# -4.846763e-17
forecast::auto.arima(effect0) # mean 0

set.seed(4)
tmp <- sort(unique(unlist(STATES_psm)))
est_model_MCPanel <- mcnnm_cv(M = Ymat[tmp,],
                              mask = Ytreat[tmp,],
                              to_estimate_u = 1, to_estimate_v = 0)
Mhat <- est_model_MCPanel$L + replicate(ncol(Ytreat[tmp,]), est_model_MCPanel$u) +
    t(replicate(nrow(Ytreat[tmp,]), est_model_MCPanel$v))
effect0 <- (Mhat - Ymat[tmp,])[Ytreat[tmp,]]
t.test(effect0)
# data:  effect0
# t = 2.253e-14, df = 2050, p-value = 1
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#     -0.01277975  0.01277975
# sample estimates:
#     mean of x
# 1.468199e-16
forecast::auto.arima(effect0) # mean 0

set.seed(1)
est_model_MCPanel <- mcnnm_cv(M = Ymat[STATES_man[[1]],],
                              mask = Ytreat[STATES_man[[1]],],
                              to_estimate_u = 1, to_estimate_v = 0)
Mhat <- est_model_MCPanel$L + replicate(ncol(Ytreat[STATES_man[[1]],]), est_model_MCPanel$u) +
    t(replicate(nrow(Ytreat[STATES_man[[1]],]), est_model_MCPanel$v))
effect0 <- (Mhat - Ymat[STATES_man[[1]],])[Ytreat[STATES_man[[1]],]]
t.test(effect0)
# data:  effect0
# t = -2.023e-13, df = 729, p-value = 1
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#     -0.0002283021  0.0002283021
# sample estimates:
#     mean of x
# -2.352543e-17
forecast::auto.arima(effect0) # mean 0

set.seed(2)
est_model_MCPanel <- mcnnm_cv(M = Ymat[STATES_man[[2]],],
                              mask = Ytreat[STATES_man[[2]],],
                              to_estimate_u = 1, to_estimate_v = 0)
Mhat <- est_model_MCPanel$L + replicate(ncol(Ytreat[STATES_man[[2]],]), est_model_MCPanel$u) +
    t(replicate(nrow(Ytreat[STATES_man[[2]],]), est_model_MCPanel$v))
effect0 <- (Mhat - Ymat[STATES_man[[2]],])[Ytreat[STATES_man[[2]],]]
t.test(effect0)
# data:  effect0
# t = 1.1632e-14, df = 406, p-value = 1
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#     -0.006152919  0.006152919
# sample estimates:
#     mean of x
# 3.640745e-17
forecast::auto.arima(effect0) # mean 0

set.seed(3)
est_model_MCPanel <- mcnnm_cv(M = Ymat[STATES_man[[3]],],
                              mask = Ytreat[STATES_man[[3]],],
                              to_estimate_u = 1, to_estimate_v = 0)
Mhat <- est_model_MCPanel$L + replicate(ncol(Ytreat[STATES_man[[3]],]), est_model_MCPanel$u) +
    t(replicate(nrow(Ytreat[STATES_man[[3]],]), est_model_MCPanel$v))
effect0 <- (Mhat - Ymat[STATES_man[[3]],])[Ytreat[STATES_man[[3]],]]
t.test(effect0)
# data:  effect0
# t = -2.6393e-14, df = 913, p-value = 1
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#     -0.003604054  0.003604054
# sample estimates:
#     mean of x
# -4.846763e-17
forecast::auto.arima(effect0) # mean 0

set.seed(4)
tmp <- sort(unique(unlist(STATES_man)))
est_model_MCPanel <- mcnnm_cv(M = Ymat[tmp,],
                              mask = Ytreat[tmp,],
                              to_estimate_u = 1, to_estimate_v = 0)
Mhat <- est_model_MCPanel$L + replicate(ncol(Ytreat[tmp,]), est_model_MCPanel$u) +
    t(replicate(nrow(Ytreat[tmp,]), est_model_MCPanel$v))
effect0 <- (Mhat - Ymat[tmp,])[Ytreat[tmp,]]
t.test(effect0)
# data:  effect0
# t = 2.253e-14, df = 2050, p-value = 1
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#     -0.01277975  0.01277975
# sample estimates:
#     mean of x
# 1.468199e-16
forecast::auto.arima(effect0) # mean 0


# Cross-validation ----

## Setting up the configuration

Y <- Ymat[!(rownames(Ymat) %in% leg1$State), ]
treat <- Ytreat[!(rownames(Ytreat) %in% leg1$State), ]

N <- nrow(treat)
T <- ncol(treat)
number_T0 = 5
T0 <- ceiling(T*((1:number_T0)*2 - 1)/(2*number_T0))
N_t <- 3 # number of states that are randomly treated, should be < N
num_runs <- 10 # number of CV runs
is_simul <- 0 # Whether to simulate Simultaneous Adoption (1) or Staggered Adoption (0)
# to_save <- 1 ## Whether to save the plot or not

## Matrices for saving RMSE values

MCPanel_RMSE_test <- matrix(0L,num_runs,length(T0))
MCPanel_RMSE_test_w <- matrix(0L,num_runs,length(T0))
DID_RMSE_test <- matrix(0L,num_runs,length(T0))
RF_RMSE_test <- matrix(0L,num_runs,length(T0))

set.seed(123456789)
for (i in c(1:num_runs)) { # i = 1; j = 1
    print(paste0(paste0("Run number ", i)," started"))
    ## Fix the treated units in the whole run for a better comparison
    treat_indices <- sample(1:N, N_t)
    for (j in c(1:length(T0))) {
        # treat_mat <- matrix(1L, N, T);
        t0 <- T0[j]
        ## Simultaneuous (simul_adapt) or Staggered adoption (stag_adapt)
        if (is_simul == 1) {
            treat_mat <- simul_adapt(Y, N_t, t0, treat_indices)
        } else {
            treat_mat <- stag_adapt(Y, N_t, t0, treat_indices)
        }
        rownames(treat_mat) <- rownames(Y)
        Y_obs <- Y * treat_mat


        ## ------
        ## MC-NNM
        ## ------
        est_model_MCPanel <- mcnnm_cv(Y_obs, treat_mat, to_estimate_u = 1, to_estimate_v = 1)
        est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
        est_model_MCPanel$msk_err <- (est_model_MCPanel$Mhat - Y)*(1 - treat_mat)
        est_model_MCPanel$test_RMSE <- sqrt((1/sum(1 - treat_mat)) * sum(est_model_MCPanel$msk_err^2))
        MCPanel_RMSE_test[i, j] <- est_model_MCPanel$test_RMSE

        ## ------
        ## MC-NNM-w
        ## ------
        est_model_MCPanel_w <- mcnnm_wc_cv(Y_obs, mask = treat_mat,
                                           X = matrix(0L,0,0),
                                           Z = Zmat,
                                           to_estimate_u = 1, to_estimate_v = 1)
        est_model_MCPanel_w$Mhat <- est_model_MCPanel_w$L + replicate(T,est_model_MCPanel_w$u) + t(replicate(N,est_model_MCPanel_w$v))
        est_model_MCPanel_w$msk_err <- (est_model_MCPanel_w$Mhat - Y)*(1 - treat_mat)
        est_model_MCPanel_w$test_RMSE <- sqrt((1/sum(1 - treat_mat)) * sum(est_model_MCPanel_w$msk_err^2))
        MCPanel_RMSE_test_w[i, j] <- est_model_MCPanel_w$test_RMSE

        ## -----
        ## DID
        ## -----
        est_model_DID <- DID(Y_obs, treat_mat)
        est_model_DID_msk_err <- (est_model_DID - Y)*(1 - treat_mat)
        est_model_DID_test_RMSE <- sqrt((1/sum(1 - treat_mat)) * sum(est_model_DID_msk_err^2))
        DID_RMSE_test[i, j] <- est_model_DID_test_RMSE


        ## -----
        ## RF
        ## -----
        # Put data back into long format to fit RF
        yrf_treat <- treat_mat %>%
            t() %>%
            as_tibble() %>%
            mutate(Date = as.Date(colnames(Ytreat))) %>%
            pivot_longer(cols = 1:nrow(Y_obs), names_to = "State", values_to = "Treat")

        yrf <- DATAnoNA %>%
            filter(!(State %in% leg1$State)) %>%
            select(-Legal) %>%
            left_join(yrf_treat, by = join_by(State, Date))
        yrf_train <- yrf %>%
            filter(Treat == 1)
        yrf_test <- yrf %>%
            filter(Treat == 0)
        mrf <- ranger(Fatals1m ~ State + Month + Weekday + Holiday + Trange + Precip + Temp,
                      importance = 'none',
                      num.trees = 500,
                      data = yrf_train)
        mrf_err <- yrf_test$Fatals1m - predict(mrf, yrf_test)$predictions
        mrf_RMSE <- sqrt(mean(mrf_err^2))
        RF_RMSE_test[i, j] <- mrf_RMSE
    }
}
# save.image(paste0("dataderived/MatrixCompletion_", Sys.Date(), ".rdata"))

## Computing means and standard errors
DID_avg_RMSE <- apply(DID_RMSE_test, 2, mean)
DID_std_error <- apply(DID_RMSE_test, 2, sd)/sqrt(num_runs)

MCPanel_avg_RMSE <- apply(MCPanel_RMSE_test, 2, mean)
MCPanel_std_error <- apply(MCPanel_RMSE_test, 2, sd)/sqrt(num_runs)

MCPanel_w_avg_RMSE <- apply(MCPanel_RMSE_test_w, 2, mean)
MCPanel_w_std_error <- apply(MCPanel_RMSE_test_w, 2, sd)/sqrt(num_runs)

RF_avg_RMSE <- apply(RF_RMSE_test, 2, mean)
RF_std_error <- apply(RF_RMSE_test, 2, sd)/sqrt(num_runs)



df1 <- structure(
    list(
        y =  c(DID_avg_RMSE,
               MCPanel_avg_RMSE,
               # MCPanel_w_avg_RMSE,
               RF_avg_RMSE),
        lb = c(DID_avg_RMSE - 1.96*DID_std_error,
               MCPanel_avg_RMSE - 1.96*MCPanel_std_error,
               # MCPanel_w_avg_RMSE - 1.96*MCPanel_w_std_error,
               RF_avg_RMSE - 1.96*RF_std_error),
        ub = c(DID_avg_RMSE + 1.96*DID_std_error,
               MCPanel_avg_RMSE + 1.96*MCPanel_std_error,
               # MCPanel_w_avg_RMSE + 1.96*MCPanel_w_std_error,
               RF_avg_RMSE + 1.96*RF_std_error),
        x = c(T0/T, T0/T , #T0/T,
              T0/T),
        Method = c(replicate(length(T0),"DID"),
                   replicate(length(T0),"MC-NNM"),
                   # replicate(length(T0),"MC-NNMw"),
                   replicate(length(T0),"RF")),
        Marker = c(replicate(length(T0),1)
                   ,replicate(length(T0),2)
                   ,replicate(length(T0),3)
                   # ,replicate(length(T0),4)
                   )

    ),
    .Names = c("y", "lb", "ub", "x", "Method", "Marker"),
    row.names = c(NA, -15L),
    class = "data.frame"
)
Marker = 1:3
summary(df1)

p = ggplot(data = df1, aes(x, y, color = Method, shape = Marker)) +
    geom_point(size = 2, position = position_dodge(width=0.1)) +
    geom_errorbar(
        aes(ymin = lb, ymax = ub),
        width = 0.1,
        linetype = "solid",
        position=position_dodge(width=0.1)) +
    scale_shape_identity() +
    guides(color = guide_legend(override.aes = list(shape = Marker))) +
    theme_light() +
    xlab(TeX('$T_0/T$')) +
    ylab("Average RMSE")# +
    #coord_cartesian(ylim = c(5, 50)) #+
# theme(axis.title=element_text(family="Times", size=14)) +
# theme(axis.text=element_text(family="Times", size=12)) +
# theme(legend.text=element_text(family="Times", size = 12)) +
# theme(legend.title=element_text(family="Times", size = 12))

cairo_pdf("images/MatrixCV.pdf", height = 4, width = 7)
print(p)
dev.off()




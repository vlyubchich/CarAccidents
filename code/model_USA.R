rm(list = ls())

library(dplyr)
library(ggplot2)
library(viridis)
library(hrbrthemes)

library(car)
# library(gamlss)

# Data ----

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
              NFatAcc1m = sum(NFatAcc1m),
              RegisteredVehicles = mean(RegisteredVehicles)
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
    mutate(Legal = as.factor(Legal),
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
# hist(D$Nacc1m)
# hist(log10(D$Nacc1m))
# with(D, Date[is.na(Precip)]) # because Daymet skips 02-29


# RF ----
library(ranger); library(randomForest)
library(pdp)
library(ICEbox)
library(caret)
library(Boruta)

NTREES <- 500
nperm <- 100 # number of permutations for importance p-values
tgrid <- expand.grid(
    mtry = 2:4,
    splitrule = "variance",
    min.node.size = c(3, 5, 10)
)


## Fatals ----
RESPONSE <- "Fatals1m"

### tune ----
# tune RF on all states
set.seed(10000)
model <- caret::train(
    Fatals1m ~ .,
    tuneGrid = tgrid,
    data = na.omit(D), method = "ranger",
    num.trees = NTREES,
    trControl = trainControl(method = "cv", number = 10, verboseIter = FALSE)
)
print(model)
# Random Forest
# 11200 samples
# 20 predictor
# No pre-processing
# Resampling: Cross-Validated (10 fold)
# Summary of sample sizes: 10081, 10079, 10080, 10080, 10079, 10080, ...
# Resampling results across tuning parameters:
#     mtry  min.node.size  RMSE       Rsquared   MAE
# 2      3             0.2549374  0.6370498  0.1654392
# 2      5             0.2542892  0.6390068  0.1647481
# 2     10             0.2535764  0.6415647  0.1641607
# 3      3             0.2112825  0.7606899  0.1292281
# 3      5             0.2118332  0.7584330  0.1295114
# 3     10             0.2119836  0.7592280  0.1297648
# 4      3             0.1799438  0.8193888  0.1029057
# 4      5             0.1798507  0.8196754  0.1031194
# 4     10             0.1819968  0.8158732  0.1045627
# Tuning parameter 'splitrule' was held constant at a value of variance
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were mtry = 4, splitrule = variance and min.node.size = 5.

tune_mtry = 4
tune_node = 5

### common PSM ----
# fit a model on all PSM-selected states
DATAnoNA <- D %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.element(State, unlist(STATES_psm))) %>%
    dplyr::select(all_of(c(predictors, RESPONSE))) %>%
    na.omit()
attributes(DATAnoNA)$na.action <- NULL

set.seed(10000)
ran <- ranger(dependent.variable.name = RESPONSE, data = DATAnoNA,
              importance = 'none',
              mtry = tune_mtry,
              min.node.size = tune_node,
              respect.unordered.factors = 'order',
              num.trees = NTREES)
ran
# Type:                             Regression
# Number of trees:                  500
# Sample size:                      8400
# Number of independent variables:  8
# Mtry:                             4
# Target node size:                 5
# Variable importance mode:         none
# Splitrule:                        variance
# OOB prediction error (MSE):       0.1338888
# R squared (OOB):                  0.02814014
(tmp <- pdp::partial(ran, pred.var = "Legal"))
# Legal      yhat
# 1 FALSE 0.3292029
# 2  TRUE 0.3185002
diff(tmp$yhat) #-0.01070266


Boruta::Boruta(y = pull(DATAnoNA[,RESPONSE]),
               x = DATAnoNA[,predictors],
               doTrace = 1, maxRuns = 100)
# Boruta performed 45 iterations in 1.956298 mins.
# 6 attributes confirmed important: Month, Precip, State, Temp, Trange and 1 more;
# 2 attributes confirmed unimportant: Holiday, Legal;

# regrow RF with importance
set.seed(10000)
rani <- ranger(dependent.variable.name = RESPONSE, data = DATAnoNA,
               importance = 'permutation',
               mtry = tune_mtry,
               min.node.size = tune_node,
               respect.unordered.factors = 'order',
               num.trees = NTREES)
sort(rani$variable.importance)
# Holiday        Legal      Weekday       Trange       Precip        Month         Temp        State
# 2.026391e-05 5.932569e-05 1.291211e-03 9.386973e-03 1.208179e-02 1.352598e-02 1.974770e-02 3.010437e-02

importance_pvalues(rani,
                   method = "altmann",
                   formula = as.formula(paste(RESPONSE, ".", sep = " ~ ")),
                   num.permutations = nperm,
                   data = DATAnoNA)
# importance     pvalue
# Legal   5.932569e-05 0.95049505
# State   3.010437e-02 0.00990099
# Month   1.352598e-02 0.79207921
# Weekday 1.291211e-03 0.02970297
# Holiday 2.026391e-05 0.57425743
# Trange  9.386973e-03 0.92079208
# Precip  1.208179e-02 0.31683168
# Temp    1.974770e-02 0.86138614

# Save PDPs
pred <- setdiff(predictors, c("Legal", "Holiday", "State")) #, "Temp", "Trange", "Precip"
ranplots <- lapply(append(as.list(pred),
                          list(c("Temp", "Trange"),
                               c("Temp", "Precip"))),
                   function(i){
                       pdp::partial(ran, pred.var = i,
                                    plot = TRUE, plot.engine = "ggplot2", rug = TRUE,
                                    grid.resolution = 30, chull = TRUE,
                                    # ice = TRUE, center = TRUE,
                                    progress = "text") + theme_minimal()
                   }
)
saveRDS(ranplots, file = "./dataderived/ranplots_comPSM_Fatals.rds")


### indiv PSM ----
sapply(STATES_psm, function(s) { # s = STATES_psm[[1]]
    set.seed(123)
    # select only the group of states
    d <- DATAnoNA %>%
        filter(is.element(State, s))
    rans <- ranger(dependent.variable.name = RESPONSE, data = d,
                   importance = 'none',
                   mtry = tune_mtry,
                   min.node.size = tune_node,
                   respect.unordered.factors = 'order',
                   num.trees = NTREES)
    tmp <- pdp::partial(rans, pred.var = "Legal")
    ransi <- ranger(dependent.variable.name = RESPONSE, data = d,
                    importance = 'permutation',
                    mtry = tune_mtry,
                    min.node.size = tune_node,
                    respect.unordered.factors = 'order',
                    num.trees = NTREES)
    pv <- importance_pvalues(ransi,
                             method = "altmann",
                             formula = as.formula(paste(RESPONSE, ".", sep = " ~ ")),
                             num.permutations = nperm,
                             data = d)
    print(s)
    print(tmp)
    print(diff(tmp$yhat))
    print(pv)
})

#### res PSM ----
# [1] "CA" "WV"
# Legal      yhat
# 1 FALSE 0.4197849
# 2  TRUE 0.3880339
# [1] -0.03175094
# importance     pvalue
# Legal   -0.0070799947 1.00000000
# State    0.0198627123 0.03960396
# Month    0.0150949579 0.74257426
# Weekday  0.0033677737 0.02970297
# Holiday  0.0001848716 0.32673267
# Trange   0.0082380985 0.96039604
# Precip   0.0173524309 0.54455446
# Temp     0.0206211850 0.93069307
# [1] "MA" "UT"
# Legal      yhat
# 1 FALSE 0.3999982
# 2  TRUE 0.3742231
# [1] -0.02577516
# importance     pvalue
# Legal   -4.559165e-04 0.88118812
# State    2.325478e-02 0.00990099
# Month    1.057102e-02 0.63366337
# Weekday  1.620144e-04 0.30693069
# Holiday  5.416757e-05 0.37623762
# Trange   1.841474e-02 0.11881188
# Precip   6.213057e-03 0.26732673
# Temp     1.181526e-02 0.88118812
# [1] "NV" "NY"
# Legal      yhat
# 1 FALSE 0.3926249
# 2  TRUE 0.3771975
# [1] -0.01542739
# importance     pvalue
# Legal   0.0028214892 0.23762376
# State   0.0153033240 0.01980198
# Month   0.0059572579 0.84158416
# Weekday 0.0021427218 0.01980198
# Holiday 0.0001136971 0.22772277
# Trange  0.0054395071 0.99009901
# Precip  0.0043091646 0.86138614
# Temp    0.0072214583 0.98019802




## NFatAcc ----
RESPONSE <- "NFatAcc1m"

### tune ----
# tune RF on all states
set.seed(10000)
model <- caret::train(
    NFatAcc1m ~ .,
    tuneGrid = tgrid,
    data = na.omit(D), method = "ranger",
    num.trees = NTREES,
    trControl = trainControl(method = "cv", number = 10, verboseIter = FALSE)
)
print(model)
# 11200 samples
# 20 predictor
# No pre-processing
# Resampling: Cross-Validated (10 fold)
# Summary of sample sizes: 10081, 10079, 10079, 10080, 10080, 10081, ...
# Resampling results across tuning parameters:
#     mtry  min.node.size  RMSE       Rsquared   MAE
# 2      3             0.2222938  0.6573865  0.15051448
# 2      5             0.2210742  0.6656189  0.14987005
# 2     10             0.2216725  0.6617224  0.15043698
# 3      3             0.1810026  0.7846843  0.11728365
# 3      5             0.1815662  0.7843521  0.11770444
# 3     10             0.1824593  0.7798416  0.11817953
# 4      3             0.1518034  0.8440442  0.09347867
# 4      5             0.1514563  0.8449424  0.09330872
# 4     10             0.1531489  0.8425941  0.09457252
# Tuning parameter 'splitrule' was held constant at a value of variance
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were mtry = 4, splitrule = variance and min.node.size = 5.

tune_mtry = 4
tune_node = 5

### common PSM ----
# fit a model on all PSM-selected states
DATAnoNA <- D %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.element(State, unlist(STATES_psm))) %>%
    dplyr::select(all_of(c(predictors, RESPONSE))) %>%
    na.omit()
attributes(DATAnoNA)$na.action <- NULL

set.seed(10000)
ran <- ranger(dependent.variable.name = RESPONSE, data = DATAnoNA,
              importance = 'none',
              mtry = tune_mtry,
              min.node.size = tune_node,
              respect.unordered.factors = 'order',
              num.trees = NTREES)
ran
# Type:                             Regression
# Number of trees:                  500
# Sample size:                      8400
# Number of independent variables:  8
# Mtry:                             4
# Target node size:                 5
# Variable importance mode:         none
# Splitrule:                        variance
# OOB prediction error (MSE):       0.1046246
# R squared (OOB):                  0.03249567
(tmp <- pdp::partial(ran, pred.var = "Legal"))
# Legal      yhat
# 1 FALSE 0.3049418
# 2  TRUE 0.2959395
diff(tmp$yhat) #-0.009002383


Boruta::Boruta(y = pull(DATAnoNA[,RESPONSE]),
               x = DATAnoNA[,predictors],
               doTrace = 1, maxRuns = 100)
# Boruta performed 99 iterations in 4.637638 mins.
# 6 attributes confirmed important: Month, Precip, State, Temp, Trange and 1 more;
# 1 attributes confirmed unimportant: Holiday;
# 1 tentative attributes left: Legal;

# regrow RF with importance
set.seed(10000)
rani <- ranger(dependent.variable.name = RESPONSE, data = DATAnoNA,
               importance = 'permutation',
               mtry = tune_mtry,
               min.node.size = tune_node,
               respect.unordered.factors = 'order',
               num.trees = NTREES)
sort(rani$variable.importance)
# Legal       Holiday       Weekday        Trange        Precip         Month          Temp         State
# -0.0001165269  0.0001015711  0.0009838014  0.0074341905  0.0093354711  0.0118937740  0.0144812623  0.0220544611

importance_pvalues(rani,
                   method = "altmann",
                   formula = as.formula(paste(RESPONSE, ".", sep = " ~ ")),
                   num.permutations = nperm,
                   data = DATAnoNA)
# importance     pvalue
# Legal   -0.0001165269 0.99009901
# State    0.0220544611 0.00990099
# Month    0.0118937740 0.51485149
# Weekday  0.0009838014 0.04950495
# Holiday  0.0001015711 0.28712871
# Trange   0.0074341905 0.91089109
# Precip   0.0093354711 0.21782178
# Temp     0.0144812623 0.78217822

# Save PDPs
pred <- setdiff(predictors, c("Legal", "Holiday", "State")) #, "Temp", "Trange", "Precip"
ranplots <- lapply(append(as.list(pred),
                          list(c("Temp", "Trange"),
                               c("Temp", "Precip"))),
                   function(i){
                       pdp::partial(ran, pred.var = i,
                                    plot = TRUE, plot.engine = "ggplot2", rug = TRUE,
                                    grid.resolution = 30, chull = TRUE,
                                    # ice = TRUE, center = TRUE,
                                    progress = "text") + theme_minimal()
                   }
)
saveRDS(ranplots, file = "./dataderived/ranplots_comPSM_NFatAcc.rds")


### indiv PSM ----
sapply(STATES_psm, function(s) { # s = STATES_psm[[1]]
    set.seed(123)
    # select only the group of states
    d <- DATAnoNA %>%
        filter(is.element(State, s))
    rans <- ranger(dependent.variable.name = RESPONSE, data = d,
                   importance = 'none',
                   mtry = tune_mtry,
                   min.node.size = tune_node,
                   respect.unordered.factors = 'order',
                   num.trees = NTREES)
    tmp <- pdp::partial(rans, pred.var = "Legal")
    ransi <- ranger(dependent.variable.name = RESPONSE, data = d,
                    importance = 'permutation',
                    mtry = tune_mtry,
                    min.node.size = tune_node,
                    respect.unordered.factors = 'order',
                    num.trees = NTREES)
    pv <- importance_pvalues(ransi,
                             method = "altmann",
                             formula = as.formula(paste(RESPONSE, ".", sep = " ~ ")),
                             num.permutations = nperm,
                             data = d)
    print(s)
    print(tmp)
    print(diff(tmp$yhat))
    print(pv)
})

#### res PSM ----
# [1] "CA" "WV"
# Legal      yhat
# 1 FALSE 0.3160747
# 2  TRUE 0.3006359
# [1] -0.01543888
# importance     pvalue
# Legal   6.924026e-06 0.58415842
# State   0.000000e+00 1.00000000
# Month   8.708612e-04 0.31683168
# Weekday 2.837281e-03 0.00990099
# Holiday 8.582953e-05 0.02970297
# Trange  9.791397e-04 0.68316832
# Precip  6.530290e-04 0.60396040
# Temp    1.102256e-03 0.64356436
# [1] "MA" "UT"
# Legal      yhat
# 1 FALSE 0.3549311
# 2  TRUE 0.3344355
# [1] -0.02049557
# importance     pvalue
# Legal   -3.455924e-04 0.89108911
# State    1.515733e-02 0.00990099
# Month    1.006287e-02 0.27722772
# Weekday  9.324183e-04 0.07920792
# Holiday -8.450188e-06 0.49504950
# Trange   1.542700e-02 0.02970297
# Precip   4.844879e-03 0.22772277
# Temp     1.117998e-02 0.52475248
# [1] "NV" "NY"
# Legal      yhat
# 1 FALSE 0.3582581
# 2  TRUE 0.3398851
# [1] -0.01837293
# importance    pvalue
# Legal   -1.341599e-03 0.9900990
# State    0.000000e+00 1.0000000
# Month    8.019573e-03 0.5940594
# Weekday  4.787134e-04 0.3762376
# Holiday -5.671848e-06 0.5346535
# Trange   3.492117e-03 0.9702970
# Precip   9.713681e-04 0.9009901
# Temp     6.083906e-03 0.9603960

# RF-GAP
Random Forest Geometry- and Accuracy-Preserving proximities

This is the official repository for the papr "Random Forest- Geometry- and Accuracy-Preserving Proximities" (https://arxiv.org/abs/2201.12682). In the paper we show that random forest (RF) predictions can be exactly determined by using RF-GAP proximities as weights in a weighted-sum regressor or weighted-majority vote classifier.  This repo provides the base code to generate the various proximity definitions described in the paper. We provide some examples below.


# Generate RF-GAP proximities:

```{r}
x <- iris[, 1:4]
y <- iris[, 5]
prox <- get_proximities(x, y, type = 'rfgap')
```

# Create 2-dimensional MDS embedding and plot

```{r}
x <- iris[, 1:4]
y <- iris[, 5]
mds <- rf_mds(x, y, type = 'rfgap')

plot(mds, y)
```

# Impute missing data
```{r}
x <- airquality[, -4]
y <- airquality[, 4]
imputed_data <- rf_impute(x, y, type = 'rfgap')
```

# Run Outlier Detection
```{r}
x <- iris[, 1:4]
y <- iris[, 5]
outlier_score <- rf_outliers(x, y, type = 'rfgap')
```

# RF-GAP
Random Forest Geometry- and Accuracy-Preserving proximities

This is the official repository for the papr "Random Forest- Geometry- and Accuracy-Preserving Proximities" (https://arxiv.org/abs/2201.12682). In the paper we show that random forest (RF) predictions can be exactly determined by using RF-GAP proximities as weights in a weighted-sum regressor or weighted-majority vote classifier.  This repo provides the base code to generate the various proximity definitions described in the paper. We provide some examples below.


# Generate RF-GAP proximities:

```{r}
x <- iris[, 1:4]
y <- iris[, 5]
prox <- get_proximities(x, y, type = 'rfgap')
```

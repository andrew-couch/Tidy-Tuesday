Capstone HW2
================
Andrew Couch
2/4/2020

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------------------ tidyverse 1.3.0 --

    ## <U+2713> ggplot2 3.2.1     <U+2713> purrr   0.3.3
    ## <U+2713> tibble  2.1.3     <U+2713> dplyr   0.8.3
    ## <U+2713> tidyr   1.0.0     <U+2713> stringr 1.4.0
    ## <U+2713> readr   1.3.1     <U+2713> forcats 0.4.0

    ## -- Conflicts --------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(broom)
df <- read.csv("HW2-des_moines_housingFull.csv")
```

``` r
predictors <- df %>% select(-(1:2))

predcorrelation <- predictors %>% cor() %>% tidy() %>% rename(predictor = ".rownames")
```

    ## Warning: 'tidy.matrix' is deprecated.
    ## See help("Deprecated")

``` r
#Creates tidy correlation matrix predictor
#Finds the top 3 correlated variables for each predictor 
#Removes identical variables and their correlation (above is 100% correlated with above)
predcorrelation %>% 
  gather("key" = "predictor2", value = "correlation", -predictor) %>% 
  filter(predictor != predictor2) %>% 
  arrange(predictor, -abs(correlation)) %>% 
  group_by(predictor) %>% 
  top_n(abs(correlation), n = 3)
```

    ## # A tibble: 165 x 3
    ## # Groups:   predictor [55]
    ##    predictor predictor2             correlation
    ##    <chr>     <chr>                        <dbl>
    ##  1 above     baths                        0.675
    ##  2 above     fireplaces                   0.646
    ##  3 above     beds                         0.630
    ##  4 AC_0      AC_1                        -1    
    ##  5 AC_0      age                          0.316
    ##  6 AC_0      condition_Below.Normal       0.278
    ##  7 AC_1      AC_0                        -1    
    ##  8 AC_1      age                         -0.316
    ##  9 AC_1      condition_Below.Normal      -0.278
    ## 10 acres     above                        0.249
    ## # … with 155 more rows

``` r
library(GGally)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## 
    ## Attaching package: 'GGally'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     nasa

``` r
#Columns 1:11 are actual continuous variables 
#The rest are one hot encoded 
predictors %>% 
  str()
```

    ## 'data.frame':    15310 obs. of  55 variables:
    ##  $ acres                 : num  0.177 0.196 0.193 0.202 0.297 0.21 0.234 0.384 0.166 0.183 ...
    ##  $ above                 : int  1104 1140 966 964 1403 1367 1350 2074 768 1188 ...
    ##  $ basement              : int  400 400 0 0 420 0 0 0 0 0 ...
    ##  $ baths                 : int  1 2 1 1 2 1 1 1 1 1 ...
    ##  $ toilets               : int  0 0 0 0 0 0 0 1 0 0 ...
    ##  $ fireplaces            : int  0 1 0 0 1 1 0 1 0 0 ...
    ##  $ beds                  : int  3 3 2 2 3 2 3 4 2 3 ...
    ##  $ rooms                 : int  3 2 2 3 3 3 5 2 2 2 ...
    ##  $ age                   : int  63 63 132 76 93 96 103 105 68 68 ...
    ##  $ yearsfromsale         : int  4 4 4 4 4 4 4 4 4 4 ...
    ##  $ car                   : int  0 1 0 0 0 0 0 3 0 0 ...
    ##  $ city_DES.MOINES       : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ city_JOHNSTON         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ city_WEST.DES.MOINES  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ city_CLIVE            : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ city_URBANDALE        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ city_ALTOONA          : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ city_BONDURANT        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ city_CROCKER.TWNSHP   : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ city_GRIMES           : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ city_POLK.CITY        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ city_PLEASANT.HILL    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ city_WINDSOR.HEIGHTS  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50315             : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ zip_50321             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50320             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50312             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50314             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50311             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50309             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50316             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50317             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50313             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50310             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50322             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50131             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50111             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50265             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50266             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50325             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50323             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50009             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50035             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50023             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50226             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50021             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50327             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ zip_50324             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ walkout_0             : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ walkout_1             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ condition_Normal      : int  1 0 0 1 0 0 1 0 0 1 ...
    ##  $ condition_Above.Normal: int  0 1 1 0 1 1 0 1 1 0 ...
    ##  $ condition_Below.Normal: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ AC_1                  : int  1 1 0 1 1 1 1 1 1 1 ...
    ##  $ AC_0                  : int  0 0 1 0 0 0 0 0 0 0 ...

``` r
#Generate Summary Statistics of the continous predictors
predictors %>% 
  select(1:11) %>% 
  gather(key = "features", value = "value") %>% 
  group_by(features) %>% 
  summarise(min = min(value),
            Q1 = quantile(value, .25),
            median = median(value),
            mean = mean(value), 
            Q3 = quantile(value, .75), 
            max = max(value),
            sd = sd(value),
            mad = mad(value),
            kurtosis = e1071::kurtosis(value),
            skewness = e1071::skewness(value))
```

    ## # A tibble: 11 x 11
    ##    features     min      Q1  median    mean      Q3    max      sd     mad
    ##    <chr>      <dbl>   <dbl>   <dbl>   <dbl>   <dbl>  <dbl>   <dbl>   <dbl>
    ##  1 above    396     1.00e+3 1.34e+3 1.46e+3 1.75e+3 8934   629.    5.43e+2
    ##  2 acres      0.046 1.72e-1 2.17e-1 2.86e-1 2.85e-1   35.1   0.426 7.41e-2
    ##  3 age        3     2.50e+1 5.50e+1 5.41e+1 7.70e+1  170    32.5   4.00e+1
    ##  4 basement   0     0.      0.      2.69e+2 4.80e+2 4320   405.    0.     
    ##  5 baths      0     1.00e+0 2.00e+0 1.66e+0 2.00e+0    6     0.751 1.48e+0
    ##  6 beds       1     2.00e+0 3.00e+0 2.94e+0 3.00e+0    7     0.732 0.     
    ##  7 car        0     0.      0.      1.03e+0 2.00e+0    3     1.18  0.     
    ##  8 firepla…   0     0.      1.00e+0 5.64e-1 1.00e+0    5     0.617 1.48e+0
    ##  9 rooms      0     2.00e+0 3.00e+0 2.87e+0 3.00e+0   14     1.13  1.48e+0
    ## 10 toilets    0     0.      0.      4.58e-1 1.00e+0    4     0.538 0.     
    ## 11 yearsfr…   2     2.00e+0 3.00e+0 2.89e+0 4.00e+0    4     0.804 1.48e+0
    ## # … with 2 more variables: kurtosis <dbl>, skewness <dbl>

``` r
#Create a df with only categorical featuers 
categoricalfeatures <- predictors %>% 
  select(-(1:11)) %>% 
  mutate(row = row_number()) %>% 
  gather(key = "key", value = "value", -row) %>% 
  filter(value == 1) %>% 
  separate(key, into = c("feature", "value"), sep = "_") %>% 
  pivot_wider(names_from = feature, values_from = value) %>% 
  select(-row)
```

``` r
#Convert all featuers to factor and data frame 
categoricalfeatures <- categoricalfeatures %>% map(as.factor) %>% as.data.frame()
```

``` r
#Create plot showing distribution of categorical features 
categoricalfeatures %>% 
  gather(key = "features", value = "value") %>% 
  drop_na() %>% 
  group_by(features, value) %>% 
  count() %>% 
  ggplot(aes(x = reorder(value, n), y = n, fill = features)) +
  geom_col() + 
  coord_flip() + 
  facet_wrap(~features, scales = "free") + 
  theme(legend.position = "none")
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

![](HW2-Analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
#Tidy table output of previous plot
categoricalfeatures %>% 
  gather(key = "features", value = "value") %>% 
  group_by(features, value) %>% 
  count(value) %>% 
  arrange(features, -n)
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

    ## # A tibble: 45 x 3
    ## # Groups:   features, value [45]
    ##    features value               n
    ##    <chr>    <chr>           <int>
    ##  1 AC       1               13995
    ##  2 AC       0                1315
    ##  3 city     DES.MOINES       8257
    ##  4 city     WEST.DES.MOINES  1628
    ##  5 city     URBANDALE        1357
    ##  6 city     JOHNSTON          840
    ##  7 city     ALTOONA           816
    ##  8 city     GRIMES            670
    ##  9 city     CLIVE             395
    ## 10 city     PLEASANT.HILL     395
    ## # … with 35 more rows

``` r
#Create two data frames, one with the categorical target the other with continuous target
dfcat <- df %>% select(-1) %>% mutate(top10per = top10per %>% as.factor())
dfnum <- df %>% select(-2) 
```

``` r
#Examining means of all numerical features by target variable
dfcat %>% 
  select(1:12) %>% 
  gather(key = "key", value = "value", -top10per) %>% 
  group_by(top10per, key) %>% 
  summarise(mean = mean(value)) %>% 
  arrange(key, top10per) %>% 
  pivot_wider(names_from = top10per, values_from = mean) 
```

    ## # A tibble: 11 x 3
    ##    key                `0`      `1`
    ##    <chr>            <dbl>    <dbl>
    ##  1 above         1322.    2579.   
    ##  2 acres            0.248    0.601
    ##  3 age             57.2     28.1  
    ##  4 basement       206.     795.   
    ##  5 baths            1.52     2.78 
    ##  6 beds             2.86     3.59 
    ##  7 car              0.836    2.61 
    ##  8 fireplaces       0.473    1.33 
    ##  9 rooms            2.71     4.19 
    ## 10 toilets          0.414    0.824
    ## 11 yearsfromsale    2.90     2.86

``` r
#Conduct a t.test on each feature grouped by target variable
#Plot the p.values of each test for each feature
library(broom)
options(scipen = 999)
dfcat %>% 
  select(1:12) %>% 
  gather(key = "key", value = "value", -top10per) %>% 
  group_by(key) %>% 
  do(t.test(value~top10per, data = (.)) %>% tidy()) %>% 
  select(key, p.value) %>% 
  ggplot(aes(x = key, y = p.value, color = key)) + 
  geom_point() + 
  geom_segment(aes(xend = key, yend = 0)) + 
  geom_hline(yintercept = .05) + 
  scale_y_sqrt() + 
  theme(legend.position = "none") +
  coord_flip()
```

![](HW2-Analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
#Measure affect size using logistic regression model and its coefficients (filtering on pvalue <= .05)

dfcat %>% 
  glm(top10per~., data = ., family = "binomial") %>% 
  tidy() %>% 
  select(term, estimate, p.value) %>% 
  mutate(p.value = round(p.value, 3)) %>% 
  filter(p.value <= .05) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = reorder(term, abs(estimate)), y = estimate, color = term)) + 
  geom_point() + 
  geom_segment(aes(xend = term, yend = 0)) + 
  theme(legend.position = "none") + 
  coord_flip()
```

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

![](HW2-Analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
#Displays each numerical featue with the target variable (sale_price, continuous)
#ALso shows how correlated the numerical featues are wtih sale_price
dfnum %>% 
  sample_n(size = 100) %>%
  select(1:12) %>% 
  gather(key = "features", value = "value", -sale_price) %>% 
  ggplot(aes(x = value, y = sale_price, color = features)) +
  geom_point(alpha = .2) + 
  geom_smooth(se = FALSE) + 
  facet_wrap(~features, scales = "free") + 
  theme(legend.position = "none")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at 0.975

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 1.025

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 1

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at 0.98

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 2.02

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 9.678e-017

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 4.0804

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at -0.015

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 2.015

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 1.2982e-016

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 4

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at -0.02

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 1.02

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 1

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at 0.945

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 2.055

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 4.3808e-016

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 1

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at -0.01

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 1.01

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 4.0401

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : pseudoinverse used at 1.99

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : neighborhood radius 2.01

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : reciprocal condition number 3.3045e-016

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric =
    ## parametric, : There are other near singularities as well. 4.0401

![](HW2-Analysis_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
#Tidy correlation table of each features correlation with sale_price (target)
dfnum %>% 
  select(1:12) %>% 
  cor() %>% 
  tidy() %>% 
  rename(., "feature" = .rownames) %>% 
  gather(key = "feature2", value = "value", -feature) %>% 
  filter(feature != feature2) %>% 
  rename(., "target" = feature2) %>% 
  filter(target == "sale_price") %>% 
  select(target, feature, value) %>% 
  arrange(-abs(value))
```

    ## Warning: 'tidy.matrix' is deprecated.
    ## See help("Deprecated")

    ## # A tibble: 11 x 3
    ##    target     feature         value
    ##    <chr>      <chr>           <dbl>
    ##  1 sale_price above          0.819 
    ##  2 sale_price baths          0.716 
    ##  3 sale_price car            0.652 
    ##  4 sale_price fireplaces     0.648 
    ##  5 sale_price basement       0.542 
    ##  6 sale_price age           -0.507 
    ##  7 sale_price rooms          0.487 
    ##  8 sale_price beds           0.468 
    ##  9 sale_price toilets        0.430 
    ## 10 sale_price acres          0.355 
    ## 11 sale_price yearsfromsale -0.0235

``` r
#Uses linear model to estimate affect size each featuer has on sale_price
#Plots the coefficents and filter any features with p.values above .05
dfnum %>% 
  do(model = lm(sale_price~., data = (.))) %>% 
  tidy(model) %>% 
  filter(p.value <= .05) %>% 
  filter(term != "(Intercept)") %>% 
  select(term, estimate) %>% 
  ggplot(aes(x = reorder(term, abs(estimate)), y = estimate, color = term)) + 
  geom_point() + 
  geom_segment(aes(xend = term, yend = 0)) + 
  theme(legend.position = "none") + 
  coord_flip()
```

![](HW2-Analysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
#Plots residuals of the model to estimate how accurate the effect size is from the linear regression model
dfnum %>% 
  do(model = lm(sale_price~., data = (.))) %>% 
  augment(model) %>% 
  select(sale_price, .std.resid) %>% 
  ggplot(aes(x = sale_price, y = .std.resid, color = .std.resid)) + 
  geom_point() + 
  theme(legend.position = "none") + 
  scale_x_log10()
```

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](HW2-Analysis_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
#Categorical vs Numerical features for regression 
#What variables are more important when predicting sale_price? Are categorical better?
dfnum %>% 
  select(1:12) %>% 
  do(model = lm(sale_price~., data = (.))) %>% 
  glance(model) %>%
  mutate(model = "num") %>% 
  rbind(dfnum %>% 
  select(-(1:12), 1) %>% 
  do(model = lm(sale_price~., data = (.))) %>% 
  glance(model) %>% 
    mutate(model = "cat")) %>% 
  gather(key = "metric", value = value, -model) %>% 
  ggplot(aes(x = model, y = value, fill = metric)) + 
  geom_col() + 
  facet_wrap(~metric, scales = "free") + 
  theme(legend.position = "none")
```

![](HW2-Analysis_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
#Numerical features seem to outperform r squared metrics 
#Numerical feautres outperform AIC and BIC metrics too
```

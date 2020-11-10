Iteration and listcols
================

## Lists

You can put anything in a list.

``` r
l = list(
  vec_numeric = 5:8,
  vec_logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
  mat = matrix(1:8, nrow = 2, ncol = 4),
  summary = summary(rnorm(100))
)
```

``` r
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $vec_logical
    ## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.45094 -0.47652 -0.11626 -0.00497  0.60663  2.83452

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

## `for` loop

Create a new list.

``` r
list_norm = 
  list(
    a = rnorm(20, mean = 3, sd = 1),
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 10, sd = .2),
    d = rnorm(20, mean = -3, sd = 1)
  )
```

``` r
list_norm
```

    ## $a
    ##  [1] 2.8471850 3.5149426 3.3020559 3.9060270 1.5588106 2.2332932 2.5589464
    ##  [8] 4.2358351 4.8653109 3.4948497 3.8783812 1.3340420 2.9361319 2.7118602
    ## [15] 3.3922903 0.7779518 4.3086134 4.5792022 2.3598161 1.6087090
    ## 
    ## $b
    ##  [1]   1.84740593  -9.04457986   4.81941369  -7.08959599   5.91440280
    ##  [6]  -8.73440840   0.94878790   1.39555035   6.11788770  -7.61486462
    ## [11]  -2.83427069   3.05144834   3.53295048   0.95317073   5.69164626
    ## [16]   0.05135899   1.26831162   6.10468308 -14.01734964  -0.79846815
    ## [21]   5.90840137  -0.58535984   5.68570374   7.55126956   2.36847687
    ## [26]   6.52466565  -3.31551829   1.91388498  -0.75028792  10.09964666
    ## 
    ## $c
    ##  [1]  9.647301  9.553753 10.283060  9.840199  9.792614 10.206767 10.117874
    ##  [8] 10.006436  9.818057  9.924685  9.742157 10.107908 10.069922 10.222942
    ## [15]  9.577248 10.142217 10.101660  9.772435 10.160093  9.914335  9.959902
    ## [22] 10.076113  9.949468 10.344929 10.240823  9.840334  9.735253 10.009586
    ## [29] 10.259824  9.752850 10.011352 10.017627  9.989689 10.055548 10.161725
    ## [36]  9.997491 10.187177  9.811653  9.851531 10.399283
    ## 
    ## $d
    ##  [1] -2.975755 -3.795180 -1.430030 -2.284192 -3.339589 -2.326083 -1.857971
    ##  [8] -2.458228 -1.593871 -1.479584 -2.910343 -2.719754 -3.766439 -3.199232
    ## [15] -1.883778 -3.931557 -4.471980 -1.262156 -3.679937 -4.532960

Pause and get my old function.

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
}
```

I can apply that function to each list element.

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.02  1.14

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.899  5.67

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.99 0.208

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.79  1.03

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
}
```

## Let’s try map\!

``` r
output = map(list_norm, mean_and_sd)
```

what if you want a different function..? Can map any function you want

``` r
output = map(list_norm, median)
```

``` r
output = map_dbl(list_norm, median, .id = "input")
```

``` r
output = map_df(list_norm, mean_and_sd, .id = "input")
```

## List columns\!

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(samp)
```

    ## $a
    ##  [1] 2.8471850 3.5149426 3.3020559 3.9060270 1.5588106 2.2332932 2.5589464
    ##  [8] 4.2358351 4.8653109 3.4948497 3.8783812 1.3340420 2.9361319 2.7118602
    ## [15] 3.3922903 0.7779518 4.3086134 4.5792022 2.3598161 1.6087090
    ## 
    ## $b
    ##  [1]   1.84740593  -9.04457986   4.81941369  -7.08959599   5.91440280
    ##  [6]  -8.73440840   0.94878790   1.39555035   6.11788770  -7.61486462
    ## [11]  -2.83427069   3.05144834   3.53295048   0.95317073   5.69164626
    ## [16]   0.05135899   1.26831162   6.10468308 -14.01734964  -0.79846815
    ## [21]   5.90840137  -0.58535984   5.68570374   7.55126956   2.36847687
    ## [26]   6.52466565  -3.31551829   1.91388498  -0.75028792  10.09964666
    ## 
    ## $c
    ##  [1]  9.647301  9.553753 10.283060  9.840199  9.792614 10.206767 10.117874
    ##  [8] 10.006436  9.818057  9.924685  9.742157 10.107908 10.069922 10.222942
    ## [15]  9.577248 10.142217 10.101660  9.772435 10.160093  9.914335  9.959902
    ## [22] 10.076113  9.949468 10.344929 10.240823  9.840334  9.735253 10.009586
    ## [29] 10.259824  9.752850 10.011352 10.017627  9.989689 10.055548 10.161725
    ## [36]  9.997491 10.187177  9.811653  9.851531 10.399283
    ## 
    ## $d
    ##  [1] -2.975755 -3.795180 -1.430030 -2.284192 -3.339589 -2.326083 -1.857971
    ##  [8] -2.458228 -1.593871 -1.479584 -2.910343 -2.719754 -3.766439 -3.199232
    ## [15] -1.883778 -3.931557 -4.471980 -1.262156 -3.679937 -4.532960

``` r
listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 x 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try some operations.

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.02  1.14

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.899  5.67

Can I just … map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.02  1.14
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.899  5.67
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.99 0.208
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.79  1.03

So … can I add a list column??

``` r
listcol_df = 
  listcol_df %>% 
  mutate(
    summary = map(samp, mean_and_sd),
    medians = map_dbl(samp, median))
```

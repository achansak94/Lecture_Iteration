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
    ## -2.44989 -0.50713  0.03303  0.08277  0.77969  2.20056

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
    ##  [1] 2.199110 2.990561 4.360735 1.510566 2.564511 1.020860 3.683141 3.236842
    ##  [9] 2.859933 3.180636 2.256399 1.560948 2.683102 3.321027 4.785449 3.253850
    ## [17] 2.148680 4.265173 4.635522 2.200723
    ## 
    ## $b
    ##  [1]   3.46586893  -1.55518902   3.38903118  -4.76031337  -2.09406020
    ##  [6]   3.16887394  -2.61684138  -1.25989865   8.74083406   4.73784609
    ## [11]  11.76804755   4.66470900   6.79955493  -0.13681430  -7.48896406
    ## [16] -12.35167076   1.28953345  -5.24238426  -0.06955512  -1.02355582
    ## [21]  -2.54938916  -4.33106621   2.40677313   1.49746752   4.66939694
    ## [26]   4.87691976  -0.03178139  -3.85838830   8.41843719   0.61411744
    ## 
    ## $c
    ##  [1]  9.990701  9.666418 10.114576  9.859709 10.199648  9.772513 10.106011
    ##  [8]  9.952046 10.070319 10.131033 10.128289 10.162819 10.121213 10.268586
    ## [15] 10.106503 10.037681 10.002401 10.093357 10.239365 10.259066  9.936367
    ## [22]  9.843281  9.524812  9.744890 10.139882 10.180376  9.755659  9.949842
    ## [29]  9.922042 10.358978  9.903916 10.027404  9.946903 10.099439 10.001377
    ## [36]  9.849976  9.942548  9.874479  9.810577  9.981097
    ## 
    ## $d
    ##  [1] -1.605053 -3.747270 -3.244510 -2.385653 -2.259130 -4.379828 -2.998866
    ##  [8] -2.211158 -2.426019 -3.774790 -1.333292 -3.959872 -5.357464 -3.979813
    ## [15] -2.753054 -3.636214 -2.594371 -2.438369 -3.332502 -4.049941

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
    ## 1  2.94  1.05

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.705  5.14

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.177

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.12  1.00

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
    ##  [1] 2.199110 2.990561 4.360735 1.510566 2.564511 1.020860 3.683141 3.236842
    ##  [9] 2.859933 3.180636 2.256399 1.560948 2.683102 3.321027 4.785449 3.253850
    ## [17] 2.148680 4.265173 4.635522 2.200723
    ## 
    ## $b
    ##  [1]   3.46586893  -1.55518902   3.38903118  -4.76031337  -2.09406020
    ##  [6]   3.16887394  -2.61684138  -1.25989865   8.74083406   4.73784609
    ## [11]  11.76804755   4.66470900   6.79955493  -0.13681430  -7.48896406
    ## [16] -12.35167076   1.28953345  -5.24238426  -0.06955512  -1.02355582
    ## [21]  -2.54938916  -4.33106621   2.40677313   1.49746752   4.66939694
    ## [26]   4.87691976  -0.03178139  -3.85838830   8.41843719   0.61411744
    ## 
    ## $c
    ##  [1]  9.990701  9.666418 10.114576  9.859709 10.199648  9.772513 10.106011
    ##  [8]  9.952046 10.070319 10.131033 10.128289 10.162819 10.121213 10.268586
    ## [15] 10.106503 10.037681 10.002401 10.093357 10.239365 10.259066  9.936367
    ## [22]  9.843281  9.524812  9.744890 10.139882 10.180376  9.755659  9.949842
    ## [29]  9.922042 10.358978  9.903916 10.027404  9.946903 10.099439 10.001377
    ## [36]  9.849976  9.942548  9.874479  9.810577  9.981097
    ## 
    ## $d
    ##  [1] -1.605053 -3.747270 -3.244510 -2.385653 -2.259130 -4.379828 -2.998866
    ##  [8] -2.211158 -2.426019 -3.774790 -1.333292 -3.959872 -5.357464 -3.979813
    ## [15] -2.753054 -3.636214 -2.594371 -2.438369 -3.332502 -4.049941

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
    ## 1  2.94  1.05

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.705  5.14

Can I just … map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.94  1.05
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.705  5.14
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.177
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.12  1.00

So … can I add a list column??

``` r
listcol_df = 
  listcol_df %>% 
  mutate(
    summary = map(samp, mean_and_sd),
    medians = map_dbl(samp, median))
```

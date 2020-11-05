Assignment 4
================

``` r
library(tidyverse)
```

    ## -- Attaching packages ---------------------------------------------------------------- tidyverse 1.3.0 --

    ## √ ggplot2 3.3.2     √ purrr   0.3.4
    ## √ tibble  3.0.3     √ dplyr   1.0.2
    ## √ tidyr   1.1.1     √ stringr 1.4.0
    ## √ readr   1.3.1     √ forcats 0.5.0

    ## -- Conflicts ------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(microbenchmark)
library(parallel)
```

# HPC

  - Problem 1: Make sure your code is nice

Rewrite the following R functions to make them faster. It is OK (and
recommended) to take a look at Stackoverflow and Google

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}

fun1alt <- function(mat) {
  rowSums(mat)
}

# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}

fun2alt <- function(mat) {
  t(apply(mat, 1, cumsum))
}


# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min      lq     mean   median       uq       max neval cld
    ##     fun1(dat) 4.614964 5.89356 4.807424 6.076182 6.352554 0.5532151   100   b
    ##  fun1alt(dat) 1.000000 1.00000 1.000000 1.000000 1.000000 1.0000000   100  a

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min       lq     mean   median       uq      max neval cld
    ##     fun2(dat) 4.436567 4.282803 3.286629 3.946864 3.644403 0.629479   100   b
    ##  fun2alt(dat) 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000   100  a

  - Problem 2: Make things run faster with parallel computing

The following function allows simulating PI

``` r
sim_pi <- function(n = 1000, i = NULL) {
  p <- matrix(runif(n*2), ncol = 2)
  mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000) # 3.132
```

    ## [1] 3.132

In order to get accurate estimates, we can run this function multiple
times, with the following code:

``` r
# This runs the simulation a 4,000 times, each with 10,000 points
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##    3.96    0.01    3.97

Rewrite the previous code using parLapply() to make it run faster. Make
sure you set the seed using clusterSetRNGStream():

``` r
cl <- parallel::makePSOCKcluster(2L)
system.time({
  parallel::clusterSetRNGStream(cl, 1231)
  ans <- unlist(parLapply(cl, 1:4000, sim_pi, n = 10000))
  print(mean(ans))
  parallel::stopCluster(cl)
})
```

    ## [1] 3.141577

    ##    user  system elapsed 
    ##    0.00    0.00    2.75

# SQL

``` r
# install.packages(c("RSQLite", "DBI"))

library(RSQLite)
```

    ## Warning: package 'RSQLite' was built under R version 4.0.3

``` r
library(DBI)
```

    ## Warning: package 'DBI' was built under R version 4.0.3

``` r
# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)
```

``` r
dbListTables(con)
```

    ## [1] "category"      "film"          "film_category"

### Question 1

``` sql
PRAGMA table_info(film)
```

<div class="knitsql-table">

| cid | name                   | type    | notnull | dflt\_value | pk |
| :-- | :--------------------- | :------ | ------: | :---------- | -: |
| 0   | film\_id               | INTEGER |       0 | NA          |  0 |
| 1   | title                  | TEXT    |       0 | NA          |  0 |
| 2   | description            | TEXT    |       0 | NA          |  0 |
| 3   | release\_year          | INTEGER |       0 | NA          |  0 |
| 4   | language\_id           | INTEGER |       0 | NA          |  0 |
| 5   | original\_language\_id | INTEGER |       0 | NA          |  0 |
| 6   | rental\_duration       | INTEGER |       0 | NA          |  0 |
| 7   | rental\_rate           | REAL    |       0 | NA          |  0 |
| 8   | length                 | INTEGER |       0 | NA          |  0 |
| 9   | replacement\_cost      | REAL    |       0 | NA          |  0 |

Displaying records 1 - 10

</div>

  - Below is the table showing the number of movies available in each
    rating category.

<!-- end list -->

``` sql
SELECT rating, COUNT(*) AS count
FROM film
GROUP by rating
```

<div class="knitsql-table">

| rating | count |
| :----- | ----: |
| G      |   180 |
| NC-17  |   210 |
| PG     |   194 |
| PG-13  |   223 |
| R      |   195 |

5 records

</div>

### Question 2

  - Below is the table showing the average replacement cost and rental
    rate for each rating
category.

<!-- end list -->

``` sql
SELECT rating, AVG(rental_rate) AS avg_rental_rate, AVG(replacement_cost) AS avg_replacement_cost
FROM film
GROUP by rating
ORDER by AVG(rental_rate)
```

<div class="knitsql-table">

| rating | avg\_rental\_rate | avg\_replacement\_cost |
| :----- | ----------------: | ---------------------: |
| G      |          2.912222 |               20.12333 |
| R      |          2.938718 |               20.23103 |
| NC-17  |          2.970952 |               20.13762 |
| PG-13  |          3.034843 |               20.40256 |
| PG     |          3.051856 |               18.95907 |

5 records

</div>

### Question 3

``` sql
PRAGMA table_info(film_category)
```

<div class="knitsql-table">

| cid | name         | type    | notnull | dflt\_value | pk |
| :-- | :----------- | :------ | ------: | :---------- | -: |
| 0   | film\_id     | INTEGER |       0 | NA          |  0 |
| 1   | category\_id | INTEGER |       0 | NA          |  0 |
| 2   | last\_update | TEXT    |       0 | NA          |  0 |

3 records

</div>

  - Below is the table showing the number of films with each category
    ID.

<!-- end list -->

``` sql
SELECT fc.category_id, COUNT(f.film_id) AS count_film
FROM film AS f 
  INNER JOIN film_category AS fc
  ON f.film_id = fc.film_id
GROUP by fc.category_id
```

<div class="knitsql-table">

| category\_id | count\_film |
| :----------- | ----------: |
| 1            |          64 |
| 2            |          66 |
| 3            |          60 |
| 4            |          57 |
| 5            |          58 |
| 6            |          68 |
| 7            |          62 |
| 8            |          69 |
| 9            |          73 |
| 10           |          61 |

Displaying records 1 - 10

</div>

### Quention 4

``` sql
PRAGMA table_info(category)
```

<div class="knitsql-table">

| cid | name         | type    | notnull | dflt\_value | pk |
| :-- | :----------- | :------ | ------: | :---------- | -: |
| 0   | category\_id | INTEGER |       0 | NA          |  0 |
| 1   | name         | TEXT    |       0 | NA          |  0 |
| 2   | last\_update | TEXT    |       0 | NA          |  0 |

3 records

</div>

  - Sports is the name of the most popular category.

<!-- end list -->

``` sql
SELECT fc.category_id, COUNT(f.film_id) AS count_film, c.name
FROM film AS f 
  INNER JOIN film_category AS fc
  ON f.film_id = fc.film_id
  INNER JOIN category AS c
  ON fc.category_id = c.category_id
GROUP by fc.category_id
ORDER by COUNT(f.film_id) DESC
```

<div class="knitsql-table">

| category\_id | count\_film | name        |
| -----------: | ----------: | :---------- |
|           15 |          74 | Sports      |
|            9 |          73 | Foreign     |
|            8 |          69 | Family      |
|            6 |          68 | Documentary |
|            2 |          66 | Animation   |
|            1 |          64 | Action      |
|           13 |          63 | New         |
|            7 |          62 | Drama       |
|           14 |          61 | Sci-Fi      |
|           10 |          61 | Games       |

Displaying records 1 - 10

</div>

### Clean up

``` r
dbDisconnect(con)
```

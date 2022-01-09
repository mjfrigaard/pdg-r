
Hi everyone (and thank you in advance for you time :)). I am working with `purrr` 
and have a question about converting a list to a tibble. Below is an example list:

``` r
library(purrr)
library(tibble)

my_list <- list(
    list(1, 
         x = 1.5, 
         y = c(2), 
         z = "A"),
    list(2, 
         x = 4.2, 
         y = c(5, 6), 
         z = "B"),
    list(3, 
         x = 8.8, 
         y = c(9, 10, 11))
)
```

When I pipe my_list to `tibble()` directly with `purrr::map()`, I get the following output:

```r
my_list %>% 
    tibble(x = map(., "x"),
           y = map(., "y"),
           z = map(., "z"))
#> # A tibble: 3 × 4
#>   .                x         y         z        
#>   <list>           <list>    <list>    <list>   
#> 1 <named list [4]> <dbl [1]> <dbl [1]> <chr [1]>
#> 2 <named list [4]> <dbl [1]> <dbl [2]> <chr [1]>
#> 3 <named list [3]> <dbl [1]> <dbl [3]> <NULL>
```

This `.` is a 'named list'. but I am not sure why the `.` is included in the construction of the tibble. I know I can remove this behavior by adding some curly brackets:

```r
my_list %>% {
    tibble(x = map(., "x"),
           y = map(., "y"),
           z = map(., "z"))}
#> # A tibble: 3 × 3
#>   x         y         z        
#>   <list>    <list>    <list>   
#> 1 <dbl [1]> <dbl [1]> <chr [1]>
#> 2 <dbl [1]> <dbl [2]> <chr [1]>
#> 3 <dbl [1]> <dbl [3]> <NULL>
```

So, I know *how* to get the behavior I want, but I am not sure *why* I get this behavior. Any help or explanation would be greatly appreciated! 

Thank you! 

<sup>Created on 2022-01-03 by the [reprex package](https://reprex.tidyverse.org) (v2.0.1)</sup>
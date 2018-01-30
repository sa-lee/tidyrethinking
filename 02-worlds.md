> All statistical modelling has these sames two frames: the *small
> world* of the model itselt and the *large world* we hope to deploy the
> model in.

-   small world = the logic and assumptions of the model
-   large world = the broad context of the deployed model, the model is
    never a full representation of the large world

Baysian inference is counting and comparing possibilities.

Example: marbles in a bag
-------------------------

Here’s some code to create the possible conjectures for the bag of 4
marbles containing some combination of black and white marbles.

    white_marble <- "\U25EF"
    black_marble <- "\U25C9"
    observed <- c(black_marble, white_marble, black_marble)

    layout_bag <- function(p, n_marbles = 4) {
      stopifnot(!(p - n_marbles > 1))
      n_black <- min(p, n_marbles + 1) - 1
      n_white <- n_marbles - n_black
      c(rep(black_marble, n_black), rep(white_marble, n_white))
    }

    conjecture <- tibble(possibility = 1:5,
                         bag = map(possibility, layout_bag),
                         elegant_bag = map_chr(bag, ~ str_c(.x, collapse = "")))
    conjecture

    ## # A tibble: 5 x 3
    ##   possibility bag       elegant_bag
    ##         <int> <list>    <chr>      
    ## 1           1 <chr [4]> ◯◯◯◯       
    ## 2           2 <chr [4]> ◉◯◯◯       
    ## 3           3 <chr [4]> ◉◉◯◯       
    ## 4           4 <chr [4]> ◉◉◉◯       
    ## 5           5 <chr [4]> ◉◉◉◉

We observe the following sequence of marbles from the bag: ◉◯◉.

There are 1 possible ways of choosing these 3 marbles from the bag.

Now for each conjecture we can count the number of ways of producing
◉◯◉.

    number_of_ways <- function(observed, bag) {
      if (!all(observed %in% bag)) return(0L)
      counter <- 1L
      for (i in seq_along(observed)) {
        counter <- counter * sum(bag %in% observed[i])
      }
      counter
    }
    conjecture <- conjecture %>% 
      mutate(n_ways = map_int(bag, ~number_of_ways(observed, .x)))
    conjecture

    ## # A tibble: 5 x 4
    ##   possibility bag       elegant_bag n_ways
    ##         <int> <list>    <chr>        <int>
    ## 1           1 <chr [4]> ◯◯◯◯             0
    ## 2           2 <chr [4]> ◉◯◯◯             3
    ## 3           3 <chr [4]> ◉◉◯◯             8
    ## 4           4 <chr [4]> ◉◉◉◯             9
    ## 5           5 <chr [4]> ◉◉◉◉             0

We have supposed that each conjecture is equally plausible, so if we
observed another ◉ we could simply include add this to ◉◯◉ and rerun the
`number_of_ways` function.

    conjecture %>%
      mutate(new_count = map_int(bag, 
                                 ~number_of_ways(c(observed, black_marble), .x)))

    ## # A tibble: 5 x 5
    ##   possibility bag       elegant_bag n_ways new_count
    ##         <int> <list>    <chr>        <int>     <int>
    ## 1           1 <chr [4]> ◯◯◯◯             0         0
    ## 2           2 <chr [4]> ◉◯◯◯             3         3
    ## 3           3 <chr [4]> ◉◉◯◯             8        16
    ## 4           4 <chr [4]> ◉◉◉◯             9        27
    ## 5           5 <chr [4]> ◉◉◉◉             0         0

Alternately, we could use our previously computed counts as a prior and
multiply it by the number of ways to produce ◉ for each possible bag.

    conjecture <- conjecture %>%
      mutate(n_ways_new_obs = map_int(bag, ~sum(.x %in% black_marble)),
             new_count = n_ways * n_ways_new_obs)
    conjecture 

    ## # A tibble: 5 x 6
    ##   possibility bag       elegant_bag n_ways n_ways_new_obs new_count
    ##         <int> <list>    <chr>        <int>          <int>     <int>
    ## 1           1 <chr [4]> ◯◯◯◯             0              0         0
    ## 2           2 <chr [4]> ◉◯◯◯             3              1         3
    ## 3           3 <chr [4]> ◉◉◯◯             8              2        16
    ## 4           4 <chr [4]> ◉◉◉◯             9              3        27
    ## 5           5 <chr [4]> ◉◉◉◉             0              4         0

What if we were told by the marble factory that black marbles are rare,
then for every ◉◉◉◯ there are two ◉◉◯◯ and three ◉◯◯◯. Then we can
update our counts using the same logic as before:

    conjecture <- conjecture %>%
      select(-new_count) %>%
      mutate(factory_count = c(0L, 3L:1L, 0L),
             new_count = n_ways*n_ways_new_obs*factory_count)
    conjecture

    ## # A tibble: 5 x 7
    ##   possibility bag       elegant_bag n_ways n_ways_new_obs factory_count
    ##         <int> <list>    <chr>        <int>          <int>         <int>
    ## 1           1 <chr [4]> ◯◯◯◯             0              0             0
    ## 2           2 <chr [4]> ◉◯◯◯             3              1             3
    ## 3           3 <chr [4]> ◉◉◯◯             8              2             2
    ## 4           4 <chr [4]> ◉◉◉◯             9              3             1
    ## 5           5 <chr [4]> ◉◉◉◉             0              4             0
    ## # ... with 1 more variable: new_count <int>

From counts to probability, the mantra of Bayes:

posterior ∝ likelihodd \* prior

In the marbles example this means the plausibility of a bag given the
observed sequence ◉◯◉ depends on the ways a bag can produce ◉◯◉
multiplied by the prior plausibility of the bag.

    conjecture_probs <- conjecture %>%
      mutate(proportion_black_marble = n_ways_new_obs / max(n_ways_new_obs),
        posterior_observed = n_ways / sum(n_ways)) %>%
      select(elegant_bag, proportion_black_marble, n_ways,  posterior_observed)
    conjecture_probs

    ## # A tibble: 5 x 4
    ##   elegant_bag proportion_black_marble n_ways posterior_observed
    ##   <chr>                         <dbl>  <int>              <dbl>
    ## 1 ◯◯◯◯                          0          0              0    
    ## 2 ◉◯◯◯                          0.250      3              0.150
    ## 3 ◉◉◯◯                          0.500      8              0.400
    ## 4 ◉◉◉◯                          0.750      9              0.450
    ## 5 ◉◉◉◉                          1.00       0              0

Building a Model
----------------

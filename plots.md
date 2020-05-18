### Load Packages and Data

``` r
library(pacman)
pacman::p_load(
  tidyverse,
  gridExtra,
  patchwork, formattable
)
### Load data ###
d000 <- read_csv("Results/saved_results_000.csv") %>% mutate(pb = 0)
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   analysis_type = col_character(),
    ##   b_sex_p_value = col_logical(),
    ##   b_cond_p_value = col_logical(),
    ##   b_sex_cond_p_value = col_logical()
    ## )

    ## See spec(...) for full column specifications.

``` r
d001 <- read_csv("Results/saved_results_001.csv") %>% mutate(pb = 1) 
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   analysis_type = col_character(),
    ##   b_sex_p_value = col_logical(),
    ##   b_cond_p_value = col_logical(),
    ##   b_sex_cond_p_value = col_logical()
    ## )
    ## See spec(...) for full column specifications.

``` r
d200 <- read_csv("Results/saved_results_200.csv") %>% mutate(pb = 0)
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   analysis_type = col_character(),
    ##   b_sex_p_value = col_logical(),
    ##   b_cond_p_value = col_logical(),
    ##   b_sex_cond_p_value = col_logical()
    ## )
    ## See spec(...) for full column specifications.

``` r
d201 <- read_csv("Results/saved_results_201.csv") %>% mutate(pb = 1)
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   analysis_type = col_character(),
    ##   b_sex_p_value = col_logical(),
    ##   b_cond_p_value = col_logical(),
    ##   b_sex_cond_p_value = col_logical()
    ## )
    ## See spec(...) for full column specifications.

``` r
d400 <- read_csv("Results/saved_results_400.csv") %>% mutate(pb = 0)
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   analysis_type = col_character(),
    ##   b_sex_p_value = col_logical(),
    ##   b_cond_p_value = col_logical(),
    ##   b_sex_cond_p_value = col_logical()
    ## )
    ## See spec(...) for full column specifications.

``` r
d401 <- read_csv("Results/saved_results_401.csv") %>% mutate(pb = 1)
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   analysis_type = col_character(),
    ##   b_sex_p_value = col_logical(),
    ##   b_cond_p_value = col_logical(),
    ##   b_sex_cond_p_value = col_logical()
    ## )
    ## See spec(...) for full column specifications.

``` r
#merge data
d <- rbind(d000, d001, d200, d201, d400, d401)         
d <- d %>% select(-c(X1))

### Load meta data ###
meta000 <- read_csv("Results/meta_analysis_results_000.csv") %>% mutate(pb = 0)
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   meta_repeat_id = col_double(),
    ##   n_exp = col_double(),
    ##   true_effect = col_double(),
    ##   b_sex_cond_meta = col_double(),
    ##   b_sex_cond_lower_meta = col_double(),
    ##   b_sex_cond_upper_meta = col_double(),
    ##   b_sex_cond_error_meta = col_double()
    ## )

``` r
meta001 <- read_csv("Results/meta_analysis_results_001.csv") %>% mutate(pb = 1) 
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   meta_repeat_id = col_double(),
    ##   n_exp = col_double(),
    ##   true_effect = col_double(),
    ##   b_sex_cond_meta = col_double(),
    ##   b_sex_cond_lower_meta = col_double(),
    ##   b_sex_cond_upper_meta = col_double(),
    ##   b_sex_cond_error_meta = col_double()
    ## )

``` r
meta200 <- read_csv("Results/meta_analysis_results_200.csv") %>% mutate(pb = 0)
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   meta_repeat_id = col_double(),
    ##   n_exp = col_double(),
    ##   true_effect = col_double(),
    ##   b_sex_cond_meta = col_double(),
    ##   b_sex_cond_lower_meta = col_double(),
    ##   b_sex_cond_upper_meta = col_double(),
    ##   b_sex_cond_error_meta = col_double()
    ## )

``` r
meta201 <- read_csv("Results/meta_analysis_results_201.csv") %>% mutate(pb = 1)
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   meta_repeat_id = col_double(),
    ##   n_exp = col_double(),
    ##   true_effect = col_double(),
    ##   b_sex_cond_meta = col_double(),
    ##   b_sex_cond_lower_meta = col_double(),
    ##   b_sex_cond_upper_meta = col_double(),
    ##   b_sex_cond_error_meta = col_double()
    ## )

``` r
meta400 <- read_csv("Results/meta_analysis_results_400.csv") %>% mutate(pb = 0)
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   meta_repeat_id = col_double(),
    ##   n_exp = col_double(),
    ##   true_effect = col_double(),
    ##   b_sex_cond_meta = col_double(),
    ##   b_sex_cond_lower_meta = col_double(),
    ##   b_sex_cond_upper_meta = col_double(),
    ##   b_sex_cond_error_meta = col_double()
    ## )

``` r
meta401 <- read_csv("Results/meta_analysis_results_401.csv") %>% mutate(pb = 1)
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   meta_repeat_id = col_double(),
    ##   n_exp = col_double(),
    ##   true_effect = col_double(),
    ##   b_sex_cond_meta = col_double(),
    ##   b_sex_cond_lower_meta = col_double(),
    ##   b_sex_cond_upper_meta = col_double(),
    ##   b_sex_cond_error_meta = col_double()
    ## )

``` r
#merge data
meta <- rbind(meta000, meta001, meta200, meta201, meta400, meta401) %>% mutate(analysis_type = "meta-analysis")
```

### Summaries

Creating summary table for appendix (pp, bglmm, and bskep)

``` r
# Summary of estimates from last pp
d_last_pp <- d %>%  group_by(true_sex_cond, pb) %>% 
  filter(expt == max(expt), analysis_type == "pp") %>% 
  summarise(
    estimate = b_sex_cond_med,
    CI_lower = b_sex_cond_lower,
    CI_upper = b_sex_cond_upper, 
    analysis_type = "pp"
    )

# Summary of estimates from bglmm and bskep
d_mean <- d %>% filter(analysis_type != "pp") %>% 
  group_by(true_sex_cond, pb, analysis_type) %>% 
  summarize(
    estimate = mean(b_sex_cond_med),
    CI_lower = mean(b_sex_cond_lower),
    CI_upper = mean(b_sex_cond_upper)
  )

# Merging summaries
d_summarized <- rbind(d_last_pp, d_mean) %>% 
  mutate(
    dist = estimate - (boot::inv.logit(true_sex_cond) - 0.5),
    dist_lower = CI_lower - (boot::inv.logit(true_sex_cond) - 0.5),
    dist_upper = CI_upper - (boot::inv.logit(true_sex_cond) - 0.5)
  )

# Make pretty
d_summarized <- d_summarized[order(d_summarized$analysis_type, d_summarized$pb, d_summarized$true_sex_cond),]

format_summarized <- d_summarized %>% 
  formattable()
```

Creating summary table for appendix (meta-analysis and pp)

``` r
# Summarizing estimates for metaanalysis 
meta_pp <- meta %>% rename(
    estimate = b_sex_cond_meta,
    CI_lower = b_sex_cond_lower_meta,
    CI_upper = b_sex_cond_upper_meta,
    true_sex_cond = true_effect
  ) %>% select(
    analysis_type, pb, estimate, CI_lower, CI_upper, true_sex_cond
  )

# Adding estimate from last pp (and ensuring correct scale)
meta_pp <- rbind(as.tibble(d_last_pp), meta_pp) %>% #see regular for d_last_pp
  mutate(
    dist = estimate - (boot::inv.logit(true_sex_cond) - 0.5),
    dist_lower = CI_lower - (boot::inv.logit(true_sex_cond) - 0.5),
    dist_upper = CI_upper - (boot::inv.logit(true_sex_cond) - 0.5)
  )
```

    ## Warning: `as.tibble()` is deprecated, use `as_tibble()` (but mind the new semantics).
    ## This warning is displayed once per session.

``` r
# Make table
meta_pp %>% formattable()
```

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:right;">
true\_sex\_cond
</th>
<th style="text-align:right;">
pb
</th>
<th style="text-align:right;">
estimate
</th>
<th style="text-align:right;">
CI\_lower
</th>
<th style="text-align:right;">
CI\_upper
</th>
<th style="text-align:right;">
analysis\_type
</th>
<th style="text-align:right;">
dist
</th>
<th style="text-align:right;">
dist\_lower
</th>
<th style="text-align:right;">
dist\_upper
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
-0.010188163
</td>
<td style="text-align:right;">
-0.0216641302
</td>
<td style="text-align:right;">
0.0009267517
</td>
<td style="text-align:right;">
pp
</td>
<td style="text-align:right;">
-0.010188163
</td>
<td style="text-align:right;">
-0.0216641302
</td>
<td style="text-align:right;">
0.0009267517
</td>
</tr>
<tr>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.024534412
</td>
<td style="text-align:right;">
0.0022355228
</td>
<td style="text-align:right;">
0.0456771370
</td>
<td style="text-align:right;">
pp
</td>
<td style="text-align:right;">
0.024534412
</td>
<td style="text-align:right;">
0.0022355228
</td>
<td style="text-align:right;">
0.0456771370
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.224249987
</td>
<td style="text-align:right;">
0.2116531677
</td>
<td style="text-align:right;">
0.2362869348
</td>
<td style="text-align:right;">
pp
</td>
<td style="text-align:right;">
-0.006808591
</td>
<td style="text-align:right;">
-0.0194054109
</td>
<td style="text-align:right;">
0.0052283561
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.212006965
</td>
<td style="text-align:right;">
0.1879289551
</td>
<td style="text-align:right;">
0.2378532720
</td>
<td style="text-align:right;">
pp
</td>
<td style="text-align:right;">
-0.019051614
</td>
<td style="text-align:right;">
-0.0431296236
</td>
<td style="text-align:right;">
0.0067946933
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.370385352
</td>
<td style="text-align:right;">
0.3566891882
</td>
<td style="text-align:right;">
0.3838209864
</td>
<td style="text-align:right;">
pp
</td>
<td style="text-align:right;">
-0.010411725
</td>
<td style="text-align:right;">
-0.0241078898
</td>
<td style="text-align:right;">
0.0030239085
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.365094091
</td>
<td style="text-align:right;">
0.3448575499
</td>
<td style="text-align:right;">
0.3848886318
</td>
<td style="text-align:right;">
pp
</td>
<td style="text-align:right;">
-0.015702987
</td>
<td style="text-align:right;">
-0.0359395281
</td>
<td style="text-align:right;">
0.0040915538
</td>
</tr>
<tr>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
-0.004528068
</td>
<td style="text-align:right;">
-0.0190726479
</td>
<td style="text-align:right;">
0.0094700098
</td>
<td style="text-align:right;">
meta-analysis
</td>
<td style="text-align:right;">
-0.004528068
</td>
<td style="text-align:right;">
-0.0190726479
</td>
<td style="text-align:right;">
0.0094700098
</td>
</tr>
<tr>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.015598431
</td>
<td style="text-align:right;">
0.0009166396
</td>
<td style="text-align:right;">
0.0309948604
</td>
<td style="text-align:right;">
meta-analysis
</td>
<td style="text-align:right;">
0.015598431
</td>
<td style="text-align:right;">
0.0009166396
</td>
<td style="text-align:right;">
0.0309948604
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.159726328
</td>
<td style="text-align:right;">
0.1469865301
</td>
<td style="text-align:right;">
0.1727636996
</td>
<td style="text-align:right;">
meta-analysis
</td>
<td style="text-align:right;">
-0.071332251
</td>
<td style="text-align:right;">
-0.0840720485
</td>
<td style="text-align:right;">
-0.0582948791
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.142155179
</td>
<td style="text-align:right;">
0.1194792708
</td>
<td style="text-align:right;">
0.1639957677
</td>
<td style="text-align:right;">
meta-analysis
</td>
<td style="text-align:right;">
-0.088903400
</td>
<td style="text-align:right;">
-0.1115793078
</td>
<td style="text-align:right;">
-0.0670628109
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.270717210
</td>
<td style="text-align:right;">
0.2576771399
</td>
<td style="text-align:right;">
0.2840308102
</td>
<td style="text-align:right;">
meta-analysis
</td>
<td style="text-align:right;">
-0.110079868
</td>
<td style="text-align:right;">
-0.1231199381
</td>
<td style="text-align:right;">
-0.0967662678
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.271205453
</td>
<td style="text-align:right;">
0.2525248207
</td>
<td style="text-align:right;">
0.2899425019
</td>
<td style="text-align:right;">
meta-analysis
</td>
<td style="text-align:right;">
-0.109591625
</td>
<td style="text-align:right;">
-0.1282722573
</td>
<td style="text-align:right;">
-0.0908545760
</td>
</tr>
</tbody>
</table>
### Plots

Plot that compares estimates from bglmm, bskep, and pp across all experiments (without publication bias)

``` r
# Remove data with publication bias
d_pb0 <- d[d$pb == 0,]

#### PLOTTING ####

# Plotting true effect 0
te0_pb0_plot <- ggplot(data = d_pb0[d_pb0$true_sex_cond == 0,]) + 
  geom_point(aes(x = expt, y = b_sex_cond_med, color = analysis_type), size = 1) +
  geom_errorbar(aes(x = expt, ymax = b_sex_cond_upper, ymin = b_sex_cond_lower, color = analysis_type, width = 1), alpha = 0.5, size = 0.8) +
  geom_line(aes(x = as.numeric(expt), y = (boot::inv.logit(true_sex_cond) - boot::inv.logit(true_base))), 
            color = 'black', size = 1) +
  scale_color_manual(values=c("goldenrod2", "seagreen4", "red")) +
  labs(x = "Experiment number", y = "Interaction Estimate", color = "Analysis Type")  + 
  ggtitle("A") + 
  theme_minimal()

# Plotting true effect 1
te1_pb0_plot <- ggplot(data = d_pb0[d_pb0$true_sex_cond == 1,]) + 
  geom_point(aes(x = expt, y = b_sex_cond_med, color = analysis_type), size = 1) +
  geom_errorbar(aes(x = expt, ymax = b_sex_cond_upper, ymin = b_sex_cond_lower, color = analysis_type, width = 1), alpha = 0.5, size = 0.8) +
  geom_line(aes(x = as.numeric(expt), y = (boot::inv.logit(true_sex_cond) - boot::inv.logit(true_base))), 
            color = 'black', size = 1) +
  scale_color_manual(values=c("goldenrod2", "seagreen4", "red")) +
  labs(x = "Experiment number", y = "Interaction Estimate", color = "Analysis Type") + 
  ggtitle("B") + 
  theme_minimal()

# Plotting true effect 2
te2_pb0_plot <- ggplot(data =  d_pb0[d_pb0$true_sex_cond == 2,]) + 
  geom_point(aes(x = expt, y = b_sex_cond_med, color = analysis_type), size = 1) +
  geom_errorbar(aes(x = expt, ymax = b_sex_cond_upper, ymin = b_sex_cond_lower, color = analysis_type, width = 1), alpha = 0.5, size = 0.8) +
  geom_line(aes(x = as.numeric(expt), y = (boot::inv.logit(true_sex_cond) - boot::inv.logit(true_base))), 
            color = 'black', size = 1) +
  scale_color_manual(values=c("goldenrod2", "seagreen4", "red")) +
  labs(x = "Experiment number", y = "Interaction Estimate", color = "Analysis Type") + 
  ggtitle("C") + 
  theme_minimal()


# Arranging plots
combined_te_pb0_plot <- te0_pb0_plot + te1_pb0_plot + te2_pb0_plot & theme(legend.position = "bottom")
combined_te_pb0_plot + plot_layout(guides = "collect")
```

![](plots_files/figure-markdown_github/figure%202-1.png)

Plot that compares distance from true effect of meta-analysis vs. pp (without publication bias)

``` r
# Calculating distance from true effect
d$abs_dist <- abs(d$b_sex_cond_med - (boot::inv.logit(d$true_sex_cond)-boot::inv.logit(d$true_base))) #absolute distance
d$dist <- d$b_sex_cond_med - (boot::inv.logit(d$true_sex_cond)-boot::inv.logit(d$true_base)) #distance

# Calculating CIs
d$b_sex_cond_lower <- d$b_sex_cond_lower - (boot::inv.logit(d$true_sex_cond)-boot::inv.logit(d$true_base))
d$b_sex_cond_upper <- d$b_sex_cond_upper - (boot::inv.logit(d$true_sex_cond)-boot::inv.logit(d$true_base))


#### PLOTTING ####

ggplot(data = meta_pp[meta_pp$pb == 0,]) +  
  geom_point(aes(x = true_sex_cond, y = dist, color = analysis_type), size = 2) +
  geom_errorbar(aes(x = true_sex_cond, ymax = dist_upper, ymin = dist_lower, 
                    color = analysis_type), width = 0.1, alpha = 0.5, size = 1) +
  geom_line(aes(x = true_sex_cond, y = 0), 
            color = 'black', size = 1) +
  scale_color_manual(values=c("royalblue", "red")) +
  labs(x = "True Interaction Effect", y = "Distance from True Interaction Estimate", color = "Analysis Type") +
  theme_minimal()
```

![](plots_files/figure-markdown_github/figure%203-1.png)

Plot that compares distance from true effect of pp with/without publication bias

``` r
# With/without publication bias
d_pb0 <- d[d$pb == 0,]
d_pb1 <- d[d$pb == 1,]

#### PLOTTING ####

# pp distance from true effect (without publication bias)
pp_pb0_plot <- ggplot(data = d_pb0[d_pb0$analysis_type == "pp",], aes(x = expt, y = abs_dist, color = as.factor(true_sex_cond)), size = 1) + 
  geom_point() + geom_line(size = 0.3) +  
  scale_color_manual(values=c("cyan4", "darkgoldenrod1", "violetred2")) +
  labs(x = "Experiment number", y = "Distance from true effect", color = "True Effect",
       title = "A")  + 
  scale_y_continuous(limit= c(0, 0.11)) +
  theme_minimal() + theme(legend.position = "bottom") 

# pp distance from true effect (with publication bias)
pp_pb1_plot <- ggplot(data = d_pb1[d_pb1$analysis_type == "pp",], aes(x = expt, y = abs_dist, color = as.factor(true_sex_cond)), size = 1) + 
  geom_point() + geom_line(size = 0.3) + 
  scale_color_manual(values=c("cyan4", "darkgoldenrod1", "violetred2")) +
  labs(x = "Experiment number", y = "Distance from true effect", color = "True Effect", 
       title = "B")  + 
  scale_y_continuous(limit= c(0, 0.11)) +
  theme_minimal() + theme(legend.position = "bottom") 

# Arranging plots
pp_pb_plot <- pp_pb0_plot + pp_pb1_plot & theme(legend.position = "bottom")
pp_pb_plot + plot_layout(guides = "collect") 
```

![](plots_files/figure-markdown_github/publication%20bias-1.png)

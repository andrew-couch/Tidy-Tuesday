TidyTuesday Spotify 1/21/20
================
Andrew Couch
1/21/2020

## R Markdown

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------------- tidyverse 1.3.0 --

    ## <U+2713> ggplot2 3.2.1     <U+2713> purrr   0.3.3
    ## <U+2713> tibble  2.1.3     <U+2713> dplyr   0.8.3
    ## <U+2713> tidyr   1.0.0     <U+2713> stringr 1.4.0
    ## <U+2713> readr   1.3.1     <U+2713> forcats 0.4.0

    ## -- Conflicts ----------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   track_id = col_character(),
    ##   track_name = col_character(),
    ##   track_artist = col_character(),
    ##   track_album_id = col_character(),
    ##   track_album_name = col_character(),
    ##   track_album_release_date = col_character(),
    ##   playlist_name = col_character(),
    ##   playlist_id = col_character(),
    ##   playlist_genre = col_character(),
    ##   playlist_subgenre = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
#Basic understanding of dataset 
spotify_songs %>% glimpse()
```

    ## Observations: 32,833
    ## Variables: 23
    ## $ track_id                 <chr> "6f807x0ima9a1j3VPbc7VN", "0r7CVbZTWZgbTCYdf…
    ## $ track_name               <chr> "I Don't Care (with Justin Bieber) - Loud Lu…
    ## $ track_artist             <chr> "Ed Sheeran", "Maroon 5", "Zara Larsson", "T…
    ## $ track_popularity         <dbl> 66, 67, 70, 60, 69, 67, 62, 69, 68, 67, 58, …
    ## $ track_album_id           <chr> "2oCs0DGTsRO98Gh5ZSl2Cx", "63rPSO264uRjW1X5E…
    ## $ track_album_name         <chr> "I Don't Care (with Justin Bieber) [Loud Lux…
    ## $ track_album_release_date <chr> "2019-06-14", "2019-12-13", "2019-07-05", "2…
    ## $ playlist_name            <chr> "Pop Remix", "Pop Remix", "Pop Remix", "Pop …
    ## $ playlist_id              <chr> "37i9dQZF1DXcZDD7cfEKhW", "37i9dQZF1DXcZDD7c…
    ## $ playlist_genre           <chr> "pop", "pop", "pop", "pop", "pop", "pop", "p…
    ## $ playlist_subgenre        <chr> "dance pop", "dance pop", "dance pop", "danc…
    ## $ danceability             <dbl> 0.748, 0.726, 0.675, 0.718, 0.650, 0.675, 0.…
    ## $ energy                   <dbl> 0.916, 0.815, 0.931, 0.930, 0.833, 0.919, 0.…
    ## $ key                      <dbl> 6, 11, 1, 7, 1, 8, 5, 4, 8, 2, 6, 8, 1, 5, 5…
    ## $ loudness                 <dbl> -2.634, -4.969, -3.432, -3.778, -4.672, -5.3…
    ## $ mode                     <dbl> 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0,…
    ## $ speechiness              <dbl> 0.0583, 0.0373, 0.0742, 0.1020, 0.0359, 0.12…
    ## $ acousticness             <dbl> 0.10200, 0.07240, 0.07940, 0.02870, 0.08030,…
    ## $ instrumentalness         <dbl> 0.00e+00, 4.21e-03, 2.33e-05, 9.43e-06, 0.00…
    ## $ liveness                 <dbl> 0.0653, 0.3570, 0.1100, 0.2040, 0.0833, 0.14…
    ## $ valence                  <dbl> 0.518, 0.693, 0.613, 0.277, 0.725, 0.585, 0.…
    ## $ tempo                    <dbl> 122.036, 99.972, 124.008, 121.956, 123.976, …
    ## $ duration_ms              <dbl> 194754, 162600, 176616, 169093, 189052, 1630…

``` r
#Probably don't need track_id, tack_album_id, playlist_id 
df <- spotify_songs %>% 
  select(-contains("id")) %>%
  glimpse()
```

    ## Observations: 32,833
    ## Variables: 20
    ## $ track_name               <chr> "I Don't Care (with Justin Bieber) - Loud Lu…
    ## $ track_artist             <chr> "Ed Sheeran", "Maroon 5", "Zara Larsson", "T…
    ## $ track_popularity         <dbl> 66, 67, 70, 60, 69, 67, 62, 69, 68, 67, 58, …
    ## $ track_album_name         <chr> "I Don't Care (with Justin Bieber) [Loud Lux…
    ## $ track_album_release_date <chr> "2019-06-14", "2019-12-13", "2019-07-05", "2…
    ## $ playlist_name            <chr> "Pop Remix", "Pop Remix", "Pop Remix", "Pop …
    ## $ playlist_genre           <chr> "pop", "pop", "pop", "pop", "pop", "pop", "p…
    ## $ playlist_subgenre        <chr> "dance pop", "dance pop", "dance pop", "danc…
    ## $ danceability             <dbl> 0.748, 0.726, 0.675, 0.718, 0.650, 0.675, 0.…
    ## $ energy                   <dbl> 0.916, 0.815, 0.931, 0.930, 0.833, 0.919, 0.…
    ## $ key                      <dbl> 6, 11, 1, 7, 1, 8, 5, 4, 8, 2, 6, 8, 1, 5, 5…
    ## $ loudness                 <dbl> -2.634, -4.969, -3.432, -3.778, -4.672, -5.3…
    ## $ mode                     <dbl> 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0,…
    ## $ speechiness              <dbl> 0.0583, 0.0373, 0.0742, 0.1020, 0.0359, 0.12…
    ## $ acousticness             <dbl> 0.10200, 0.07240, 0.07940, 0.02870, 0.08030,…
    ## $ instrumentalness         <dbl> 0.00e+00, 4.21e-03, 2.33e-05, 9.43e-06, 0.00…
    ## $ liveness                 <dbl> 0.0653, 0.3570, 0.1100, 0.2040, 0.0833, 0.14…
    ## $ valence                  <dbl> 0.518, 0.693, 0.613, 0.277, 0.725, 0.585, 0.…
    ## $ tempo                    <dbl> 122.036, 99.972, 124.008, 121.956, 123.976, …
    ## $ duration_ms              <dbl> 194754, 162600, 176616, 169093, 189052, 1630…

``` r
#Looking at playlist genre and their subgenre
df %>% 
  select(playlist_genre, playlist_subgenre) %>% 
  count(playlist_genre, playlist_subgenre) %>% 
  ggplot(aes(x = reorder(playlist_subgenre, -n), y = n, fill = playlist_genre)) + 
  geom_col() + 
  coord_flip() + 
  facet_wrap(~playlist_genre, scales = "free") + 
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none") 
```

![](TidyTuesdaySpotifyAnalysis_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#These are some long subgenre names 
#Progressive electro house, indie poptimisim, neo soul are genres that I am not familiar with 
```

``` r
df %>% 
  select(playlist_genre) %>% 
  count(playlist_genre) %>% 
  ggplot(aes(x = reorder(playlist_genre, -n), y = n, fill = playlist_genre)) + 
  geom_col()
```

![](TidyTuesdaySpotifyAnalysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#EDM, rap, and pop are still popular however, Rock isnt as popular 
#Are songs labeled with multiple genres and subgenres? 
```

``` r
#What songs appear in multiple playlist genres?
df %>% 
  select(track_name, playlist_genre) %>% 
  unique() %>% 
  count(track_name) %>% 
  filter(n > 1) %>% 
  arrange(track_name) 
```

    ## # A tibble: 2,871 x 2
    ##    track_name                                             n
    ##    <chr>                                              <int>
    ##  1 'Till I Collapse                                       3
    ##  2 $ave Dat Money (feat. Fetty Wap & Rich Homie Quan)     2
    ##  3 (No One Knows Me) Like the Piano                       2
    ##  4 ...Baby One More Time                                  2
    ##  5 ¿Cual es tu plan?                                      2
    ##  6 ¿Quien Tu Eres?                                        2
    ##  7 +                                                      2
    ##  8 <U+30AC><U+30E9><U+30B9><U+306E>PALM TREE                                      2
    ##  9 <U+30DC><U+30A4><U+30B9><U+30E1><U+30E2> No. 5                                       2
    ## 10 <U+541B><U+306E><U+30CF><U+30FC><U+30C8><U+306F><U+30DE><U+30EA><U+30F3><U+30D6><U+30EB><U+30FC>                               2
    ## # … with 2,861 more rows

``` r
#A lot of songs appear in more than 1 however only one appears in 3
```

``` r
#What about subgenres? 
df %>% 
  select(track_name, playlist_subgenre) %>% 
  unique() %>% 
  count(track_name) %>% 
  filter(n > 1) %>% 
  arrange(-n, track_name)
```

    ## # A tibble: 4,441 x 2
    ##    track_name       n
    ##    <chr>        <int>
    ##  1 Breathe         14
    ##  2 Forever         14
    ##  3 Stay            14
    ##  4 Paradise        13
    ##  5 Alive           12
    ##  6 Memories        12
    ##  7 Dance Monkey    11
    ##  8 Lost            11
    ##  9 Poison          11
    ## 10 Without You     11
    ## # … with 4,431 more rows

``` r
#A lot of songs appear in more than 1 subgenre.
```

``` r
#Lets look at the distribution of overall subgenre appearances 
df %>% 
  select(track_name, playlist_subgenre) %>% 
  unique() %>% 
  count(track_name) %>% 
  filter(n > 1) %>% 
  ggplot(aes(x = n)) + 
  geom_density() + 
  scale_x_log10()
```

![](TidyTuesdaySpotifyAnalysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
df %>% 
  select(track_name, playlist_subgenre) %>% 
  unique() %>% 
  count(track_name) %>% 
  filter(n > 1) %>% 
  summary()
```

    ##   track_name              n         
    ##  Length:4441        Min.   : 2.000  
    ##  Class :character   1st Qu.: 2.000  
    ##  Mode  :character   Median : 2.000  
    ##                     Mean   : 2.692  
    ##                     3rd Qu.: 3.000  
    ##                     Max.   :14.000

``` r
#Lets look at it subgenre appearances by genre 
df %>% 
  select(track_name, playlist_genre, playlist_subgenre) %>% 
  unique() %>% 
  group_by(playlist_genre, playlist_genre) %>% 
  count(track_name) %>% 
  filter(n > 1) %>% 
  ggplot(aes(x = n, fill = playlist_genre)) + 
  geom_density() + 
  scale_x_log10() +
  facet_wrap(~playlist_genre, scales = "free")
```

![](TidyTuesdaySpotifyAnalysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# iris %>% 
#   group_by(Species) %>% 
#   do(model = lm(Sepal.Length~Sepal.Width + Petal.Length, data = .)) %>% 
#   broom::augment(model)
```

``` r
#Lets look at the spotify features 
df %>% 
  select(9:20) %>% 
  summary()
```

    ##   danceability        energy              key            loudness      
    ##  Min.   :0.0000   Min.   :0.000175   Min.   : 0.000   Min.   :-46.448  
    ##  1st Qu.:0.5630   1st Qu.:0.581000   1st Qu.: 2.000   1st Qu.: -8.171  
    ##  Median :0.6720   Median :0.721000   Median : 6.000   Median : -6.166  
    ##  Mean   :0.6548   Mean   :0.698619   Mean   : 5.374   Mean   : -6.720  
    ##  3rd Qu.:0.7610   3rd Qu.:0.840000   3rd Qu.: 9.000   3rd Qu.: -4.645  
    ##  Max.   :0.9830   Max.   :1.000000   Max.   :11.000   Max.   :  1.275  
    ##       mode         speechiness      acousticness    instrumentalness   
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000000  
    ##  1st Qu.:0.0000   1st Qu.:0.0410   1st Qu.:0.0151   1st Qu.:0.0000000  
    ##  Median :1.0000   Median :0.0625   Median :0.0804   Median :0.0000161  
    ##  Mean   :0.5657   Mean   :0.1071   Mean   :0.1753   Mean   :0.0847472  
    ##  3rd Qu.:1.0000   3rd Qu.:0.1320   3rd Qu.:0.2550   3rd Qu.:0.0048300  
    ##  Max.   :1.0000   Max.   :0.9180   Max.   :0.9940   Max.   :0.9940000  
    ##     liveness         valence           tempo         duration_ms    
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :  0.00   Min.   :  4000  
    ##  1st Qu.:0.0927   1st Qu.:0.3310   1st Qu.: 99.96   1st Qu.:187819  
    ##  Median :0.1270   Median :0.5120   Median :121.98   Median :216000  
    ##  Mean   :0.1902   Mean   :0.5106   Mean   :120.88   Mean   :225800  
    ##  3rd Qu.:0.2480   3rd Qu.:0.6930   3rd Qu.:133.92   3rd Qu.:253585  
    ##  Max.   :0.9960   Max.   :0.9910   Max.   :239.44   Max.   :517810

``` r
#The music features are scaled 0-1 besides key, loudness, tempo, deuration_ms
```

``` r
spotifyDf <- df %>% select(9:20)
#Lets convertt duration_ms to minutes and seconds 
spotifyDf <-  spotifyDf %>% 
  mutate(minutes = duration_ms/60000) %>% 
  select(-duration_ms)
```

``` r
#Convert key numbers in letters using http://openmusictheory.com/pitch(Class).html
musickey <- data.frame(notation = c(0,1,2,3,4,5,6,7,8,9,10,11), 
                       key = c("C", "C#","D","D#","E","F","F#","G","G#","A","Bb","B"))

spotifyDf <- spotifyDf %>% 
  inner_join(musickey, by = c("key" = "notation")) %>% 
  mutate(key = key.y) %>% 
  select(-key.y)
```

``` r
#What keys are used in the Spotify songs? 
spotifyDf %>% 
  select(key,mode) %>%
  mutate(mode = if_else(mode == 0, "Minor Key", "Major Key")) %>% 
  group_by(mode) %>% 
  count(key) %>% 
  mutate(key = as.factor(key)) %>% 
  ggplot(aes(x = key, y =n, fill = key)) + 
  geom_col() + 
  facet_wrap(~mode, scales = "free")
```

![](TidyTuesdaySpotifyAnalysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
#C major makes sense for its popularity and also G and C#
#Minor keys are interesting, B minor is the most popular by a decent amount
```

``` r
#Lets look at  the distribtuion of the spotify features for each key and its relative mode
options(scipen = 999)
spotifyDf %>% 
  select(-minutes, -tempo, -loudness) %>% 
  rename("musickey" = key, "major" = mode) %>% 
  mutate(major = if_else(major == 1, "major", "minor")) %>% 
  gather(key = "key", value = "value", -musickey,-major) %>% 
  arrange(major, musickey, key, value) %>% 
  ggplot(aes(x = value, fill = key, group = key)) + 
  geom_density(alpha = .5) +
  scale_x_sqrt() + 
  scale_y_sqrt() + 
  facet_grid(musickey~major, scales = "free")
```

![](TidyTuesdaySpotifyAnalysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
#This plot is not easy to read and is hard to get any instights
```

``` r
#Let's make a better plot by looking at the means of each key and its relative mode and spotify features 
spotifyDf %>% 
  rename("musickey" = key) %>% 
  mutate(mode = if_else(mode == 1, "Major", "Minor")) %>% 
  gather(key = "key", value = "value", -musickey,-mode) %>% 
  group_by(musickey, mode, key) %>% 
  summarise(mean = mean(value)) %>% 
  ggplot(aes(x = musickey, y = mean, fill = musickey)) + 
  geom_col() + 
  coord_flip() + 
  facet_wrap(~key + mode, scales = "free") + 
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none")
```

![](TidyTuesdaySpotifyAnalysis_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
#This plot makes it easier to find the differences for each key 
#Why is C# the highest for speechiness? 
#Valence with major and minor is interesting especially since traditionally minor is for sad music and major is for happy music 
#I would have expected minor valence to be much smaller than major valence
```

``` r
#Is major better than minor? 
spotifyDf %>% 
  mutate(mode = if_else(mode == 1, "Major", "Minor")) %>% 
  select(-key) %>% 
  gather(key = "key", value = "value", -mode) %>% 
  group_by(mode) %>% 
  ggplot(aes(x = mode, y = value, fill = key)) + 
  geom_boxplot() + 
  facet_wrap(~key, scales = "free")
```

![](TidyTuesdaySpotifyAnalysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
#Visual inspection shows major and minor means may be the same 
```

``` r
spotifyDf %>% 
  mutate(mode = if_else(mode == 1, "Major", "Minor")) %>% 
  select(-key) %>% 
  gather(key = "key", value = "value", -mode) %>% 
  group_by(key) %>% 
  do(broom::tidy(t.test(value~mode, data = .))) %>% 
  select(key, p.value) %>% 
  ggplot(aes(x = reorder(key, p.value), y = p.value, color = key)) + 
  geom_point() + 
  geom_segment(aes(xend = key, yend = 0)) + 
  geom_hline(yintercept = .05, color = "red", linetype = "dotted") + 
  theme(axis.text.x = element_text(angle = 90))
```

![](TidyTuesdaySpotifyAnalysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
#T test reveals speechiness, danceability, loudness, minutes, and tempo means may be different due to pvalues being statistically significant with a cut-off value of .05
```

``` r
#Analyzing relationship of spotify features, are danceaility and energy correlated? What about valence (happieness)? 

spotifyDf %>% 
  select(-key, -mode, -minutes) %>% 
  as.matrix() %>% 
  cor() %>% 
  corrplot::corrplot.mixed()
```

![](TidyTuesdaySpotifyAnalysis_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
spotifyDf %>% 
  select(-key, -mode, -minutes) %>% 
  as.matrix() %>% 
  cor()
```

    ##                  danceability      energy    loudness speechiness acousticness
    ## danceability      1.000000000 -0.08607316  0.02533509  0.18172133 -0.024519058
    ## energy           -0.086073156  1.00000000  0.67662452 -0.03214961 -0.539744630
    ## loudness          0.025335088  0.67662452  1.00000000  0.01033898 -0.361638165
    ## speechiness       0.181721334 -0.03214961  0.01033898  1.00000000  0.026091985
    ## acousticness     -0.024519058 -0.53974463 -0.36163817  0.02609199  1.000000000
    ## instrumentalness -0.008655078  0.03324658 -0.14782402 -0.10342419 -0.006850273
    ## liveness         -0.123859417  0.16122305  0.07761260  0.05542591 -0.077243449
    ## valence           0.330523257  0.15110330  0.05338356  0.06465910 -0.016844738
    ## tempo            -0.184084351  0.14995111  0.09376736  0.04460329 -0.112723913
    ##                  instrumentalness     liveness     valence       tempo
    ## danceability         -0.008655078 -0.123859417  0.33052326 -0.18408435
    ## energy                0.033246579  0.161223049  0.15110330  0.14995111
    ## loudness             -0.147824018  0.077612601  0.05338356  0.09376736
    ## speechiness          -0.103424193  0.055425906  0.06465910  0.04460329
    ## acousticness         -0.006850273 -0.077243449 -0.01684474 -0.11272391
    ## instrumentalness      1.000000000 -0.005507043 -0.17540218  0.02333527
    ## liveness             -0.005507043  1.000000000 -0.02055977  0.02101780
    ## valence              -0.175402179 -0.020559772  1.00000000 -0.02573215
    ## tempo                 0.023335266  0.021017804 -0.02573215  1.00000000

``` r
spotifyDf %>% 
  select(-key, -mode, -minutes) %>% 
  as.matrix() %>% 
  cor() %>% 
  broom::tidy() %>% 
  gather(key = "key", value = "value", -".rownames") %>% 
  filter(.rownames != key) %>% 
  distinct(value, .keep_all = TRUE) %>% 
  arrange(-abs(value)) 
```

    ## Warning: 'tidy.matrix' is deprecated.
    ## See help("Deprecated")

    ## # A tibble: 36 x 3
    ##    .rownames    key               value
    ##    <chr>        <chr>             <dbl>
    ##  1 loudness     energy            0.677
    ##  2 acousticness energy           -0.540
    ##  3 acousticness loudness         -0.362
    ##  4 valence      danceability      0.331
    ##  5 tempo        danceability     -0.184
    ##  6 speechiness  danceability      0.182
    ##  7 valence      instrumentalness -0.175
    ##  8 liveness     energy            0.161
    ##  9 valence      energy            0.151
    ## 10 tempo        energy            0.150
    ## # … with 26 more rows

``` r
#Loudness and energy are positively correlated
#Accousticness and energy are negatively correltated 
#This makes sense loudness = energy
#Accoustiness is a measurement of how likely the song is accoustic which makes sense for it being negatively correlated with loudness
#However, it is interesting how accousticness is negatively correlated with energy
```

``` r
#What keys have the most amount of energy? What about danceability? 
spotifyDf %>% 
  mutate(mode = if_else(mode == 1, "Major", "Minor")) %>% 
  rename(., "musickey" = key) %>% 
  mutate(musickey = paste(musickey, mode)) %>% 
  select(-mode, -minutes) %>% 
  gather(key = "key", value = "value", -musickey) %>% 
  group_by(key, musickey) %>% 
  summarise(mean = mean(value)) %>% 
  group_by(key) %>% 
  top_n(mean, n =5) %>% 
  arrange(-mean) %>% 
  ggplot(aes(x = tidytext::reorder_within(musickey, mean, key) , y = mean, fill = musickey)) + 
  tidytext::scale_x_reordered() +
  geom_col() + 
  coord_flip() + 
  facet_wrap(~key, scales = "free") + 
  theme(legend.position = "none")
```

![](TidyTuesdaySpotifyAnalysis_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
#This time I am going to ignore the mode to get a better summary of the keys 

spotifyDf %>% 
  select(-mode, -minutes) %>% 
  rename(., musickey = "key") %>% 
  gather(key = "key", value = "value", -musickey) %>% 
  group_by(key, musickey) %>% 
  summarise(mean = mean(value)) %>% 
  group_by(key) %>% 
  top_n(mean, n =5) %>% 
  arrange(-mean) %>% 
  ggplot(aes(x = tidytext::reorder_within(musickey, mean, key) , y = mean, fill = musickey)) + 
  tidytext::scale_x_reordered() +
  geom_col() + 
  coord_flip() + 
  facet_wrap(~key, scales = "free") + 
  theme(legend.position = "none")
```

![](TidyTuesdaySpotifyAnalysis_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
#Let's compare the keys and its mean differences 
#Since I want to compare the means of more than 2 groups, I will use a one-way ANOVA test 
spotifyDf %>% 
  select(-mode) %>% 
  rename(., "musickey" = key) %>% 
  gather(key = "features", value = "value", -musickey) %>% 
  group_by(features) %>% 
  do(broom::tidy(aov(value~musickey, data = .))) %>% 
  select(p.value) %>% 
  filter(!is.na(p.value)) %>% 
  ggplot(aes(x = features, y = p.value, color = features)) + 
  geom_point() + 
  geom_segment(aes(xend = features, yend = 0)) + 
  geom_hline(yintercept = .05, color = "red", linetype = "dashed") + 
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none")
```

    ## Adding missing grouping variables: `features`

![](TidyTuesdaySpotifyAnalysis_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
#Every group has p-values less than .05 which means it is statistically significant
#At least one mean within the musickeys are different from the rest of the group
```

``` r
#Looking at the means of every music key by spotify features 
spotifyDf %>% 
  select(-mode) %>% 
  rename(., "musickey" = key) %>% 
  gather(key = "features", value = "value", -musickey) %>% 
  group_by(musickey,features) %>% 
  summarise(mean = mean(value)) %>% 
  mutate(mean = if_else(features == "loudness", -mean, mean)) %>% 
  ggplot(aes(x = tidytext::reorder_within(musickey, mean, features) , y = mean, fill = musickey)) +
  geom_col(size = 1) + 
  tidytext::scale_x_reordered() + 
  coord_flip() +
  facet_wrap(~features, scales = "free") + 
  theme(legend.position = "none")
```

![](TidyTuesdaySpotifyAnalysis_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

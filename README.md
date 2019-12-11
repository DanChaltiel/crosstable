---
    output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->






# biostat2

Ceci est un fork du super package <a href='https://github.com/eusebe/biostat2'>`biostat2`</a> de David Hajage.

Il est centré sur la fonction `cross` qui permet de générer très facilement les *statistiques descriptives* d'une étude et s'intègre naturellement au package `officer` permettant de faire un rapport automatisé.

## Installation


```r
install.packages("devtools")
remotes::install_github("DanChaltiel/biostat2")
```

## Utilisation

### Description

On décrit une table avec une formule. On utilise `cbind` pour récupérer une dataframe en sortie, mais on pourrait aussi utiliser des `+` et récupérer une liste de descriptions. Par défaut, `cross` donnera la moyenne, écart-type, médiane, IQR, min, max, nombre d'observations et nombre de manquants.


```r
cross(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
#>            .id        label  variable         setosa    versicolor       virginica
#> 1 Sepal.Length Sepal.Length Min / Max      4.3 / 5.8       4.9 / 7       4.9 / 7.9
#> 2 Sepal.Length Sepal.Length Med [IQR]    5 [4.8;5.2] 5.9 [5.6;6.3]  6.5 [6.23;6.9]
#> 3 Sepal.Length Sepal.Length Moy (std)    5.01 (0.35)   5.94 (0.52)     6.59 (0.64)
#> 4 Sepal.Length Sepal.Length    N (NA)         50 (0)        50 (0)          50 (0)
#> 5 Petal.Length Petal.Length Min / Max        1 / 1.9       3 / 5.1       4.5 / 6.9
#> 6 Petal.Length Petal.Length Med [IQR] 1.5 [1.4;1.58]  4.35 [4;4.6] 5.55 [5.1;5.88]
#> 7 Petal.Length Petal.Length Moy (std)    1.46 (0.17)   4.26 (0.47)     5.55 (0.55)
#> 8 Petal.Length Petal.Length    N (NA)         50 (0)        50 (0)          50 (0)
cross(cbind(Sepal.Length, Petal.Length, Species) ~ ., data = iris) #le '.' représente l'absence de variable de groupe
#>             .id        label   variable          value
#> 1  Sepal.Length Sepal.Length  Min / Max      4.3 / 7.9
#> 2  Sepal.Length Sepal.Length  Med [IQR]  5.8 [5.1;6.4]
#> 3  Sepal.Length Sepal.Length  Moy (std)    5.84 (0.83)
#> 4  Sepal.Length Sepal.Length     N (NA)        150 (0)
#> 5  Petal.Length Petal.Length  Min / Max        1 / 6.9
#> 6  Petal.Length Petal.Length  Med [IQR] 4.35 [1.6;5.1]
#> 7  Petal.Length Petal.Length  Moy (std)    3.76 (1.77)
#> 8  Petal.Length Petal.Length     N (NA)        150 (0)
#> 9       Species      Species     setosa    50 (33.33%)
#> 10      Species      Species versicolor    50 (33.33%)
#> 11      Species      Species  virginica    50 (33.33%)
```

On peut spécifier des calculs spécifiques dans la formule avec la fonction `I()`, on peut utiliser n'importe quelle fonction de description (ajouter les arguments à la suite, comme l'argument `probs` de la fonction `quantile` ici) et on peut demander des totaux en ligne, en colonne ou les deux.


```r
cross(cbind(Sepal.Length, I(Sepal.Width^2)) ~ Species, iris, funs=quantile, probs=c(1/3, 2/3), total="line") #T1 & T2 by Species
#>                .id            label           variable setosa versicolor virginica Total
#> 1     Sepal.Length     Sepal.Length quantile 33.33333%    4.9        5.7       6.3   5.4
#> 2     Sepal.Length     Sepal.Length quantile 66.66667%    5.1        6.1      6.77   6.3
#> 3 I(Sepal.Width^2) I(Sepal.Width^2) quantile 33.33333%  10.46       7.29      7.84  8.41
#> 4 I(Sepal.Width^2) I(Sepal.Width^2) quantile 66.66667%  12.25       8.41      9.41 10.24
```

Les pourcentages sont donnés par défaut par ligne, colonne et cellule, mais on peut restreindre avec l'argument `margin`. L'argument `test` permet de faire le test adéquat en fonction de règles définies.


```r
cross(alcgp ~ tobgp, esoph, margin="line", total="both", test = TRUE)
#>     .id label  variable    0-9g/day       10-19       20-29         30+       Total
#> 1 alcgp alcgp 0-39g/day  6 (26.09%)  6 (26.09%)  5 (21.74%)  6 (26.09%) 23 (26.14%)
#> 2 alcgp alcgp     40-79  6 (26.09%)  6 (26.09%)  6 (26.09%)  5 (21.74%) 23 (26.14%)
#> 3 alcgp alcgp    80-119  6 (28.57%)  6 (28.57%)  4 (19.05%)  5 (23.81%) 21 (23.86%)
#> 4 alcgp alcgp      120+  6 (28.57%)  6 (28.57%)  5 (23.81%)  4 (19.05%) 21 (23.86%)
#> 5 alcgp alcgp     Total 24 (27.27%) 24 (27.27%) 20 (22.73%) 20 (22.73%)   88 (100%)
#>                                                      p
#> 1 p value: 0.9999 (Fisher's Exact Test for Count Data)
#> 2 p value: 0.9999 (Fisher's Exact Test for Count Data)
#> 3 p value: 0.9999 (Fisher's Exact Test for Count Data)
#> 4 p value: 0.9999 (Fisher's Exact Test for Count Data)
#> 5 p value: 0.9999 (Fisher's Exact Test for Count Data)
```

Si le groupement n'a que deux niveau, il est possible de calculer un effet grâce à l'argument `effect`.

```r
cross(cbind(hp, mpg) ~ factor(am), mtcars, effect=T, test = TRUE, show.method=F)
#> Warning in wilcox.test.default(x = c(110, 175, 105, 245, 62, 95, 123, 123, : cannot compute
#> exact p-value with ties
#>   .id label  variable                 0              1
#> 1  hp    hp Min / Max          62 / 245       52 / 335
#> 2  hp    hp Med [IQR] 175 [116.5;192.5]   109 [66;113]
#> 3  hp    hp Moy (std)    160.26 (53.91) 126.85 (84.06)
#> 4  hp    hp    N (NA)            19 (0)         13 (0)
#> 5 mpg   mpg Min / Max       10.4 / 24.4      15 / 33.9
#> 6 mpg   mpg Med [IQR] 17.3 [14.95;19.2] 22.8 [21;30.4]
#> 7 mpg   mpg Moy (std)      17.15 (3.83)   24.39 (6.17)
#> 8 mpg   mpg    N (NA)            19 (0)         13 (0)
#>                                                                         effect      p
#> 1 Difference in means (bootstrap CI) (0 minus 1): 33.42 CI95%[-15.49 to 82.33] 0.0437
#> 2 Difference in means (bootstrap CI) (0 minus 1): 33.42 CI95%[-15.49 to 82.33] 0.0437
#> 3 Difference in means (bootstrap CI) (0 minus 1): 33.42 CI95%[-15.49 to 82.33] 0.0437
#> 4 Difference in means (bootstrap CI) (0 minus 1): 33.42 CI95%[-15.49 to 82.33] 0.0437
#> 5    Difference in means (t-test CI) (0 minus 1): -7.24 CI95%[-10.85 to -3.64] 0.0003
#> 6    Difference in means (t-test CI) (0 minus 1): -7.24 CI95%[-10.85 to -3.64] 0.0003
#> 7    Difference in means (t-test CI) (0 minus 1): -7.24 CI95%[-10.85 to -3.64] 0.0003
#> 8    Difference in means (t-test CI) (0 minus 1): -7.24 CI95%[-10.85 to -3.64] 0.0003
```

Enfin, si la variable de groupe est numérique, `cross` sortira les coefficients de corrélation

```r
cross(cbind(Sepal.Length, Sepal.Width) ~ cbind(Petal.Length, Petal.Width), iris)
#>            .id        label variable Petal.Length variable Petal.Width
#> 1 Sepal.Length Sepal.Length  pearson         0.87  pearson        0.82
#> 2  Sepal.Width  Sepal.Width  pearson        -0.43  pearson       -0.37
```

### Reporting

Il est possible de transformer une table en HTML via la fonction `cross_to_flextable())`.






















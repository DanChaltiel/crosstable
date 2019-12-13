
<!-- README.md is generated from README.Rmd. Please edit that file -->

# biostat2

Ceci est un fork du super package
<a href='https://github.com/eusebe/biostat2'>`biostat2`</a> de David
Hajage.

Il est centré sur la fonction `cross` qui permet de générer très
facilement les *statistiques descriptives* d’une étude et s’intègre
naturellement au package `officer` permettant de faire un rapport
automatisé.

## Installation

``` r
install.packages("devtools")
remotes::install_github("DanChaltiel/biostat2")
library(biostat2)
```

## Utilisation

### Description

On décrit une table avec une formule. On utilise `cbind` pour récupérer
une dataframe en sortie, mais on pourrait aussi utiliser des `+` et
récupérer une liste de descriptions par variable. A droite du `~` on
trouve la variable de groupe ou un `.` sinon. Par défaut, `cross`
donnera la moyenne, écart-type, médiane, IQR, min, max, nombre
d’observations et nombre de manquants.

``` r
cross(cbind(Sepal.Length, Petal.Length) ~ Species, data=iris)
#>            .id        label  variable         setosa    versicolor
#> 1 Sepal.Length Sepal.Length Min / Max      4.3 / 5.8       4.9 / 7
#> 2 Sepal.Length Sepal.Length Med [IQR]    5 [4.8;5.2] 5.9 [5.6;6.3]
#> 3 Sepal.Length Sepal.Length Moy (std)    5.01 (0.35)   5.94 (0.52)
#> 4 Sepal.Length Sepal.Length    N (NA)         50 (0)        50 (0)
#> 5 Petal.Length Petal.Length Min / Max        1 / 1.9       3 / 5.1
#> 6 Petal.Length Petal.Length Med [IQR] 1.5 [1.4;1.58]  4.35 [4;4.6]
#> 7 Petal.Length Petal.Length Moy (std)    1.46 (0.17)   4.26 (0.47)
#> 8 Petal.Length Petal.Length    N (NA)         50 (0)        50 (0)
#>         virginica
#> 1       4.9 / 7.9
#> 2  6.5 [6.23;6.9]
#> 3     6.59 (0.64)
#> 4          50 (0)
#> 5       4.5 / 6.9
#> 6 5.55 [5.1;5.88]
#> 7     5.55 (0.55)
#> 8          50 (0)

cross(cbind(Sepal.Length, Petal.Length, Species) ~ ., data=iris)
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

On peut spécifier des calculs spécifiques dans la formule avec la
fonction `I()`, on peut utiliser n’importe quelle fonction de
description (ajouter les arguments à la suite, comme l’argument `probs`
de la fonction `quantile` ici) et on peut demander des totaux en ligne,
en colonne ou les
deux.

``` r
cross(cbind(Sepal.Length, I(Sepal.Width^2)) ~ Species, iris, funs=quantile, probs=c(1/3, 2/3), total="line") #T1 & T2 by Species
#>                .id            label           variable setosa versicolor
#> 1     Sepal.Length     Sepal.Length quantile 33.33333%    4.9        5.7
#> 2     Sepal.Length     Sepal.Length quantile 66.66667%    5.1        6.1
#> 3 I(Sepal.Width^2) I(Sepal.Width^2) quantile 33.33333%  10.46       7.29
#> 4 I(Sepal.Width^2) I(Sepal.Width^2) quantile 66.66667%  12.25       8.41
#>   virginica Total
#> 1       6.3   5.4
#> 2      6.77   6.3
#> 3      7.84  8.41
#> 4      9.41 10.24
```

Les pourcentages sont donnés par défaut par ligne, colonne et cellule,
mais on peut restreindre avec l’argument `margin`. L’argument `test`
permet de faire le test adéquat en fonction de règles définies.

``` r
cross(alcgp ~ tobgp, esoph, margin="line", total="both", test=TRUE)
#>     .id label  variable    0-9g/day       10-19       20-29         30+
#> 1 alcgp alcgp 0-39g/day  6 (26.09%)  6 (26.09%)  5 (21.74%)  6 (26.09%)
#> 2 alcgp alcgp     40-79  6 (26.09%)  6 (26.09%)  6 (26.09%)  5 (21.74%)
#> 3 alcgp alcgp    80-119  6 (28.57%)  6 (28.57%)  4 (19.05%)  5 (23.81%)
#> 4 alcgp alcgp      120+  6 (28.57%)  6 (28.57%)  5 (23.81%)  4 (19.05%)
#> 5 alcgp alcgp     Total 24 (27.27%) 24 (27.27%) 20 (22.73%) 20 (22.73%)
#>         Total                                                    p
#> 1 23 (26.14%) p value: 0.9999 (Fisher's Exact Test for Count Data)
#> 2 23 (26.14%) p value: 0.9999 (Fisher's Exact Test for Count Data)
#> 3 21 (23.86%) p value: 0.9999 (Fisher's Exact Test for Count Data)
#> 4 21 (23.86%) p value: 0.9999 (Fisher's Exact Test for Count Data)
#> 5   88 (100%) p value: 0.9999 (Fisher's Exact Test for Count Data)
```

Si le groupement n’a que deux niveau, il est possible de calculer un
effet grâce à l’argument
`effect`.

``` r
cross(cbind(mpg, qsec) ~ factor(am), mtcars, effect=T, test=TRUE, show.method=F)
#>    .id label  variable                   0                   1
#> 1  mpg   mpg Min / Max         10.4 / 24.4           15 / 33.9
#> 2  mpg   mpg Med [IQR]   17.3 [14.95;19.2]      22.8 [21;30.4]
#> 3  mpg   mpg Moy (std)        17.15 (3.83)        24.39 (6.17)
#> 4  mpg   mpg    N (NA)              19 (0)              13 (0)
#> 5 qsec  qsec Min / Max        15.41 / 22.9         14.5 / 19.9
#> 6 qsec  qsec Med [IQR] 17.82 [17.18;19.17] 17.02 [16.46;18.61]
#> 7 qsec  qsec Moy (std)        18.18 (1.75)        17.36 (1.79)
#> 8 qsec  qsec    N (NA)              19 (0)              13 (0)
#>                                                                      effect
#> 1 Difference in means (t-test CI) (0 minus 1): -7.24 CI95%[-10.85 to -3.64]
#> 2 Difference in means (t-test CI) (0 minus 1): -7.24 CI95%[-10.85 to -3.64]
#> 3 Difference in means (t-test CI) (0 minus 1): -7.24 CI95%[-10.85 to -3.64]
#> 4 Difference in means (t-test CI) (0 minus 1): -7.24 CI95%[-10.85 to -3.64]
#> 5    Difference in means (t-test CI) (0 minus 1): 0.82 CI95%[-0.48 to 2.12]
#> 6    Difference in means (t-test CI) (0 minus 1): 0.82 CI95%[-0.48 to 2.12]
#> 7    Difference in means (t-test CI) (0 minus 1): 0.82 CI95%[-0.48 to 2.12]
#> 8    Difference in means (t-test CI) (0 minus 1): 0.82 CI95%[-0.48 to 2.12]
#>        p
#> 1 0.0003
#> 2 0.0003
#> 3 0.0003
#> 4 0.0003
#> 5 0.2057
#> 6 0.2057
#> 7 0.2057
#> 8 0.2057
```

Enfin, si la variable de groupe est numérique, `cross` sortira les
coefficients de
corrélation.

``` r
cross(cbind(Sepal.Length, Sepal.Width) ~ cbind(Petal.Length, Petal.Width), iris)
#>            .id        label variable Petal.Length variable Petal.Width
#> 1 Sepal.Length Sepal.Length  pearson         0.87  pearson        0.82
#> 2  Sepal.Width  Sepal.Width  pearson        -0.43  pearson       -0.37
```

### Labels

`cross` est interfacé avec la fonction `label` du package `Hmisc`, ce
qui permet d’avoir des tableaux plus clairs.

``` r
library(Hmisc)
# ?mtcars
mtcars2=mtcars
label(mtcars2$mpg) = "Miles/(US) gallon"
label(mtcars2$qsec) = "1/4 mile time in seconds"
mtcars2$am = factor(mtcars2$am, levels=0:1, labels=c("automatic", "manual"))
cross(cbind(mpg, qsec) ~ am, mtcars2)
#>    .id label  variable           automatic              manual
#> 1  mpg   mpg Min / Max         10.4 / 24.4           15 / 33.9
#> 2  mpg   mpg Med [IQR]   17.3 [14.95;19.2]      22.8 [21;30.4]
#> 3  mpg   mpg Moy (std)        17.15 (3.83)        24.39 (6.17)
#> 4  mpg   mpg    N (NA)              19 (0)              13 (0)
#> 5 qsec  qsec Min / Max        15.41 / 22.9         14.5 / 19.9
#> 6 qsec  qsec Med [IQR] 17.82 [17.18;19.17] 17.02 [16.46;18.61]
#> 7 qsec  qsec Moy (std)        18.18 (1.75)        17.36 (1.79)
#> 8 qsec  qsec    N (NA)              19 (0)              13 (0)
```

## Reporting

### Tableaux HTML

Il est possible de transformer une table en HTML via la fonction
`cross_to_flextable())`.

``` r
cross(cbind(agegp, ncases) ~ tobgp, esoph, margin="line", test = TRUE) %>% cross_to_flextable
```

<center>

<img src="img/cross_esoph.png" height="300px"/>

</center>

``` r
cross(cbind(Sepal.Length, Sepal.Width) ~ Species, iris, test = TRUE, total="column") %>% cross_to_flextable
```

<center>

<img src="img/cross_iris.png" height="250px"/>

</center>

``` r
cross(cbind(mpg, qsec) ~ factor(am), mtcars2, effect=T, test=TRUE, show.method=F) %>% cross_to_flextable
```

<center>

<img src="img/cross_mtcars.png" height="250px"/>

</center>

### Intégration avec `officer`

Grâce au package `officer`, il est possible de réaliser des fichiers MS
Word. On peut ajouter des tableaux `cross` de cette façon :

    ```r
    library(officer)
    library(dplyr) #for the pipe operator
    c1 = cross(cbind(Sepal.Length, Petal.Length) ~ Species, test=TRUE, show.method=F, data=iris)
    c2 = cross(cbind(mpg, qsec) ~ factor(am), mtcars2, test=TRUE, show.method=F)
    read_docx() %>% 
        body_add_title("Cross + officer = <3", 1) %>% 
        body_add_title("Premier exemple : iris", 2) %>% 
        body_add_crosstable(c1) %>% 
        body_add_title("Deuxième exemple : mtcars", 2) %>% 
        body_add_crosstable(c2, auto.fit=T) %>% 
        print("exemples/cross_officer.docx")
    ```

A noter toutefois que les tableaux devront être agrandis avec
l’ajustement automatique de Word, l’argument `auto.fit` pouvant
largement dépasser les marges de la page. Le fichier de sortie est donc
disponible ici :
[cross\_officer.docx](exemples/cross_officer.docx)

### Intégration avec `Rmarkdown`

<!-- Intégration Rmd in Rmd : la coloration syntaxique RStudio est mauvaise mais le code est bon ! -->

<!-- https://stackoverflow.com/questions/53226493/add-markdown-code-chunk-to-r-markdown-document#comment104859956_53226493 -->

Tricoter (`knitr::knit()` ou via **RStudio**) ce code `Rmd` produit un
fichier MS Word. L’avantage ici est l’utilisation de `bookdown` pour
générer la numérotation automatique des tableaux.

```` markdown

---
title: "Iris"
output: bookdown::word_document2
---
    
```{r setup, include=FALSE}
library(biostat2)
library(flextable)
library(dplyr) #pour le pipe %>% 
```

Table iris is given in Table \@ref(tab:irisTable).

```{r description, echo=FALSE, results='asis'}
cat("<caption> (\\#tab:irisTable) Table Iris </caption> \n\r ")
cross(cbind(Sepal.Length, Sepal.Width) ~ Species, iris, test = TRUE, total="column") %>% cross_to_flextable %>% autofit
```
````

Le fichier `Rmarkdown` est disponible ici :
[cross\_markdown.Rmd](exemples/cross_markdown.Rmd) et le fichier de
sortie est disponible ici :
[cross\_markdown.docx](exemples/cross_markdown.docx)

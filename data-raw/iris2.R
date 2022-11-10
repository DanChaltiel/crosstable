

iris2 = iris %>%
    expss::apply_labels(
        Species = "Specie",
        Sepal.Length = "Length of Sepal",
        Sepal.Width = "Width of Sepal",
        Petal.Length = "Length of Petal",
        Petal.Width = "Width of Petal"
    ) %>%
    as_tibble()

usethis::use_data(iris2)

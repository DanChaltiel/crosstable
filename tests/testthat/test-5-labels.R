


# Labelling ---------------------------------------------------------------
test_that("Labelling", {
    iris_label = tibble::tibble(
        name=c("Sepal.Length", "Sepal.Width",
               "Petal.Length", "Petal.Width", "Species"),
        label=c("Length of Sepals", "Width of Sepals",
                "Length of Petals", "Width of Petals", "Specie name")
    )
    x=import_labels(iris, iris_label)
    expect_equivalent(
        x %>% map(attributes) %>% map("label") %>% unlist,
        iris_label$label
    )
    
    xx=set_label(mtcars2, "The mtcars2 dataset", object=TRUE) %>%
        mutate(cyl=remove_label(cyl))
    
    expect_null(get_label(xx$cyl))
    expect_equal(get_label(xx$mpg), "Miles/(US) gallon")
    expect_equal(get_label(xx$cyl, default="foobar"), "foobar")
    expect_equal(get_label(xx, object=TRUE), "The mtcars2 dataset")
    expect_equal(get_label(xx), #TODO snapshot testthat v3
                 c(mpg = "Miles/(US) gallon", cyl = "cyl", disp = "Displacement (cu.in.)",
                   hp = "Gross horsepower", drat = "Rear axle ratio", wt = "Weight (1000 lbs)",
                   qsec = "1/4 mile time", vs = "Engine", am = "Transmission",
                   gear = "Number of forward gears", carb = "Number of carburetors",
                   hp_date = "Some nonsense date", qsec_posix = "Date+time"))
})




# import_labels()---------------------------------------------------------


#missing one name
iris_label = tibble::tribble(
    ~name,          ~label,
    "Sepal.Length", "Length of Sepals",
    # "Sepal.Width",  "Width of Sepals",
    "Petal.Length", "Length of Petals",
    "Petal.Width",  "Width of Petals",
    "Species",      "Specie name"
)

#missing one variable
df = iris %>% select(-Petal.Width)

test_that("Import labels: standard is OK", {
    df2 = import_labels(df, iris_label)
    expect_equal(get_label(df2$Sepal.Length), "Length of Sepals")
    expect_null(get_label(df2$Sepal.Width))
    expect_equal(get_label(df2$Petal.Length), "Length of Petals")
    expect_equal(get_label(df2$Species), "Specie name")
})

test_that("Import labels: warnings", {
    expect_warning(import_labels(df, iris_label, verbose_label = T), 
                   "Name .* not found")
    expect_warning(import_labels(df, iris_label, verbose_name = T), 
                   "Variable .* did not have any label")
    #both
    expect_warning(import_labels(df, iris_label, verbose_label = T, verbose_name = T), 
                   "Variable .* did not have any label")
    expect_warning(import_labels(df, iris_label, verbose_label = T, verbose_name = T), 
                   "Name .* not found")
})

test_that("Import labels: errors", {
    
    #error no save
    remove_last_save()
    expect_error(import_labels(iris), 
                 "There is no saved labels")
    expect_error(import_labels(iris), 
                 "There is no saved labels")
    
    #error duplicates
    iris_label_dup = iris_label
    iris_label_dup[5,] = list("Petal.Length", "xxxxx")
    iris_label_dup[6,] = list("Petal.Width",  "ttttt")
    expect_error(import_labels(iris, iris_label_dup), 
                 "Duplicated column names")
})

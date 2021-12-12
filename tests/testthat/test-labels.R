



# Getting labels ----------------------------------------------------------

test_that("Labelling dataframes", {
    xx = mtcars2 %>%
        dplyr::mutate(cyl=remove_label(cyl))
    
    expect_null(get_label(xx$cyl))
    # expect_equal(get_label(xx$cyl, default="foobar"), "foobar")
    # expect_equal(get_label(xx$mpg), "Miles/(US) gallon")
    # expect_equal(get_label(xx), #TODO snapshot testthat v3
    #              c(mpg = "Miles/(US) gallon", cyl = "cyl", disp = "Displacement (cu.in.)",
    #                hp = "Gross horsepower", drat = "Rear axle ratio", wt = "Weight (1000 lbs)",
    #                qsec = "1/4 mile time", vs = "Engine", am = "Transmission",
    #                gear = "Number of forward gears", carb = "Number of carburetors",
    #                hp_date = "Some nonsense date", qsec_posix = "Date+time"))
    
    expect_snapshot(get_label(xx))
})

test_that("Labelling unnamed dataframes/lists", {
    xx_noname = mtcars2 %>% as.data.frame() %>% remove_label() %>% unname()
    expect_setequal(get_label(xx_noname), NA)
    expect_setequal(get_label(xx_noname, default="foo"), "foo")
    
    expect_error(get_label(xx_noname, default=c("foo", "bar")), 
                 "`default` should be either length 1 or the same length as `x`")
    
})

test_that("Labelling objects", {
    xx=set_label(mtcars2, "The mtcars2 dataset", object=TRUE)
    expect_equal(get_label(xx, object=TRUE), "The mtcars2 dataset")
    
})

test_that("Labelling nested lists (get)", {
    x = list(
        list(1,2,3),
        list(iris=iris2, mtcars2),
        list("foo"="bar", "ffoo"="bbar")
    )
    expect_snapshot(get_label(x))
    expect_snapshot(get_label(x, simplify=FALSE))
    # expect_equal(get_label(x), 
    #              c(NA, NA, NA, iris.Sepal.Length = "Length of Sepal", iris.Sepal.Width = "Width of Sepal", 
    #                iris.Petal.Length = "Length of Petal", iris.Petal.Width = "Width of Petal", 
    #                iris.Species = "Specie", mpg = "Miles/(US) gallon", cyl = "Number of cylinders", 
    #                disp = "Displacement (cu.in.)", hp = "Gross horsepower", drat = "Rear axle ratio", 
    #                wt = "Weight (1000 lbs)", qsec = "1/4 mile time", vs = "Engine", 
    #                am = "Transmission", gear = "Number of forward gears", carb = "Number of carburetors", 
    #                hp_date = "Some nonsense date", qsec_posix = "Date+time", foo = "foo", 
    #                ffoo = "ffoo"))
    # expect_equal(get_label(x, simplify=FALSE), 
    #              list(c(NA, NA, NA), 
    #                   c(iris.Sepal.Length = "Length of Sepal", 
    #                     iris.Sepal.Width = "Width of Sepal", iris.Petal.Length = "Length of Petal", 
    #                     iris.Petal.Width = "Width of Petal", iris.Species = "Specie", 
    #                     mpg = "Miles/(US) gallon", cyl = "Number of cylinders", disp = "Displacement (cu.in.)", 
    #                     hp = "Gross horsepower", drat = "Rear axle ratio", wt = "Weight (1000 lbs)", 
    #                     qsec = "1/4 mile time", vs = "Engine", am = "Transmission", gear = "Number of forward gears", 
    #                     carb = "Number of carburetors", hp_date = "Some nonsense date", 
    #                     qsec_posix = "Date+time"), 
    #                   c(foo = "foo", ffoo = "ffoo")))
})



# Setting labels ----------------------------------------------------------


test_that("Labelling nested lists (set)", {
    x = list(
        list(1,2,3),
        list(iris=iris2, mtcars2),
        list("foo"="bar", "ffoo"="bbar")
    )
    xx=set_label(x, "not foobar at all")
    expect_setequal(get_label(xx), "not foobar at all")
})

test_that("Copying labels", {
    x = mtcars2 %>% 
        mutate(mpg2=as.numeric(mpg)+1, 
               mpg3=copy_label_from(mpg2, mpg))
    expect_null(get_label(x$mpg2))
    expect_equal(get_label(x$mpg3), "Miles/(US) gallon")
})

test_that("Removing labels", {
    x = mtcars2$mpg
    x2 = remove_label(mtcars2$mpg)
    
    expect_equal(get_label(x), "Miles/(US) gallon")
    expect_null(get_label(x2))
    
    expect_type(x, "double")
    expect_type(x2, "double")
    
    #limit case
    expect_null(remove_label(NULL))
})




# Importing labels --------------------------------------------------------

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
    import_labels(df, iris_label, verbose_label = T) %>% 
        expect_warning(class="missing_label_name_warning")
    import_labels(df, iris_label, verbose_name = T) %>%
        expect_warning(class="missing_label_warning")
    #both
    import_labels(df, iris_label, verbose_label = T, verbose_name = T) %>% 
        expect_warning(class="missing_label_name_warning") %>% 
        expect_warning(class="missing_label_warning")
})

test_that("Import labels: errors", {
    
    #error no save
    remove_last_save()
    expect_error(import_labels(iris), 
                 class="labels_import_null_error")
    
    #error duplicates
    iris_label_dup = iris_label
    iris_label_dup[5,] = list("Petal.Length", "xxxxx")
    iris_label_dup[6,] = list("Petal.Width",  "ttttt")
    expect_error(import_labels(iris, iris_label_dup), 
                 class="labels_import_dupkey_error")
    
    #deprecation
    import_labels(iris, iris_label, verbose=TRUE) %>%  
        expect_warning(class="missing_label_warning") %>% 
        lifecycle::expect_deprecated()
})

test_that("Applying labels", {
    x = apply_labels(iris, 
                     Sepal.Length="Length of Sepals", 
                     Petal.Width="Width of Petals")
    expect_equal(get_label(x),
                 c(Sepal.Length = "Length of Sepals", Sepal.Width = "Sepal.Width", 
                   Petal.Length = "Petal.Length", Petal.Width = "Width of Petals", 
                   Species = "Species"))
})


# Save/import labels ------------------------------------------------------


test_that("Save/import", {
    remove_last_save()
    x = mtcars2 %>%
        save_labels() %>%
        dplyr::transmute(disp=as.numeric(disp)+1) %>%
        import_labels()
    expect_equal(get_label(x$disp), "Displacement (cu.in.)")
})




# Utils -------------------------------------------------------------------


test_that("rename_dataframe_with_labels ", {
    x=rename_dataframe_with_labels(mtcars2)
    expect_equal(names(x), unname(get_label(mtcars2)))
})


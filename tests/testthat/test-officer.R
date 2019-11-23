
crosstables = list(
    simple_test = cross(cbind(...) ~ ., esoph, test = TRUE),
    double_test = cross(cbind(mpg,cyl,disp) ~ factor(am), mtcars, test = TRUE),
    triple_test = cross(cbind(...) ~ Species, iris, test = TRUE),
    simple_no_test = cross(cbind(...) ~ ., esoph, test = F),
    double_no_test = cross(cbind(mpg,cyl,disp) ~ factor(am), mtcars, test = F),
    triple_no_test = cross(cbind(...) ~ Species, iris, test = F)
    #des warnings sur wilcoxon mais OSEF
)

#TODO body_add_crosstable when compacted before function

test_that("crosstables don't throw errors in officer", {
    doc <- read_docx()
    #test with compact=F et compact=T
    for (i in names(crosstables)) {
        print(i)
        crosstable = crosstables[[i]]
        doc = doc %>% 
            body_add_title(i, 1) %>%
            body_add_title("Not compacted", 2) %>%
            body_add_crosstable(crosstable, show.test.name = F, auto.fit = T) %>%
            body_add_break %>%
            body_add_title("Compacted in function", 2) %>%
            body_add_crosstable(crosstable, TRUE, show.test.name = F, auto.fit = T) %>% 
            body_add_break %>% 
            body_add_title("Compacted before function", 2) %>%
            body_add_normal("TODO") %>% 
            # body_add_crosstable(compact(crosstable), show.test.name = F, auto.fit = T) %>% 
            body_add_break 
    }
    print(doc, "test_cross_officer.docx")
})



test_that("crosstables are OK as flextables", {
    library(purrr)
    cross=biostat2::cross
    library(dplyr)
    library(officer)
    library(Hmisc)
    showNA = c("no", "ifany")
    test = c(T,F)
    effect = c(T,F)
    label = T
    total = list(F,T,1,2)
    
    #TODO, si total et (NA dans variable BY), mettre un footer "les chiffres sont pas bons"
    mtcars2 = mtcars %>% 
        mutate(
            gear=as.factor(gear), 
            gear=ifelse(row_number() %in% 17:18, NA, gear),
            drat=ifelse(between(drat, 3.5, 3.7), NA, drat),
            am=ifelse(am==0, "automatic", "manual"),
            am=ifelse(row_number() %in% 3:4, NA, am)
        )
    label(mtcars2$drat) = "Rear axle ratio"
    label(mtcars2$gear) = "Number of forward gears"
    label(mtcars2$am) = "Transmission"
    
    x = expand.grid(showNA, test, effect, label, total, stringsAsFactors = F) %>% arrange #32 possibilités
    cross_tables = x %>% pmap(~{
        # print(paste(..1, ..2, ..3, ..4, ..5))
        cross(
            cbind(drat, gear) ~ am,
            data=mtcars2,
            showNA = ..1,
            margin = 2,
            test=..2,
            effect=..3,
            label=..4,
            total=..5
        )
    })
    
    # # Une table au hasard
    # x %>% sample_n(1) %>% pmap(~{
    #     print(paste(..1, ..2, ..3, ..4, ..5, sep=" -- "))
    #     cross(
    #         cbind(drat, gear) ~ am,
    #         data=mtcars2,
    #         margin = 2,
    #         show.method = F,
    #         showNA = ..1,
    #         test=..2,
    #         effect=..3,
    #         label=..4,
    #         total=..5
    #     )
    # }) %>% first %>% cross_to_flextable
    
})

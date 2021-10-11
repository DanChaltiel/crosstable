
#Located in examples/





# Officer v0.3 ------------------------------------------------------------
devtools::install_version("officer", "0.3.19")
packageVersion("officer")

# officer::styles_info(officer::read_docx()) %>% arrange(style_type)


doc1 = read_docx() %>%
    body_add_title("Older officer versions", 1) %>% 
    body_add_normal("Crosstable v{packageVersion('crosstable')}, officer v{packageVersion('officer')}.", 1) %>% 
    body_add_normal("") %>% 
    body_add_normal("As you can see in Tables \\@ref(tab1) and others, ",
                    " and in Figures \\@ref(fig1) and others, ",
                    "the iris dataset is about flowers.") %>%
    
    body_add_title("Tables", 2) %>% 
    body_add_table_legend("default", bookmark="tab1") %>%
    body_add_table_legend("default == style=strong + legend_style=Normal", 
                          style="strong", 
                          legend_style="Normal") %>%
    body_add_table_legend("style=NULL, legend_style=Normal", 
                          style=NULL, 
                          legend_style="Normal") %>%
    body_add_table_legend("style=strong, legend_style=Table Caption", 
                          style="strong", 
                          legend_style="Table Caption") %>%
    body_add_table_legend("style=NULL, legend_style=centered", 
                          style=NULL, 
                          legend_style="centered") %>%
    body_add_table_legend("style=Titre2Car, legend_style=centered", 
                          style="Titre 2 Car", 
                          legend_style="centered") %>%
    
    
    body_add_title("Figures", 2) %>% 
    body_add_figure_legend("default", bookmark="fig1") %>%
    body_add_figure_legend("default == style=strong + legend_style=Normal", 
                           style="strong", 
                           legend_style="Normal") %>%
    body_add_figure_legend("style=NULL, legend_style=Normal", 
                           style=NULL, 
                           legend_style="Normal") %>%
    body_add_figure_legend("style=strong, legend_style=Image Caption", 
                           style="strong", 
                           legend_style="Image Caption") %>%
    body_add_figure_legend("style=NULL, legend_style=centered", 
                           style=NULL, 
                           legend_style="centered") %>%
    body_add_figure_legend("style=Titre2Car, legend_style=centered", 
                           style="Titre 2 Car", 
                           legend_style="centered") %>%
    identity()

# write_and_open(doc1)

saveRDS(doc1, "examples/test_officer_v0.3.19_crosstable.rds")
write_and_open(doc1, "examples/test_officer_v0.3.19_crosstable.docx")



# Officer v0.4 ------------------------------------------------------------

install.packages("officer") #0.4+
packageVersion("officer")

#"Table Caption" (default) style is bold italic 12
fp = fp_text_lite(underlined=TRUE, font.size=15)
fp2 = fp_text_lite(italic=TRUE, font.size=10)
doc2 = read_docx() %>%
# doc2 = readRDS("examples/test_officer_v0.3.19_crosstable.rds") %>% 
#     body_add_break() %>%
    # body_add_title("Newer officer versions (v0.4+)", 1) %>%
    body_add_title("Crosstable v{packageVersion('crosstable')}, officer v{packageVersion('officer')}.", 1) %>%
    # body_add_normal("Crosstable v{packageVersion('crosstable')}, officer v{packageVersion('officer')}.", 1) %>%
    body_add_normal("") %>%
    body_add_normal("As you can see in Tables \\@ref(tab1), \\@ref(tab2) and others, ",
                    " and in Figures \\@ref(fig1) and others, ",
                    "the iris dataset is about flowers.") %>%
    body_add_normal("Note that the format in the text matches the format in `name_format`.") %>%
    body_add_normal("fp = underlined, size=15") %>%
    body_add_normal("fp2 = italic, size=9") %>%
    # body_add_list(c("fp = underlined, size=15", 
    #                 "fp2 = italic, size=9")) %>%

    body_add_title("Tables", 2) %>%
    body_add_table_legend("default (legend_style=Normal, name_format=bold)", bookmark="tab1") %>%
    body_add_table_legend("legend_style=default (normal), name_format=fp", bookmark="tab2",
                          name_format=fp) %>%
    body_add_table_legend("legend_style=default (normal), name_format=fp2",
                          name_format=fp2) %>%
    body_add_table_legend("legend_style=centered, name_format=fp2",
                          name_format=fp2, legend_style="centered") %>%
    body_add_table_legend("legend_style=Balloon Text, name_format=NULL",
                          legend_style="Balloon Text") %>%
    body_add_table_legend("legacy: legend_style=Normal, style=strong",
                          legacy=TRUE, style="strong", legend_style="Normal") %>%

    body_add_title("Figures", 2) %>%
    body_add_figure_legend("Legend of a figure", bookmark="fig1") %>%
    identity()

# write_and_open(doc2)

saveRDS(doc2, "examples/test_officer_v0.4_crosstable.rds")
write_and_open(doc2, "examples/test_officer_v0.4_crosstable.docx")

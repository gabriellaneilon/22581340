Regression_Plot <- function(extracted_data){

    dfreg <- extracted_data %>%
        select(location, greatest_new_cases, average_density)

    Reglist <-
        dfreg %>% group_by(continent) %>%
        do(reg = lm(as.formula("greatest_new_cases ~ average_density"), data = .)) %>%
        # Make each taster a list so we can use map:
        group_split(location)


    g <- Reglist %>% map(~Reg_Graph_Creator(.)) %>% cowplot::plot_grid(plotlist = ., ncol = 1)

    g

}

plot_relationship <- function(covid_data) {
    continents <- c("Asia", "Africa", "Europe", "North America", "South America", "Oceania")
    severity <- covid_data %>%
        filter(!is.na(excess_mortality) & !is.na(human_development_index) & !is.na(continent) & continent %in% continents) %>%
        group_by(location) %>%
        summarize(new_deaths = sum(new_deaths_smoothed_per_million, na.rm = TRUE),
                  total_smokers = sum(female_smokers + male_smokers, na.rm = TRUE))

    threshold <- median(severity$total_smokers)
    severity$StatementTrue <- severity$total_smokers > threshold

    # Calculate the percentage of continents where the statement is true
    percentage <- sum(severity$StatementTrue) / nrow(severity) * 100

    # Create the scatter plot
    smoke <- ggplot(severity, aes(x = total_smokers, y = new_deaths)) +
        geom_point() +
        geom_hline(yintercept = threshold, linetype = "dashed", color = "red") +
        theme_minimal() +
        labs(title = "Relationship between Total Smokers and New Deaths from COVID-19",
             x = "Total Smokers",
             y = "New Deaths") +
        annotate("text", x = 5000, y = 1000, label = paste0("Total Countries: ", nrow(severity))) +
        annotate("text", x = 5000, y = 920, label = paste0("Statement True: ", round(percentage, 2), "%"),
                 color = "blue")


    #diabetes

    diabetes <- covid_data %>%
        filter(!is.na(excess_mortality) & !is.na(human_development_index) & !is.na(continent) & continent %in% continents) %>%
        group_by(location) %>%
        summarize(new_deaths = sum(new_deaths_smoothed_per_million, na.rm = TRUE),
                  total_diabetes = sum(diabetes_prevalence, na.rm = TRUE))

    threshold2 <- median(diabetes$total_diabetes)
    diabetes$StatementTrue <- diabetes$total_diabetes > threshold2

    # Calculate the percentage of continents where the statement is true
    percentage2<- sum(diabetes$StatementTrue) / nrow(diabetes) * 100

    # Create the scatter plot
    diabetes_plot <- ggplot(diabetes, aes(x = total_diabetes, y = new_deaths)) +
        geom_point() +
        geom_hline(yintercept = threshold2, linetype = "dashed", color = "red") +
        theme_minimal() +
        labs(title = "Relationship between Diabetes and New Deaths from COVID-19",
             x = "Diabetes",
             y = "New Deaths") +
        annotate("text", x = 500, y = 750, label = paste0("Total Countries: ", nrow(diabetes))) +
        annotate("text", x = 500, y = 700, label = paste0("Statement True: ", round(percentage2, 2), "%"),
                 color = "blue")

    return(grid.arrange(smoke, diabetes_plot, nrow = 2))

}

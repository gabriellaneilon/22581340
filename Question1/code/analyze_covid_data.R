analyze_covid_data <- function(covid_data) {
    # Filter relevant columns
    continents=c("Asia", "Africa", "Europe", "North America", "South America", "Oceania")


    # Calculate summary statistics
    summary_stats <- covid_data %>%
        filter(continent %in% continents) %>%
        group_by(continent) %>%
        summarize(new_deaths = sum(new_deaths_smoothed_per_million, na.rm = TRUE),
                  new_cases = sum(new_cases_smoothed_per_million, na.rm = TRUE),
                  vaccination_rollout = sum(new_vaccinations_smoothed_per_million, na.rm = TRUE),
                  avg_gdp_per_capita = sum(gdp_per_capita, na.rm = TRUE))

    # Create the plot
    plot <- ggplot(summary_stats) +
        geom_point(aes(x = avg_gdp_per_capita, y = vaccination_rollout, size = new_deaths, color = continent)) +
        labs(x = "GDP per capita", y = "Vaccination Rollout", size = "New Deaths", color = "Continent") +
        theme_minimal() +
        scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
        scale_y_continuous(labels = function(y) format(y, scientific = FALSE))



    # Return the plot
    return(plot)
}




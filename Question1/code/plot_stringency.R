#now put it into a function
plot_stringency <- function(covid_data) {
    continents=c("Asia", "Africa", "Europe", "North America", "South America", "Oceania")
    continent_data <- covid_data %>%
        filter(continent %in% continents) %>%
        group_by(continent) %>%
        summarise(new_deaths = sum(new_deaths_smoothed_per_million, na.rm = TRUE),
                  new_cases = sum(new_cases_smoothed_per_million, na.rm = TRUE),
                  avg_stringency = mean(stringency_index, na.rm = TRUE))

    ggplot(continent_data) +
        geom_point(aes(x = new_deaths, y = new_cases, size = avg_stringency, color = continents)) +
        labs(x = "New Deaths", y = "New Cases", size = "Average Stringency", color = "Continent") +
        theme_minimal() +
        scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
        scale_y_continuous(labels = function(y) format(y, scientific = FALSE))
}

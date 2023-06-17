create_style_evolution_plot <- function(data, title) {
    tfidf <- data %>%
        distinct(album, acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence, tempo) %>%
        arrange(album)

    long_data <- tfidf %>%
        tidyr::pivot_longer(cols = -album, names_to = "variable", values_to = "value")

    ggplot(long_data, aes(x = album, y = value, fill = variable)) +
        geom_col(show.legend = FALSE) +
        labs(x = "Variable", y = "Value", title = title) +
        theme_minimal() +
        facet_wrap(~variable, scales = "free", ncol = 5) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))


}

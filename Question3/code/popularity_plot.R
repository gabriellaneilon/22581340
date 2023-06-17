popularity_plot <- function(data, title) {
    data%>%
        filter(!is.na(album)) %>%  # Exclude rows with NA album
        mutate(album = factor(album),  # Specify the correct order of the album names
               Text = ifelse(popularity == min(popularity, na.rm = TRUE), glue::glue("Lowest: {album}, ({name}) :{popularity}"),
                             ifelse(popularity == max(popularity, na.rm = TRUE), glue::glue("Highest: {album}, ({name}) :{popularity}"),
                                    NA_character_))) %>%
        ggplot() +
        geom_boxplot(aes(x = album, y = popularity, fill = album), alpha = 0.4) +
        geom_jitter(aes(x = album, y = popularity, color = album), size = 3, alpha = 0.8) +
        ggrepel::geom_text_repel(aes(x = album, y = popularity, label = Text), force = TRUE, size = 3) +  # Adjust the size parameter for smaller text
        theme_classic() +
        labs(x = "Albums", y = "Popularity",
             title = title) +
        scale_color_hue(l = 40, c = 35) +
        guides(fill = FALSE, color = FALSE)+
        theme(axis.title.x = element_text(face = "bold", colour = "black"),
              axis.text.x = element_text(face = "italic", angle = 90),
              axis.title.y = element_text(face = "bold", colour = "black"),
              text = element_text(size = 12, family = "Times"))


}

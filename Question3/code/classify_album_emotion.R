classify_album_emotion <- function(data, title) {
    # Define the threshold values
    threshold <- list(acousticness = 0.2952089,
                      danceability = 0.4178667,
                      energy = 0.5618405,
                      instrumentalness = 0.2120298,
                      tempo = 124.3586,
                      loudness = -9.406095,
                      speechiness = 0.04195873,
                      valence = 0.2333286)

    data %>%
        mutate(emotion = ifelse(acousticness > threshold$acousticness &
                                    danceability < threshold$danceability |
                                    energy < threshold$energy &
                                    instrumentalness > threshold$instrumentalness |
                                    tempo < threshold$tempo &
                                    loudness > threshold$loudness |
                                    speechiness < threshold$speechiness |
                                    valence < threshold$valence, "Sad", "Happy")) %>%
        group_by(album, emotion) %>%
        summarise(count = n()) %>%
        mutate(proportion = count / sum(count)) %>%
        ggplot(aes(x = "", y = proportion, fill = emotion)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        facet_wrap(~album) +
        labs(x = NULL, y = NULL, fill = "Emotion", title = title) +
        theme_minimal() +
        theme(legend.position = "right") +
        scale_fill_manual(values = c("Sad" = "lightblue", "Happy" = "gold"))
}

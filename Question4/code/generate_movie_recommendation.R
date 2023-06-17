generate_movie_recommendation <- function(data, name, production_countries, imdb_score, genres) {
    # Filter movies based on the provided features
    filtered_movies <- data %>%
        filter(
            name %in% name,
            production_countries == production_countries,
            imdb_score >= imdb_score,
            imdb_score %in% imdb_score
        )

    # Sort the movies by popularity or score, depending on preference
    sorted_movies <- filtered_movies %>%
        arrange(desc(tmdb_popularity))

    # Return the top recommended movie
    if (nrow(sorted_movies) > 0) {
        top_movie <- sorted_movies[1, "title"]
        return(top_movie)
    } else {
        return("No movie recommendation found.")
    }
}

library(dplyr)
library(tidyverse)

# Function to take a directory, filename, and dataframe and write it to CSV (creating the dir if necessary)
write_csv_to_dir = function(dataframe, output_dir, output_filename) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  write_csv(dataframe, paste0(output_dir,"/",output_filename))
  
}


# R function to create a table by combining the individual
# and area based tree stats calculated in the previous steps

# tmp_dir: where temp files containing
# stats for each tao are stored
# out_file_location: where the final table of stats that
# will be used for decision making is stored
# predicted_trees_file: TAO that is analysed
combine_stats <- function(tmp_dir,
                          output_dir,
                          predicted_trees_filepath) {
    predicted_tree_dataset_name <-
        tools::file_path_sans_ext(basename(predicted_trees_filepath))

    match_stats_filepath <- file.path(
        tmp_dir,
        paste0(predicted_tree_dataset_name, "_match_stats.csv")
    )

    match_stats <- read.csv(match_stats_filepath)

    area_based_stats_filepath <- file.path(
        tmp_dir,
        paste0(predicted_tree_dataset_name, "_areabased_stats.csv")
    )

    area_based_stats <- read.csv(area_based_stats_filepath)


    # Combine the area-based stats and individual-tree-detection stats (for each tree height category and tree position class), making sure to match them based on tree height categry and tree position class

    combine_stats <- full_join(match_stats %>% select(-X),
        area_based_stats %>% select(-X),
        by = c("height_cat", "canopy_position")
    ) %>%
        dplyr::select(
            canopy_position,
            height_cat,
            everything()
        )

    combine_stats$predicted_tree_dataset_name <- predicted_tree_dataset_name

    combine_stats <- combine_stats %>%
        # mutate_if(is.numeric, round, digits = 3) %>%
        rename(
            area_density_mae = mean_abs_err,
            area_density_bias = mean_bias,
            area_density_correlation = correlation,
            area_density_mae_percent = mean_abs_err_pct
        )
    
    output_dir = file.path(output_dir, "tree_detection_evals")
    
    write_csv_to_dir(dataframe = combine_stats,
                     output_dir = output_dir,
                     output_filename = paste0("stats_",predicted_tree_dataset_name,".csv"))

}

# Function to save statistics to go as input into correlation plots
corr_plot_stats <- function(csv_filepath) {
    stem_predicted <- read_csv(csv_filepath, show_col_types = FALSE)

    countN <- nrow(stem_predicted)

    rmse <- sqrt(mean(
        (stem_predicted$predicted_tree_height - stem_predicted$observed_tree_height)^2
    ))

    r2 <- cor(stem_predicted$predicted_tree_height, stem_predicted$observed_tree_height)^2

    stats <- list("stats" = c("count" = countN, "rmse" = rmse, "r2" = r2))
    return(stats)
}

# Function to save ggplot objects to go as input into correlation plots
corr_plot_obj <- function(csv_filepath, output_dir) {
    csv_basename <- tools::file_path_sans_ext(basename(csv_filepath))

    canopy_position <- gsub(".*[^all|single]", "", csv_basename)

    # plot_title will need to be modified depending on the
    # agreed upon naming convention
    plot_title <- gsub("trees_matched_", "", csv_basename)


    stem_predicted <- read_csv(csv_filepath, show_col_types = FALSE)

    out_fold <- file.path(
        output_dir,
        "correlation_figures"
    )

    if (!dir.exists(out_fold)) {
        dir.create(out_fold,
            recursive = TRUE,
            showWarnings = FALSE
        )
    }

    out_plot_filepath <- file.path(
        out_fold,
        paste0(plot_title, ".png")
    )

    out <- list(
        "plot_objs" = c(
            "plot_title" = plot_title,
            "out_plot_filepath" = out_plot_filepath,
            "canopy_position" = canopy_position
        ),
        "df" = as.data.frame(stem_predicted)
    )

    return(out)
}

# Function to save the correlation plot itself by taking the outputs from the
# two functions above
corr_plot <- function(corr_plot_stats_i,
                      corr_plot_obj_i,
                      width,
                      height,
                      units) {
    df <- as.data.frame(corr_plot_obj_i[["df"]])
    plot_title <- corr_plot_obj_i[["plot_objs"]][["plot_title"]]
    canopy_position <- corr_plot_obj_i[["plot_objs"]][["canopy_position"]]
    fname <- corr_plot_obj_i[["plot_objs"]][["out_plot_filepath"]]

    r2 <- corr_plot_stats_i[["stats"]][["r2"]]
    rmse <- corr_plot_stats_i[["stats"]][["rmse"]]
    countN <- corr_plot_stats_i[["stats"]][["count"]]

    corr_plot_name <- ggplot(
        df,
        aes(x = observed_tree_height, y = predicted_tree_height),
    ) +
        geom_abline(slope = 1, intercept = 0, color = "red") +
        labs(
            x = "Observed tree height (m)",
            y = "Predicted tree height (m)",
            title = paste(
                plot_title,
                "\nTree Position :",
                str_to_sentence(canopy_position, locale = "en")
            ),
            subtitle = paste(
                "Rsq :", round(r2, 3),
                "\nRMSE :", round(rmse, 3),
                "\nNumber of matched trees :",
                countN
            )
        ) +
        geom_point() +
        theme_bw(15)

    ggsave(
        filename = fname,
        plot = corr_plot_name,
        width = width,
        height = height,
        units = units,
        device = "png"
    )
}

# Function to create correlation plots of tree heights in
# groundtruth and TAO datasets
make_height_corr_plots <- function(output_dir, predicted_trees_filepath) {
    predicted_tree_dataset_name <-
        tools::file_path_sans_ext(basename(predicted_trees_filepath))

    tree_pos <- c("all", "overstory")

    csv_filepath <- list.files(
        file.path(
            output_dir,
            "matched_tree_lists"
        ),
        pattern = paste0(
            "trees_matched_", predicted_tree_dataset_name, "_", tree_pos, ".csv",
            collapse = "|"
        ),
        all.files = TRUE,
        full.names = TRUE
    )

    corr_plots_call <- function(csv_filepath, output_dir) {
        # Calculate statistics for correlation plot
        corr_plot_stats_i <- lapply(csv_filepath, corr_plot_stats)

        # Save ggplot objects for correlation plot
        corr_plot_obj_i <- mapply(
            corr_plot_obj, csv_filepath, output_dir,
            SIMPLIFY = FALSE, USE.NAMES = FALSE
        )

        # Make plots
        mapply(
            corr_plot,
            corr_plot_stats_i,
            corr_plot_obj_i,
            20,
            20,
            "cm"
        )
    }

    mapply(corr_plots_call, csv_filepath, output_dir)
}

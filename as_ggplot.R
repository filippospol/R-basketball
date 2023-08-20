pacman::p_load(ggplot2,gt,gtsummary,magick)
as_ggplot <- function(x, ...) {
  
  if (!inherits(x, c("gt_tbl", "gtsummary"))) stop("`x=` must be a 'gt' or 'gtsummary' table", call. = FALSE)
  
  if (inherits(x, "gtsummary")) x <- gtsummary::as_gt(x)
  
  path_gt_table_image <- fs::file_temp(ext = "png")
  gt_table_image <- gt::gtsave(x, filename = path_gt_table_image, ...)
  
  table_img <-
    magick::image_read(path_gt_table_image) %>%
    magick::image_ggplot(interpolate = TRUE)
  
  table_img
}




useless_df_grow <- function(initial_rows = 3,
                           initial_cols = 2,
                           iterations   = 4,
                           n_cols       = 2,  
                           m_rows       = 3) {
  

  make_col_names <- function(indices) paste0("C_", indices)
  
  df <- as.data.frame(
    matrix(
      data = round(runif(initial_rows * initial_cols, 1, 100)),
      nrow = initial_rows,
      ncol = initial_cols,
      dimnames = list(NULL, make_col_names(seq_len(initial_cols)))
    )
  )
  
  for (i in seq_len(iterations)) {
    
    current_rows <- nrow(df)
    current_cols <- ncol(df)
    
    new_rows <- as.data.frame(
      matrix(
        data     = round(runif(m_rows * current_cols, 1, 100)),
        nrow     = m_rows,
        ncol     = current_cols,
        dimnames = list(NULL, names(df))
      )
    )
    df <- rbind(df, new_rows)
     
    new_col_indices <- (current_cols + 1):(current_cols + n_cols)
    new_cols <- as.data.frame(
      matrix(
        data     = round(runif(nrow(df) * n_cols, 1, 100)),
        nrow     = nrow(df),
        ncol     = n_cols,
        dimnames = list(NULL, make_col_names(new_col_indices))
      )
    )
    df <- cbind(df, new_cols)
  }
  return(df)
}


set.seed(2908)
final_df <- useless_df_grow(2,2,120,1,3)

head(final_df,5)
tail(final_df,5)

#####
# ToDo: add possiblity to just add columns or just rows!
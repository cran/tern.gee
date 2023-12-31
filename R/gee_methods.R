#' Methods for GEE Models
#'
#' Additional methods which can simplify working with the GEE result object.
#' @name gee_methods
#' @returns `VarCorr()` returns the estimated covariance matrix, and
#'   `QIC()` returns the QIC value.
NULL

#' @rdname gee_methods
#' @importFrom nlme VarCorr
#'
#' @param x (`tern_gee`)\cr result of [fit_gee()].
#' @inheritParams nlme::VarCorr
#'
#' @exportS3Method
VarCorr.tern_gee <- function(x, sigma = 1, ...) { # nolint
  dim_mat <- length(x$visit_levels)
  tmp <- id_mat <- diag(dim_mat)
  corest <- x$geese$alpha

  # Start with lower-triangular matrix part.
  lower_mat <- switch(x$corstr,
    unstructured = , # Since this is the same as exchangeable, we can do this.
    exchangeable = {
      tmp[lower.tri(tmp)] <- corest
      tmp
    },
    ar1 = {
      row_col_diff <- row(tmp) - col(tmp)
      tmp[lower.tri(tmp)] <- corest^(row_col_diff[lower.tri(row_col_diff)])
      tmp
    },
    `m-dependent` = {
      row_col_diff <- row(tmp) - col(tmp)
      tmp[lower.tri(tmp)] <- corest[row_col_diff[lower.tri(row_col_diff)]]
      tmp
    }
  )

  # Construct the full symmetric matrix.
  mat <- lower_mat + t(lower_mat) - id_mat
  rownames(mat) <- colnames(mat) <- x$visit_levels
  mat
}

#' @rdname gee_methods
#' @importFrom geepack QIC
#'
#' @param object (`tern_gee`)\cr result of [fit_gee()].
#' @inheritParams geepack::QIC
#'
#' @exportS3Method
QIC.tern_gee <- function(object, ...) { # nolint
  object$qic
}

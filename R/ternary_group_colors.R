# ---- Fixed colours for categorical groups in the ternary plots ----
# Optional Parameter 2 as a categorical grouping used to hand out colours in
# order of first appearance from a pastel palette (Set3). The same group
# then got another colour in a second dataset that lists its groups in a
# different order, and pale yellow / grey groups were hard to see on white.
# Here a group's colour depends only on its name: the groups of the column
# (all of them, not just the selected ones) are sorted naturally and take
# the palette colours in that order, so two datasets with the same groups
# give the same colours whatever the row order or the group selection.

# Dark, colour-blind-friendly qualitative colours (Okabe-Ito and Tol sets,
# without their pale yellows).
TERNARY_GROUP_PALETTE <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9",
                           "#332288", "#AA4499", "#117733", "#882255", "#44AA99", "#999933")

#' Colour of each categorical group, decided by the group's name
#'
#' @param groups All group values of the column (any order, repeats allowed).
#' @return A named character vector, group name -> colour, in natural sorted
#'   order of the names. More groups than palette colours are coloured with
#'   `grDevices::hcl.colors(n, "Dark 3")`.
#' @export
group_colors_by_name <- function(groups) {
  nm <- natural_sort_ids(groups[!is.na(groups)])
  n <- length(nm)
  if (n == 0) return(setNames(character(0), character(0)))
  cols <- if (n <= length(TERNARY_GROUP_PALETTE)) TERNARY_GROUP_PALETTE[seq_len(n)] else grDevices::hcl.colors(n, "Dark 3")
  setNames(cols, nm)
}

#' rctflow diagram
#'
#' data obtained from kaggle opensourcedata
#' @description
#' a package that creates a flowchart describing RCT data, with one control and one sham group using `DiagrammeR`
#'
#' @param data A data frame with one row per participant that includes at least:
#'   - `subject`: unique participant identifier (no `NA` for randomized subjects).
#'   - a grouping variable (e.g. `Group`) that indicates randomization to
#'     active vs sham/control.
#' @param col_name Optional character string used in the diagram (for example,
#'   as a dataset label or title; e.g., `"RCT_tDCS"`).
#'
#' @returns A `DiagrammeR` htmlwidget object containing the flowchart.
#' @author Jil Soni, Lamya Serhir
#' @source Example dataset: `RCT_tDCS`
#' @examples
#' \dontrun{
#' library(DiagrammeR)
#'
#' # Example: create a flowchart from RCT_tDCS
#' rctflow(
#'   data    = RCT_tDCS,
#'   group_col = "Group"
#' )
#' }
#' @export
rctflow <- function(data,
                    group_col,
                    group_labels = NULL) {
  n_total <- nrow(data)
  group_vec <- data[[group_col]]
  group_counts <- table(group_vec)
  labels <- names(group_counts)
  n_group1 <- unname(group_counts[1])
  n_group2 <- unname(group_counts[2])
  label_group1 <- labels[1]
  label_group2 <- labels[2]

  library(DiagrammeR)
  grViz(sprintf("
    digraph {

      graph [layout = dot, rankdir = TB, splines = ortho]

      node [
        shape = box,
        style = 'rounded, filled',
        fillcolor = whitesmoke,
        color = black,
        fontname = Helvetica,
        fontsize = 12,
        width = 3
      ]

      A [label = 'Enrollment', fillcolor = lightblue]
      B [label = 'Assessed for eligibility']
      C_spine [
        shape = point,
        width = 0,
        height = 0,
        label = ''
      ]
      C [label = 'Randomised n=%d']

      D [label = 'Excluded\\n• Not meeting criteria\\n• Declined\\n• Other reasons']
      E [shape=point, width=0, label='', style=invis]
      G [style = invis, shape = point, width = 0, height = 0]
      H [style = invis, shape = point, width = 0, height = 0]

      F [label='Allocated to %s (n=%d)']
      M [label='Allocation', shape=box, fillcolor=lightblue, fixedsize=true, width=1.8]
      I [label='Allocated to %s (n=%d)']
      J [label='6 month follow up 1']
      L [label='Follow Up', shape=box, fillcolor=lightblue, fixedsize=true, width=1.8]
      K [label='6 month follow up 2']
      O [label='Final Analysis 1']
      Q [label='Analysis', shape=box, fillcolor=lightblue, fixedsize=true, width=1.8]
      P [label='Final Analysis 2']

      { rank = min; A }
      { rank = same; C_spine; D }
      { rank = same; F; I }
      { rank = same; E; H }
      { rank = same; F; M; I }
      { rank = same; J; L; K }
      { rank = same; O; Q; P }
      { rank = same; G; E; H }

      G -> E [ arrowhead = none, weight = 100, minlen = 1]
      E -> H [ arrowhead = none, weight = 100, minlen = 1]

      A -> B [style = invis, constraint = false]
      B -> C_spine [arrowhead = none]
      C_spine -> C
      C_spine -> D [arrowhead = none]
      C -> E [arrowhead=none]
      G -> F
      H -> I
      F -> M [style=invis, arrowhead=none]
      M -> I [style=invis, arrowhead=none]
      F -> J
      I -> K
      J -> L [style=invis, arrowhead=none]
      L -> K [style=invis, arrowhead=none]
      J -> O
      K -> P
      O -> Q [style=invis, arrowhead=none]
      Q -> P [style=invis, arrowhead=none]
    }
  ",
                n_total,
                label_group1, n_group1,
                label_group2, n_group2))
}


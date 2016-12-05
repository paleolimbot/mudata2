
#' Biplot an object using ggplot
#' 
#' @param x the object to biplot
#' @param ... passed to other methods
#' 
#' @export
biplotgg <- function(x, ...) UseMethod("biplotgg")

#' @rdname biplotgg
#' @export
biplotgg.qtag.long <- function(x, column=NULL, namesx=NULL, namesy=NULL, values=NULL, 
                               errors=NULL, labeller=ggplot2::label_value, ...) {
  # essential to have things be aggregated
  x <- aggregate(x, mean, err=sd(., na.rm = TRUE)/sum(!is.na(.)))
  
  els <- NULL
  quals <- qualifiers(x)
  if(is.null(values)) {
    values <- values(x)
  } else {
    if(!(values %in% names(x))) stop("Column ", values, "was not found in x")
  }
  if(is.null(errors)) {
    if("err" %in% names(x)) {
      errors <- "err"
    }
  } else {
    if(!(errors %in% names(x))) stop("Column ", errors, "was not found in x")
  }
  if(is.null(column)) {
    column <- quals[length(quals)]
  } else {
    if(!(column %in% names(x))) stop("Column ", column, "was not found in x")
  }
  if(is.null(namesx)) {
    namesx <- unique(as.character(x[[column]]))
  }
  if(any(!((c(namesx, namesy) %in% x[[column]])))) stop("Not all names were found in ", column)
  
  if(is.null(namesy)) {
    els <- dplyr::do(
          dplyr::group_by(data.frame(i=1:(length(namesx)-1), stringsAsFactors = FALSE), i), {
        data.frame(vary=namesx[.$i], varx=namesx[(.$i+1):length(namesx)], stringsAsFactors=F)
      })
    els$i <- NULL
    namesy <- namesx[1:(length(namesx)-1)]
    namesx <- namesx[2:length(namesx)]
  } else {
    els <- expand.grid(varx=namesx, vary=namesy, stringsAsFactors = FALSE)
  }
  
  joincols <- quals[quals != column]
  tmp <- dplyr::do(dplyr::group_by(els, varx, vary), {
    if(.$varx == .$vary) {
      data.frame()
    } else {
      xs <- x[x[[column]]==.$varx, c(quals, values, errors)]
      ys <- x[x[[column]]==.$vary, c(quals, values, errors)]
      dplyr::inner_join(xs, ys, by=joincols)
    }
  })
  
  tmp$varx <- factor(tmp$varx, levels=namesx)
  tmp$vary <- factor(tmp$vary, levels=namesy)
  ggplot2::ggplot(tmp, ggplot2::aes_string(x=paste0(values, '.x'), y=paste0(values, '.y'), ...)) + 
    ggplot2::geom_point() + 
    #ggplot2::geom_errorbar(ggplot2::aes(ymax=values.x+erry, ymin=valy-erry)) +
    #ggplot2::geom_errorbarh(ggplot2::aes(xmax=valx+errx, xmin=valx-errx)) +
    ggplot2::facet_grid(vary~varx, scales="free", labeller = labeller) +
    ggplot2::labs(x=NULL, y=NULL)
}

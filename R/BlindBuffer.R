
#' @title BlindBuffer
#' @description Generates a buffer for each line segment with the blind spot near the line removed.
#'
#' @importFrom terra geom vect
#'
#' @param Line An object of class SpatVector that must be a line.
#' @param BuffDist The distance to be buffered.
#' @param startpt The distance for the blind spot buffer.
#' @param quadsegs positive integer. Number of line segments to use to draw a quart circle.  Passed to terra buffer function.
#'
#' @author Dylan Schertz
#'
#' @examples
#' BlindBuffer(Line, 685, 22)
#' @export



##################################
### Buffer with blind spot removed

BlindBuffer <- function(Line, BuffDist, EraseDist, quadsegs = 4){
  TempBuffer <- buffer(Line[1, ], width = 50, singlesided = TRUE,
                       quadsegs = quadsegs)

  OutBuffer <- TempBuffer[0,]

  for(i in 1:nrow(Line)){
    Buffer <- buffer(Line[i, ], width = BuffDist, singlesided = TRUE,
                     quadsegs = quadsegs)

    EraseBuffer <- buffer(Line[i, ], width = EraseDist, singlesided = TRUE,
                          quadsegs = quadsegs)

    OutBufferSeg <- terra::erase(Buffer, EraseBuffer)

    OutBuffer <- rbind(OutBuffer, OutBufferSeg)

  }

  return(OutBuffer)
}

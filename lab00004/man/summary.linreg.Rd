\name{summary.linreg}
\alias{summary.linreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.linreg(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (X) 
{
    S1 <- X$coefficients
  S2 <- X$standard_error
  S3 <- X$t_values
  S4 <- X$p_values
  S5 <- X$Residual_standard_error
  result <- data.frame(
    Coefficient = S1,
    Standard_Error = S2,
    t_value = S3,
    p_value = S4,
    Significance = character(length(S4)),  # 创建用于存储显著性符号的列
    stringsAsFactors = FALSE
  )
  
  # 为每个系数计算显著性符号
  for (i in seq_along(S4)) {
    if (S4[i] < 0.001) {
      result$Significance[i] <- "***"
    } else if (S4[i] < 0.01) {
      result$Significance[i] <- "**"
    } else if (S4[i] < 0.05) {
      result$Significance[i] <- "*"
    } else {
      result$Significance[i] <- ""
    }
  }
  
  # 打印结果
  print(result, row.names = TRUE)
  
  cat("Residual standard error:", S5, "on", X$degrees_of_freedom,"degrees of freedom")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

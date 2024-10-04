\name{linreg}
\alias{linreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 ~~function to do ... 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
linreg(formula, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function (formula, data) 
{
    X <- model.matrix(formula, data)
    y <- data[[all.vars(formula)[1]]]
    beta_hat <- solve(t(X) \%*\% X) \%*\% t(X) \%*\% y
    y_hat <- X \%*\% beta_hat
    e_hat <- y - y_hat
    n <- nrow(X)
    p <- ncol(X)
    df <- n - p
    sigma_square_hat <- as.numeric(t(e_hat) \%*\% e_hat/df)
    var_beta_hat <- sigma_square_hat * solve(t(X) \%*\% X)
    t_values <- beta_hat/(sqrt(diag(var_beta_hat)))
    p_values <- 2 * pt(-abs(t_values), df)
    qr_decomp <- qr(X)
    Q <- qr.Q(qr_decomp)
    R <- qr.R(qr_decomp)
    beta_hat1 <- solve(R) \%*\% t(Q) \%*\% y
    var_beta_hat1 <- sigma_square_hat * solve(t(R) \%*\% R)
    result <- list(coefficients = beta_hat, fitted_values = y_hat, 
        residuals = e_hat, degrees_of_freedom = df, residual_variance = sigma_square_hat, 
        variance_coefficients = var_beta_hat, t_values = t_values, 
        p_values = p_values, coefficients1 = beta_hat1, variance_coefficients1 = var_beta_hat1)
    class(result) <- "linreg"
    return(result)
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

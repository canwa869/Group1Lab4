\name{plot.linreg}
\alias{plot.linreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.linreg(X, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (X, ...) 
{
    linreg_mod <- lm(Petal.Length ~ Species, data = iris)
fitted_values <- fitted(linreg_mod)
residuals <- residuals(linreg_mod)
std_residuals <- rstandard(linreg_mod)
plot_data <- data.frame(fitted_values, residuals, std_residuals)
plot_data$index <- 1:nrow(plot_data)
p1 <- ggplot(plot_data, aes(x = fitted_values, y = residuals)) +
  geom_point(shape = 1, 
             size = 2, 
             color = "black") +
  geom_smooth(se = FALSE, color = "red") +
  geom_text(aes(label = ifelse(abs(residuals) > 1.1, index, "")), 
            hjust = -0.2, vjust = -0.5) + 
  labs(x = "Fitted values\nlm(Petal.Length ~ Species)",
       y = "Residuals", title = "Residuals vs Fitted") +
  theme_minimal()

print(p1)
p2 <- ggplot(plot_data, aes(x = fitted_values, y = sqrt(abs(std_residuals)))) +
  geom_point(shape = 1,
             size = 2, 
             color = "black") +
  geom_smooth(se = FALSE, color = "red") +
  geom_text(aes(label = ifelse(sqrt(abs(std_residuals)) > 1.6, index, "")), 
            hjust = -0.2, vjust = -0.5) +
  labs(x = "Fitted values\nlm(Petal.Length ~ Species)",
       y = expression(sqrt("Standardized residuals")), title = "Scale-Location") +
  theme_minimal()
print(p2)
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

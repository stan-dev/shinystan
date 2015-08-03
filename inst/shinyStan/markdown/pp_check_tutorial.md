## Using Stan and ShinyStan for posterior predictive checking

In this tutorial we do the following:

1. Generate some fake data to play with
2. Write code for a simple Stan model
3. Fit the model using **RStan** 
4. Use **ShinyStan** for graphical posterior predictive checks

### Data

First we'll generate some fake data in R to use for this example

    # Number of observations 
    N <- 100

    # Model matrix (with column of 1s for intercept and one covariate)
    X <- cbind(Const = 1, X1 = rnorm(N))
    K <- ncol(X)

    # Generate fake outcome y
    beta <- c(2, 1/2) # pick intercept and coefficient
    sigma <- 1 # standard deviation
    y <- rnorm(N, mean = X %*% beta, sd = sigma) # generate data


### Stan code

Now we can write Stan code for a simple linear regression model.

    data {
      int           N ; # integer, number of observations
      int           K ; # integer, number of columns in model matrix
      matrix[N,K]   X ; # N by K model matrix
      vector[N]     y ; # vector of N observations
    }
    
    parameters {
      real<lower=0> sigma ; # real number > 0, standard deviation
      vector[K]     beta ;  # K-vector of regression coefficients
    }
    
    model {
      beta ~ normal(0, 5) ;       # prior for betas
      sigma ~ cauchy(0, 2.5) ;    # prior for sigma
      y ~ normal(X*beta, sigma) ; # vectorized likelihood
    }
    
    generated quantities {
    # Here we do the simulations from the posterior predictive distribution
      vector[N] y_rep ; # vector of same length as the data y
      for (n in 1:N) 
        y_rep[n] <- normal_rng(X[n]*beta, sigma) ;
    }

In this case the posterior predictive distribution we want to simulate from is the normal distribution with mean and standard deviation updated to reflect the posterior draws of `beta` and `sigma`. 

The code in the `generated quantities` block will be evaluated for each posterior draw of the parameters. For example, if we have 100 post-warmup iterations then we will have 100 `y_rep` vectors, each of length `N`. 

### Fit the model

If we've saved our Stan code in a file called `stan_code.stan` then we can run this model with **RStan** and then launch **ShinyStan** like this:

    library(rstan)
    library(ShinyStan)
    
    # Prepare the data we'll need as a list
    stan_data <- list(y = y, X = X, N = N, K = K)
    
    # Fit the model
    stanfit <- stan(file = "stan_code.stan", data = stan_data)
    
    # Launch ShinyStan
    launch_shinystan(stanfit)


### Graphical posterior predictive checks with ShinyStan

Once we've launched **ShinyStan** we can navigate to the page for posterior predictive checking. In the dropdown menus it will ask us to select the object containing our data from our R global environment and the name of the paramter from our model containing the posterior predictive replications. So we enter `y` and `y_rep`, respectively. 

**ShinyStan** will then generate graphics that will aid in checking the fit of our model including comparisons of the distribution of the observed data to the distributions of the posterior predictive replications, distributions of test statistics, and residual plots.

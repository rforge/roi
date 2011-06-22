## utilities.R

###############################################################
## ROI helper functions


.xtQx <-
function(Q, x)
{
    ## Value of quadratic form t(x) %*% Q %*% x.
    ## As we implement simple triplet matrices in S3, we could only have
    ## %*% and crossprod methods if we created S3 generics for these ...
    if(is.simple_triplet_matrix(Q))
        sum(Q$v * x[Q$i] * x[Q$j])
    else
        c(crossprod(x, Q %*% x))
}

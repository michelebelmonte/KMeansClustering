# KMeansClustering

This is an implementation of the K Means Clustering algorithm: it's mainly motivated by the fact that I wanted to understand better the algo itself.

The implemenation was inspared by: https://github.com/akabe/ocaml-numerical-analysis/tree/master/k-means,
but along the way I decided to introduce some changes:

1. avoid mutable state

2. make some concept more explicit (cluster, distance function etc)

In the next iterations, I would like:

1. introduce a signature for the clustering module

2. split the test.ml in diffent file, one for each module

3. use functors to generate the clustering module, instead of passing functions as parameters for the initialize function

4. improve comments

5. improve building

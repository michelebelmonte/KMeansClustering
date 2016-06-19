# KMeansClustering

This is an implementation of the K Means Clustering algorithm: it's mainly motivated by the fact that I wanted to understand better the algo itself.

The implemenation was inspired by: https://github.com/akabe/ocaml-numerical-analysis/tree/master/k-means (which is also the source of the data in the dataset file),
but along the way I decided to introduce some changes:

1. avoid mutable state (even though this means degraded performance)

2. make some concept more explicit (cluster, distance function etc) (even though the code is now less compact)

The main files of the project are:

1. ClusteringTools.ml: this contains the logic to execute clustering on list of items.

2. IterativeTools: this contains the logic to run an iterative computation.

Using the signatures contained in ClusteringTools, one defines types for which he wants to execute a clustering computaion, while IterativeTools defines how a centain computation can run iteratively: implementing the CluterIterable we can run clustering as an iterative process.

In the next iterations, I would like:

1. split the test.ml in diffent file, one for each module

2. improve comments

3. improve building

Classification
================

Section 3: Classification Methods
---------------------------------

### 3.1 Decision Trees

-   If the target variable is discrete, then it's called *classification tree*, whereas for continuous outcome, it's called *regression tree*.
-   **CART** (Classification And Regression Tree) analysis is an umbrella term used for both of the above procedures.
-   Algorithm for constructing decision trees usually work top-down, by choosing a variable at each step that best splits the set of items. The "best" split is decided by [metrics](https://en.wikipedia.org/wiki/Decision_tree_learning) that measures the homogeneity of the target variable within the subsets. For a set of ![J](http://chart.apis.google.com/chart?cht=tx&chl=J "J") classes, suppose ![i\\in\\{1, 2, \\ldots, J\\}](http://chart.apis.google.com/chart?cht=tx&chl=i%5Cin%5C%7B1%2C%202%2C%20%5Cldots%2C%20J%5C%7D "i\in\{1, 2, \ldots, J\}"), and let ![p\_i](http://chart.apis.google.com/chart?cht=tx&chl=p_i "p_i") be the fraction of items labeled with class ![i](http://chart.apis.google.com/chart?cht=tx&chl=i "i") in the set.
    -   Gini Impurity: ![Gini(E) = \\sum\_{i=1}^Jp\_i(1-p\_i) = 1- \\sum\_{i=1}^Jp\_i^2](http://chart.apis.google.com/chart?cht=tx&chl=Gini%28E%29%20%3D%20%5Csum_%7Bi%3D1%7D%5EJp_i%281-p_i%29%20%3D%201-%20%5Csum_%7Bi%3D1%7D%5EJp_i%5E2 "Gini(E) = \sum_{i=1}^Jp_i(1-p_i) = 1- \sum_{i=1}^Jp_i^2")
    -   Information Gain: ![H(E) = -\\sum\_{i=1}^Jp\_i\\log\_2p\_i](http://chart.apis.google.com/chart?cht=tx&chl=H%28E%29%20%3D%20-%5Csum_%7Bi%3D1%7D%5EJp_i%5Clog_2p_i "H(E) = -\sum_{i=1}^Jp_i\log_2p_i")

### 3.2 **Random Forest**

-   Reference: **Element of Statistical Learning**
-   Random forest: decision tree --&gt; tree bagging --&gt; random forest
    -   random forests differs in one way from tree bagging. They use a modified tree learning algorithm that selects, at each candidate split in the learning process, **a random subset of the features**.
    -   the reason for doing this is the correlation of the trees in an ordinary bootstrap sample: if one or a few features are very strong predictors for the response variable, these features will be selected in many of the B trees, causing them to be correlated.
    -   Typically, for a classification problem with ![p](http://chart.apis.google.com/chart?cht=tx&chl=p "p") features, ![\\sqrt{p}](http://chart.apis.google.com/chart?cht=tx&chl=%5Csqrt%7Bp%7D "\sqrt{p}") (rounded down) features are used in each split. For regression problems, the inventors recommend ![p/3](http://chart.apis.google.com/chart?cht=tx&chl=p%2F3 "p/3") (round down) with a minimum node size of 5 as the default.
-   Algorithm
    -   From ![b=1](http://chart.apis.google.com/chart?cht=tx&chl=b%3D1 "b=1") to B
        1.  Draw a bootstrapped sample ![Z^{\\ast}](http://chart.apis.google.com/chart?cht=tx&chl=Z%5E%7B%5Cast%7D "Z^{\ast}") of size ![N](http://chart.apis.google.com/chart?cht=tx&chl=N "N") from the training set.
        2.  Grow a random-forest tree ![T\_b](http://chart.apis.google.com/chart?cht=tx&chl=T_b "T_b") to the bootstrapped data, by recursively repeating the following steps for each terminal node of the tree, until node size is reached.
            -   randomly select ![m](http://chart.apis.google.com/chart?cht=tx&chl=m "m") variables from the ![p](http://chart.apis.google.com/chart?cht=tx&chl=p "p") variables.
            -   pick the best variable/split-point among the ![m](http://chart.apis.google.com/chart?cht=tx&chl=m "m").
            -   split the node into two daughter nodes.
    -   Output the emsemble of trees ![\\{T\_1, \\ldots, T\_B\\}](http://chart.apis.google.com/chart?cht=tx&chl=%5C%7BT_1%2C%20%5Cldots%2C%20T_B%5C%7D "\{T_1, \ldots, T_B\}")
    -   Then the prediction at a new point ![x](http://chart.apis.google.com/chart?cht=tx&chl=x "x")
        -   Regression: ![\\hat{f}^B(x) = \\frac{1}{B}\\sum\_{i=1}^BT\_i(x)](http://chart.apis.google.com/chart?cht=tx&chl=%5Chat%7Bf%7D%5EB%28x%29%20%3D%20%5Cfrac%7B1%7D%7BB%7D%5Csum_%7Bi%3D1%7D%5EBT_i%28x%29 "\hat{f}^B(x) = \frac{1}{B}\sum_{i=1}^BT_i(x)")
        -   Classification: the majority vote.
-   Feature
    -   Simpler to train and tune

    -   Because of random selection of variables, the trees trained are identically distributed. This is different from boosting methods, where trees are growed in an adaptive way to reduce bias, and thus are not i.d.
    -   Out of bag sample (OOB) can play a similar role as N-fold cross-validation. For each observation ![(x\_i, y\_i)](http://chart.apis.google.com/chart?cht=tx&chl=%28x_i%2C%20y_i%29 "(x_i, y_i)"), construct the random forest predictor by averaging those trees that is built without this sample. When OOB error stabilize, the training can be terminated.
    -   OOB can measure variable importance, i.e., the prediction strength of each variable. The importance of ![j](http://chart.apis.google.com/chart?cht=tx&chl=j "j")th variable is measured by the decreased accuracy when the values of this variable are randomly permuted in the OOB samples. This process will be done for each of the trees, and resulting accuracy is averaged.

-   R package is [*randomForest*](https://cran.r-project.org/web/packages/randomForest/index.html)

### 3.3 K-Nearest Means

-   Reference: <https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm> and **Element of Statistical Learning**.
-   Algorithm
    -   calculate the pairwise distances (<span style="color:red">Euclidian distance</span> for continuous variable, <span style="color:red">hamming distance \[overlap metric\]</span> for discrete variable)
    -   for sample ![i](http://chart.apis.google.com/chart?cht=tx&chl=i "i"), get its ![k](http://chart.apis.google.com/chart?cht=tx&chl=k "k")-nearest neighbors
    -   tabulate and see the majority class label of the ![k](http://chart.apis.google.com/chart?cht=tx&chl=k "k") neighbors
    -   assign label to sample ![i](http://chart.apis.google.com/chart?cht=tx&chl=i "i").
    -   repeat step 1-step 4 until all samples are classified.
-   Features:
    -   ![k](http://chart.apis.google.com/chart?cht=tx&chl=k "k")-nearest neighbor is often successful where the decision boundary is irregular.
    -   One drawback of nearest-neighbor rules in general is the computational load, both in finding the neighbors and storing the entire training set
-   R package is [*FastKNN*](https://cran.r-project.org/web/packages/FastKNN/index.html)

### 3.4 Naive Bayes

-   Often called Bayes classifier, whose name comes from the Bayes rule
    $$ P(y|\\mathbf X) = \\frac{P(y)\\cdot P(\\mathbf X|y)}{P(\\mathbf X)} = \\frac{P(y)\\cdot P(\\mathbf X|y)}{\\sum\_{y}P(\\mathbf X, y)} $$
-   Assumptions: Features are independent of each other given label
-   *conditional independence*, ![p(x\_1, x\_2|y) = p(x\_1|y)\*p(x\_2|y)](http://chart.apis.google.com/chart?cht=tx&chl=p%28x_1%2C%20x_2%7Cy%29%20%3D%20p%28x_1%7Cy%29%2Ap%28x_2%7Cy%29 "p(x_1, x_2|y) = p(x_1|y)*p(x_2|y)"), or equivalently, ![p(x\_1|y) = p(x\_1|x\_2, y)](http://chart.apis.google.com/chart?cht=tx&chl=p%28x_1%7Cy%29%20%3D%20p%28x_1%7Cx_2%2C%20y%29 "p(x_1|y) = p(x_1|x_2, y)")
-   under this assumption, we have ![p(\\mathbf x|y) = \\prod\_{i=1}^kp(x\_i|y)](http://chart.apis.google.com/chart?cht=tx&chl=p%28%5Cmathbf%20x%7Cy%29%20%3D%20%5Cprod_%7Bi%3D1%7D%5Ekp%28x_i%7Cy%29 "p(\mathbf x|y) = \prod_{i=1}^kp(x_i|y)")
-   Bayes estimator can be used if for some specific ![x\_i](http://chart.apis.google.com/chart?cht=tx&chl=x_i "x_i"), ![p(x\_i|y)=0](http://chart.apis.google.com/chart?cht=tx&chl=p%28x_i%7Cy%29%3D0 "p(x_i|y)=0"), by maximizing posterior estimate.
-   Algorithm
    -   Given training data
    -   Learn ![P(y)](http://chart.apis.google.com/chart?cht=tx&chl=P%28y%29 "P(y)")
    -   Learn ![P(\\mathbf x|y=1), P(\\mathbf x|y=2), \\ldots, P(\\mathbf x|y=k)](http://chart.apis.google.com/chart?cht=tx&chl=P%28%5Cmathbf%20x%7Cy%3D1%29%2C%20P%28%5Cmathbf%20x%7Cy%3D2%29%2C%20%5Cldots%2C%20P%28%5Cmathbf%20x%7Cy%3Dk%29 "P(\mathbf x|y=1), P(\mathbf x|y=2), \ldots, P(\mathbf x|y=k)")
    -   Compute ![P(y|\\mathbf x) = P(y)\\cdot P(\\mathbf X|y)/\\sum\_{y}P(\\mathbf X, y)](http://chart.apis.google.com/chart?cht=tx&chl=P%28y%7C%5Cmathbf%20x%29%20%3D%20P%28y%29%5Ccdot%20P%28%5Cmathbf%20X%7Cy%29%2F%5Csum_%7By%7DP%28%5Cmathbf%20X%2C%20y%29 "P(y|\mathbf x) = P(y)\cdot P(\mathbf X|y)/\sum_{y}P(\mathbf X, y)")
    -   Predict with decision theory or use ![\\arg \\max\_YP(y|\\mathbf x)](http://chart.apis.google.com/chart?cht=tx&chl=%5Carg%20%5Cmax_YP%28y%7C%5Cmathbf%20x%29 "\arg \max_YP(y|\mathbf x)")

### 3.5 Emsemble Methods

-   Bagging stands for **B**ootstrapped **Agg**regat**ing**
    -   A learning algorithm is unstable if small changes in the training data can produce large changes in the output hypothesis. ![\\rightarrow](http://chart.apis.google.com/chart?cht=tx&chl=%5Crightarrow "\rightarrow") high variance.
    -   Bagging have little benefits when used with stable learning algorithm.
    -   Bagging works best for high variance and low bias algorithm.
-   Boosting
    -   looking at errors from previous classifiers to decide what to focus on the next iteration over data.
    -   more weights on 'hard' examples --- those we have committed mistakes in the previous iteration.
-   Adaboost algorithm
    -   input
        -   Learn - Base learning algorithm
        -   S --- set of N labeled training examples.
    -   output
        -   ![H = \[H\_1, \\cdots, H\_L, weighted~votes~(\\alpha\_1, \\ldots, \\alpha\_L)\]](http://chart.apis.google.com/chart?cht=tx&chl=H%20%3D%20%5BH_1%2C%20%5Ccdots%2C%20H_L%2C%20weighted~votes~%28%5Calpha_1%2C%20%5Cldots%2C%20%5Calpha_L%29%5D "H = [H_1, \cdots, H_L, weighted~votes~(\alpha_1, \ldots, \alpha_L)]")
    -   Steps: Let ![D\_l](http://chart.apis.google.com/chart?cht=tx&chl=D_l "D_l") be the distribution of round ![l](http://chart.apis.google.com/chart?cht=tx&chl=l "l") for the training set.
        -   initialize ![D\_1(i) = 1/N](http://chart.apis.google.com/chart?cht=tx&chl=D_1%28i%29%20%3D%201%2FN "D_1(i) = 1/N") (i.e., uniform distribution)
        -   For ![l = 1, \\ldots, L](http://chart.apis.google.com/chart?cht=tx&chl=l%20%3D%201%2C%20%5Cldots%2C%20L "l = 1, \ldots, L"), Do
            -   ![h\_l = learn (S, D\_l)](http://chart.apis.google.com/chart?cht=tx&chl=h_l%20%3D%20learn%20%28S%2C%20D_l%29 "h_l = learn (S, D_l)")
            -   ![\\epsilon\_l = error(h\_l, S, D\_l)](http://chart.apis.google.com/chart?cht=tx&chl=%5Cepsilon_l%20%3D%20error%28h_l%2C%20S%2C%20D_l%29 "\epsilon_l = error(h_l, S, D_l)")
            -   ![\\alpha\_l = \\frac{1}{2}\\ln\\frac{1-\\epsilon\_l}{\\epsilon\_l}](http://chart.apis.google.com/chart?cht=tx&chl=%5Calpha_l%20%3D%20%5Cfrac%7B1%7D%7B2%7D%5Cln%5Cfrac%7B1-%5Cepsilon_l%7D%7B%5Cepsilon_l%7D "\alpha_l = \frac{1}{2}\ln\frac{1-\epsilon_l}{\epsilon_l}")
            -   ![D\_{l+1}(i) = D\_l(i)\\times e^{\\alpha\_i}](http://chart.apis.google.com/chart?cht=tx&chl=D_%7Bl%2B1%7D%28i%29%20%3D%20D_l%28i%29%5Ctimes%20e%5E%7B%5Calpha_i%7D "D_{l+1}(i) = D_l(i)\times e^{\alpha_i}") if ![h\_l(x\_i) \\neq y\_i](http://chart.apis.google.com/chart?cht=tx&chl=h_l%28x_i%29%20%5Cneq%20y_i "h_l(x_i) \neq y_i"), else ![D\_{l+1}(i) = D\_l(i)\\times e^{-\\alpha\_i}](http://chart.apis.google.com/chart?cht=tx&chl=D_%7Bl%2B1%7D%28i%29%20%3D%20D_l%28i%29%5Ctimes%20e%5E%7B-%5Calpha_i%7D "D_{l+1}(i) = D_l(i)\times e^{-\alpha_i}")
    -   Note that ![\\epsilon\_l &lt; 0.5](http://chart.apis.google.com/chart?cht=tx&chl=%5Cepsilon_l%20%3C%200.5 "\epsilon_l < 0.5") implies ![\\alpha\_l &gt; 0](http://chart.apis.google.com/chart?cht=tx&chl=%5Calpha_l%20%3E%200 "\alpha_l > 0"), which means weight decreases for correct examples.
    -   Feature:
        -   Boosting is often, though not always, robust to overfitting (Schapire 1989).
        -   Test error continues to decrease even after training error goes to 0.
        -   Sensitive to noise and outliers, as compared to bagging that is robust to outliers.

### 3.6 Support Vector Machine

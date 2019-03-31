## fastNaiveBayes 1.1.1

### New Features
- Detect distribution. Automatically determine the distributions of a matrix for use with 
  mixed Naive Bayes model
- A threshold for the standard deviation for the Gaussian event model. This way one can ensure 
  that probabilities are real numbers and not NaN's due to standard deviation being 0.

### Bug Fixes

### Other Changes
- Expanded unit tests.
- Changed comparison to other packages in vignette
- small change to bernoulli predict function

## fastNaiveBayes 1.0.1

### New Features
-
### Bug Fixes
- Fixed bug in Gaussian predict function.

### Other Changes
- Changed Readme
- Changed description
- Added unit tests and Travis-ci

## fastNaiveBayes 1.0.0

Initial Release of package

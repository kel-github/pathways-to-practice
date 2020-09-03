# Attaining practice parameters for each participant

We sought to determine whether the relationship between practice trial and RT, for each participant and condition, was best described using a [power function](https://www.semanticscholar.org/paper/Mechanisms-of-Skill-Acquisition-and-the-Law-of-Newell/8f410281861aae69abb29bda90497c86ca929823) or an [exponential function](https://link.springer.com/article/10.3758/BF03212979).

Our power function was defined as:

$RT = a \cdot t^{b}$ 					(1)

where $RT$ = response time, $t$ = trial number, and $a$ and $b$ reflect the two free parameters.

Our exponential function was defined as:

$RT = a \cdot b^{t}$ 					(2)

To fit the functions, we took advantage of the relationship between the logarithm and the exponential. For the power function (1), we take the log of both sides (log base here refers to natural log, but any base would apply), to give:

$\log{RT} = \log{a} + b \cdot \log{t}$  		(3)

which, if the data does follow a power function, would give a straight line/linear relationship between the regressors and the DV.

For the exponential function, we know that the following should also result in a straight line, if there exists an exponential relationship between trial number and RT:

$\log{RT} = a +  b \cdot t$ 			(4)

Given that we can linearise the relationship between our regressors and the DV, we can estimate the $a$ and $b$ parameters for equations (3) and (4) using linear regression. We can then attain our power function parameters (1) from the model fit using equation (3) using the property that the exponential is the inverse of the logarithm, specifically:

$e^{\log{RT}} = e^{\log{a} + b \cdot \log{t}}$ 			(5)

which simplifies to:

$RT = a \cdot t^b$ which is the same as (1).

To attain the parameters for (2) we exponentiate (4) to get:

$RT = e^{a} \cdot e^{b \cdot t}$ 

We fit equations (3) and (4) separately to the data from each subject and condition using the lm() function in R. For each fit we computed the root mean square error (RMSE), and summed this across all model fits. The total RMSE was lower for the power function than for the exponential function for the multitasking data (pwr: 7852.74 vs exp: 8604.87) and for the visual search data (pwr: 6279.7 vs exp: 6916.69), so we retain the power function as the winning model.

We then take the parameters estimates for each subject x condition from the fitted functions for further analysis.

We were also interested describing the rate of change for each participant and condition. As the first derivative of our power function is calculated as follows:

                $\frac{dy}{dx} = a \cdot b \cdot t^{b-1}$        (6)

We computed the $a \cdot b$ and the $b-1$ terms for each participant and condition, for further analysis.


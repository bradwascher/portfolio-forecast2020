# 2020 Election Forecast Based on Approval Polls and Down-Ballot Results
This script specifies a statistical model forecasting the 2020 United States presidential election.

The forecast follows a Bayesian structure: prior information is paired with new data to arrive at posterior estimates. In this case, the prior was informed by a blend of state-level presidential job approval ratings and previous down-ballot statewide election results. These estimates were then paired with state-level polling data, gathered manually by the author throughout the campaign season, to refine the predictions further.

The model itself was coded in the R and Stan statistical languages, and draws heavily from [Linzer (2013)](https://votamatic.org/wp-content/uploads/2013/07/Linzer-JASA13.pdf), [Kremp (2016)](https://www.slate.com/features/pkremp_forecast/report.html), and particularly [Heidemanns, Gelman, and Morris (2020)](http://www.stat.columbia.edu/~gelman/research/published/hdsr_forecasting.pdf). For this reason, it's best to think of this model as an exploration of alternatives: in a world where traditional election forecasting tools have faced higher scrutiny, how well do other methods fare? That is also why it's appropriate to use [the Economist's model](https://github.com/TheEconomist/us-potus-model) (with a few key tweaks) as a baseline and point of reference.

The model's performance was tested by retroactively fitting the model to predict the outcome of the 2020 presidential contest between Donald Trump and Joe Biden in all 50 states and D.C. The forecast predicted  a 98% chance of an electoral college victory for Joe Biden, a higher mark compared to other 2020 forecasts. The model performed less admirably than standard forecasts in a few key regards: margins notwithstanding, only 43 states were “called” correctly, and a handful of states fell outside of the forecast’s credibility intervals. The below plots illustrate key themes.

### Estimates nationwide and in key states
![alt text](https://github.com/bradwascher/portfolio-forecast2020/blob/main/images/nationwide.png)

### Histogram of electoral votes
![alt text](https://github.com/bradwascher/portfolio-forecast2020/blob/main/images/ev%20histogram.png)

### Predictions vs actual results
![alt text](https://github.com/bradwascher/portfolio-forecast2020/blob/main/images/predictions%20vs%20real.png)

### Difference between polls and national vote
![alt text](https://github.com/bradwascher/portfolio-forecast2020/blob/main/images/difference%20between%20polls%20and%20national%20vote.png)

### Bias by pollster
![alt text](https://github.com/bradwascher/portfolio-forecast2020/blob/main/images/bias%20by%20pollster.png)

### Bias by mode
![alt text](https://github.com/bradwascher/portfolio-forecast2020/blob/main/images/bias%20by%20mode.png)

### Bias by sample space
![alt text](https://github.com/bradwascher/portfolio-forecast2020/blob/main/images/bias%20by%20sample%20space.png)

### Brier scores
![alt text](https://github.com/bradwascher/portfolio-forecast2020/blob/main/images/brier%20scores.PNG)

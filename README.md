# forfunforecast2020
This script specifies a statistical model forecasting the 2020 United States presidential election.

The forecast follows a Bayesian structure: prior information is paired with new data to arrive at posterior estimates. In this case, the prior was informed by a blend of state-level presidential job approval ratings and previous down-ballot statewide election results. These estimates were then paired with state-level polling data, gathered manually by the author throughout the campaign season, to refine the predictions further.

The model itself was coded in the R and Stan statistical languages, and draws heavily from Linzer (2013), Kremp (2016), and particularly Heidemanns, Gelman, and Morris (2020). For this reason, it's best to think of this model as an exploration of alternatives: in a world where traditional election forecasting tools have faced higher scrutiny, how well do other methods fare? That is also why it's appropriate to use the Economist's model (with a few key tweaks) as a baseline and point of reference.

The model's performance was tested by retroactively fitting the model to predict the outcome of the 2020 presidential contest between Donald Trump and Joe Biden in all 50 states and D.C. The forecast predicted  a 98% chance of an electoral college victory for Joe Biden, a higher mark compared to other 2020 forecasts. The model performed less admirably than standard forecasts in a few key regards: margins notwithstanding, only 43 states were “called” correctly, and a handful of states fell outside of the forecast’s credibility intervals. The below plots illustrate key themes.

![nationwide](https://github.com/bradwascher/forfunforecast2020/assets/38922214/ebc35dca-6a0f-4f85-bbf2-9d34b6a291d3)

![ev histogram](https://github.com/bradwascher/forfunforecast2020/assets/38922214/2d65f194-cf1f-4d25-8202-84bce87e6d05)

![predictions vs real](https://github.com/bradwascher/forfunforecast2020/assets/38922214/62ed604f-6e78-4746-bd18-358892d3dec1)

![difference between polls and national vote](https://github.com/bradwascher/forfunforecast2020/assets/38922214/41e3deaa-6d94-4e1f-a438-a30bc96906fe)

![bias by pollster](https://github.com/bradwascher/forfunforecast2020/assets/38922214/8a1af5df-bd5c-482a-b095-4cb36758316a)

![bias by mode](https://github.com/bradwascher/forfunforecast2020/assets/38922214/376e1e0d-24ba-451c-9553-a256758b2d7b)

![bias by sample space](https://github.com/bradwascher/forfunforecast2020/assets/38922214/4b9f7793-cbe9-4d8e-84f8-bf2c26ca1232)

![brier scores](https://github.com/bradwascher/forfunforecast2020/assets/38922214/5ae53d88-d415-4e1f-b509-4520f24b96c0)


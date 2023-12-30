# NBA-RANKINGS-2024

The model is a game-level classier that predicts the probability of either team winning the match-up based on historical box score stats. No assumptions are used to make predictions; the only judgment applied is the choices made for feature engineering and modeling framework.

What Does the Model Predict?
-----------

The model predicts the probability of either team in the match-up winning. It does not predict points scored.

How the Model Works
-----------

The model is based on a two-step procedure:

- Create PCAs based on box score statistics and player height. The PCAs capture player characteristics and are based on normalized features, such as three-point tendency, share of rebounds, true shooting percentage, etc.

- The PCAs are based on data leading up to the game. No data from the game itself is used, except for injury status.

- Train a shallow xgBoost model using the PCAs and a home court indicator. The model is trained on data since the 2014-2015 season. One-game-ahead in-sample AUC IS 0.73 and the one-game-ahead out-of-sample AUC is 0.71.

- Team performance is not included in the models. Predictions are purely based on roster composition and home-court advantage.


Benefits of this approach:

- Data engineering is simple since it relies on box score stats.
- This is a player level model — which means that it can adapt to changes to rosters, injuries, and line-ups. No need to wait and see how the team does post roster changes.
- We can also use it to simulate how roster changes can affect the outcome of a given match-up.

Downsides to this approach:

- It relies purely on box score stats. No advanced stats based on play-by-play data are included.
- The minute allocation algorithm is based on simple moving averages. In real life, coaches adapt to the opponent. 

About the code
-----------

1 - read_and_transform_data: 
The step that reads the raw data from the hoopR package, creates the features, and organizes the data for training.

2 - train_model.R:
Trains and validates the xgBoost model

3 - plot.R
Creates the scatter plot to analyze predicted versus actual team rankings.



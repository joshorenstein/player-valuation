## Player Valuation Project <br/>

###### The evaluations: <br/>
[Player Evaluations](https://github.com/joshorenstein/player-valuation/blob/master/Josh%20Orenstein%20Player%20Valuations.pdf) <br/>

###### The goal: <br/>
Use the training data to find correlations and variable importance with Run Value/100 to analyze the 5 pitchers in the test set.
* Step 1: Create some metrics from the data (e.g., first pitch strike %, zone %, perceived velocity)
* Step 2: Remove correlated predictors and do correlation analysis
* Step 3: Run random forest tree models on splits of the data (by pitch type and pitcher arm side) to find most important features to model out Run Value/100
* Step 4: Make some summary stats and do analysis <br/>

###### The scripts: <br/>
* Download and process data <br/>
* Feature selection removing correlated predictors and finding variable importance of predictors <br/>
* Create summary stats  <br/>
* Some random queries of the data <br/>

###### Visuals: <br/>
Tableau was used to sketch some visuals of the data. Here are some examples  <br/>
[Release Point](https://github.com/joshorenstein/player-valuation/blob/master/dashboard-images/release-point.PNG) <br/>
[Plate Location](https://github.com/joshorenstein/player-valuation/blob/master/dashboard-images/plate-location.PNG) <br/>
[Pitch Trajectory](https://github.com/joshorenstein/player-valuation/blob/master/dashboard-images/pitch-trajectory.PNG) <br/>

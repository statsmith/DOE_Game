# DOE Game
The objective of this web app is to provide a simple, quick, risk-free way for researchers to get comfortable with their Design of Experiments (DOE) strategy and process.

# Background
The famous statistician Ronald Fisher is credited with saying the best time to design an experiment is after you have done it.  This seems to make us uncomfortable!  We want certainty, and we want it before we spend resources.  Questions such as "Did I create the right design?", "How do I know if there is curvature?", "Do I have enough power?", and so on frequently precede experiments.  In addition, we seem to want all of the answers after one experiment, when in fact DOE is best executed sequentially.  Learning from smaller designs that push us towards an optimal is typically more efficient than trying to conduct one larger, comprehensive experiment.

# To Execute / Run Code
library(shiny) <br>
runGitHub("DOE_Game", "statsmith")

# Directions
When the web app starts or refreshes, a new (hidden) surface is created.  Your job is to find the optimal (maximum) value with as few runs as possible!
* Select the type of experiment you want to run (screening or response surface)
* If you pick response surface, you can choose to run the corner points, star points, or both
* You can also run the response surface design in blocks - run the corner points first, and then the star points if desired.
* Select ranges for the factors (X1, X2)
* Select the number of reps you want to run
* Click the "Experiment" button to generate data and the visual model
* Iterate - select additional points / ranges to learn more about your particular surface
* When you are ready to guess the optimal coordinates, click the "Guess Optimal" button
* Enter your "optimal" coordinates (X1, X2) and click the "Submit My Guess" button
* The true surface will be shown with the actual optimal value, your guess, and the differences!







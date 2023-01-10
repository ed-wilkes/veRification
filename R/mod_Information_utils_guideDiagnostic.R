guideDiagnostic <- function() {
  HTML(
    "<b>Data input tab:</b><br>

       <p style='margin-left: 20px'>
       <b>Data input:</b><br>
       - Using the <b>[Browse]</b> button, find and select the .csv/.xls/.xlsx file containing your data<br>
       - Indicate with the checkbox whether the first row of your data contains the column names<br>
       - <b>NB:</b> you can collapse any of the boxes within the <b>[-]</b> button in the top right of each box<br>
       <br>

       <b>Your data:</b><br>
       - You can view and search through your data once it has loaded<br>
       - Click on any rows which you wish to exclude from further analysis (this can be reversed by clicking on them again)<br>
       <br>

       <b>Column selection:</b><br>
       - Select the columns within your data as indicated<br>
       - Select the value of your outcome column that represents a 'positive' case<br>
       - Press the <b>[Fit model]</b> button when you are ready<br>
       - Note that, depending on the size of your data set, this may take a few minutes to run<br>
       </p>

    <b>Plots and analysis tab:</b><br>

       <p style='margin-left: 20px'>
       <b>Model fit:</b><br>
       - The posterior draws of the expected value of the posterior predictive distribution of the model are plotted against the input data
       <br>

      <b>Model coefficients:</b><br>
      - A summary of the posterior distributions of the model's parameters are shown<br>
      <br>

      <b>Decision curve analysis:</b><br>
      - The results of the decision curve analysis are shown with a random sample of 100 posterior draws shown in grey<br>
      <br>
       </p>

    <b>Bayesian model diagnostics tab:</b><br>
    - The MCMC traces and full posterior distributions for the model's parameters can be viewed under the <b>[Bayesian model diagnostics]</b> tab<br>
    <br>
   "
  )
}

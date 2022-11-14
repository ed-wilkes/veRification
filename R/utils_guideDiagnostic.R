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
       - Choose the type of curve you wish to produce from your data. If your 'positive' is rare, it is advised to use PR curves<br>
       - Select the value of your outcome column that represents a 'positive' case<br>
       - Press the <b>[Fit curves]</b> button when you are ready<br>
       - Note that, depending on the size of your data set, this may take a minute or two to run<br>
       </p>

    <b>Plots and analysis tab:</b><br>
    - You can view the bootstrapped curves produced from your data in the <b>[Plots and analysis]</b> tab<br>
    <br>

       <p style='margin-left: 20px'>
       <b>Bootstrapped curves:</b><br>
       - The results of a random sample of 100 of the 1,000 bootstrap (with replacement) draws of the data are shown (grey)<br>
       - The baseline curve from the total data set is also shown (blue)<br>
       - The optimum threshold is shown as a single point<br>
       <br>

      <b>Confusion matrix at optimal threshold:</b><br>
      - You can view the contigency table/2x2 table/confusion matrix produced with the optimal analyte value threshold<br>
      <br>

      <b>Bootstrapping results:</b><br>
      - The bootstrapped values for the various parameters from the chosen curve are shown with the chosen confidence interval<br>
      <br>

      <b>Diagnostic threshold:</b><br>
      - The bootstrapped distribution of optimal analyte value thresholds are shown<br>
      <br>
       </p>
   "
  )
}

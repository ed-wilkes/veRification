guideTruenessRef <- function() {
  HTML(
    "<b>Data input tab:</b><br>

        <p style='margin-left: 20px'>
        <b>Data input:</b><br>
        - Using the <b>[Browse]</b> button, find and select the .csv/.xls/.xlsx file containing your data<br>
        - Indicate with the checkbox whether the first row of your data contains the column names<br>
        - Indicate with the checkbox whether your measurand has been assayed in duplicate<br>
        - Enter the mean value determined for your reference material<br>
        - Enter the measure of variance quoted for your reference material<br>
        - Choose the measure of variation that is quoted for your reference material<br>
        - If SEM is quoted, then you MUST enter the number of observations used to calculate this value<br>
        - <b>NB:</b> you can collapse any of the boxes within the <b>[-]</b> button in the top right of each box<br>
        <br>

       <b>Your data:</b><br>
       - You can view and search through your data once it has loaded<br>
       - Click on any rows which you wish to exclude from further analysis (this can be reversed by clicking on them again)<br>
       <br>

       <b>Column selection:</b><br>
       - Select the columns within your data as indicated<br>
       <br>

       <b>Duplicate selection:</b><br>
       - This box will only appear if the duplicate analysis checkbox has been ticked<br>
       - Select the column that represents the duplicate measurement of your analyte<br>
       </p>

    <b>Plots tab:</b><br>
    - You can view the plot generated from your data within the <b>[Plots]</b> tab<br>
    <br>

        <p style='margin-left: 20px'>
       <b>Statistical modelling:</b><br>
       - The posterior distribution of the mean value is summarised<br>
       - The Bayes factor based on the posterior and prior distributions is also shown<br>
       </p>

    <b>Bayesian model results tab:</b><br>
    - The MCMC traces and full posterior distributions for each model's parameters can be viewed under the <b>[Bayesian model results]</b> tab<br>
    <br>
    "
  )
}

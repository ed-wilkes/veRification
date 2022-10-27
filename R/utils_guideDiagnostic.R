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
       <br>
       </p>

    <b>Plots and analysis tab:</b><br>
    - You can view the curve produced from your data in the <b>[Plots and analysis]</b> tab<br>
    - You can also view the contigency table/2x2 table/confusion matrix produced with the given analyte value threshold under the same tab<br>
    <br>

       <p style='margin-left: 20px'>
       <b>Summary statistics</b><br>
       - The relevant area-under-the-curve is shown<br>
       - The classification statistics (sensitivity, specificity, etc.) are shown and are calculated <i>dynamically</i> given the analyte threshold value chosen within the <b>[Diagnostic threshold]</b> box<br>
       - <b>NB:</b> If the analyte threshold value is changed within the <b>[Diagnostic threshold]</b> box, these statistics will automatically be recalculated<br>
       <br>
       </p>
   "
  )
}

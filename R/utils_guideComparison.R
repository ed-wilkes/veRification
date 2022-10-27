guideComparison <- function() {
  HTML(
    "<b>Data input tab:</b><br>

       <p style='margin-left: 20px'>
       <b>Data input:</b><br>
       - Using the <b>[Browse]</b> button, find and select the .csv/.xls/.xlsx file containing your data<br>
       - Indicate with the checkbox whether the first row of your data contains the column names<br>
       - Indicate with the checkbox whether your measurand has been assayed in duplicate<br>
       - Enter the name of your analyte (this can be left blank)<br>
       - Enter the name of the first method used to measure your analyte (this can be left blank)<br>
       - Enter the nae of the second method used to measure your analyte (this can be left blank)<br>
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
       - Select the columns that represent the duplicate measurements of your analyte<br>
       <br>

       <b>Analysis settings:</b><br>
       - Select the regression method, correlation coefficient, and statistical test you wish to use for your analysis (these MUST be selected)<br>
       - If you wish to perform a Bland-Altman analysis, select the y-axis you wish to plot (i.e., absolute vs relative (%) difference)<br>
       - Once you are ready, press the <b>[Fit model]</b> button
       - Note that the Bayesian measurement error model will indefinitely take longer to run, so please be patient<br>
       </p>

    <b>Regression analysis tab:</b><br>
    - Your data are plotted within the <b>[Regression analysis]</b> tab<br>
    - You can interact with the plot (hover, zoom, export, etc.) using the mouse<br>
    <br>

       <p style='margin-left: 20px'>
       <b>Model parameters:</b><br>
       - The parameters of the regression model fitted to your data (Passing-Bablok, Deming, etc.) and shown with the associated 95% confidence intervals in parentheses<br>
       <br>

       <b>Statistical inference:</b><br>
       - The results of your chosen statistical inferences are shown, including the value of the relevant test statistic and the associated P-value<br>
       - <b>NB:</b> The false positive rate for the frequentist tests is automatically set to 5% (0.05) - P-values below this threshold will flag red and produce a warning symbol<br>
       </p>

    <b>Bland-Altman analysis tab</b><br>
    - The Bland-Altman with your chosen y-axis is shown within the <b>[Bland-Altman analysis]</b> tab<br>
    - You can interact with the plot (hover, zoom, export, etc.) using the mouse<br>
    <br>

       <p style='margin-left: 20px'>
       <b>Summary statistics</b><br>
       - The mean, SD, and SEM of the absolute/relative differences between the two methods are shown<br>
       <br>
    "
  )
}

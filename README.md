# MultipleResponseR

R functions for munging and visualizing survey questions that allow for multiple responses.

* load SurveyMonkeyXLS - reads in a an XLS export of response data from SurveyMonkey, identifies multiple response questions, and reshapes the data to long form
* multipleResponsePlot - plots a bar graph of the survey responses from a multiple response question, categorizing and color coding responses by the respondent's answer to another question in the survey (for example, by age group or gender). Also overlays what the response dis would look like if the category factor has no effect on the responses (a chi-square test's "expected" distribution) 

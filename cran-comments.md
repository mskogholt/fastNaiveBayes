Previous comments
-----------------

### Second Comments:

New submission

Found the following (possibly) invalid DOIs: DOI:
<https://doi.org/10.3115/1067807> From: DESCRIPTION Message: Invalid DOI

The Title field should be in title case. Current version is: 'Extremely
Fast Implementation of a Naive Bayes classifier' In title case that is:
'Extremely Fast Implementation of a Naive Bayes Classifier'

The Description field contains (2003)
<doi:https://doi.org/10.3115/1067807> This implementation offers Please
enclose URLs in angle brackets (&lt;...&gt;). The Description field
contains (2003) <doi:https://doi.org/10.3115/1067807> This
implementation offers Please write DOIs as <doi:10.prefix/suffix>.

Flavor: r-devel-linux-x86\_64-debian-gcc, r-devel-windows-ix86+x86\_64
Check: DESCRIPTION meta-information, Result: NOTE Malformed Description
field: should contain one or more complete sentences.

### First Comments:

Thanks, please do not capitalize words like "Classifier" in your
Description text.

Please add more details about your package in the Description text.

If there are references describing the methods/algorithms in your
package, please add these in the Description field of your DESCRIPTION
file in the form authors (year) &lt;doi:...&gt; authors (year)
&lt;arXiv:...&gt; authors (year, ISBN:...) with no space after 'doi:',
'arXiv:' and angle brackets for auto-linking.

Should all be fixed now.

R CMD check results
-------------------

There were no ERRORs, WARNINGs, or NOTEs.

Downstream dependencies
-----------------------

Matrix:
<https://r-forge.r-project.org/R/?group_id=61&log=check_x86_64_windows&pkg=Matrix&flavor=patched>
# Help
## for maintainers
To learn more about the mechanism, incl. where schemas - the cornerstone of the data validation - are defined "physically", you can:


A) install the app and launch the documentation like so:
```
library(remotes)
install_github("eLTER-RI/validiraptor)

run_site()
```
or

B) visit the documentation at [https://elter-ri.github.io/validiraptor/](https://elter-ri.github.io/validiraptor/)

## for users

### what this app does:
- it takes the first row of a dataset you provide as *semicolon* separated values (viz. a textfile where columns are separated by ";") including column names in the first row.
- it checks the first data row = observation = instance for compliance with one of the rulesets specified in "Field Specification for data reporting" ([Peterseil & Geiger 2020](https://drive.google.com/file/d/1ud7ZKScn3k5PUW0_QvA1JzBpg9iX1UC7/view?ths=true&pli=1))
- it lists any violations of the specification
- it discloses the applied rules in a browsable tree (Button "Schema details")

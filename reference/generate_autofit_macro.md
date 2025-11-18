# Generate a macro file for autofitting

Autofitting using existing tools in flextable should be enough for most
cases. For the others, here is a VBA macro which autofits all tables
from inside MS Word. This function generates a file that can be imported
into MS Word in order to use this macro. The macro file should be
imported only once per computer.

## Usage

``` r
generate_autofit_macro()
```

## Value

Nothing, called for its side effects

## Installation

- In the `R` console, run `generate_autofit_macro()` to generate the
  file `crosstable_autofit.bas` in your working directory.

- In MS Word, press Alt+F11 to open the VB Editor.

- In the Editor, go to `File` \> `Import` or press `Ctrl+M` to open the
  import dialog, and import `crosstable_autofit.bas`. There should now
  be a "CrosstableMacros" module in the "Normal" project.

- Run the macro, either from the VB Editor or from `View` \> `Macros` \>
  `View Macros` \> `Run`.

This process will make the macro accessible from any Word file on this
computer. Note that, in the Editor, you can also drag the module to your
document project to make the macro accessible only from this file. The
file will have to be named with the `docm` extension though.

## Author

Dan Chaltiel

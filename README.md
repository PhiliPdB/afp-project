# Typed Spreadsheet

This is a project in Haskell for the Advanced Functional Programming course given in the Computing Science master at Utrecht University. Here, we created a library for typed spreadsheets. This library solves some problems one can encounter when using spreadsheets, such as running formulas on missing values or on values of the wrong type.

For this, we created a spreadsheet datatype containing typed columns, and we also created a typed formula language, to ensure type safety. The library can import and export CSV files. Examples of how to use the library can be found in the `Demo` module in the Demo branch.

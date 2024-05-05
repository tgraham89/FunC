# FunC
FunC implementation for PLT Spring 2024

# Authors
Brendan Fay, Daniel Lee, Jesse Chong, Cecilia Shen, Syed Muhammad Raza, Thomas Graham

# How To Run For Hello World
- Run "make setup"
- Run "make hello_world"

# Utility Commands
- make build (builds ast, scanner, parser, and semant)
- make scanner
- make parser
- make semant
- make 
- make setup (setup dev env for ocaml)

# Unit Testing Commands
- There are unit tests available for each layer of FunC (ast, scanner, parser, sast, semant, irgen... as well as e2e testing).
- Each unit test file has documentation on what the test actually does. It is at the top of the file.
- The e2e testing is unique because it exists as a script within the make_file, and not as a ocaml file.
  - More details on the e2e testing is inside the make file as comments.
- To run all of the unit tests: `make unit_tests`
- To run UTs on a specific part of FunC (`make unit_tests` will run all of these from top-to-bottom):
  - `make unit_tests_ast`
  - `make unit_tests_scanner`
  - `make unit_tests_parser`
  - `make unit_tests_sast`
  - `make unit_tests_semant`
  - `make unit_tests_irgen`
  - `make unit_tests_e2e`


# Other Test Commands
- make hello_world (makes and semantically checks the hello_world.tb file)
- make for_loop (runs the for_loop semantic check test)

# Dependencies
- ocaml
- ocamlbuild

# Status
We have a working scanner, parser, and semantics.ml for our hello_world.tb. This can be run using the "make hello_world" command.

We have tests case for the ast "unit_tests_ast.ml" and preliminary test cases for the scanner "unit_tests_scanner.ml". This can be run using the "make unit_tests" command.

You can run all tests using the above commands. The output will show certain warnings. We will fix these
prior to the final submission.

We still need to implement structs, unary operators, scope resolution, and other key features. We also need
to implement the conversion to IR. We plan to complete these by the deadline.






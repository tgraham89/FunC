# FunC
FunC implementation for PLT Spring 2024

# Authors
Brendan Fay, Daniel Lee, Jesse Chong, Cecilia Shen, Syed Muhammad Raza, Thomas Graham

# Installing FunC

- Clone the FunC git repository

> git clone https://github.com/tgraham89/FunC.git/

- Navigate to the FunC folder

> cd FunC

- Compile the FunC compiler. This will run all unit tests as well

> make
  
- To remove the generated files from compiling and running tests

> make clean    

# Running your first program

You need to have LLVM 13 or 17 installed. To compile and run a simple program:

- Compile your program

> ./func.native -l <name_of_program>.func> <name_of_program>.out         

- Run your program

> lli <name_of_program>.out

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

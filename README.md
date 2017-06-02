cpc: Curry Program Coverage tool
================================

This package contains the implementation of the tool CPC
which allows to show the coverage of Curry programs after
their execution.

After the installation of this package with

    > cpm install

the CPC executable (named `cpc`) is installed in the standard `bin`
directory of CPM. Then you can call `cpc` with various options.
To see a short description of all options, invoke

    > cpc --help

The `cpc` options are:

    -t           Print a statistical report of the given files.
    -s           Print the calculated files with span information.
    -e           Run the instrumented code and collects ticks data.
    -x           Print the collected ticks.
    -m           Print the collected meta information.
    -p           Pretty print the files with colored ticks.
    -o           Optimize ticks.
    -h           Build a HTML colored output.
    -c           Clean old data files before execution.
    --main=MAIN  Do not instrument module MAIN for program coverage

Furthermore, a list of module names must be provided. These modules
are instrumented for program coverage. The first module must contain
an operation `main` which will be executed to record and show the
coverage information. With the option `--main=MAIN`, one can
also explicitly define this main module but then it will not be instrumented
for program coverage. This is useful when you use CurryCheck
to test modules and want to show the coverage after testing.

For instance, the following command shows statistics and
the program `Rev` in pretty-printed form where expressions not executed
are colored:

    > cpc -t -c -p Rev

The next example shows the usage of CPC with CurryCheck.
This command runs CurryCheck on the modules `Nats` and `RevTest`
and generates a main test module `TEST` and keeps all intermediate files:

    > curry check -k --mainprog=TEST Nats RevTest

Now we can executed the test modules again and collect statistics
about program coverage for the test modules:

    > cpc -t -c --main=TEST Nats_PUBLIC RevTest_PUBLIC

This package contains in the directory `scripts` a simple shell script
to do this in one shot:

    > .../scripts/currycheck_cpc Nats RevTest
 
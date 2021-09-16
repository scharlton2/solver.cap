README.TXT

                                    CAP

                         Culvert Analysis Program

                     CAP - Version 97-08c  1999/11/15

Instructions for installation, execution, and testing are provided
below.  After installation, see the file doc/Cap.txt in the CAP
installation directory for summary information on CAP.

For assistance, enhancement requests, or to report bugs, contact the
Hydrologic Analysis Software Support Program by sending e-mail to
h2osoft@usgs.gov.


                          TABLE OF CONTENTS

                          A. DISTRIBUTION FILES
                          B. DOCUMENTATION
                          C. EXTRACTING FILES
                          D. INSTALLING
                          E  COMPILING
                          F. RUNNING THE PROGRAM
                          G. TESTING


A. DISTRIBUTION FILES

The following distribution packages (containing the software, test data sets,
and information files) are currently available for UNIX systems:

  Cap97.08c.DGUX.tar.gz    - Compiled for Data General AViiON under DG/UX 4.11
  Cap97.08c.Solaris.tar.gz - Compiled for Sun UltraSPARC-II under Solaris 2.6
  Cap97.08c.SGI.tar.gz     - Compiled for Silicon Graphics Indigo (32-bit)
			     under Irix 6.5
  Cap97.08c.source.tar.gz  - Source code only


B. DOCUMENTATION

Bodhaine, G.L., 1968, Measurement of peak discharges at culverts by
   indirect methods:  U.S. Geological Survey Techniques of
   Water-Resources Investigations, book 3, chap. A3, 60 p.

Fulford, J.M., 1998, User's guide to the U.S. Geological Survey Culvert 
   Analysis Program, Version 97-08:  U.S. Geological Survey
   Water-Resources Investigations Report 98-4166, 70 p.

   A Portable Document Format (PDF) file of the above report can be found
   after installation in the doc subdirectory; for other formats of the
   report, see http://water.usgs.gov/software/Cap.html.

Printed copies of most USGS publications can be obtained from:

           USGS Information Services 
           Box 25286 
           Denver Federal Center 
           Denver, CO 80225 

           electronic mail: infoservices@usgs.gov 
           Phone:  1-888-ASK-USGS


C. EXTRACTING FILES

The compressed tar file Cap97.08c.OS.tar.gz contains all the files needed
to install and test CAP on a computer with a particular operating
system, where OS is a string indicating the operating system for which
the distribution is intended.  If a version is not available for your
operating system, the file Cap97.08c.source.tar.gz contains the source
code and all other files needed to compile, install, and test the
software on a UNIX-based computer.  For either type of distribution, the
directory Cap97.08c is created (or overwritten) when the files are
extracted from the tar file.  If the Cap97.08c directory already exists,
you may want to delete or rename it before extracting the files.  

Follow the steps below to extract the files from a distribution tar file.
The steps assume that CAP will be installed under /usr/opt/wrdapp, where
the wrdapp directory may be a separate file system mounted at
/usr/opt/wrdapp (wrdapp stands for Water Resources Division
application).  You may install the software in another location, but you
must change each occurrence of "/usr/opt/wrdapp" in all the following
instructions to the alternate location that you choose.

  Steps in extracting files                 explanation
  ----------------------------------------  -----------------------------------
  mv Cap97.08c.____.tar.gz /usr/opt/wrdapp  If the tar file is not already in
                                            the directory where you want the
                                            distribution installed, move it
                                            there.

  cd /usr/opt/wrdapp                        If you are not in the directory
                                            where the tar file is located, go
                                            there.

  gunzip Cap97.08c.____.tar.gz              Uncompress the distribution file.

  tar -xvpof Cap97.08c.____.tar             Extract the files contained in
                                            the tar file.

This creates the following directory structure (the contents of each
directory are shown to the right):

   Cap97.08c           file NOTICE.TXT and this README.TXT file
     `-----bin         compiled executable
     `-----doc         documentation files (see file Contents.txt)
     `-----src         Makefile and source code
     `-----test        scripts to run verification tests
     `-----data        standard data sets used in verification tests

Notes:  a) The bin subdirectory is not included in the
           Cap97.08c.source.tar.gz distribution; it is created during
           compilation.
        b) Source code is included only with the Cap97.08c.source.tar.gz 
           distribution.
        c) It is recommended that no user files be kept in the Cap97.08c
           directory structure.  If you plan to put files in the Cap97.08c
           directory structure, do so only by creating subdirectories.


D. INSTALLING

To make the executable (Cap) easy to use, a link to the executable
should be placed in a directory that is included in each user's search
path.  To do so, change to the Cap97.08c/src directory and use the
following command:

    make install BINDIR=directory_for_links

A link to the executable will be placed in the directory assigned to
BINDIR.  For example, if each user's search path consists of

    /usr/bin:/usr/opt/bin:/usr/local/bin

you can use the command

    make install BINDIR=/usr/local/bin

to make the executable accessible from any directory without requiring
the full pathname of the software's location.  Note that to create and
delete links to the CAP executable, the installer must have sufficient
rights in the directory assigned to BINDIR.


E. COMPILING

If a compiled version of the software is not available for your computer,
or if you want to build the executable yourself, follow the instructions
in this section.

The source code is provided in the Cap97.08c.source.tar.gz distribution
so that users can generate the executable themselves.  Little or no
support can be provided for users generating their own versions of the
software.  In general, to compile a new version of the software, you will
need:

        a) a Fortran compiler,
        b) familiarity with make, the compiler, and the UNIX 
           operating system.

As provided in the Cap97.08c.source.tar.gz distribution, the Makefile and
source code are set up for use under Solaris.

To generate a new executable, do the following:

1.  Change directory to the source directory:
        cd Cap97.08c/src

2.  Modify the beginning of the file named Makefile to correctly specify 
    system-dependent variables:
     
        F77          Fortran compiler name
        FFLAGS       Fortran compiler flags
 
3.  Use the make program to initiate compilation of the source code and
    installation of the software:
        make [BINDIR=directory_for_links]

    where the optional BINDIR variable is used to specify a directory that
    is already included in each user's PATH.

    The above make command will:
        a. create the directories Cap97.08c/bin and BINDIR if they do not
           already exist,
        b. compile the source code,
        c. place the executable (Cap) in Cap97.08c/bin, and
        d. place a link to the executable in BINDIR, if specified.


F. RUNNING THE PROGRAM

After CAP is properly installed (see INSTALLING, above) in a directory
that is included in the user's PATH, the program can be executed by
typing "Cap".  CAP prompts for type of program output (detailed report or
table), output file name, run title, input file name containing culvert
geometry, the culvert section id, input file name containing approach
data, and approach cross section id.


G. TESTING

Test data sets are provided to verify that the program is correctly
installed and running on the system.  The tests may also be looked at as
examples of how to use the program.  Test are run in the Cap97.08c/test
directory.  The Cap97.08c/data directory contains the input data and the
expected results for each test.  Type the following commands to test the
program installation:

     [path]/test.sh [start [stop]]

where:  path  = path to the script
                use "." if running the tests in the Cap97.08c/test directory,
                use full pathname if running the tests from another directory;
                do _NOT_ run the tests in the Cap97.08c/data directory.
        start = the number of the first test to perform, default = 1
        stop  = the number of the last test to perform, default = 7

For example:

     command                                 what happens
     --------------------------------------  --------------------------------
     ./test.sh                               runs all tests
     ./test.sh 1 1                           runs the first test
     ./test.sh 2 3                           runs tests 2 and 3
     /usr/opt/wrdapp/Cap97.08c/test/test.sh  runs all tests


After the tests are completed, the results are compared to the expected
results (found in the Cap97.08c/data directory.)  See the file check.log; if
all goes well, there should be no differences.

To clean up after the tests, type the command:

     [path]/clean.sh

where path is as described above.

Note:  The standard data sets included with the source code in
       Cap97.08c.source.tar.gz were created on a Sun UltraSPARC-II system.
       You may notice slight numeric differences in the results on other
       computers.  These are generally due to different round-off algorithms
       and the different architecture of the central processing unit chip.
       Slight differences in output formats may occur on other computers,
       particularly for the value 0.0.

The tests are described in the table below, where 'test' is the test number
and the 'usage' column indicates how a file is used, with i for input and
o for output.

test  description of test and files                  file name & usage
----  --------------------------------------------   -----------------
  1   Mercer Creek culvert example

      CAP input data                                 mercer.dat     i
      Program output                                 mercer.out     o

  2   Pigeon House Creek culvert example

      CAP input data                                 pigeon.dat     i
      Program output                                 pigeon.out     o

  3   Rio Grande Conveyance Channel example,
      barrels 1 and 2

      CAP input data                                 riogran.dat    i
      Program output--barrel 1                       riogran1.out   o
      Program output--barrel 2                       riogran2.out   o

  4   TWRI creek culvert example based on
      examples 3 and 5 in USGS Techniques of
      Water-Resources Investigations (TWRI),
      book 3, chapter A3

      CAP input data                                 cultwri.dat    i
      Program output                                 cultwri.out    o
      Three-parameter table report                   TABLES30.DAT   o

  5   TWRI concrete pipe culvert example based
      on TWRI example 8; metric units and *CC
      records used for culvert computations

      CAP input data                                 twri8.dat      i
      Program output                                 twri8.out      o

  6   Vertical elliptical culvert section example

      CAP input data                                 vellip.dat     i
      Program output                                 vellip.out     o

  7   Vertical elliptical culvert section example
      using *CS records

      CAP input data                                 vellipcs.dat   i
      Program output                                 vellipcs.out   o

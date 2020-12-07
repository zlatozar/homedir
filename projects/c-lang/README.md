## Basic Project Structure

```
├── AUTHORS
├── COPYING
├── ChangeLog
├── Makefile.am
├── README.md
├── configure.ac
├── bin
├── docs
├── devops
│
├── libs
│   ├── lib1
│   │   ├── src
│   │   │   ├── file1.c
│   │   │   ...
│   │   │
│   │   ├── include/
│   │   │   │
│   │   │   ├── file1.h
│   │   │   ...
│   │   ├── extern/
│   │   │     │
│   │   │     ...
│   │   ├── README.md
│   │   ├── Makefile.am
│   │   └── tests
│   │       ├── Makefile.am
│   │       └── lib_test.c
│   │
│   ├── lib2
        ├ ...
...     ...
|   |
│   └── macros
│       ├── README.md
│       ├── Makefile.am
│       ├── macros.h
│       ...
│
├── reconf
└── src (core)
    ├── Makefile.am
    ├── file1.c
    └── main.c
```

```
bin/     binaries
res/     data files for compilation but not source files
libs/    libraries
log/     program logs
obj/     build object files
src/     source files
include/      header files
extern/       external common libs accessed through symbolic links
doc/          documentation
devops/       DevOps team
```

## Dependencies

### Unit Testing
[Unity](https://github.com/ThrowTheSwitch/Unity)

## Tools

1. [Doxygen](https://www.doxygen.nl/index.html)
2. [Valgrind](https://valgrind.org/)

## Books

### Basics

- _"From Hardware to Software: an introduction to computers"_ by Graham Lee
- _"Fundamentals of Operating Systems"_ by Eager

### C Language

#### Basics

- [Introduction to C](http://ix.cs.uoregon.edu/~norris/cis330books/ThinkingInC/Index.html)
- _"The C Programming Language"_ by Kernighan, Ritchie

#### C Philosophy

- [Programming Paradigms lectures from Stanford](https://www.youtube.com/watch?v=Ps8jOj7diA0&list=PL5BD86273FEF4DB0B)
- _"Pointers on C Paperback"_ by Kenneth Reek
- [C exercism](https://exercism.io/tracks/c)
- _Effective C: An Introduction to Professional C Programming"_ by Robert C. Seacord 
- _"C Interfaces and Implementations: Techniques for Creating Reusable Software"_ by David Hanson
  [source code](https://github.com/zlatozar/cii)
- _"Functional C"_ by Hartel
  [book and source](https://research.utwente.nl/en/publications/functional-c)

 
### Building Tools

- _"AUTOTOOLS. A Practitioner’s Guide to GNU Autoconf, Automake, and Libtool"_ by John Calcote

### Shell

- _"Linux & Unix Shell Programming"_ by David Tansley


### LEX&YACC

- _"Compiler Engineering Using Pascal"_ by Peter Capon and Pete Jinks 
  [source code](http://www.cs.man.ac.uk/~pjj/book.html)
- _"Introduction to Compiler Construction with Unix"_ by Axel T. Schreiner


### Domain Driven Design

- _"Domain Modeling Made Functional"_ by Scott Wlaschin
- _"Patterns, Principles, and Practices of Domain-Driven Design"_ by Scott Millett and Nick Tune


### OS

- _"The Logical Design of Operating Systems"_ by Alan C. Shaw 
- _"Understanding UNIX/LINUX Programming: A Guide to Theory and Practice"_ by Bruce Molay


### OS Advanced Level

- _"Operating Systems: A Design Oriented Approach"_ by Crowley
- _"The Linux Programming Interface: A Linux and UNIX System Programming"_ by Michael Kerrisk

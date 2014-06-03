Lightweight Modular Staging (LMS) is a runtime code generation approach. 
This framework, LMS-Core, provides a library of core components for building 
high performance code generators and embedded compilers in Scala. 

Closely related projects are [Delite](https://github.com/stanford-ppl/Delite/),
a framework for heterogeneous parallel domain specific languages (DSLs),
and [Scala-Virtualized](https://github.com/tiarkrompf/scala-virtualized/),
a set of minimal extensions to the Scala compiler to make embedding DSLs
more seamless.

### Background:

- [LMS website](http://scala-lms.github.io)

- [LMS paper](http://infoscience.epfl.ch/record/150347/files/gpce63-rompf.pdf)

- [Delite website](http://stanford-ppl.github.com/Delite/)

- [Scala-Virtualized wiki](https://github.com/TiarkRompf/scala-virtualized/wiki)


### How to build:

1. Install the [SBT](http://www.scala-sbt.org/) build tool.

2. Run `sbt test` to run the test suite.

3. Run `sbt publish-local` to install LMS-Core for use in other projects.


### Troubleshooting

If you get the following exception when running `sbt publish-local`: 

```
java.lang.OutOfMemoryError: PermGen space
Error during sbt execution: java.lang.OutOfMemoryError: PermGen space
tsf-444-wpa-1-148:lms noetzli$ export SBT_OPTS=-XX:MaxPermSize=256m
```

Use `export SBT_OPTS="-XX:MaxPermSize=256m -Xmx2048m"`.

### License:

Copyright 2010-2014, EPFL. Licensed under the Apache License, Version 2.0: http://www.apache.org/licenses/LICENSE-2.0

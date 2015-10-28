
Pandoc PlantUML Diagrams
========================

[![Build Status](https://travis-ci.org/thriqon/pandoc-plantuml-diagrams.svg?branch=master)](https://travis-ci.org/thriqon/pandoc-plantuml-diagrams)

Renders PlantUML diagrams for use with Pandoc as part
of the compilation process.

Installation
------------

This filter depends on an executable `plantuml.jar`, which can be acquired from
their homepage:

    $ cabal install pandoc-plantuml-diagrams
    $ wget http://sourceforge.net/projects/plantuml/files/plantuml.jar/download -O plantuml.jar

Usage
-----

Declare code blocks in the source markdown like:

    ```{#fig:actors .uml caption="See this!!!"}
    @startuml
    Alice -> Bob: Authentication Request
    Bob --> Alice: Authentication Response

    Alice -> Bob: Another authentication Request
    Alice <-- Bob: another authentication Response
    @enduml
    ```

and then call `pandoc` with `--filter=pandoc-plantuml-diagrams`.



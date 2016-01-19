# xesenrich

This is an [XES] log enricher, which can take several XES log formatted files and detect and inject obstacles in the main log file.

[XES]: http://www.xes-standard.org/

## Installation

xesenrich is written in [Clojure]
If you don't have it, download and install [leiningen].

Then you can clone this repository, and use lein to build a usable version of the sofware, by running this command in the wesenrich folder:

	$ lein uberjar

[Clojure]: http://clojure.org/
[leiningen]: http://leiningen.org/#install

## Usage

Xesenrich takes, for now, only the file to enrich as an argument, and only injects "on-leave" type of obstacles.

    $ java -jar xesenrich-0.1.0-standalone.jar my_log.xes

It produces a result.xml file as a result.

## Todos
### Support conversion from BPMN to CPN
- timed transition support
### Support multiple log files
- multiple execution instances, for example, provided as path to a folder.
### Configurable obstacles
- Currently only injection of a single type of obstacle is supported

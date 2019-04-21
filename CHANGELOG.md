# 1.3.17 (2019-04-21)

* RequestSquigglyContextProvider: adding hook method to get the response status code
* General: using AnyDeepName.ID instead of "**"

# 1.3.16 (2019-02-04)

* Adding abilty to use a dash in a field - issue #60
* Adding collectify/setify/listify to SquigglyUtils

# 1.3.15 (2019-01-28)

* Adding objectify methods in SquigglyUtils - issue #56

# 1.3.14 (2018-10-03)

* jar is now OSGI compliant

# 1.3.13 (2018-09-19)

* updated to servlet spec to minimum version of 3.0.1, which eliminates the need for response wrapping

# 1.3.12 (2018-09-19)

* Only apply filter from request if response code is 2xx as mentioned in issue #50

# 1.3.11 (2018-04-26)

* Fixed #39

# 1.3.10 (2018-03-15)

* Fixed ConcurrentModificationException as specified in issue #36

# 1.3.9 (2018-02-20)

* Fixed exclude filtering issue with deeply nested arrays

# 1.3.8 (2018-01-01)

* Fixed negative dot paths when multiple paths were specified

# 1.3.6 (2017-10-02)

* Added support for @JsonProperty
* Added ability to place @PropertyView on getters and setters
* Added ability to include/exclude base fields from nested objects

# 1.3.5 (2017-09-14)

* Changed dependency from antlr to antlr-runtime, which saves about 11 MB

# 1.3.4 (2017-05-01)

* Fixed NullPointerException when using @JsonView

# 1.3.3 (2017-04-28)

* Added support for @JsonUnwrapped

# 1.3.2 (2017-04-26)

* Enabled initializing multiple object mappers at once
* Allowed specifying of '[]' in addition to '{}' for nested filters because Tomcat 8 errors out if {} characters aren't escaped
* Updated spring boot example to configure all object mappers in the bean factory

# 1.3.1 (2017-04-23)

* when two fields of the same name are specified, their nested properties are merged

# 1.3.0 (2017-04-09)

* Added support regex filters
* Added support for dot syntax
* Created examples for spring boot, dropwizard, servlet, and standalone
* specifying a field that has nested fields will now implicitly include the base fields

# 1.2.0 (2017-04-02)

* ANTLR grammar used for parsing squiggly expressions
* no longer required to provide wildcard when negating properties
* improved request caching
* Java 7 now minimum required version


# 1.1.2 (2016-10-20)

* Added ability to retrieve metrics - [Issue #2](https://github.com/bohnman/squiggly-java/issues/2)
* Added ability to retrieve config and sources

# 1.1.1 (2016-10-20)

* if there are 2 exact matches, the latter is chosen.

# 1.1.0 (2016-10-20)

* Now supporting JDK6+
* Added field specificity logic  - [Issue #1](https://github.com/bohnman/squiggly-java/issues/1)
* Added ability to exclude fields

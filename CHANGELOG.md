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

* Added ability to retrieve metrics - [Issue #2](https://github.com/bohnman/squiggly-filter-jackson/issues/2)
* Added ability to retrieve config and sources

# 1.1.1 (2016-10-20)

* if there are 2 exact matches, the latter is chosen.

# 1.1.0 (2016-10-20)

* Now supporting JDK6+
* Added field specificity logic  - [Issue #1](https://github.com/bohnman/squiggly-filter-jackson/issues/1)
* Added ability to exclude fields
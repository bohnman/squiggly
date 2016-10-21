# Squiggly Filter For Jackson

## Contents

* [What is it?](#what-is-it)
* [Prerequisites](#prerequisites)
* [Installation](#installation)
* [General Usage](#general-usage)
* [Reference Object](#reference-object)
* [Top-Level Filters](#top-level-filters)
* [Nested Filters](#nested-filters)
* [Other Filters](#other-filters)
* [Resolving Conflicts](#resolving-conflicts)
* [Excluding Fields](#exluding-fields)
* [Property Views](#property-views)
* [More Examples](#more-examples)
* [Custom Integration](#custom-integration)
* [Changing the Defaults](#changing-the-defaults)
* [Metrics](#metrics)


## <a name="what-is-it"></a>What is it?

The Squiggly Filter is a [Jackson JSON](http://wiki.fasterxml.com/JacksonHome) PropertyFilter, which selects properties 
of an object/list/map using a subset of the [Facebook Graph  API filtering syntax](https://developers.facebook.com/docs/graph-api/using-graph-api/).

## <a name="prerequisites"></a>Requirements

- Java 6+
- [Commons Lang 3](https://commons.apache.org/proper/commons-lang/)
- [Google Guava](https://github.com/google/guava)
- [Jackson JSON](http://wiki.fasterxml.com/JacksonHome) (version 2.6+)

## <a name="installation"></a>Installation

### Maven

```xml
<dependency>
    <groupId>com.github.bohnman</groupId>
    <artifactId>squiggly-filter-jackson</artifactId>
    <version>1.1.2</version>
</dependency>
```

## <a name="general-usage"></a>General Usage

```java
ObjectMapper objectMapper = Squiggly.init(new ObjectMapper(), "assignee{firstName}");
Issue object = new Issue();         // replace this with your object/collection/map here
System.out.println(SquigglyUtils.stringify(objectMapper, object));
```

Alternatively, if you need more control over configuring the ObjectMapper, you can do it this way:

```java
String filterId = SquigglyPropertyFilter.FILTER_ID;
SquigglyPropertyFilter propertyFilter = new SquigglyPropertyFilter("assignee{firstName}");  // replace with your filter here
SimpleFilterProvider filterProvider = new SimpleFilterProvider().addFilter(filterId, propertyFilter);

ObjectMapper objectMapper = new ObjectMapper();
objectMapper.setFilterProvider(filterProvider);
objectMapper.addMixIn(Object.class, SquigglyPropertyFilterMixin.class);

Issue object = new Issue();         // replace this with your object/collection/map here
System.out.println(SquigglyUtils.stringify(objectMapper, object));
```

## <a name="reference-object"></a>Reference Object

For the filtering examples, let's use an the example object of type Issue

```json
{
  "id": "ISSUE-1",
  "issueSummary": "Dragons Need Fed",
  "issueDetails": "I need my dragons fed pronto.",
  "reporter": {
    "firstName": "Daenerys",
    "lastName": "Targaryen"
  },
  "assignee": {
    "firstName": "Jorah",
    "lastName": "Mormont"
  },
  "actions": [
    {
      "id": null,
      "type": "COMMENT",
      "text": "I'm going to let Daario get this one.",
      "user": {
        "firstName": "Jorah",
        "lastName": "Mormont"
      }
    },
    {
      "id": null,
      "type": "CLOSE",
      "text": "All set.",
      "user": {
        "firstName": "Daario",
        "lastName": "Naharis"
      }
    }
  ],
  "properties": {
    "priority": "1",
    "email": "motherofdragons@got.com"
  }
}
```

## <a name="top-level-filters"></a>Top-Level Filters

### Select No Fields
```java
String filter = "";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, issue));
// prints {}
```

### Select Single Field
```java
filter = "id";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, issue));
// prints {"id":"ISSUE-1"}
```

### Select Multiple Fields
```java
filter = "id,issueSummary"
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, issue));
// prints {"id":"ISSUE-1", "issueSummary":"Dragons Need Fed"}
```

### Select Fields Using Wildcards
```java
filter = "issue*";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, issue));
// prints {"issueSummary":"Dragons Need Fed", "issueDetails": "I need my dragons fed pronto."}
```

### Select All Fields
```java
filter = "**";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, issue));
// prints the same json as our example object
```

### Select All Fields of object, but only base fields of associated objects (more on this later)
```java
filter = "*";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, issue));
/* prints the following:
{
  "id": "ISSUE-1",
  "issueSummary": "Dragons Need Fed",
  "issueDetails": "I need my dragons fed pronto.",
  "reporter": {
    "firstName": "Daenerys",
    "lastName": "Targaryen"
  },
  "assignee": {
    "firstName": "Jorah",
    "lastName": "Mormont"
  },
  "actions": [
    {
      "id": null,
      "type": "COMMENT",
      "text": "I'm going to let Daario get this one.."
    },
    {
      "id": null,
      "type": "CLOSE",
      "text": "All set."
    }
  ],
  "properties": {
    "priority": "1",
    "email": "motherofdragons@got.com"
  }
}
*/
```

## <a name="nested-filters"></a>Nested Filters

### Select Single Nested Field

```java
String filter = "assignee{firstName}";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, issue));
// prints {"assignee":{"firstName":"Jorah"}}
```

### Select Multiple Nested Fields

```java
String filter = "actions{text,type}";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, issue));
// prints {"actions":[{"type":"COMMENT","text":"I'm going to let Daario get this one.."},{"type":"CLOSE","text":"All set."}]}
// NOTE: use can also use wildcards (e.g. actions{t*})
```

### Select Same Field From Different Nested Objects

```java
String filter = "assignee|reporter{firstName}";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, issue));
// prints {"reporter":{"firstName":"Daenerys"},"assignee":{"firstName":"Jorah"}}
```

### Select Deeply Nested Field

```java
String filter = "actions{user{lastName}}";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, issue));
// prints {"actions":[{"user":{"lastName":"Mormont"}},{"user":{"lastName":"Naharis"}}]}
```

## <a name="other-filters"></a>Other Filters

### Selecting from Maps

Selecting from maps is the same as selecting from objects.  Instead of selecting from fields you are selecting
from keys.  The main downside of selecting from maps that their matches are unable to be cached.

```java
Map<String, Object> map = new HashMap<>();
map.put("foo", "bar");
map.put("bear", "baz");
String filter = "foo";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, map));
// prints {"foo":"bar"}
```

### Selecting from Collections (Lists/Arrays/Etc).
Selecting from collection just assumes the top-level objects are the elements in the collection, not the collection
itself. 

```java
List<User> list = Arrays.asList(
    new User("Peter", "Dinklage"),
    new User("Lena", "Heady")
);

String filter = "firstName";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, list));
// prints [{"firstName":"Peter"}, {"firstName":"Lena"}]
```

## <a name="#resolving-conflicts"></a> Resolving Conflicts

When a filter includes two criteria that match the same field, the one that is more specific wins.

For example, if the filter is "**,reporter{firstName}", then all fields will be excluded. However, the reporter field 
will only include the firstName field.


## <a name="#exluding-fields"></a> Excluding Fields

In order to exclude fields, you need to prefix the field name with a minus sign (-).

Let's look at an example

```java
String filter = "-reporter";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, issue));
// prints {}
```

**Why didn't that work?**  Excluding fields only works if you have also included fields.  In this case,  we didn't include
any fields to begin with.

Let's try this again.

```java
String filter = "**,-reporter";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, issue));
// prints everything except the reporter field
```

That's better.  This "excluding only included fields" applies to nested filters as well.

For example, this won't work:

```java
String filter = "**,reporter{-firstName}";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, issue));
// prints everything, however the reporter object will be empty
```

But this will:

```java
String filter = "**,reporter{**,-firstName}";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, issue));
// prints everything, the reporter object will only have the firstName excluded
```

One final note.  Excluded fields can't have nested filters
```java
String filter = "**,-reporter{firstName}";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, issue));
// throws an exception
```

## <a name="property-views"></a>Property Views

In addition to selecting fields by name, you can assign a name to a group of fields.  This is called a property view.

### Reference Objects

Let's use these reference objects for the examples.

```java
@Target(FIELD)
@Retention(RUNTIME)
@Documented
@PropertyView({"super"})
public @interface SuperView {
}

class Address {
    String line1 = "55 Hollywood Blvd.";
    String line2 = "";
    String city = "Hollywood";
    String state = "CA";
    
    @SuperView
    double lat;
    
    @SuperView
    double lon;
}

class User {
    String firstName = "Peter";
    String lastName = "Dinklage";
    
    @PropertyView("secret")
    String phone = "555-555-1212";
    
    @SuperView
    Address address;

}
```

### The Base View

If nothing is annotated on a field, it is assumed to belong to the "base" view.  There is @BaseView convenience
annotation, but it's not needed.  See [Changing Defaults](#changing-the-defaults) to alter this behavior.

In the case of User, fields firstName and lastName belong to the "base" view.

```java
String filter = "base";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, user));
// prints {"firstName":"Peter","lastName","Dinklage"}
```

### Using the @PropertyView Annotation

If you look at the phone field of the User class, you'll notice the @PropertyView("secret") annotation on the phone
field.  This indicates that the phone field belongs to the "secret" view.
 
 ```java
 String filter = "secret";
 ObjectMapper mapper = Squiggly.init(mapper, filter);
 System.out.printlin(SquigglyUtils.stringify(mapper, user));
 // prints {"firstName":"Peter","lastName","Dinklage", "phone":"555-555-1212"}
  ```
  
**Wait a minute!**  Why was the firstName and lastName field included?  Even though we specified a certain view, the
base fields are always included.  See [Changing Defaults](#changing-the-defaults) to alter this behavior.

Note that you can also specifiy multiple views in the annotation - @PropertyView({"one", "two", "three"})) 

### Using a Derived Annotation

If you look at the address field of the User class, you'll notice the @SuperView annotation.  Looking at the @SuperView
declaration, you'll notice it is annotated with a @PropertyView("super").  This is how you create a derived annotation.

Let's try it out.

```java
String filter = "super";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, user));
// prints {"firstName":"Peter","lastName","Dinklage", "address":{"line1":"55 Hollywood Blvd.","line2":"","city":"Hollywood","state":"CA"}}
```
 
**Wait another minute!** The Address class has @SuperView annotations as well.  Why weren't they include?  Well, the 
view only applies to the current level.  In order to get the super views of the address, you would have to specifiy a
 filter "super{super}".  See [Changing Defaults](#changing-the-defaults) to alter this behavior.
 
## <a name="more-examples"></a>More Examples
 
There are more examples in the test directory. 
 
## <a name="custom-integration"></a>Custom Integration

Imagine you are building a webapp where you want to specify the fields on the querystring.
 
E.g. /some/path?fields=a,b{c} 

You'll notice in all of our examples, we passed in a filter expression that never changes.  This doesn't
work well for the case of specifying filters on a querystring.

Enter the SquigglyContextProvider.  This allows you to customize how to retrieve the fields.

Let's see how we might implement it for webapps.

### Generic Servlet Webapp

Squiggly Filter provides some convenience classes for integration with the Servlet API.

NOTE: the servlet API is declared as an optional dependency in this project.  You will need to specify this dependency
in your project's pom.xml.
 
In order to make this work, you need to:

1. Register the SquigglyRequestFilter
2. Register the RequestSquigglyContextProvider

E.g.

In web.xml

```xml
<filter> 
    <filter-name>squigglyFilter</filter-name>
    <filter-class>com.github.bohnman.squiggly.web.SquigglyRequestFilter</filter-class> 
</filter> 
<filter-mapping> 
    <filter-name>squigglyFilter</filter-name>
    <url-pattern>/**</url-pattern> 
</filter-mapping>
```

In the part of your code where you register the ObjectMapper that renders the JSON to the client:

```java
Squiggly.init(objectMapper, new RequestSquigglyContextProvider());

```

NOTE: These classes are here purely for convenience.  You may choose to implement this differently if you are using a 
specific framework (JEE, Spring, etc.)

### Spring Boot Web Application

The following is an example of how to use the Squiggly Filter with a Spring Boot web application:
 
 ```java
 @SpringBootApplication
 public class Application {
     @Bean
     public ObjectMapper objectMapper() {
         return Squiggly.init(new ObjectMapper(), new RequestSquigglyContextProvider());
     }
 
     @Bean
     public FilterRegistrationBean squigglyRequestFilter() {
         FilterRegistrationBean filter = new FilterRegistrationBean();
         filter.setFilter(new SquigglyRequestFilter());
         filter.setOrder(1);
         return filter;
     }
}     
 ```

### Dropwizard

The following is an example of how to use the Squiggly Filter with Dropwizard:

```java
public class HelloWorldApplication extends Application<HelloWorldConfiguration> {

    @Override
    public void run(HelloWorldConfiguration configuration, Environment environment) {
        environment.servlets()
            .addFilter("Squiggly-Filter", new SquigglyRequestFilter())
            .addMappingForUrlPatterns(EnumSet.allOf(DispatcherType.class), true, "/*");
        Squiggly.init(environment.getObjectMapper(), new RequestSquigglyContextProvider());
    }
}
```


## <a name="changing-the-defaults"></a>Changing Defaults

You have the ability to customize Squiggly by setting placing a file called squiggly.properties in your classpat.  
### Cache Config

The following properties are used to control various caches in Squiggly Filter.  Internally, these properties get 
converted to a Guava 
[CacheBuilderSpec](https://google.github.io/guava/releases/19.0/api/docs/index.html?com/google/common/cache/CacheBuilderSpec.html).
Please refer to to the documentation to see all the values that are available.

- parser.nodeCache.spec=maximumSize=10000
- filter.pathCache.spec=maximumSize=10000
- property.descriptorCache.spec=&lt;empty&gt;

### Enable/Disable adding non-annotated fields to the "base" view
- property.addNonAnnotatedFieldsToBaseView=true

### Enable/Disable implicit inclusion of base fields when specify a view
- filter.implicitlyIncludeBaseFields=true

When set to false, base fields are not included when specifying a view

### Enable/Disable View Propagation to Nested Filters
- filter.propagateViewToNestedFilters=false

When set to true, views are propagated to nested filters

## Getting Config Info

Squiggly Filter provides 2 methods to get information about configuration.

`SquigglyConfig.asMap()` will return a map of the merged config like that looks like the following:

```json
{
  "filter.implicitlyIncludeBaseFields": "true",
  "filter.pathCache.spec": "maximumSize=10000",
  "filter.propagateViewToNestedFilters": "false",
  "parser.nodeCache.spec": "maximumSize=10000",
  "property.addNonAnnotatedFieldsToBaseView": "true",
  "property.descriptorCache.spec": ""
}
```

`SquigglyConfig.asSourceMap()` will return a map of the config keys and paths where the key was retrieved like the
following:

```json
{
  "filter.implicitlyIncludeBaseFields": "file:/path/one/squiggly.default.properties",
  "filter.pathCache.spec": "file:/path/one/squiggly.default.properties",
  "filter.propagateViewToNestedFilters": "file:/path/one/squiggly.default.properties",
  "parser.nodeCache.spec": "file:/path/two/squiggly.properties",
  "property.addNonAnnotatedFieldsToBaseView": "file:/path/two/squiggly.properties",
  "property.descriptorCache.spec": "file:/path/two/squiggly.properties"
}
```


## <a name="metrics"></a>Metrics

Squiggly Filter provides an API for obtaining various metrics about the library, such as cache statistics.  This allows
users to monitor and adjust configuration as needed.

To use the metrics, you can do something the like following:

```java
Map<String, Object> metrics = SquigglyMetrics.asMap();
System.out.println(SquigglyUtils.stringify(new ObjectMapper(), metrics));
```

This will print the following:

```json
{
  "squiggly.filter.pathCache.averageLoadPenalty": 0,
  "squiggly.filter.pathCache.evictionCount": 0,
  "squiggly.filter.pathCache.hitCount": 0,
  "squiggly.filter.pathCache.hitRate": 1,
  "squiggly.filter.pathCache.loadExceptionCount": 0,
  "squiggly.filter.pathCache.loadExceptionRate": 0,
  "squiggly.filter.pathCache.loadSuccessCount": 0,
  "squiggly.filter.pathCache.missCount": 0,
  "squiggly.filter.pathCache.missRate": 0,
  "squiggly.filter.pathCache.requestCount": 0,
  "squiggly.filter.pathCache.totalLoadTime": 0,
  "squiggly.parser.nodeCache.averageLoadPenalty": 0,
  "squiggly.parser.nodeCache.evictionCount": 0,
  "squiggly.parser.nodeCache.hitCount": 0,
  "squiggly.parser.nodeCache.hitRate": 1,
  "squiggly.parser.nodeCache.loadExceptionCount": 0,
  "squiggly.parser.nodeCache.loadExceptionRate": 0,
  "squiggly.parser.nodeCache.loadSuccessCount": 0,
  "squiggly.parser.nodeCache.missCount": 0,
  "squiggly.parser.nodeCache.missRate": 0,
  "squiggly.parser.nodeCache.requestCount": 0,
  "squiggly.parser.nodeCache.totalLoadTime": 0,
  "squiggly.property.descriptorCache.averageLoadPenalty": 0,
  "squiggly.property.descriptorCache.evictionCount": 0,
  "squiggly.property.descriptorCache.hitCount": 0,
  "squiggly.property.descriptorCache.hitRate": 1,
  "squiggly.property.descriptorCache.loadExceptionCount": 0,
  "squiggly.property.descriptorCache.loadExceptionRate": 0,
  "squiggly.property.descriptorCache.loadSuccessCount": 0,
  "squiggly.property.descriptorCache.missCount": 0,
  "squiggly.property.descriptorCache.missRate": 0,
  "squiggly.property.descriptorCache.requestCount": 0,
  "squiggly.property.descriptorCache.totalLoadTime": 0
}
```
 

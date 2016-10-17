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
* [Property Views](#property-views)
* [More Examples](#more-examples)
* [Custom Integration](#custom-integration)
* [Changing the Defaults](#changing-the-defaults)


## <a name="what-is-it"></a>What is it?

The Squiggly Filter is a [Jackson JSON](http://wiki.fasterxml.com/JacksonHome) PropertyFilter, which selects
including/excluding properties of an object/list/map using a subset of the 
[Facebook Graph  API filtering syntax](https://developers.facebook.com/docs/graph-api/using-graph-api/).

## <a name="prerequisites"></a>Prerequisites

This project requires Java 8, Commons Collections 3, and Google Guava, and the Jackson JSON library.

## <a name="installation"></a>Installation

### Maven

```xml
<dependency>
    <groupId>com.github.bohnman</groupId>
    <artifactId>squiggly-filter-jackson</artifactId>
    <version>1.0.7</version>
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

For the filtering examples, let's an the example object of type Issue

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

Selecting from maps is the same as selecting from objects.  However, instead of selecting from fields you are selecting
from keys.  The main downside of selecting from maps that their lookups are unable to be cached.

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
Selecting from collection just assumes the top-level object is the element in the collection. 

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

## <a name="property-views"></a>Property Views

In addition to selecting fields by name, you can assign a name to a group of fields.  This is called a property view.

### Reference Objects

Let's use these reference objects for our base view

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
annotation, but it's not needed

In the case of User, fields firstName and lastName belong to the "base" view.

```java
String filter = "base";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, user));
// prints {"firstName":"Peter","lastName","Dinklage"}
```

### Using the @PropertyView Annottion

If you look at the phone field of the User class, you'll notice the @PropertyView("secret") annotation on the phone
field.  This indicates that the phone field belongs to the "secret" view.
 
 ```java
 String filter = "secret";
 ObjectMapper mapper = Squiggly.init(mapper, filter);
 System.out.printlin(SquigglyUtils.stringify(mapper, user));
 // prints {"firstName":"Peter","lastName","Dinklage", "phone":"555-555-1212"}
  ```
  
**Wait a minute!**  Why was the firstName and lastName field included?  Even though we specified a certain view, the
base fields are always included.

Note that you can also specifiy multiple views in the annotation - @PropertyView({"one", "two", "three"})) 

### Using a Derived Annotation

If you look at the address field of the User class, you'll notice the @SuperView annotation.  Looking at the @SuperView
declaration, you'll notice a @PropertyView("super").  This is how you create a derived annotation

Let's try it out.

```java
String filter = "super";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.printlin(SquigglyUtils.stringify(mapper, user));
// prints {"firstName":"Peter","lastName","Dinklage", "address":{"line1":"55 Hollywood Blvd.","line2":"","city":"Hollywood","state":"CA"}}
```
 
**Wait another minute!** The Address class has @SuperView annotations as well.  Why weren't they include?  Well, the 
view only applies to the current level.  In order to get the super views of the address, you would have to specifiy a
 filter "super{super}".
 
## <a name="more-examples"></a>More Examples
 
There are more examples in the test package 
 
## <a name="custom-integration"></a>Custom Integration

Imagine you are building a web server where you want to specify the fields on the querystring.
 
E.g. /some/path?fields=a|b{c} 

You'll notice in all of our examples, we passed in a filter expression that never changes.  This doesn't
work well for the case of specifying filters on a querystring.

Enter the SquigglyContextProvider.  This allows you to customize how to retrieve the fields.

Let's see how we might implement it for webapps.

### Generic Servlet Webapp

This library provides some convenience classes for working with the Servlet API.

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

In the part of your code where you register the ObjectMapper:

```java
Squiggly.init(objectMapper, new RequestSquigglyContextProvider());

```

NOTE: You may choose to implement this differently if you are using a specific framework (JEE, Spring, etc.)

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
        environment.servlets().addFilter("Squiggly-Filter", new SquigglyRequestFilter()).addMappingForUrlPatterns(EnumSet.allOf(DispatcherType.class), true, "/*");
        Squiggly.init(environment.getObjectMapper(), new RequestSquigglyContextProvider());
    }
}
```


## <a name="changing-the-defaults"></a>Changing Defaults

You have the ability to customize Squiggly by setting System properties.  These properties should be set before any 
Squiggly code is called.

### Cache Config

The following properties get converted to a Guava 
[CacheBuilderSpec](https://google.github.io/guava/releases/19.0/api/docs/index.html?com/google/common/cache/CacheBuilderSpec.html).
Please refer to to the documentation to see all the values that are available.

- parser.nodeCache.spec=maximumSize=10000
- filter.pathCache.spec=maximumSize=10000
- property.descriptorCache.spec=&lt;empty&gt;

### Enable/Disable implicit inclusion of base fields
- filter.implicitlyIncludeBaseFields=true

When set to false, base fields are not included when specifying a view

### Enable/Disable View Propagation to Nested Filters
- filter.propagateViewToNestedFilters=false

When set to true, views are propagated to nested filters

 

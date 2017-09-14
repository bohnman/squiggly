## Important Note

As of version 1.3.2, the preferred way to specify nested filters is to use square brackets intead of braces.

Preferred: `assignee[firstName]`

No longer Preferred but will still work: `assignee{firstName}`

The reason for this is that newer versions of Tomcat no longer allow braces to be specified on the url without being
escaped.  Square brackets are still permitted in the url and it is preferred to make the syntax url friendly.

# Squiggly Filter For Jackson

## Contents

* [What is it?](#what-is-it)
* [Prerequisites](#prerequisites)
* [Installation](#installation)
* [General Usage](#general-usage)
* [Reference Object](#reference-object)
* [Top-Level Filters](#top-level-filters)
* [Nested Filters](#nested-filters)
* [Dot Syntax](#dot-syntax)
* [Regex Filters](#regex-filters)
* [Other Filters](#other-filters)
* [Resolving Conflicts](#resolving-conflicts)
* [Excluding Fields](#excluding-fields)
* [Property Views](#property-views)
* [More Examples](#more-examples)
* [Custom Integration](#custom-integration)
* [Changing the Defaults](#changing-the-defaults)
* [Metrics](#metrics)


## <a name="what-is-it"></a>What is it?

The Squiggly Filter is a [Jackson JSON](http://wiki.fasterxml.com/JacksonHome) PropertyFilter, which selects properties 
of an object/list/map using a subset of the [Facebook Graph  API filtering syntax](https://developers.facebook.com/docs/graph-api/using-graph-api/).

Probably the most common use of this library is to filter fields on the querystring like so:

```
?fields=id,reporter[firstName]
```

Integrating Squiggly into your webapp is covered in [Custom Integration](#custom-integration).

## <a name="prerequisites"></a>Requirements

- Java 7+
- [ANTLR](http://www.antlr.org/)
- [Commons Lang 3](https://commons.apache.org/proper/commons-lang/)
- [Google Guava](https://github.com/google/guava)
- [Jackson JSON](http://wiki.fasterxml.com/JacksonHome) (version 2.6+)

## <a name="installation"></a>Installation

### Maven

```xml
<dependency>
    <groupId>com.github.bohnman</groupId>
    <artifactId>squiggly-filter-jackson</artifactId>
    <version>1.3.5</version>
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
SquigglyPropertyFilter propertyFilter = new SquigglyPropertyFilter("assignee[firstName]");  // replace with your filter here
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
System.out.println(SquigglyUtils.stringify(mapper, issue));
// prints {}
```

### Select Single Field
```java
filter = "id";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
// prints {"id":"ISSUE-1"}
```

### Select Multiple Fields
```java
filter = "id,issueSummary"
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
// prints {"id":"ISSUE-1", "issueSummary":"Dragons Need Fed"}
```

### Select Fields Using Wildcards
```java
filter = "issue*";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
// prints {"issueSummary":"Dragons Need Fed", "issueDetails": "I need my dragons fed pronto."}
```

### Select All Fields
```java
filter = "**";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
// prints the same json as our example object
```

### Select All Fields of object, but only base fields of associated objects (more on this later)
```java
filter = "*";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
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
String filter = "assignee[firstName]";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
// prints {"assignee":{"firstName":"Jorah"}}
```

### Select Multiple Nested Fields

```java
String filter = "actions[text,type]";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
// prints {"actions":[{"type":"COMMENT","text":"I'm going to let Daario get this one.."},{"type":"CLOSE","text":"All set."}]}
// NOTE: use can also use wildcards (e.g. actions{t*})
```

### Select Same Field From Different Nested Objects

```java
String filter = "(assignee,reporter)[firstName]";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
// prints {"reporter":{"firstName":"Daenerys"},"assignee":{"firstName":"Jorah"}}
```

### Select Deeply Nested Field

```java
String filter = "actions[user[lastName]]";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
// prints {"actions":[{"user":{"lastName":"Mormont"}},{"user":{"lastName":"Naharis"}}]}
```


## <a name="dot-syntax"></a>Dot Syntax

As an alternative to using the braces syntax for nested filter, you can use the dot syntax

```java
String filter = "assignee.firstName";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
// prints {"assignee":{"firstName":"Jorah"}}

```

You can exclude fields using the dot syntax.  Note that the exclusion applies to the last field.

```java
String filter = "-assignee.firstName";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
// prints {"assignee":{"lastName":"Mormont"}}
```

You can also combine the dot syntax with the nested syntax.

```java
String filter = "actions.user[firstName]";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
// prints {"actions":[{"user":{"firstName":"Jorah"}},{"user":{"firstName":"Daario"}}]}
```

One limitation is that you cannot use the | syntax with the dot syntax

```java
String filter = "(actions.user,assignee)[firstName]";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
// throws exception
```

## <a name="regex-filters"></a>Regex Filters

In addition to using wildcards, you can also use regular expressions.

Here's an example:

```java
String filter = "~iss[a-z]e.*~";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
// prints {"issueSummary":"Dragons Need Fed","issueDetails":"I need my dragons fed pronto."}
```

Notice the tildes mark the begin and of the regex pattern.

You can also specifiy a case insensitive match.

```java
String filter = "~iss[a-z]esumm.*~i";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
// prints {"issueSummary":"Dragons Need Fed"}
```

Why use tildes and not forward slashes for regular expressions?  Tildes are query string friendly and forward slashes 
are not.
  
However, you may use forward slashes if you like.

```java
String filter = "/iss[a-z]esumm.*/i";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
// prints {"issueSummary":"Dragons Need Fed"}
```

## <a name="other-filters"></a>Other Filters

### Selecting from Maps

Selecting from maps is the same as selecting from objects.  Instead of selecting from fields, you are selecting
from keys.  The main downside of selecting from maps that their matches are unable to be cached.

```java
Map<String, Object> map = new HashMap<>();
map.put("foo", "bar");
map.put("bear", "baz");
String filter = "foo";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, map));
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
System.out.println(SquigglyUtils.stringify(mapper, list));
// prints [{"firstName":"Peter"}, {"firstName":"Lena"}]
```

## <a name="resolving-conflicts"></a> Resolving Conflicts

When a filter includes two criteria that match the same field, the one that is more specific wins.

For example, if the filter is "**,reporter[firstName]", then all fields will be excluded. However, the reporter field 
will only include the firstName field.

Specificity is determined using the following logic:

- an exact name is the most specific
- a ** is the least specific
- a * is the second to least specific
- otherwise, the number of non-wildcard characters is counted, the higher the number, the more specific
- if two filters have the same specificity, the latter one is chosen


## <a name="excluding-fields"></a> Excluding Fields

In order to exclude fields, you need to prefix the field name with a minus sign (-).

Let's look at an example

```java
String filter = "-reporter";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
// prints everything except the reporter field
``` 

Here's an example excluding a nested field:

```java
String filter = "**,reporter[-firstName]";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
// prints everything, the reporter object will only have the firstName excluded
```

NOTE:  Excluded fields can't have nested filters
```java
String filter = "**,-reporter[firstName]";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, issue));
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
System.out.println(SquigglyUtils.stringify(mapper, user));
// prints {"firstName":"Peter","lastName","Dinklage"}
```

### Using the @PropertyView Annotation

If you look at the phone field of the User class, you'll notice the `@PropertyView("secret")` annotation on the phone
field.  This indicates that the phone field belongs to the "secret" view.
 
 ```java
 String filter = "secret";
 ObjectMapper mapper = Squiggly.init(mapper, filter);
 System.out.println(SquigglyUtils.stringify(mapper, user));
 // prints {"firstName":"Peter","lastName","Dinklage", "phone":"555-555-1212"}
  ```
  
**Wait a minute!**  Why was the firstName and lastName field included?  Even though we specified a certain view, the
base fields are always included.  See [Changing Defaults](#changing-the-defaults) to alter this behavior.

Note that you can also specifiy multiple views in the annotation - `@PropertyView({"one", "two", "three"}))` 

### Using a Derived Annotation

If you look at the address field of the User class, you'll notice the @SuperView annotation.  Looking at the @SuperView
declaration, you'll notice it is annotated with a @PropertyView("super").  This is how you create a derived annotation.

Let's try it out.

```java
String filter = "super";
ObjectMapper mapper = Squiggly.init(mapper, filter);
System.out.println(SquigglyUtils.stringify(mapper, user));
// prints {"firstName":"Peter","lastName","Dinklage", "address":{"line1":"55 Hollywood Blvd.","line2":"","city":"Hollywood","state":"CA"}}
```
 
**Wait another minute!** The Address class has @SuperView annotations as well.  Why weren't they include?  Well, the 
view only applies to the current level.  In order to get the super views of the address, you would have to specifiy a
 filter "super[super]".  See [Changing Defaults](#changing-the-defaults) to alter this behavior.
 
## <a name="more-examples"></a>More Examples
 
There are more examples in the test directory. 
 
## <a name="custom-integration"></a>Custom Integration

Imagine you are building a webapp where you want to specify the fields on the querystring.
 
E.g. `/some/path?fields=a,b{c} ``

You'll notice in all of our examples, we passed in a filter expression that never changes.  This doesn't
work well for the case of specifying filters on a querystring.

Enter the SquigglyContextProvider.  This interface allows you to customize how to retrieve the fields.

### The RequestSquigglyContextProvider

All servlet-based integrations use the RequestSquigglyContextProvider, which has the general initialization in the form of:

```java
Squiggly.init(objectMapper, new RequestSquigglyContextProvider());
```

### Automatically wrapping fields with an outer filter

Let's say you have the following class called Page that looks like this:

```java
public class Page<T> {

    private final int pageNumber;
    private final int pageSize;
    private final List<T> items;

    public ListResponse(List<T> items, int pageNumber, int pageSize) {
        this.items = checkNotNull(items);
        this.pageNumber = pageNumber;
        this.pageSize = pageSize;
    }

    public List<T> getItems() {
        return items;
    }
    
    public int getPageNumber() {
        return pageNumber;
    }
    
    public int getPageSize() {
        return pageSize;
    }
}
```

Let's say you have an endpoint called /issues that looks like this:

```java
public Page<Issue> findIssues(String query, int pageNumber, int pageSize) {
    List<Issue> issues = issueService.findIssues(query, pageNumber, pageSize);
    return new Page<Issue>(issues, pageNumber, pageSize);
}
```

In order to get specify the issue property, you now have to wrap all filters with items[] like so:
 
```
GET /issues?fields=items{id}&query=some-query&&pageNumber=1&pageSize=10
``` 

This is kind of annoying.  Fortunately, we can avoid this inconvenience by using a hook method in RequestSquigglyContextProvider.

Here's how it would look:

```java
Squiggly.init(objectMapper, new RequestSquigglyContextProvider() {
    @Override
    protected String customizeFilter(String filter, HttpServletRequest request, Class beanClass) {
        if (filter != null && Page.class.isAssignableFrom(beanClass)) {
            filter = "items[" + filter + "]";
        }

        return filter;
    }
});
```

Now you can do this:

```
GET /issues?fields=id&query=some-query&&pageNumber=1&pageSize=10
```

### Generic Servlet Webapp

You can find an example of using Squiggly Filter in a webapp under the [examples/servlet](examples/servlet) directory.

### Spring Boot Web Application

You can find an example of using Squiggly Filter in Spring Boot under the [examples/spring-boot](examples/spring-boot) directory.

### Dropwizard

You can find an example of using Squiggly Filter in Dropwizard under the [examples/dropwizard](examples/dropwizard) directory.


## <a name="changing-the-defaults"></a>Changing Defaults

You have the ability to customize Squiggly by creating a file called squiggly.properties in the root of the classpath.  
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

`SquigglyConfig.asMap()` will return a map of the merged config that looks like the following:

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

`SquigglyConfig.asSourceMap()` will return a map of the config keys and paths where the entry was retrieved  that looks 
like the following:

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
 

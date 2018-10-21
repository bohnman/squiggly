# Squiggly Filter Jackson Spring Data Rest Example

This example shows how to use Squiggly Filter with Spring Data Rest.


```bash
mvn spring-boot:run
```

In another terminal, you can request the issue json by doing the following:

To get the raw JSON
```bash
curl -s -g 'http://localhost:8080/people'
```

To filter using Squiggly

```bash
curl -s -g 'http://localhost:8080/people?fields=_embedded.people.firstName'
```
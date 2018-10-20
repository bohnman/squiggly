# Squiggly Filter Jackson Servlet Example

This example shows how to use Squiggly Filter in a servlet environment.

This example uses the Issue object described in the main documentation.

To run the example, cd to the examples/servlet directory on the command line type the following:

```bash
mvn jetty:run
```

In another terminal, you can request the issue json by doing the following:

1) To print the raw json
```bash
curl -s -g 'http://localhost:8080/issues'
```

2) To filter the raw json
```bash
curl -s -G 'http://localhost:8080/issues' --data-urlencode 'fields=id,assignee[firstName]'
```
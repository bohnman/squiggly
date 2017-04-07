# Squiggly Filter Jackson Standalone Example

This example shows how to use Squiggly Filter in plain vanilla Java.

This example uses the Issue object described in the main documentation.

To run the example, cd to the examples/standalone directory on the command line type the following:

1) To print the raw json
```bash
mvn compile exec:java
```

2) To filter the raw json
```bash
mvn compile exec:java -Dexec.args='id,issueSummary'
```
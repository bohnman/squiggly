# Squiggly Filter Jackson Spring Data JPA Hibernate Example

This example shows how to use Squiggly Filter  with Spring Data JPA (Hibernate implementation).
Specifically, this examples shows how to ignore lazy collections by default unless specified by Squiggly. 


```bash
mvn spring-boot:run
```

In another terminal, you can request the issue json by doing the following:

To get the raw JSON
```bash
curl -s -g 'http://localhost:8080/hotel'
```

You'll notice that the reviews property is null.  This is because the jackson datatype Hibernate 5 module sets lazy
loaded properties to null by default.

To load the reviews using Squiggly, you just have to include in the fields filter

```bash
curl -s -g 'http://localhost:8080/hotel?fields=reviews.rating'
```

You'll notice on the ``SampleDataJpaApplication.main`` method we override the ``serializeAsIncludedField`` method on the context provide which tells 
lazy collections to initialize if they are included within a Squiggly filter.
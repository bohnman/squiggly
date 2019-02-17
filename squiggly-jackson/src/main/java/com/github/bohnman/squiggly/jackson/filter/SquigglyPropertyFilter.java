package com.github.bohnman.squiggly.jackson.filter;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonStreamContext;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.BeanPropertyWriter;
import com.fasterxml.jackson.databind.ser.PropertyWriter;
import com.fasterxml.jackson.databind.ser.impl.SimpleBeanPropertyFilter;
import com.fasterxml.jackson.databind.ser.std.MapProperty;
import com.github.bohnman.core.json.path.CoreJsonPath;
import com.github.bohnman.core.json.path.CoreJsonPathElement;
import com.github.bohnman.squiggly.core.context.SquigglyContext;
import com.github.bohnman.squiggly.core.function.invoke.SquigglyFunctionInvoker;
import com.github.bohnman.squiggly.core.parser.node.ExpressionNode;
import com.github.bohnman.squiggly.core.parser.node.FilterNode;
import com.github.bohnman.squiggly.core.parser.node.StatementNode;
import com.github.bohnman.squiggly.jackson.Squiggly;

import javax.annotation.Nullable;
import javax.annotation.concurrent.ThreadSafe;
import java.io.IOException;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static com.github.bohnman.core.lang.CoreAssert.notNull;
import static com.github.bohnman.squiggly.core.match.SquigglyExpressionMatcher.ALWAYS_MATCH;
import static com.github.bohnman.squiggly.core.match.SquigglyExpressionMatcher.NEVER_MATCH;


/**
 * A Jackson @{@link com.fasterxml.jackson.databind.ser.PropertyFilter} that filters objects using squiggly syntax.
 * <p>Here are some examples of squiggly syntax:</p>
 * <pre>
 *    // grab the id and name fields
 *    id,name
 *
 *    // grab the id and nested first name and last name from a the user property
 *    id,user{firstName,lastName}
 *
 *    // grab the full object graph
 *    **
 *
 *    // grab just the base fields
 *    base
 *
 *    // grab all fields of the current object, but just the base fields of nested objects
 *    *
 *
 *    // grab fields starting with eco
 *    eco*
 *
 *    // grab fields ending with Time
 *    *Time
 *
 *    // grab fields containing Weight
 *    *Weight*
 *
 *    // grab the firstName field of the nested employee and manager objects
 *    employee{firstName},manager{firstName}
 *    employee|manager{firstName}
 *
 *    // grab all fields annotated with @PropertyView("hardware") or a derived annotation
 *    hardware
 * </pre>
 */
@SuppressWarnings("unchecked")
@ThreadSafe
public class SquigglyPropertyFilter extends SimpleBeanPropertyFilter {

    public static final String FILTER_ID = "squigglyFilter";

    private final Squiggly squiggly;

    /**
     * Constructor.
     *
     * @param squiggly squiggly
     */
    public SquigglyPropertyFilter(Squiggly squiggly) {
        this.squiggly = notNull(squiggly);
    }

    @Override
    protected boolean include(final BeanPropertyWriter writer) {
        throw new UnsupportedOperationException("Cannot call include without JsonGenerator");
    }


    @Override
    protected boolean include(final PropertyWriter writer) {
        throw new UnsupportedOperationException("Cannot call include without JsonGenerator");
    }


    @Override
    public void serializeAsField(final Object pojo, JsonGenerator jgen, final SerializerProvider provider,
                                 final PropertyWriter writer) throws Exception {


        ExpressionNode match = match(writer, jgen);

        SquigglyFunctionInvoker functionInvoker = squiggly.getFunctionInvoker();

        if (match != null && match != NEVER_MATCH) {
            if (match.getKeyFunctions().isEmpty() && match.getValueFunctions().isEmpty()) {
                squiggly.getSerializer().serializeAsIncludedField(pojo, jgen, provider, writer);
            } else if (writer instanceof BeanPropertyWriter) {
                BeanPropertyWriter beanPropertyWriter = (BeanPropertyWriter) writer;
                String name = "" + functionInvoker.invoke(writer.getName(), writer.getName(), pojo, match.getKeyFunctions());
                Object beanValue = beanPropertyWriter.get(pojo);
                Object value = functionInvoker.invoke(beanValue, beanValue, pojo, match.getValueFunctions());
                squiggly.getSerializer().serializeAsConvertedField(pojo, jgen, provider, writer, name, value);
            } else if (writer instanceof MapProperty) {
                MapProperty mapProperty = (MapProperty) writer;
                String name = "" + functionInvoker.invoke(writer.getName(), writer.getName(), pojo, match.getKeyFunctions());
                Object value = functionInvoker.invoke(pojo, pojo, pojo, match.getValueFunctions());
                squiggly.getSerializer().serializeAsConvertedField(pojo, jgen, provider, writer, name, value);
            } else {
                squiggly.getSerializer().serializeAsIncludedField(pojo, jgen, provider, writer);
            }
        } else if (!jgen.canOmitFields()) {
            squiggly.getSerializer().serializeAsExcludedField(pojo, jgen, provider, writer);
        }
    }

    private ExpressionNode match(final PropertyWriter writer, final JsonGenerator jgen) {
        JsonStreamContext streamContext = getStreamContext(jgen);

        if (streamContext == null) {
            return ALWAYS_MATCH;
        }

        if (!squiggly.getContextProvider().isFilteringEnabled()) {
            return ALWAYS_MATCH;
        }

        CoreJsonPath path = getPath(writer, streamContext);
        SquigglyContext context = squiggly.getContextProvider().getContext(path.getFirst().getBeanClass(), squiggly);

        String filter = context.getFilter();
        FilterNode parsedFilter = context.getParsedFilter();
        List<StatementNode> statements = parsedFilter.getStatements();

        if (statements.isEmpty()) {
            return NEVER_MATCH;
        }

        if (statements.size() > 1) {
            throw new IllegalArgumentException("Currently, only a single statement is supported for property filters.");
        }

        return squiggly.getExpressionMatcher().match(path, filter, statements.get(0));
    }

    private CoreJsonPath getPath(PropertyWriter writer, JsonStreamContext sc) {
        LinkedList<CoreJsonPathElement> elements = new LinkedList<>();

        if (sc != null) {
            elements.add(new CoreJsonPathElement(writer.getName(), sc.getCurrentValue(), getValue(sc.getCurrentValue(), writer)));
            sc = sc.getParent();
        }

        while (sc != null) {
            if (sc.getCurrentName() != null && sc.getCurrentValue() != null) {
                elements.addFirst(new CoreJsonPathElement(sc.getCurrentName(), sc.getCurrentValue()));
            }
            sc = sc.getParent();
        }

        return new CoreJsonPath(elements);
    }

    private Object getValue(Object pojo, PropertyWriter writer) {
        if (writer instanceof BeanPropertyWriter) {
            try {
                return ((BeanPropertyWriter) writer).get(pojo);
            } catch (Exception e) {
                return null;
            }
        }

        if (writer instanceof MapProperty && pojo instanceof Map) {
            return ((Map) pojo).get(writer.getName());
        }

        return null;
    }

    private JsonStreamContext getStreamContext(JsonGenerator jgen) {
        return jgen.getOutputContext();
    }

    public static void main(String[] args) throws IOException {

//        Map<String, Object> map = new HashMap<>();
//        map.put("foo", "bar");

//        String filter = "name=name.firstName";
//        String filter = "nickNames@filter(::$.name.@equals('rbohn'))";
        String filter = "$=''";
        Person person = new Person("Ryan", "Bohn", 38, "rbohn", "bohnman", "doogie");
//        Person person = new Person(new Name("Ryan", "Bohn"));
//        Person person = new Person(null);
//        mapper.writeValue(System.out, Squiggly.builder().build().apply((JsonNode) mapper.valueToTree(person), filter));
        Squiggly squiggly = Squiggly.init("firstName=@o(foo:bar)");
//        Squiggly squiggly = Squiggly.init();
        ObjectMapper mapper = squiggly.apply(new ObjectMapper());
//        mapper.writeValue(System.out, person);
//        mapper.writerWithDefaultPrettyPrinter().writeValue(System.out, squiggly.apply(mapper, person, "nickNames.@map(::priority+1)"));
        mapper.writerWithDefaultPrettyPrinter().writeValue(System.out, person);
        System.err.println();
        System.err.println();
        System.err.println();
        System.err.println();
        System.out.println();
        System.out.println();
        System.out.println();
    }

//    public static class Person {
//        private final Name name;
//
//        public Person(Name name) {
//            this.name = name;
//        }
//
//        public Name getName() {
//            return name;
//        }
//    }
//
//    public static class Name {
//        private String firstName;
//        private String lastName;
//
//        public Name(String firstName, String lastName) {
//            this.firstName = firstName;
//            this.lastName = lastName;
//        }
//
//        public String getFirstName() {
//            return firstName;
//        }
//
//        public String getLastName() {
//            return lastName;
//        }
//    }


    public static class NickName implements Comparable<NickName> {
        private static final AtomicInteger SEQUENCE = new AtomicInteger();
        private final String name;
        private final int priority = SEQUENCE.incrementAndGet();

        public NickName(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

        @Override
        public String toString() {
            return name;
        }

        public String getChuck() {
            return "chuckie";
        }

        public int[] getNumbers() {
            return new int[]{1, 5, 9, 14, 9};
        }

        public int getPriority() {
            return priority;
        }

        @Override
        public int compareTo(@Nullable NickName o) {
            return (o == null) ? -1 : name.compareTo(o.name);
        }
    }

    public static class Person {
        private final String firstName;
        private final String lastName;
        private List<NickName> nickNames;
        private NickName firstNickName;
        //        @JsonIgnore
        private int age;

        public Person(String firstName, String lastName, int age, String... nickNames) {
            this.age = age;
            this.firstName = firstName;
            this.lastName = lastName;
            this.nickNames = Arrays.stream(nickNames).map(NickName::new).collect(Collectors.toList());
            this.firstNickName = this.nickNames.isEmpty() ? null : this.nickNames.get(0);
        }

        public int getAge() {
            return age;
        }

        public String getFirstName() {
            return firstName;
        }

        public String getLastName() {
            return lastName;
        }

        public NickName getFirstNickName() {
            return firstNickName;
        }

        public List<NickName> getNickNames() {
            return nickNames;
        }

        public String getNullProperty() {
            return null;
        }

        public int[] getNumbers() {
            return new int[]{1, 1, 5, 9, 14, 9};
        }


    }
}
